pub mod axum;
pub mod openapi;
pub mod parser;
pub mod schema;

// Re-export commonly used types
pub use openapi::{InfoBuilder, OpenApiSpec};
pub use parser::{Route, TypeInfo};

use anyhow::Result;
use std::collections::HashMap;
use std::fs;
use std::path::Path;
use walkdir::WalkDir;

/// Generate OpenAPI spec from a source directory
///
/// This is the main entry point for build.rs usage.
///
/// # Example
///
/// ```no_run
/// use openapi_build_gen::{generate_spec, InfoBuilder};
///
/// let spec = generate_spec("src")
///     .expect("Failed to generate spec")
///     .with_info(
///         InfoBuilder::new("My API", "1.0.0")
///             .description("My awesome API")
///             .build()
///     );
///
/// spec.write_json_to_file("openapi.json").expect("Failed to write spec");
/// ```
pub fn generate_spec(source_dir: impl AsRef<Path>) -> Result<OpenApiSpec> {
    let mut all_routes = Vec::new();
    let mut all_schemas = HashMap::new();

    // Walk through all Rust source files
    for entry in WalkDir::new(source_dir.as_ref())
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("rs"))
    {
        let path = entry.path();

        if let Ok((routes, schemas)) = parse_file(path) {
            all_routes.extend(routes);
            all_schemas.extend(schemas);
        }
    }

    Ok(OpenApiSpec::new("API", "1.0.0", all_routes, all_schemas))
}

/// Parse a Rust source file and extract API metadata
pub fn parse_file(path: &Path) -> Result<(Vec<parser::Route>, HashMap<String, parser::TypeInfo>)> {
    let content = fs::read_to_string(path)?;
    let syntax_tree = syn::parse_file(&content)?;

    let mut routes = Vec::new();
    let mut handlers = HashMap::new();
    let mut type_schemas = HashMap::new();

    // First pass: collect all structs with serde derives and handler functions
    for item in &syntax_tree.items {
        match item {
            syn::Item::Fn(func) => {
                let handler_name = func.sig.ident.to_string();
                let (request_type, response_type) = axum::parse_handler_function(func);
                handlers.insert(handler_name, (request_type, response_type));
            }
            syn::Item::Struct(item_struct) => {
                // Only extract structs with Serialize/Deserialize
                if schema::has_serde_derives(item_struct) {
                    let type_info = schema::extract_struct_info(item_struct);
                    type_schemas.insert(type_info.name.clone(), type_info);
                }
            }
            _ => {}
        }
    }

    // Second pass: extract routes from router functions
    for item in syntax_tree.items {
        if let syn::Item::Fn(func) = item {
            // Look for router setup functions
            if let Some(mut router_routes) = axum::extract_routes_from_function(&func) {
                // Enrich routes with handler type information
                for route in &mut router_routes {
                    if let Some((req_type, resp_type)) = handlers.get(&route.handler) {
                        route.request_body = req_type.clone();
                        route.response_body = resp_type.clone();
                    }
                }
                routes.extend(router_routes);
            }
        }
    }

    Ok((routes, type_schemas))
}
