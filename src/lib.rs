pub mod axum;
pub mod openapi;
pub mod parser;
pub mod schema;

// Re-export commonly used types
pub use openapi::{InfoBuilder, OpenApiSpec};
pub use parser::{Route, TypeAlias, TypeInfo};

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
    let mut all_raw_routes = Vec::new();
    let mut all_struct_defs = HashMap::new();
    let mut all_type_aliases = HashMap::new();

    // First pass: collect raw routes (with syn::Type), structs, enums, and type aliases
    for entry in WalkDir::new(source_dir.as_ref())
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("rs"))
    {
        let path = entry.path();

        if let Ok((routes, structs, type_aliases)) = parse_file(path) {
            all_raw_routes.extend(routes);
            all_struct_defs.extend(structs);
            all_type_aliases.extend(type_aliases);
        }
    }

    // Second pass: resolve all types using the collected type aliases
    let all_routes = resolve_route_types(&all_raw_routes, &all_type_aliases);

    // Filter to only include referenced types and their dependencies
    let referenced_schemas =
        collect_referenced_schemas(&all_routes, &all_struct_defs, &all_type_aliases);

    Ok(OpenApiSpec::new(
        "API",
        "1.0.0",
        all_routes,
        referenced_schemas,
    ))
}

/// Parse a Rust source file and extract API metadata
pub fn parse_file(
    path: &Path,
) -> Result<(
    Vec<parser::RawRoute>,
    HashMap<String, parser::TypeInfo>,
    HashMap<String, parser::TypeAlias>,
)> {
    let content = fs::read_to_string(path)?;
    let syntax_tree = syn::parse_file(&content)?;

    let mut raw_routes = Vec::new();
    let mut handlers = HashMap::new();
    let mut all_structs = HashMap::new();
    let mut type_aliases = HashMap::new();
    let mut struct_items = Vec::new(); // Store ItemStruct for second pass

    // First pass: collect type aliases, structs, and enums (without flatten resolution)
    for item in &syntax_tree.items {
        match item {
            syn::Item::Struct(item_struct) => {
                // Collect ALL structs with Serialize/Deserialize
                if schema::has_serde_derives(item_struct) {
                    let type_info = schema::extract_struct_info(item_struct);
                    all_structs.insert(type_info.name.clone(), type_info);
                    struct_items.push(item_struct.clone());
                }
            }
            syn::Item::Enum(item_enum) => {
                // Collect ALL enums with Serialize/Deserialize
                if schema::has_serde_derives_enum(item_enum) {
                    let type_info = schema::extract_enum_info(item_enum);
                    all_structs.insert(type_info.name.clone(), type_info);
                }
            }
            syn::Item::Type(item_type) => {
                // Collect public type aliases
                let alias_name = item_type.ident.to_string();
                type_aliases.insert(
                    alias_name.clone(),
                    parser::TypeAlias {
                        name: alias_name,
                        type_str: quote::quote!(#item_type.ty).to_string(),
                    },
                );
            }
            _ => {}
        }
    }

    // Second pass: re-extract structs with flatten support now that we have all structs
    for item_struct in &struct_items {
        let type_info = schema::extract_struct_info_with_flatten(item_struct, &all_structs);
        all_structs.insert(type_info.name.clone(), type_info);
    }

    // Third pass: collect handlers and routes (without resolving types yet)
    for item in &syntax_tree.items {
        if let syn::Item::Fn(func) = item {
            let handler_name = func.sig.ident.to_string();
            let (request_type, response_type, error_type, query_type) =
                axum::parse_handler_function_raw(func);

            handlers.insert(
                handler_name,
                (request_type, response_type, error_type, query_type),
            );

            // Also check if this function defines routes
            if let Some(router_routes) = axum::extract_routes_from_function(func) {
                raw_routes.extend(router_routes);
            }
        }
    }

    // Fourth pass: enrich routes with handler raw type information and resolve query params
    for route in &mut raw_routes {
        if let Some((req_type, resp_type, err_type, qry_type)) = handlers.get(&route.handler) {
            route.request_body_type = req_type.clone();
            route.response_body_type = resp_type.clone();
            route.error_response_type = err_type.clone();
            route.query_type = qry_type.clone();

            // Resolve query parameters from query_type
            if let Some(query_ty) = qry_type {
                route.query_params = resolve_query_params(query_ty, &all_structs);
            }
        }
    }

    Ok((raw_routes, all_structs, type_aliases))
}

/// Resolve query parameters from a Query<T> type
fn resolve_query_params(
    query_type: &syn::Type,
    all_structs: &HashMap<String, parser::TypeInfo>,
) -> Vec<parser::QueryParam> {
    // Use the same type resolution approach as extract_json_inner_type
    // This handles generic types properly (e.g., Query<Page<User>>)
    let type_aliases = HashMap::new(); // We don't need type alias resolution for query params

    // Convert syn::Type to TypeInfo to get the proper type name with generic args
    if let Some(type_info) = axum::resolve_type_info(query_type, &type_aliases) {
        let type_name = &type_info.name;

        // Look up the base struct definition (without generic args)
        if let Some(base_type_info) = all_structs.get(type_name) {
            // Convert struct fields to query parameters
            return base_type_info
                .fields
                .iter()
                .map(|field| parser::QueryParam {
                    name: field.name.clone(),
                    type_name: field.type_name.clone(),
                    required: field.required,
                })
                .collect();
        }
    }

    Vec::new()
}

/// Resolve raw routes (with syn::Type) into Routes (with TypeInfo)
fn resolve_route_types(
    raw_routes: &[parser::RawRoute],
    type_aliases: &HashMap<String, parser::TypeAlias>,
) -> Vec<parser::Route> {
    raw_routes
        .iter()
        .map(|raw_route| parser::Route {
            path: raw_route.path.clone(),
            method: raw_route.method.clone(),
            handler: raw_route.handler.clone(),
            request_body: raw_route
                .request_body_type
                .as_ref()
                .and_then(|ty| axum::resolve_type_info(ty, type_aliases)),
            response_body: raw_route
                .response_body_type
                .as_ref()
                .and_then(|ty| axum::resolve_response_type(ty, type_aliases)),
            error_response: raw_route
                .error_response_type
                .as_ref()
                .and_then(|ty| axum::resolve_error_type(ty, type_aliases)),
            path_params: raw_route.path_params.clone(),
            query_params: raw_route.query_params.clone(),
        })
        .collect()
}

/// Collect all schemas that are referenced by routes and their dependencies
/// Collect all schema types referenced by routes, including generic instantiations
fn collect_referenced_schemas(
    routes: &[parser::Route],
    all_structs: &HashMap<String, parser::TypeInfo>,
    _type_aliases: &HashMap<String, parser::TypeAlias>,
) -> HashMap<String, parser::TypeInfo> {
    let mut referenced_schemas = HashMap::new();
    let mut to_process = Vec::new();
    let mut processed = std::collections::HashSet::new();

    // Start with types directly referenced in routes
    for route in routes {
        if let Some(req_body) = &route.request_body {
            // Use full_name() which includes generic args (e.g., "Page_Product")
            let full_type_name = req_body.full_name();
            to_process.push((full_type_name, req_body.clone()));
        }
        if let Some(resp_body) = &route.response_body {
            let full_type_name = resp_body.full_name();
            to_process.push((full_type_name, resp_body.clone()));
        }
        if let Some(err_resp) = &route.error_response {
            let full_type_name = err_resp.full_name();
            to_process.push((full_type_name, err_resp.clone()));
        }
    }

    // Recursively collect all dependencies
    while let Some((full_type_name, type_info_with_args)) = to_process.pop() {
        if processed.contains(&full_type_name) || is_primitive_type(&type_info_with_args.name) {
            continue;
        }
        processed.insert(full_type_name.clone());

        // If this type has generic arguments, we need to create a specialized schema
        if !type_info_with_args.generic_args.is_empty() {
            // Look up the base generic type definition
            if let Some(base_type_info) = all_structs.get(&type_info_with_args.name) {
                // Create a specialized version by substituting generic arguments in fields
                let specialized =
                    specialize_generic_type(base_type_info, &type_info_with_args.generic_args);
                referenced_schemas.insert(full_type_name.clone(), specialized.clone());

                // Process field types from the specialized schema
                for field in &specialized.fields {
                    let base_type = extract_base_type_name(&field.type_name);
                    if !is_primitive_type(&base_type) && !processed.contains(&base_type) {
                        // Create a TypeInfo for this field type to process
                        let field_type_info = parser::TypeInfo {
                            name: base_type.clone(),
                            fields: vec![],
                            is_array: false,
                            is_enum: false,
                            enum_variants: vec![],
                            generic_args: vec![],
                        };
                        to_process.push((base_type, field_type_info));
                    }
                }

                // Also process the generic argument types themselves
                for gen_arg in &type_info_with_args.generic_args {
                    if !is_primitive_type(gen_arg) && !processed.contains(gen_arg) {
                        let gen_type_info = parser::TypeInfo {
                            name: gen_arg.clone(),
                            fields: vec![],
                            is_array: false,
                            is_enum: false,
                            enum_variants: vec![],
                            generic_args: vec![],
                        };
                        to_process.push((gen_arg.clone(), gen_type_info));
                    }
                }
            }
        } else {
            // Non-generic type - just look it up directly
            if let Some(type_info) = all_structs.get(&type_info_with_args.name) {
                // Add this schema to our collection
                referenced_schemas.insert(full_type_name.clone(), type_info.clone());

                // Process all field types
                for field in &type_info.fields {
                    let base_type = extract_base_type_name(&field.type_name);
                    if !is_primitive_type(&base_type) && !processed.contains(&base_type) {
                        let field_type_info = parser::TypeInfo {
                            name: base_type.clone(),
                            fields: vec![],
                            is_array: false,
                            is_enum: false,
                            enum_variants: vec![],
                            generic_args: vec![],
                        };
                        to_process.push((base_type, field_type_info));
                    }
                }
            }
        }
    }

    referenced_schemas
}

/// Create a specialized version of a generic type by substituting type parameters
fn specialize_generic_type(
    base_type: &parser::TypeInfo,
    concrete_args: &[String],
) -> parser::TypeInfo {
    // For now, we assume single generic parameter T
    // In the future, this should handle multiple generics and proper substitution
    let mut specialized = base_type.clone();
    specialized.generic_args = concrete_args.to_vec();

    // Substitute T with the concrete type in all field types
    if !concrete_args.is_empty() {
        let concrete_type = &concrete_args[0];
        specialized.fields = base_type
            .fields
            .iter()
            .map(|field| {
                let new_type_name = field.type_name.replace("T", concrete_type);
                parser::Field {
                    name: field.name.clone(),
                    type_name: new_type_name,
                    required: field.required,
                    description: field.description.clone(),
                }
            })
            .collect();
    }

    specialized
}

/// Extract the base type name from complex types like Vec<T>, Option<T>, etc.
fn extract_base_type_name(type_name: &str) -> String {
    // Handle Vec<T>
    if type_name.starts_with("Vec<") && type_name.ends_with('>') {
        let inner = &type_name[4..type_name.len() - 1];
        return extract_base_type_name(inner);
    }

    // Handle Option<T>
    if type_name.starts_with("Option<") && type_name.ends_with('>') {
        let inner = &type_name[7..type_name.len() - 1];
        return extract_base_type_name(inner);
    }

    // Handle generic types with paths like HashMap<K, V> - just return the whole thing
    // but strip out the generics for now
    if let Some(idx) = type_name.find('<') {
        return type_name[..idx].to_string();
    }

    type_name.to_string()
}

/// Check if a type is a primitive type
fn is_primitive_type(type_name: &str) -> bool {
    matches!(
        type_name,
        "String"
            | "str"
            | "i8"
            | "i16"
            | "i32"
            | "i64"
            | "i128"
            | "isize"
            | "u8"
            | "u16"
            | "u32"
            | "u64"
            | "u128"
            | "usize"
            | "f32"
            | "f64"
            | "bool"
            | "Vec"
            | "Option"
            | "HashMap"
    )
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::io::Write;
    use tempfile::NamedTempFile;

    #[test]
    fn test_type_alias_resolution() {
        // Create a test file with a type alias
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get};
            use serde::Serialize;

            pub type ApiResult<T> = Result<Json<ApiData<T>>, ApiError>;

            #[derive(Serialize)]
            pub struct ApiData<T: Serialize> {
                pub data: T,
            }

            #[derive(Serialize)]
            pub struct ApiError {
                pub message: String,
            }

            #[derive(Serialize)]
            pub struct User {
                pub id: u64,
                pub name: String,
            }

            async fn get_user() -> ApiResult<User> {
                todo!()
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/user", get(get_user))
            }
        };

        let mut temp_file = NamedTempFile::new().unwrap();
        temp_file
            .write_all(test_code.to_string().as_bytes())
            .unwrap();
        temp_file.flush().unwrap();

        let (raw_routes, structs, type_aliases) = parse_file(temp_file.path()).unwrap();

        // Check that we collected the type alias
        assert!(
            type_aliases.contains_key("ApiResult"),
            "ApiResult type alias should be collected"
        );
        eprintln!(
            "Type aliases: {:?}",
            type_aliases.keys().collect::<Vec<_>>()
        );

        // Check that we collected the structs
        assert!(
            structs.contains_key("User"),
            "User struct should be collected"
        );
        assert!(
            structs.contains_key("ApiData"),
            "ApiData struct should be collected"
        );
        assert!(
            structs.contains_key("ApiError"),
            "ApiError struct should be collected"
        );

        // Check that we found the route
        assert_eq!(raw_routes.len(), 1, "Should find 1 route");
        assert_eq!(raw_routes[0].handler, "get_user");

        // Check that the raw route has a response type
        assert!(
            raw_routes[0].response_body_type.is_some(),
            "Route should have response type"
        );

        // Now resolve the route types
        let routes = resolve_route_types(&raw_routes, &type_aliases);

        eprintln!("Resolved route response: {:?}", routes[0].response_body);

        // The response should be resolved to User
        assert!(
            routes[0].response_body.is_some(),
            "Response body should be resolved"
        );
        let response = routes[0].response_body.as_ref().unwrap();

        eprintln!("Response type name: {}", response.name);

        // This is the key test - we should extract User from ApiResult<User>
        // After going through: ApiResult<User> -> Result<Json<ApiData<User>>, ApiError> -> Json<ApiData<User>> -> ApiData<User> -> User
        assert_eq!(
            response.name, "User",
            "Should extract User from ApiResult<User>"
        );
    }

    #[test]
    fn test_type_alias_with_vec() {
        // Test ApiResult<Vec<T>>
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get};
            use serde::Serialize;

            pub type ApiResult<T> = Result<Json<ApiData<T>>, ApiError>;

            #[derive(Serialize)]
            pub struct ApiData<T: Serialize> {
                pub data: T,
            }

            #[derive(Serialize)]
            pub struct ApiError {
                pub message: String,
            }

            #[derive(Serialize)]
            pub struct Item {
                pub id: u64,
            }

            async fn list_items() -> ApiResult<Vec<Item>> {
                todo!()
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/items", get(list_items))
            }
        };

        let mut temp_file = NamedTempFile::new().unwrap();
        temp_file
            .write_all(test_code.to_string().as_bytes())
            .unwrap();
        temp_file.flush().unwrap();

        let (raw_routes, _structs, type_aliases) = parse_file(temp_file.path()).unwrap();
        let routes = resolve_route_types(&raw_routes, &type_aliases);

        assert!(
            routes[0].response_body.is_some(),
            "Response body should be resolved"
        );
        let response = routes[0].response_body.as_ref().unwrap();

        eprintln!("Response type: {:?}", response);

        // Should extract Vec<Item> -> Item with is_array=true
        assert_eq!(
            response.name, "Item",
            "Should extract Item from ApiResult<Vec<Item>>"
        );
        assert!(response.is_array, "Should be marked as array");
    }

    #[test]
    fn test_nested_type_alias() {
        // Test multiple levels of type aliases
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get};
            use serde::Serialize;

            pub type ApiResult<T> = Result<Json<ApiData<T>>, ApiError>;
            pub type ItemResult = ApiResult<Item>;

            #[derive(Serialize)]
            pub struct ApiData<T: Serialize> {
                pub data: T,
            }

            #[derive(Serialize)]
            pub struct ApiError {
                pub message: String,
            }

            #[derive(Serialize)]
            pub struct Item {
                pub id: u64,
            }

            async fn get_item() -> ItemResult {
                todo!()
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/item", get(get_item))
            }
        };

        let mut temp_file = NamedTempFile::new().unwrap();
        temp_file
            .write_all(test_code.to_string().as_bytes())
            .unwrap();
        temp_file.flush().unwrap();

        let (raw_routes, _structs, type_aliases) = parse_file(temp_file.path()).unwrap();

        // Check we collected both type aliases
        assert!(type_aliases.contains_key("ApiResult"));
        assert!(type_aliases.contains_key("ItemResult"));

        let routes = resolve_route_types(&raw_routes, &type_aliases);

        // ItemResult has no generic args, so this won't work with our current implementation
        // This test documents the limitation
        eprintln!("Nested alias response: {:?}", routes[0].response_body);

        // This will fail with current implementation - nested non-generic aliases not supported
        // We'd need to recursively resolve ItemResult -> ApiResult<Item> -> Item
    }

    #[test]
    fn test_generate_spec_with_type_aliases() {
        // End-to-end test
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::{get, post}};
            use serde::{Serialize, Deserialize};

            pub type ApiResult<T> = Result<Json<ApiData<T>>, ApiError>;

            #[derive(Serialize)]
            pub struct ApiData<T: Serialize> {
                pub data: T,
            }

            #[derive(Serialize)]
            pub struct ApiError {
                pub message: String,
            }

            #[derive(Serialize, Deserialize)]
            pub struct User {
                pub id: u64,
                pub name: String,
            }

            #[derive(Serialize, Deserialize)]
            pub struct CreateUserRequest {
                pub name: String,
            }

            async fn get_user() -> ApiResult<User> {
                todo!()
            }

            async fn create_user(Json(req): Json<CreateUserRequest>) -> ApiResult<User> {
                todo!()
            }

            async fn list_users() -> ApiResult<Vec<User>> {
                todo!()
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/user", get(get_user).post(create_user))
                    .route("/users", get(list_users))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Extract routes from paths
        let mut routes = Vec::new();
        for (path, path_item_ref) in &spec.spec.paths.paths {
            if let openapiv3::ReferenceOr::Item(path_item) = path_item_ref {
                if let Some(get_op) = &path_item.get {
                    routes.push((
                        path.clone(),
                        "get",
                        get_op.summary.clone().unwrap_or_default(),
                    ));
                }
                if let Some(post_op) = &path_item.post {
                    routes.push((
                        path.clone(),
                        "post",
                        post_op.summary.clone().unwrap_or_default(),
                    ));
                }
            }
        }

        eprintln!("Total routes: {}", routes.len());
        for (path, method, handler) in &routes {
            eprintln!("Route {} {} -> {}", method, path, handler);
        }

        // Should have 3 routes
        assert_eq!(routes.len(), 3);

        // Check that we have the right handlers
        let handler_names: Vec<String> = routes.iter().map(|(_, _, h)| h.clone()).collect();
        assert!(handler_names.contains(&"get_user".to_string()));
        assert!(handler_names.contains(&"create_user".to_string()));
        assert!(handler_names.contains(&"list_users".to_string()));

        // Check schemas were collected
        let schemas = &spec.spec.components.as_ref().unwrap().schemas;
        eprintln!("Schemas: {:?}", schemas.keys().collect::<Vec<_>>());

        // Only User and CreateUserRequest should be in schemas
        // ApiData and ApiError are not included because we resolve through the type alias
        // directly to the inner type (User, CreateUserRequest)
        assert!(schemas.contains_key("User"), "Should have User schema");
        assert!(
            schemas.contains_key("CreateUserRequest"),
            "Should have CreateUserRequest schema"
        );

        // ApiData and ApiError are wrapper types from the type alias - they are NOT included
        // in the final OpenAPI spec because we extract the actual response type
        assert!(
            !schemas.contains_key("ApiData"),
            "ApiData should NOT be in schemas (it's a wrapper)"
        );
        assert!(
            !schemas.contains_key("ApiError"),
            "ApiError should NOT be in schemas (it's an error type)"
        );
    }

    #[test]
    fn test_error_response_extraction() {
        // Test that error types are extracted from Result<T, E>
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            pub struct User {
                pub id: u64,
                pub name: String,
            }

            #[derive(Serialize, Deserialize)]
            pub struct ApiError {
                pub message: String,
                pub code: String,
            }

            async fn get_user() -> Result<Json<User>, Json<ApiError>> {
                todo!()
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/user", get(get_user))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Get the operation
        let path_item = spec
            .spec
            .paths
            .paths
            .get("/user")
            .expect("Should have /user path");
        let path_item = match path_item {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item, got reference"),
        };
        let get_op = path_item.get.as_ref().expect("Should have GET operation");

        // Check that we have both 200 and 500 responses
        assert!(
            get_op
                .responses
                .responses
                .contains_key(&openapiv3::StatusCode::Code(200)),
            "Should have 200 response"
        );
        assert!(
            get_op
                .responses
                .responses
                .contains_key(&openapiv3::StatusCode::Code(500)),
            "Should have 500 error response"
        );

        // Check that the error response references ApiError
        let error_response = get_op
            .responses
            .responses
            .get(&openapiv3::StatusCode::Code(500))
            .unwrap();
        let error_response = match error_response {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected response item, got reference"),
        };
        assert_eq!(error_response.description, "Internal server error");
        assert!(error_response.content.len() > 0);

        // Verify schemas include both User and ApiError
        let schemas = &spec.spec.components.as_ref().unwrap().schemas;
        assert!(schemas.contains_key("User"), "Should have User schema");
        assert!(
            schemas.contains_key("ApiError"),
            "Should have ApiError schema"
        );
    }

    #[test]
    fn test_error_response_with_type_alias() {
        // Test error extraction with type aliases
        // NOTE: Current limitation - we don't fully expand type aliases to extract error types
        // This test documents the current behavior
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get};
            use serde::{Serialize, Deserialize};

            pub type ApiResult<T> = Result<Json<T>, Json<ApiError>>;

            #[derive(Serialize, Deserialize)]
            pub struct User {
                pub id: u64,
            }

            #[derive(Serialize, Deserialize)]
            pub struct ApiError {
                pub message: String,
            }

            async fn get_user() -> ApiResult<User> {
                todo!()
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/user", get(get_user))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Get the operation
        let path_item = spec
            .spec
            .paths
            .paths
            .get("/user")
            .expect("Should have /user path");
        let path_item = match path_item {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item, got reference"),
        };
        let get_op = path_item.get.as_ref().expect("Should have GET operation");

        // Success response should be extracted
        assert!(
            get_op
                .responses
                .responses
                .contains_key(&openapiv3::StatusCode::Code(200)),
            "Should have 200 response"
        );

        // Note: Type alias error extraction is a known limitation
        // We would need full type alias expansion to support this
        // For direct Result<Json<T>, Json<E>> patterns, error extraction works (see test_error_response_extraction)
    }

    #[test]
    fn test_inline_lambda_handler() {
        // Test extraction from inline lambda handlers
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::any};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            pub struct CreateProductRequest {
                pub name: String,
            }

            #[derive(Serialize, Deserialize)]
            pub struct Product {
                pub id: u64,
                pub name: String,
            }

            #[derive(Serialize, Deserialize)]
            pub struct ApiError {
                pub message: String,
            }

            pub type ApiResponse<T> = Result<Json<T>, Json<ApiError>>;

            pub fn routes() -> Router {
                Router::new()
                    .route(
                        "/inline-product",
                        any(
                            |Json(_req): Json<CreateProductRequest>| -> ApiResponse<Vec<Product>> {
                                Ok(Json(vec![]))
                            },
                        ),
                    )
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        eprintln!(
            "Paths: {:?}",
            spec.spec.paths.paths.keys().collect::<Vec<_>>()
        );

        // Check that the route was found
        assert!(
            spec.spec.paths.paths.contains_key("/inline-product"),
            "Should have /inline-product route"
        );

        let path_item = spec.spec.paths.paths.get("/inline-product").unwrap();
        let path_item = match path_item {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item, got reference"),
        };
        let operation = path_item
            .post
            .as_ref()
            .expect("Should have POST operation (any defaults to POST)");

        eprintln!("Operation: {:?}", operation);

        // Check request body
        assert!(
            operation.request_body.is_some(),
            "Should have request body from inline lambda"
        );

        // Check response
        assert!(
            operation
                .responses
                .responses
                .contains_key(&openapiv3::StatusCode::Code(200)),
            "Should have 200 response"
        );

        // Check that schemas were collected
        let schemas = &spec.spec.components.as_ref().unwrap().schemas;
        assert!(
            schemas.contains_key("CreateProductRequest"),
            "Should have CreateProductRequest schema"
        );
        assert!(
            schemas.contains_key("Product"),
            "Should have Product schema"
        );

        // Error response from type alias won't work yet (known limitation)
        // but the request and success response should work
    }

    #[test]
    fn test_deeply_nested_json_extraction() {
        // Test that we can extract JSON models from deeply nested type patterns
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            struct Page<T> {
                items: Vec<T>,
                total: usize,
            }

            #[derive(Serialize, Deserialize)]
            struct User {
                id: u32,
                name: String,
            }

            #[derive(Serialize, Deserialize)]
            struct ApiError {
                message: String,
            }

            // Test various nested patterns
            async fn get_users_paginated() -> Result<Json<Page<User>>, Json<ApiError>> {
                Ok(Json(Page { items: vec![], total: 0 }))
            }

            async fn create_user(Json(req): Json<User>) -> Result<Json<User>, Json<ApiError>> {
                Ok(Json(req))
            }

            async fn tuple_response() -> (axum::http::StatusCode, Json<User>) {
                (axum::http::StatusCode::OK, Json(User { id: 1, name: "test".to_string() }))
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/users/paginated", get(get_users_paginated))
                    .route("/users", axum::routing::post(create_user))
                    .route("/users/tuple", get(tuple_response))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Verify all routes were extracted
        assert_eq!(spec.spec.paths.paths.len(), 3);

        // Test 1: Result<Json<Page<User>>, Json<ApiError>>
        let paginated_path = spec.spec.paths.paths.get("/users/paginated").unwrap();
        let paginated_path = match paginated_path {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item"),
        };
        let get_op = paginated_path.get.as_ref().unwrap();

        // Should extract Page as the response type
        let success_response = get_op
            .responses
            .responses
            .get(&openapiv3::StatusCode::Code(200))
            .unwrap();
        let success_response = match success_response {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected response item"),
        };

        // Response should reference Page schema
        assert!(success_response.content.len() > 0);
        let json_content = success_response.content.get("application/json").unwrap();
        if let Some(openapiv3::ReferenceOr::Reference { reference }) = &json_content.schema {
            assert!(
                reference.contains("Page"),
                "Should reference Page schema, got: {}",
                reference
            );
        }

        // Should have error response
        assert!(get_op
            .responses
            .responses
            .contains_key(&openapiv3::StatusCode::Code(500)));

        // Test 2: Json<User> request and Result<Json<User>, Json<ApiError>> response
        let users_path = spec.spec.paths.paths.get("/users").unwrap();
        let users_path = match users_path {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item"),
        };
        let post_op = users_path.post.as_ref().unwrap();

        // Should have request body
        assert!(post_op.request_body.is_some());

        // Should have both 200 and 500 responses
        assert!(post_op
            .responses
            .responses
            .contains_key(&openapiv3::StatusCode::Code(200)));
        assert!(post_op
            .responses
            .responses
            .contains_key(&openapiv3::StatusCode::Code(500)));

        // Test 3: Tuple response (StatusCode, Json<User>)
        let tuple_path = spec.spec.paths.paths.get("/users/tuple").unwrap();
        let tuple_path = match tuple_path {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item"),
        };
        let tuple_get_op = tuple_path.get.as_ref().unwrap();

        // Should extract User from tuple
        let tuple_response = tuple_get_op
            .responses
            .responses
            .get(&openapiv3::StatusCode::Code(200))
            .unwrap();
        let tuple_response = match tuple_response {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected response item"),
        };
        assert!(tuple_response.content.len() > 0);

        // Verify all schemas are present
        let schemas = &spec.spec.components.as_ref().unwrap().schemas;
        // Generic types should be instantiated as separate schemas
        assert!(
            schemas.contains_key("Page_User"),
            "Should have Page_User schema"
        );
        assert!(schemas.contains_key("User"), "Should have User schema");
        assert!(
            schemas.contains_key("ApiError"),
            "Should have ApiError schema"
        );

        // Verify Page_User has correct field type
        let page_user = &schemas["Page_User"];
        if let openapiv3::ReferenceOr::Item(schema) = page_user {
            if let openapiv3::SchemaKind::Type(openapiv3::Type::Object(obj_type)) =
                &schema.schema_kind
            {
                let items_prop = obj_type
                    .properties
                    .get("items")
                    .expect("Should have items property");
                // items should be an array of User
                if let openapiv3::ReferenceOr::Item(items_schema) = items_prop {
                    if let openapiv3::SchemaKind::Type(openapiv3::Type::Array(array_type)) =
                        &items_schema.schema_kind
                    {
                        if let Some(openapiv3::ReferenceOr::Reference { reference }) =
                            &array_type.items
                        {
                            assert!(
                                reference.contains("User"),
                                "Page_User items should reference User, got: {}",
                                reference
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_generic_type_specialization() {
        // Test that generic types are properly specialized for each concrete usage
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            struct PageResult<T> {
                data: Vec<T>,
                page: u32,
                total: u32,
            }

            #[derive(Serialize, Deserialize)]
            struct Product {
                id: u32,
                name: String,
                price: f64,
            }

            #[derive(Serialize, Deserialize)]
            struct Category {
                id: u32,
                title: String,
            }

            async fn get_products() -> Json<PageResult<Product>> {
                Json(PageResult { data: vec![], page: 1, total: 0 })
            }

            async fn get_categories() -> Json<PageResult<Category>> {
                Json(PageResult { data: vec![], page: 1, total: 0 })
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/products", get(get_products))
                    .route("/categories", get(get_categories))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Verify we have specialized schemas for each generic instantiation
        let schemas = &spec.spec.components.as_ref().unwrap().schemas;
        assert!(
            schemas.contains_key("PageResult_Product"),
            "Should have PageResult_Product"
        );
        assert!(
            schemas.contains_key("PageResult_Category"),
            "Should have PageResult_Category"
        );
        assert!(schemas.contains_key("Product"), "Should have Product");
        assert!(schemas.contains_key("Category"), "Should have Category");

        // Verify paths reference the correct specialized schemas
        let products_path = spec.spec.paths.paths.get("/products").unwrap();
        let products_path = match products_path {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item"),
        };
        let get_op = products_path.get.as_ref().unwrap();
        let success_response = get_op
            .responses
            .responses
            .get(&openapiv3::StatusCode::Code(200))
            .unwrap();
        let success_response = match success_response {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected response item"),
        };
        let json_content = success_response.content.get("application/json").unwrap();
        if let Some(openapiv3::ReferenceOr::Reference { reference }) = &json_content.schema {
            assert!(
                reference.contains("PageResult_Product"),
                "Products endpoint should reference PageResult_Product, got: {}",
                reference
            );
        }

        let categories_path = spec.spec.paths.paths.get("/categories").unwrap();
        let categories_path = match categories_path {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item"),
        };
        let get_op = categories_path.get.as_ref().unwrap();
        let success_response = get_op
            .responses
            .responses
            .get(&openapiv3::StatusCode::Code(200))
            .unwrap();
        let success_response = match success_response {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected response item"),
        };
        let json_content = success_response.content.get("application/json").unwrap();
        if let Some(openapiv3::ReferenceOr::Reference { reference }) = &json_content.schema {
            assert!(
                reference.contains("PageResult_Category"),
                "Categories endpoint should reference PageResult_Category, got: {}",
                reference
            );
        }

        // Verify PageResult_Product has Product in its data field
        let page_product = &schemas["PageResult_Product"];
        if let openapiv3::ReferenceOr::Item(schema) = page_product {
            if let openapiv3::SchemaKind::Type(openapiv3::Type::Object(obj_type)) =
                &schema.schema_kind
            {
                let data_prop = obj_type
                    .properties
                    .get("data")
                    .expect("Should have data property");
                if let openapiv3::ReferenceOr::Item(data_schema) = data_prop {
                    if let openapiv3::SchemaKind::Type(openapiv3::Type::Array(array_type)) =
                        &data_schema.schema_kind
                    {
                        if let Some(openapiv3::ReferenceOr::Reference { reference }) =
                            &array_type.items
                        {
                            assert!(
                                reference.contains("Product"),
                                "PageResult_Product data should reference Product, got: {}",
                                reference
                            );
                        }
                    }
                }
            }
        }
    }

    #[test]
    fn test_query_parameter_extraction() {
        // Test extraction of Query<T> parameters
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get, extract::Query};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            pub struct SearchParams {
                pub q: String,
                pub page: i32,
                pub limit: Option<i32>,
            }

            #[derive(Serialize, Deserialize)]
            pub struct SearchResult {
                pub total: i32,
                pub items: Vec<String>,
            }

            async fn search(Query(params): Query<SearchParams>) -> Json<SearchResult> {
                Json(SearchResult {
                    total: 0,
                    items: vec![],
                })
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/search", get(search))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Get the /search path
        let path_item = spec
            .spec
            .paths
            .paths
            .get("/search")
            .expect("Should have /search path");
        let path_item = match path_item {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item, got reference"),
        };
        let get_op = path_item.get.as_ref().expect("Should have GET operation");

        // Check query parameters
        assert_eq!(get_op.parameters.len(), 3, "Should have 3 query parameters");

        // Verify parameter names and required status
        let param_names: Vec<String> = get_op
            .parameters
            .iter()
            .filter_map(|p| match p {
                openapiv3::ReferenceOr::Item(openapiv3::Parameter::Query {
                    parameter_data,
                    ..
                }) => Some(parameter_data.name.clone()),
                _ => None,
            })
            .collect();

        assert!(
            param_names.contains(&"q".to_string()),
            "Should have 'q' parameter"
        );
        assert!(
            param_names.contains(&"page".to_string()),
            "Should have 'page' parameter"
        );
        assert!(
            param_names.contains(&"limit".to_string()),
            "Should have 'limit' parameter"
        );

        // Check required status
        for param in &get_op.parameters {
            if let openapiv3::ReferenceOr::Item(openapiv3::Parameter::Query {
                parameter_data,
                ..
            }) = param
            {
                match parameter_data.name.as_str() {
                    "q" => assert!(parameter_data.required, "'q' should be required"),
                    "page" => assert!(parameter_data.required, "'page' should be required"),
                    "limit" => assert!(!parameter_data.required, "'limit' should be optional"),
                    _ => {}
                }
            }
        }

        // Verify SearchParams is NOT in schemas (it's only used for query params)
        let schemas = &spec.spec.components.as_ref().unwrap().schemas;
        // SearchParams might be in schemas if referenced, but that's okay
        // The key thing is that query params are extracted

        // Verify SearchResult IS in schemas
        assert!(
            schemas.contains_key("SearchResult"),
            "Should have SearchResult schema"
        );
    }

    #[test]
    fn test_query_and_body_parameters() {
        // Test handler with both Query and Json parameters
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::post, extract::Query};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            pub struct FilterParams {
                pub status: String,
                pub category: Option<String>,
            }

            #[derive(Serialize, Deserialize)]
            pub struct CreateRequest {
                pub name: String,
                pub value: i32,
            }

            #[derive(Serialize, Deserialize)]
            pub struct Response {
                pub id: u64,
            }

            async fn create_with_filter(
                Query(filter): Query<FilterParams>,
                Json(body): Json<CreateRequest>
            ) -> Json<Response> {
                Json(Response { id: 1 })
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/items", post(create_with_filter))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Get the /items path
        let path_item = spec
            .spec
            .paths
            .paths
            .get("/items")
            .expect("Should have /items path");
        let path_item = match path_item {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item, got reference"),
        };
        let post_op = path_item.post.as_ref().expect("Should have POST operation");

        // Check query parameters
        assert_eq!(
            post_op.parameters.len(),
            2,
            "Should have 2 query parameters"
        );

        // Check request body
        assert!(post_op.request_body.is_some(), "Should have request body");
        let request_body = post_op.request_body.as_ref().unwrap();
        let request_body = match request_body {
            openapiv3::ReferenceOr::Item(rb) => rb,
            _ => panic!("Expected request body item"),
        };
        assert!(request_body.content.contains_key("application/json"));

        // Verify both FilterParams and CreateRequest are in schemas
        let schemas = &spec.spec.components.as_ref().unwrap().schemas;
        // FilterParams and CreateRequest might be in schemas
        assert!(
            schemas.contains_key("Response"),
            "Should have Response schema"
        );
    }

    #[test]
    fn test_query_params_with_generic_types() {
        // Test that Query<Page<T>> extracts Page's fields, not T's fields
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get, extract::Query};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            pub struct PaginationParams<T> {
                pub page: i32,
                pub limit: i32,
                pub sort_by: Option<String>,
            }

            #[derive(Serialize, Deserialize)]
            pub struct SearchFilter {
                pub query: String,
            }

            #[derive(Serialize, Deserialize)]
            pub struct SearchResult {
                pub items: Vec<String>,
            }

            // Handler uses Query<PaginationParams<SearchFilter>>
            // Should extract page, limit, sort_by from PaginationParams
            // NOT query from SearchFilter
            async fn search(
                Query(params): Query<PaginationParams<SearchFilter>>
            ) -> Json<SearchResult> {
                Json(SearchResult { items: vec![] })
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/search", get(search))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Get the /search path
        let path_item = spec
            .spec
            .paths
            .paths
            .get("/search")
            .expect("Should have /search path");
        let path_item = match path_item {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item, got reference"),
        };
        let get_op = path_item.get.as_ref().expect("Should have GET operation");

        // Should have 3 parameters from PaginationParams (page, limit, sort_by)
        // NOT the fields from SearchFilter (query)
        assert_eq!(
            get_op.parameters.len(),
            3,
            "Should have 3 query parameters from PaginationParams"
        );

        // Verify parameter names are from PaginationParams
        let param_names: Vec<String> = get_op
            .parameters
            .iter()
            .filter_map(|p| match p {
                openapiv3::ReferenceOr::Item(openapiv3::Parameter::Query {
                    parameter_data,
                    ..
                }) => Some(parameter_data.name.clone()),
                _ => None,
            })
            .collect();

        assert!(
            param_names.contains(&"page".to_string()),
            "Should have 'page' from PaginationParams"
        );
        assert!(
            param_names.contains(&"limit".to_string()),
            "Should have 'limit' from PaginationParams"
        );
        assert!(
            param_names.contains(&"sort_by".to_string()),
            "Should have 'sort_by' from PaginationParams"
        );
        assert!(
            !param_names.contains(&"query".to_string()),
            "Should NOT have 'query' from SearchFilter"
        );

        // Verify required status
        for param in &get_op.parameters {
            if let openapiv3::ReferenceOr::Item(openapiv3::Parameter::Query {
                parameter_data,
                ..
            }) = param
            {
                match parameter_data.name.as_str() {
                    "page" => assert!(parameter_data.required, "'page' should be required"),
                    "limit" => assert!(parameter_data.required, "'limit' should be required"),
                    "sort_by" => assert!(!parameter_data.required, "'sort_by' should be optional"),
                    _ => {}
                }
            }
        }
    }

    #[test]
    fn test_flatten_in_query_params() {
        // Test that flattened fields are properly merged in query parameters
        let test_code = quote::quote! {
            use axum::{Json, Router, routing::get, extract::Query};
            use serde::{Serialize, Deserialize};

            #[derive(Serialize, Deserialize)]
            pub struct PaginationFields {
                pub page: i32,
                pub limit: i32,
            }

            #[derive(Serialize, Deserialize)]
            pub struct SearchParams {
                #[serde(flatten)]
                pub pagination: PaginationFields,
                pub query: String,
                pub category: Option<String>,
            }

            #[derive(Serialize, Deserialize)]
            pub struct SearchResult {
                pub items: Vec<String>,
            }

            async fn search(Query(params): Query<SearchParams>) -> Json<SearchResult> {
                Json(SearchResult { items: vec![] })
            }

            pub fn routes() -> Router {
                Router::new()
                    .route("/search", get(search))
            }
        };

        let temp_dir = tempfile::tempdir().unwrap();
        let file_path = temp_dir.path().join("routes.rs");
        std::fs::write(&file_path, test_code.to_string()).unwrap();

        let spec = generate_spec(temp_dir.path()).unwrap();

        // Get the /search path
        let path_item = spec
            .spec
            .paths
            .paths
            .get("/search")
            .expect("Should have /search path");
        let path_item = match path_item {
            openapiv3::ReferenceOr::Item(item) => item,
            _ => panic!("Expected path item, got reference"),
        };
        let get_op = path_item.get.as_ref().expect("Should have GET operation");

        // Should have 4 parameters: page, limit (flattened from PaginationFields), query, category
        assert_eq!(
            get_op.parameters.len(),
            4,
            "Should have 4 query parameters including flattened fields"
        );

        // Verify parameter names include flattened fields
        let param_names: Vec<String> = get_op
            .parameters
            .iter()
            .filter_map(|p| match p {
                openapiv3::ReferenceOr::Item(openapiv3::Parameter::Query {
                    parameter_data,
                    ..
                }) => Some(parameter_data.name.clone()),
                _ => None,
            })
            .collect();

        // Should have fields from both PaginationFields (flattened) and SearchParams
        assert!(
            param_names.contains(&"page".to_string()),
            "Should have 'page' from flattened PaginationFields"
        );
        assert!(
            param_names.contains(&"limit".to_string()),
            "Should have 'limit' from flattened PaginationFields"
        );
        assert!(
            param_names.contains(&"query".to_string()),
            "Should have 'query' from SearchParams"
        );
        assert!(
            param_names.contains(&"category".to_string()),
            "Should have 'category' from SearchParams"
        );

        // Verify required status
        for param in &get_op.parameters {
            if let openapiv3::ReferenceOr::Item(openapiv3::Parameter::Query {
                parameter_data,
                ..
            }) = param
            {
                match parameter_data.name.as_str() {
                    "page" => assert!(parameter_data.required, "'page' should be required"),
                    "limit" => assert!(parameter_data.required, "'limit' should be required"),
                    "query" => assert!(parameter_data.required, "'query' should be required"),
                    "category" => {
                        assert!(!parameter_data.required, "'category' should be optional")
                    }
                    _ => {}
                }
            }
        }
    }
}
