# openapi-build-gen

A Rust code analyzer that generates OpenAPI documentation from Axum web service source code using the `syn` crate for AST parsing.

**Designed for use in `build.rs`** - Automatically generates OpenAPI specs at compile time that are embedded in your binary as a static string constant.

## Features

- Parses Axum router definitions to extract API routes
- Supports nested routers using `.nest()` for path prefixes
- Detects HTTP methods (GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS)
- Extracts path parameters from routes (`:id` or `{id}` syntax)
- Analyzes handler function signatures to identify request/response types
- Extracts struct definitions with Serde macros to generate schema components
- Generates valid OpenAPI 3.0 specification
- Zero runtime dependencies - outputs a simple JSON string constant

## Framework Support

| Framework | Router Parsing | Nested Routes | Handler Analysis | Status |
|-----------|----------------|---------------|------------------|--------|
| Axum      | ✅             | ✅            | ✅               | Supported |
| Actix-web | ❌             | ❌            | ❌               | Planned |
| Rocket    | ❌             | ❌            | ❌               | Planned |
| Warp      | ❌             | ❌            | ❌               | Planned |

## Usage in build.rs (Recommended)

Add to your `Cargo.toml`:

```toml
[build-dependencies]
openapi-build-gen = "0.1"
```

That's it! No runtime dependencies needed.

Create a `build.rs` file:

```rust
use openapi_build_gen::{generate_spec, InfoBuilder};
use std::env;
use std::path::PathBuf;

fn main() {
    println!("cargo:rerun-if-changed=src");

    let out_dir = env::var("OUT_DIR").unwrap();
    let openapi_rs = PathBuf::from(&out_dir).join("openapi.rs");

    // Generate OpenAPI spec from src directory
    generate_spec("src")
        .expect("Failed to generate spec")
        .with_info(
            InfoBuilder::new("My API", env!("CARGO_PKG_VERSION"))
                .description("A comprehensive API built with Axum")
                .contact(
                    Some("API Team".to_string()),
                    Some("https://example.com".to_string()),
                    Some("api@example.com".to_string()),
                )
                .license("MIT", Some("https://opensource.org/licenses/MIT".to_string()))
                .build(),
        )
        .write_rust_to_file(&openapi_rs, "OPENAPI_SPEC")
        .expect("Failed to write openapi.rs");
}
```

Include the generated module in your application:

```rust
use axum::{routing::get, Router};

// Include the generated OpenAPI module
mod openapi {
    include!(concat!(env!("OUT_DIR"), "/openapi.rs"));
}

#[tokio::main]
async fn main() {
    let app = Router::new()
        .route("/openapi.json", get(get_openapi_spec))
        // ... your other routes
        ;
    
    // ... start server
}

/// Serve the OpenAPI spec as JSON
async fn get_openapi_spec() -> ([(axum::http::HeaderName, &'static str); 1], &'static str) {
    use axum::http::header::CONTENT_TYPE;
    ([(CONTENT_TYPE, "application/json")], openapi::OPENAPI_JSON)
}
```

### Optional: Modify at Runtime

If you want to modify the spec at runtime, you can parse the JSON string yourself:

```rust
// Add serde_json to your dependencies if needed
use serde_json::Value;

async fn get_openapi_spec() -> axum::Json<Value> {
    let mut spec: Value = serde_json::from_str(openapi::OPENAPI_JSON)
        .expect("Generated OpenAPI JSON is always valid");
    
    // Modify the spec at runtime if needed
    if let Some(info) = spec.get_mut("info") {
        if let Some(obj) = info.as_object_mut() {
            obj.insert("x-api-id".to_string(), serde_json::json!("custom-12345"));
        }
    }
    
    axum::Json(spec)
}
```

## CLI Usage

You can also use it as a command-line tool to generate OpenAPI JSON files:

```bash
cargo install openapi-build-gen

# Generate openapi.json from source directory
openapi-build-gen src/

# Specify custom output file
openapi-build-gen src/ api-spec.json
```

## How It Works

The generator uses the `syn` crate to parse Rust source files and extract:

1. **Router Definitions**: Scans for Axum `Router::new()` chains with:
   - `.route()` calls for individual endpoints
   - `.nest()` calls for nested routers with path prefixes
2. **Handler Functions**: Analyzes function signatures to extract:
   - Request body types from `Json<T>` extractors
   - Response types from `Json<T>` return values
   - Path parameters from `Path<T>` extractors
3. **Schema Definitions**: Extracts structs with `Serialize`/`Deserialize` derives to generate component schemas

### Nested Router Support

The generator fully supports Axum's `.nest()` method for organizing routes:

```rust
fn api_routes() -> Router {
    Router::new()
        .route("/health", get(health_check))
        .nest("/api", Router::new()
            .nest("/v1", Router::new()
                .route("/users", get(list_users))
                .route("/users/:id", get(get_user))
            )
            .route("/status", get(api_status))
        )
        .nest("/admin", Router::new()
            .route("/metrics", get(admin_metrics))
        )
}

// Generates routes:
// GET /health
// GET /api/v1/users
// GET /api/v1/users/:id
// GET /api/status
// GET /admin/metrics
```

Path parameters work in both the nest path and individual routes:

```rust
Router::new()
    .nest("/users/:user_id", Router::new()
        .route("/posts", get(user_posts))
        .route("/posts/:post_id", get(user_post))
    )

// Generates:
// GET /users/:user_id/posts (with user_id param)
// GET /users/:user_id/posts/:post_id (with user_id and post_id params)
```

## Serde Attributes Support

The generator correctly handles serde attributes for JSON field naming:

```rust
#[derive(Serialize, Deserialize)]
#[serde(rename_all = "camelCase")]
pub struct UserProfile {
    pub user_id: u64,        // becomes "userId" in JSON
    pub first_name: String,  // becomes "firstName" in JSON
    pub last_name: String,   // becomes "lastName" in JSON
}

#[derive(Serialize, Deserialize)]
pub struct Product {
    pub id: u64,
    #[serde(rename = "productName")]
    pub name: String,        // becomes "productName" in JSON
    #[serde(skip)]
    pub internal_field: String,  // excluded from schema
}
```

## Generated Output

The `build.rs` generates a Rust module with:

- `OPENAPI_JSON`: A `&'static str` containing the OpenAPI spec as a JSON string

This allows you to:
1. Serve the static spec directly with zero overhead
2. Parse and modify the spec at runtime if needed (by adding serde_json)
3. Zero runtime dependencies - just a string constant embedded at compile time

## Example

See the complete example in `examples/api-server/` which demonstrates:
- Custom OpenAPI info (title, description, contact, license)
- Automatic route detection from Axum handlers
- Schema generation from serde structs
- Serving the spec via HTTP endpoint

Run it with:
```bash
cd examples/api-server
cargo run
# Visit http://localhost:3000/openapi.json
```

## Supported Features

| Feature | Support |
|---------|---------|
| Axum router definitions | ✅ |
| Nested routers with `.nest()` | ✅ |
| Multi-level nesting | ✅ |
| HTTP methods (GET, POST, PUT, DELETE, PATCH, HEAD, OPTIONS) | ✅ |
| Path parameters (`:id` or `{id}` syntax) | ✅ |
| Path parameters in nested routes | ✅ |
| Request body extraction from `Json<T>` | ✅ |
| Response body extraction from `Json<T>` | ✅ |
| Serde struct schema generation | ✅ |
| Required vs optional fields (`Option<T>`) | ✅ |
| Nested types and arrays (`Vec<T>`) | ✅ |
| `#[serde(rename = "...")]` | ✅ |
| `#[serde(rename_all = "...")]` | ✅ |
| `#[serde(skip)]` | ✅ |
| `#[serde(skip_serializing)]` | ✅ |

## Future Enhancements

- Support for other web frameworks (Actix, Rocket, Warp)
- Doc comment extraction for descriptions
- Custom attribute parsing for additional metadata
- Support for more complex extractors (State, Extension, etc.)
- Type alias resolution
- Enum support for schemas

## License

MIT
