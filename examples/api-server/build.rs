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
                .license(
                    "MIT",
                    Some("https://opensource.org/licenses/MIT".to_string()),
                )
                .build(),
        )
        .write_rust_to_file(&openapi_rs, "OPENAPI_SPEC")
        .expect("Failed to write openapi.rs");

    println!("cargo:warning=OpenAPI spec generated at {:?}", openapi_rs);

    // Tell Cargo to set an environment variable with the path
    println!("cargo:rustc-env=OPENAPI_RS_PATH={}", openapi_rs.display());
}
