use openapi_build_gen::generate_spec;

fn main() {
    let rs = generate_spec("examples/api-server/src")
        .expect("Failed to generate spec")
        .to_rust_code("OPENAPI_JSON")
        .expect("failed to generate Rust code");

    eprintln!("{}", rs);
}
