use anyhow::Result;
use openapi_build_gen::generate_spec;
use std::path::PathBuf;

fn main() -> Result<()> {
    let args: Vec<String> = std::env::args().collect();

    if args.len() < 2 {
        eprintln!("Usage: openapi-build-gen <source-directory> [output-file]");
        eprintln!("  source-directory: Path to Rust source code directory");
        eprintln!("  output-file: Optional output file path (defaults to openapi.json)");
        std::process::exit(1);
    }

    let source_dir = PathBuf::from(&args[1]);
    let output_file = if args.len() >= 3 {
        PathBuf::from(&args[2])
    } else {
        PathBuf::from("openapi.json")
    };

    println!("Scanning directory: {}", source_dir.display());

    let spec = generate_spec(&source_dir)?;

    println!("\nGenerating OpenAPI specification...");
    spec.write_json_to_file(&output_file)?;
    println!("OpenAPI spec written to: {}", output_file.display());

    Ok(())
}
