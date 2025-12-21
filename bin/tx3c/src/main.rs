use clap::{Parser, Subcommand};
use serde_json::json;
use std::{collections::HashMap, path::PathBuf};

mod tii;

#[derive(Parser)]
#[command(name = "tx3c")]
#[command(about = "Tx3 language compiler CLI")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build and compile Tx3 source files
    Build {
        /// Source file path
        source: PathBuf,
        /// Output file path (defaults to source with .json extension)
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build { source, output } => {
            build_command(source, output)?;
        }
    }

    Ok(())
}

fn build_command(
    source: PathBuf,
    output: Option<PathBuf>,
) -> Result<(), Box<dyn std::error::Error>> {
    let mut ws = tx3_lang::Workspace::from_file(&source)?;

    ws.parse()?;
    ws.analyze()?;
    ws.lower()?;

    let ast = ws.ast().ok_or("Failed to get AST")?;

    let mut tii = tii::TiiFile {
        tii: tii::TiiInfo {
            version: "v1beta0".to_string(),
        },
        protocol: tii::Protocol {
            name: "tx3".to_string(),
            version: "1.0.0".to_string(),
            description: None,
        },
        transactions: HashMap::new(),
        environments: HashMap::new(),
        components: Some(tii::Components {
            schemas: HashMap::new(),
        }),
    };

    for tx in ast.txs.iter() {
        let tir = ws
            .tir(&tx.name.value)
            .ok_or_else(|| format!("Failed to get TIR for transaction: {}", tx.name.value))?;

        let params = tx3_tir::reduce::find_params(tir);

        let params_schema = tii::infer_schema_from_params(&params);

        // Convert TIR to bytes
        let bytes = tx3_tir::interop::to_vec(tir);

        // Hex-encode the bytes
        let hex_string = hex::encode(&bytes);

        // Add to output map with transaction name as key
        tii.transactions.insert(
            tx.name.value.clone(),
            tii::Transaction {
                description: None,
                tir: tx3_tir::interop::json::TirEnvelope {
                    content: hex_string,
                    encoding: tx3_tir::interop::json::BytesEncoding::Hex,
                    version: "v1beta0".to_string(),
                },
                params: params_schema,
            },
        );
    }

    // Create final JSON object
    let output_json = json!(tii);

    // Determine output path
    let output_path = output.unwrap_or_else(|| source.with_extension("tii"));

    // Write JSON to file
    std::fs::write(&output_path, serde_json::to_string_pretty(&output_json)?)?;

    println!(
        "Compiled {} transactions to {}",
        source.display(),
        output_path.display()
    );

    Ok(())
}
