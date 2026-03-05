use clap::{Parser, Subcommand};

mod build;
mod codegen;
mod tii;

#[derive(Parser)]
#[command(name = "tx3c", version, author, about)]
#[command(about = "Tx3 language compiler CLI")]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    /// Build and compile Tx3 source files
    Build(build::Args),
    /// Render codegen templates from a TII file
    Codegen(codegen::Args),
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let cli = Cli::parse();

    match cli.command {
        Commands::Build(args) => build::run(args)?,
        Commands::Codegen(args) => codegen::run(args)?,
    }

    Ok(())
}
