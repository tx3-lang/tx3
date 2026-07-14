//! `tx3c decode` — the reverse of `build`.
//!
//! `build` goes source → compiled artifact; `decode` goes compiled artifact →
//! report, sharing the same `--emit` axis. The input is polymorphic by design:
//! today a published `.tii`, but future compiled inputs slot into this one
//! verb rather than spawning new ones (cf. `protoc --decode`).

use clap::Parser;
use std::path::PathBuf;

use crate::build::EmitKind;
use crate::tii::types::{BytesEncoding, TiiFile};

#[derive(Parser)]
pub struct Args {
    /// A compiled TII artifact to decode.
    #[arg(long)]
    pub tii: PathBuf,

    /// What to emit. Currently only `tir-json`.
    #[arg(short, long, value_delimiter = ',', value_enum)]
    pub emit: Vec<EmitKind>,

    /// Transaction name to decode; required by `--emit tir-json`.
    #[arg(long)]
    pub tx: Option<String>,
}

pub fn run(args: Args) -> anyhow::Result<()> {
    let bytes = std::fs::read(&args.tii)
        .map_err(|e| anyhow::anyhow!("reading {}: {e}", args.tii.display()))?;
    let tii: TiiFile = serde_json::from_slice(&bytes)
        .map_err(|e| anyhow::anyhow!("parsing TII {}: {e}", args.tii.display()))?;

    if !args.emit.contains(&EmitKind::TirJson) {
        anyhow::bail!("nothing to decode: pass --emit tir-json");
    }

    let name = args
        .tx
        .as_deref()
        .ok_or_else(|| anyhow::anyhow!("--emit tir-json requires --tx <NAME>"))?;

    let envelope = &tii
        .transactions
        .get(name)
        .ok_or_else(|| anyhow::anyhow!("transaction '{}' not found in TII", name))?
        .tir;

    let raw = match envelope.encoding {
        BytesEncoding::Hex => {
            hex::decode(&envelope.content).map_err(|e| anyhow::anyhow!("decoding hex TIR: {e}"))?
        }
        BytesEncoding::Base64 => anyhow::bail!(
            "interface TIR is base64-encoded, which this tx3c does not support; \
             ask the publisher to re-publish"
        ),
    };

    let version = tx3_tir::encoding::TirVersion::try_from(envelope.version.as_str())
        .map_err(|e| anyhow::anyhow!("unsupported TIR version: {e}"))?;
    let any = tx3_tir::encoding::from_bytes(&raw, version)
        .map_err(|e| anyhow::anyhow!("decoding TIR: {e}"))?;
    let tx3_tir::encoding::AnyTir::V1Beta0(tx) = any;

    // Same shape as `build --emit tir-json`, so a consumer can't tell whether
    // the TIR came from source or from a published artifact.
    println!("{}", serde_json::to_string(&tx)?);

    Ok(())
}
