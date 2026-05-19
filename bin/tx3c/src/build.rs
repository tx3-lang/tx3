use clap::{Parser, ValueEnum};
use std::path::PathBuf;

use crate::diagnostics::{self, DiagnosticsFormat};
use crate::tii;

use std::str::FromStr;

#[derive(Debug, Clone)]
pub struct ProfileKV<T> {
    pub profile: String,
    pub value: T,
}

impl<T> ProfileKV<T>
where
    T: FromStr,
    T::Err: ToString,
{
    pub fn parse(s: &str) -> Result<Self, String> {
        let mut parts = s.splitn(2, ':');

        let profile = parts
            .next()
            .filter(|p| !p.is_empty())
            .ok_or("profile must not be empty")?
            .to_string();

        let value = parts
            .next()
            .filter(|v| !v.is_empty())
            .ok_or("value must not be empty")?
            .parse::<T>()
            .map_err(|e| e.to_string())?;

        Ok(Self { profile, value })
    }
}

fn profile_env_file(s: &str) -> Result<ProfileKV<PathBuf>, String> {
    ProfileKV::parse(s)
}

/// What `build` should produce. Multiple targets are additive. When the list
/// is empty, `build` runs the front end (parse + analyze) and stops *before*
/// lowering, writing no artifact — this is the "check" semantic (cf.
/// `rustc --emit=metadata`).
#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum EmitKind {
    /// Transaction Invocation Interface JSON file (the default when `-o` is used).
    Tii,
    /// The lowered v1beta0 TIR of a single `--tx`, as JSON, to stdout.
    TirJson,
}

#[derive(Parser)]
pub struct Args {
    pub source: PathBuf,

    #[arg(short, long)]
    pub output: Option<PathBuf>,

    #[arg(short, long, value_delimiter = ',', value_enum)]
    pub emit: Vec<EmitKind>,

    /// Transaction name; required by `--emit tir-json`.
    #[arg(long)]
    pub tx: Option<String>,

    /// How analyzer diagnostics are rendered.
    #[arg(long, value_enum, default_value_t = DiagnosticsFormat::Human)]
    pub diagnostics_format: DiagnosticsFormat,

    #[arg(long)]
    pub protocol_scope: Option<String>,

    #[arg(long)]
    pub protocol_name: Option<String>,

    #[arg(long)]
    pub protocol_version: Option<String>,

    #[arg(long)]
    pub protocol_description: Option<String>,

    /// Per-profile env files (profile:path)
    #[arg(
        long = "profile-env-file",
        value_parser = profile_env_file,
        action = clap::ArgAction::Append
    )]
    pub profile_env_files: Vec<ProfileKV<PathBuf>>,

    /// Force inclusion of a profile in the TII output (even without env vars)
    #[arg(long = "profile", action = clap::ArgAction::Append)]
    pub profiles: Vec<String>,
}

pub fn run(args: Args) -> anyhow::Result<()> {
    let emit_tii = args.emit.contains(&EmitKind::Tii);
    let emit_tir_json = args.emit.contains(&EmitKind::TirJson);
    let needs_lower = emit_tii || emit_tir_json;
    let format = args.diagnostics_format;

    let mut ws = tx3_lang::Workspace::from_file(&args.source)?;

    // Parse + analyze (the front end). A parse failure is itself reported
    // through the diagnostics channel so JSON consumers get a single,
    // uniform contract regardless of which phase failed.
    if let Err(e) = ws.parse() {
        return diagnostics::report_fatal(format, e);
    }
    ws.analyze()?;

    let errors = ws
        .analisis()
        .map(|r| r.errors.clone())
        .unwrap_or_default();

    diagnostics::report_analysis(format, &errors)?;
    if !errors.is_empty() {
        // `report_analysis` already emitted JSON and (for JSON) exited; this
        // path is the human branch.
        anyhow::bail!("check failed with {} error(s)", errors.len());
    }

    // No artifact requested ⇒ this was a check; nothing left to do.
    if !needs_lower {
        return Ok(());
    }

    ws.lower()?;

    if emit_tir_json {
        let name = args
            .tx
            .as_deref()
            .ok_or_else(|| anyhow::anyhow!("--emit tir-json requires --tx <NAME>"))?;
        let tir = ws
            .tir(name)
            .ok_or_else(|| anyhow::anyhow!("transaction '{}' not found", name))?;
        println!("{}", serde_json::to_string(tir)?);
    }

    if emit_tii {
        tii::emit_tii(args, &ws)?;
    }

    Ok(())
}
