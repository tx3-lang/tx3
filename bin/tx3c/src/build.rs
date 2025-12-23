use anyhow::{anyhow, Context};
use clap::Parser;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::PathBuf,
};
use tx3_tir::reduce::ArgValue;

use serde_json::json;

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

#[derive(Parser)]
pub struct Args {
    pub source: PathBuf,

    #[arg(short, long)]
    pub output: Option<PathBuf>,

    #[arg(short, long, value_delimiter = ',')]
    pub emit: Vec<String>,

    #[arg(long)]
    pub apply_env_file: Option<PathBuf>,

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
    profile_env_files: Vec<ProfileKV<PathBuf>>,
}

fn load_env_file(path: Option<&PathBuf>) -> anyhow::Result<BTreeMap<String, ArgValue>> {
    let Some(path) = path else {
        return Ok(BTreeMap::new());
    };

    let source = std::fs::read_to_string(path).context("loading env file")?;

    let env = dotenv_parser::parse_dotenv(&source)
        .map_err(|e| anyhow!("Failed to parse env file: {}", e))?;

    let env = env.into_iter().map(|(k, v)| (k, v.into())).collect();

    Ok(env)
}

fn infer_available_profiles(args: &Args) -> HashSet<String> {
    let mut profiles = HashSet::new();

    for entry in args.profile_env_files.iter() {
        profiles.insert(entry.profile.clone());
    }

    // TODO: inspect other per-profile args in case there's other profiles
    // defined

    profiles
}

fn emit_tii(args: Args, ws: &tx3_lang::Workspace) -> anyhow::Result<()> {
    let ast = ws.ast().ok_or(anyhow!("Failed to get AST"))?;

    let env_schema = tii::infer_env_schema(&ast);

    let mut tii = tii::TiiFile {
        tii: tii::TiiInfo {
            version: tii::TII_VERSION.to_string(),
        },
        protocol: tii::Protocol {
            scope: args.protocol_scope.clone().unwrap_or("unknown".to_string()),
            name: args.protocol_name.clone().unwrap_or("unknown".to_string()),
            version: args.protocol_version.clone().unwrap_or("0.0.1".to_string()),
            description: args.protocol_description.clone(),
            environment: Some(env_schema),
        },
        transactions: HashMap::new(),
        profiles: HashMap::new(),
        components: None,
    };

    for tx in ast.txs.iter() {
        let tir = ws
            .tir(&tx.name.value)
            .ok_or_else(|| anyhow!("Failed to get TIR for transaction: {}", tx.name.value))?;

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
                    version: tx3_tir::model::v1beta0::IR_VERSION.to_string(),
                },
                params: params_schema,
            },
        );
    }

    for profile in infer_available_profiles(&args) {
        let env_file = args
            .profile_env_files
            .iter()
            .find(|entry| entry.profile == profile);

        let env = load_env_file(env_file.map(|entry| &entry.value))?;

        let env_json = tx3_tir::interop::json::to_json_object(env);

        tii.profiles.insert(
            profile,
            tii::Profile {
                description: None,
                environment: env_json,
            },
        );
    }

    let output_json = json!(tii);

    let output_path = args
        .output
        .unwrap_or_else(|| args.source.with_extension("tii"));

    std::fs::write(&output_path, serde_json::to_string_pretty(&output_json)?)?;

    println!(
        "Compiled {} to {}",
        args.source.display(),
        output_path.display()
    );

    Ok(())
}

pub fn run(args: Args) -> anyhow::Result<()> {
    let mut ws = tx3_lang::Workspace::from_file(&args.source)?;

    ws.parse()?;
    ws.analyze()?;
    ws.lower()?;

    let apply_env = load_env_file(args.apply_env_file.as_ref())?;

    ws.apply_args(&apply_env)?;

    if args.emit.contains(&"tii".to_string()) {
        emit_tii(args, &ws)?;
    }

    Ok(())
}
