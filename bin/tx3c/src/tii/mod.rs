use anyhow::{anyhow, Context};
use schemars::Schema;
use serde_json::json;
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    path::PathBuf,
};

pub mod schema;
pub mod types;

use tx3_lang::ast;
pub use types::*;

use schema::{infer_env_schema, infer_tx_params_schema, SchemaCtx};

use crate::build::Args;

fn infer_available_profiles(args: &Args) -> HashSet<String> {
    let mut profiles = HashSet::new();

    for entry in args.profile_env_files.iter() {
        profiles.insert(entry.profile.clone());
    }

    // Add profiles from --profile flag (forced inclusion)
    for profile in args.profiles.iter() {
        profiles.insert(profile.clone());
    }

    profiles
}

// This is not to be meant as a comprehensive conversion between String -> TIR, it's just a
// mapping between the string representation of a value in a dotfile to its JSON representation.
//
// The only casess that mappers are those where there's a natural JSON representation (number and boolean so far).
//
// The strict mapping of values onto TIR artifacts ocurrs in the tx3-sdk at _resolve-time_.
fn map_dotfile_value_to_json(value: &str, target: &ast::Type) -> serde_json::Value {
    match target {
        ast::Type::Int => {
            if let Ok(i) = value.parse::<i128>() {
                json!(i)
            } else {
                json!(value)
            }
        }
        ast::Type::Bool => {
            if let Ok(b) = value.parse::<bool>() {
                json!(b)
            } else {
                json!(value)
            }
        }
        _ => json!(value),
    }
}

fn infer_environment_values_from_dotfile(
    env: &BTreeMap<String, String>,
    ast: &ast::Program,
) -> serde_json::Value {
    let mut obj = serde_json::Map::new();

    if let Some(def) = &ast.env {
        for field in def.fields.iter() {
            let key = field.name.clone();

            if let Some(value) = env.get(key.as_str()) {
                obj.insert(key, map_dotfile_value_to_json(value, &field.r#type));
            }
        }
    }

    serde_json::Value::Object(obj)
}

fn infer_party_values_from_dotfile(
    env: &BTreeMap<String, String>,
    ast: &ast::Program,
) -> serde_json::Value {
    let mut obj = serde_json::Map::new();

    for party in &ast.parties {
        let key = party.name.value.to_lowercase();
        if let Some(value) = env.get(key.as_str()) {
            obj.insert(key, json!(value));
        }
    }

    serde_json::Value::Object(obj)
}

fn load_dotfile(path: Option<&PathBuf>) -> anyhow::Result<BTreeMap<String, String>> {
    let Some(path) = path else {
        return Ok(BTreeMap::new());
    };

    let source = std::fs::read_to_string(path).context("loading env file")?;

    let env = dotenv_parser::parse_dotenv(&source)
        .map_err(|e| anyhow!("Failed to parse env file: {}", e))?;

    let env = env
        .into_iter()
        .map(|(k, v)| (k.to_lowercase(), v))
        .collect();

    Ok(env)
}

pub fn emit_tii(args: Args, ws: &tx3_lang::Workspace) -> anyhow::Result<()> {
    let ast = ws.ast().ok_or(anyhow!("Failed to get AST"))?;

    // Accumulates custom-type schemas referenced by env + params, emitted under
    // `components.schemas`.
    let mut ctx = SchemaCtx::new(ast);

    let env_schema = infer_env_schema(&mut ctx, ast);

    let mut tii = TiiFile {
        tii: TiiInfo {
            version: TII_VERSION.to_string(),
        },
        protocol: Protocol {
            scope: args.protocol_scope.clone().unwrap_or("unknown".to_string()),
            name: args.protocol_name.clone().unwrap_or("unknown".to_string()),
            version: args.protocol_version.clone().unwrap_or("0.0.1".to_string()),
            description: args.protocol_description.clone(),
        },
        environment: Some(env_schema),
        parties: HashMap::new(),
        transactions: HashMap::new(),
        profiles: HashMap::new(),
        components: None,
    };

    for party in ast.parties.iter() {
        tii.parties.insert(
            party.name.value.to_lowercase(),
            Party {
                description: party.docstring.clone(),
            },
        );
    }

    for tx in ast.txs.iter() {
        let tir = ws
            .tir(&tx.name.value)
            .ok_or_else(|| anyhow!("Failed to get TIR for transaction: {}", tx.name.value))?;

        let params_schema = infer_tx_params_schema(&mut ctx, tx);

        // Convert TIR to bytes
        let (bytes, version) = tx3_tir::encoding::to_bytes(tir);

        // Hex-encode the bytes
        let hex_string = hex::encode(&bytes);

        // Add to output map with transaction name as key
        tii.transactions.insert(
            tx.name.value.clone(),
            Transaction {
                description: tx.docstring.clone(),
                tir: TirEnvelope {
                    content: hex_string,
                    encoding: BytesEncoding::Hex,
                    version: version.to_string(),
                },
                params: params_schema,
            },
        );
    }

    if !ctx.schemas.is_empty() {
        let schemas = ctx
            .schemas
            .into_iter()
            .map(|(name, value)| Ok((name, serde_json::from_value(value)?)))
            .collect::<anyhow::Result<HashMap<String, Schema>>>()?;
        tii.components = Some(Components { schemas });
    }

    for profile in infer_available_profiles(&args) {
        let env_file = args
            .profile_env_files
            .iter()
            .find(|entry| entry.profile == profile);

        let dotfile = load_dotfile(env_file.map(|entry| &entry.value))?;

        let environment = infer_environment_values_from_dotfile(&dotfile, ast);
        let parties = infer_party_values_from_dotfile(&dotfile, ast);

        tii.profiles.insert(
            profile,
            Profile {
                description: None,
                environment,
                parties,
            },
        );
    }

    // Temp: Ensure there's always a "local" profile
    if !tii.profiles.contains_key("local") {
        tii.profiles.insert(
            "local".to_string(),
            Profile {
                description: None,
                environment: json!({}),
                parties: json!({}),
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
