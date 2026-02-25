use clap::Parser;
use std::path::PathBuf;

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
    let mut ws = tx3_lang::Workspace::from_file(&args.source)?;

    ws.parse()?;
    ws.analyze()?;
    ws.lower()?;

    if args.emit.contains(&"tii".to_string()) {
        tii::emit_tii(args, &ws)?;
    }

    Ok(())
}
