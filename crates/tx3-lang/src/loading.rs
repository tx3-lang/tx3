use std::{
    io::BufRead as _,
    path::{Path, PathBuf},
};

use crate::{analyzing, parsing, ArgValue, Protocol};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Parsing error: {0}")]
    Parsing(#[from] parsing::Error),

    #[error("Analyzing error: {0}")]
    Analyzing(#[from] analyzing::Error),

    #[error("Invalid environment file: {0}")]
    InvalidEnvFile(String),
}

pub type ArgMap = std::collections::HashMap<String, ArgValue>;

fn load_env_file(path: &Path) -> Result<ArgMap, Error> {
    let file = std::fs::File::open(path)?;
    let reader = std::io::BufReader::new(file);
    let mut env = std::collections::HashMap::new();

    for line in reader.lines() {
        let line = line?;
        let line = line.trim();

        // Skip empty lines and comments
        if line.is_empty() || line.starts_with('#') {
            continue;
        }

        // Split on first equals sign
        let mut parts = line.splitn(2, '=');

        let var_name = parts
            .next()
            .ok_or_else(|| Error::InvalidEnvFile("Missing variable name".into()))?
            .trim()
            .to_string();

        let var_value = parts
            .next()
            .ok_or_else(|| Error::InvalidEnvFile("Missing value".into()))?
            .trim()
            .to_string();

        env.insert(var_name, ArgValue::String(var_value));
    }

    Ok(env)
}

pub struct ProtocolLoader {
    code_file: Option<PathBuf>,
    code_string: Option<String>,
    env_file: Option<PathBuf>,
    env_args: std::collections::HashMap<String, ArgValue>,
    analyze: bool,
}

impl ProtocolLoader {
    pub fn from_file(file: impl AsRef<std::path::Path>) -> Self {
        Self {
            code_file: Some(file.as_ref().to_owned()),
            code_string: None,
            env_file: None,
            env_args: std::collections::HashMap::new(),
            analyze: true,
        }
    }

    pub fn from_string(code: String) -> Self {
        Self {
            code_file: None,
            code_string: Some(code),
            env_file: None,
            env_args: std::collections::HashMap::new(),
            analyze: true,
        }
    }

    pub fn with_env_file(mut self, env_file: PathBuf) -> Self {
        self.env_file = Some(env_file);
        self
    }

    pub fn with_env_arg(mut self, name: impl Into<String>, value: impl Into<ArgValue>) -> Self {
        self.env_args.insert(name.into(), value.into());
        self
    }

    pub fn skip_analyze(mut self) -> Self {
        self.analyze = false;
        self
    }

    pub fn load(self) -> Result<Protocol, Error> {
        let code = match (self.code_file, self.code_string) {
            (Some(file), None) => std::fs::read_to_string(file)?,
            (None, Some(code)) => code,
            _ => unreachable!(),
        };

        let mut ast = parsing::parse_string(&code)?;

        if self.analyze {
            analyzing::analyze(&mut ast)?;
        }

        let mut env_args = std::collections::HashMap::new();

        if let Some(env_file) = &self.env_file {
            let env = load_env_file(env_file)?;

            for (key, value) in env {
                env_args.insert(key, value);
            }
        }

        for (key, value) in self.env_args {
            env_args.insert(key, value);
        }

        let proto = Protocol { ast, env_args };

        Ok(proto)
    }
}
