use std::{
    io::BufRead as _,
    path::{Path, PathBuf},
};

use crate::{analyzing, ast, parsing, ArgValue, Protocol};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Parsing error: {0}")]
    #[diagnostic(transparent)]
    Parsing(#[from] parsing::Error),

    #[error("Analyzing error")]
    Analyzing(#[from] analyzing::AnalyzeReport),

    #[error("Invalid environment file: {0}")]
    InvalidEnvFile(String),
}

use crate::cardano::load_externals;

/// Parses a Tx3 source file into a Program AST.
///
/// # Arguments
///
/// * `path` - Path to the Tx3 source file to parse
///
/// # Returns
///
/// * `Result<Program, Error>` - The parsed Program AST or an error
///
/// # Errors
///
/// Returns an error if:
/// - The file cannot be read
/// - The file contents are not valid Tx3 syntax
/// - The AST construction fails
///
/// # Example
///
/// ```no_run
/// use tx3_lang::loading::parse_file;
/// let program = parse_file("path/to/program.tx3").unwrap();
/// ```
pub fn parse_file(path: &str) -> Result<ast::Program, Error> {
    let input = std::fs::read_to_string(path)?;
    let mut program = parsing::parse_string(&input)?;
    // Should it be configurable by trix.toml? A path for imports like "../onchain" and all imports
    // would be really clean
    let base_path = std::path::Path::new(path)
        .parent()
        .unwrap_or(std::path::Path::new("."));
    process_imports(&mut program, base_path)?;
    Ok(program)
}

fn process_imports(program: &mut ast::Program, base_path: &Path) -> Result<(), Error> {
    for import in &program.imports {
        let full_path = base_path.join(&import.path);
        let path_str = full_path.to_str().ok_or_else(|| {
            Error::Io(std::io::Error::new(
                std::io::ErrorKind::InvalidInput,
                "Invalid path",
            ))
        })?;
        let external_types = load_externals(path_str)?;

        if let Some(ref mut scope) = program.scope {
            if let Some(scope_mut) = std::rc::Rc::get_mut(scope) {
                scope_mut.symbols.extend(external_types);
            }
        } else {
            program.scope = Some(std::rc::Rc::new(ast::Scope {
                symbols: external_types,
                parent: None,
            }));
        }
    }
    Ok(())
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
        let code = match (&self.code_file, &self.code_string) {
            (Some(file), None) => std::fs::read_to_string(file)?,
            (None, Some(code)) => code.clone(),
            _ => unreachable!(),
        };

        let mut ast = parsing::parse_string(&code)?;

        if let Some(file) = &self.code_file {
            let base_path = file.parent().unwrap_or(std::path::Path::new("."));
            process_imports(&mut ast, base_path)?;
        }

        if self.analyze {
            analyzing::analyze(&mut ast).ok()?;
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

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn smoke_test_parse_file() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let _ = parse_file(&format!("{}/../..//examples/transfer.tx3", manifest_dir)).unwrap();
    }

    #[test]
    fn test_cardano_import() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let path = format!("{}/../../examples/cip_imports.tx3", manifest_dir);
        let program = parse_file(&path).unwrap();

        assert!(program.scope.is_some());
        let scope = program.scope.as_ref().unwrap();

        assert!(scope.symbols.contains_key("Int"));
        assert!(scope.symbols.contains_key("AssetName"));
        assert!(scope.symbols.contains_key("Datum"));
    }
}
