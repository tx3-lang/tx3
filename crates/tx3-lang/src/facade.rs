use std::collections::{BTreeMap, HashMap};

use tx3_tir::reduce::{Apply, ArgValue};

use crate::{analyzing, ast, lowering, parsing};

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
pub enum Error {
    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("Missing main code")]
    MissingMain,

    #[error("Parsing error: {0}")]
    #[diagnostic(transparent)]
    Parsing(#[from] parsing::Error),

    #[error("Analyzing error")]
    Analyzing(#[from] analyzing::AnalyzeReport),

    #[error("Apply error: {0}")]
    Apply(#[from] tx3_tir::reduce::Error),
}

pub type Code = String;

pub struct Workspace {
    main: Option<Code>,
    ast: Option<ast::Program>,
    analisis: Option<analyzing::AnalyzeReport>,
    tir: HashMap<String, tx3_tir::model::v1beta0::Tx>,
}

impl Workspace {
    pub fn from_file(main: impl AsRef<std::path::Path>) -> Result<Self, Error> {
        let main = std::fs::read_to_string(main.as_ref())?;

        Ok(Self {
            main: Some(main),
            ast: None,
            analisis: None,
            tir: HashMap::new(),
        })
    }

    pub fn from_string(main: Code) -> Self {
        Self {
            main: Some(main),
            ast: None,
            analisis: None,
            tir: HashMap::new(),
        }
    }

    fn ensure_main(&self) -> Result<&Code, Error> {
        if self.main.is_none() {
            return Err(Error::MissingMain);
        }

        Ok(self.main.as_ref().unwrap())
    }

    pub fn parse(&mut self) -> Result<(), Error> {
        let main = self.ensure_main()?;
        let ast = parsing::parse_string(main)?;
        self.ast = Some(ast);
        Ok(())
    }

    fn ensure_ast(&mut self) -> Result<(), Error> {
        if self.ast.is_none() {
            self.parse()?;
        }

        Ok(())
    }

    pub fn ast(&self) -> Option<&ast::Program> {
        self.ast.as_ref()
    }

    pub fn analyze(&mut self) -> Result<(), Error> {
        self.ensure_ast()?;

        let ast = self.ast.as_mut().unwrap();

        self.analisis = Some(analyzing::analyze(ast));

        Ok(())
    }

    pub fn ensure_analisis(&mut self) -> Result<(), Error> {
        if self.analisis.is_none() {
            self.analyze()?;
        }

        Ok(())
    }

    pub fn analisis(&self) -> Option<&analyzing::AnalyzeReport> {
        self.analisis.as_ref()
    }

    pub fn lower(&mut self) -> Result<(), Error> {
        self.ensure_analisis()?;

        let analisis = self.analisis().unwrap();

        if !analisis.errors.is_empty() {
            return Err(Error::from(analisis.clone()));
        }

        let ast = self.ast.as_ref().unwrap();

        for tx in ast.txs.iter() {
            let tir = lowering::lower(ast, &tx.name.value).unwrap();
            self.tir.insert(tx.name.value.clone(), tir);
        }

        Ok(())
    }

    pub fn ensure_tir(&mut self) -> Result<(), Error> {
        if self.tir.is_empty() {
            self.lower()?;
        }

        Ok(())
    }

    pub fn tir(&self, name: &str) -> Option<&tx3_tir::model::v1beta0::Tx> {
        self.tir.get(name)
    }

    pub fn apply_args(&mut self, args: &BTreeMap<String, ArgValue>) -> Result<(), Error> {
        self.ensure_tir()?;

        let values = self.tir.drain();
        let mut new_tir = HashMap::new();

        for (key, tir) in values {
            let tir = tir.apply_args(args)?;
            new_tir.insert(key, tir);
        }

        self.tir = new_tir;

        Ok(())
    }
}

#[cfg(test)]
pub mod tests {
    use super::*;

    #[test]
    fn smoke_test_happy_path() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");

        let mut workspace =
            Workspace::from_file(&format!("{manifest_dir}/../..//examples/transfer.tx3")).unwrap();

        workspace.parse().unwrap();
        workspace.analyze().unwrap();
        workspace.lower().unwrap();
    }
}
