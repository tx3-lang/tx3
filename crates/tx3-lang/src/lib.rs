//! The Tx3 language
//!
//! This crate provides the parser, analyzer and lowering logic for the Tx3
//! language.
//!
//! # Parsing
//!
//! ```
//! let program = tx3_lang::parsing::parse_string("tx swap() {}").unwrap();
//! ```
//!
//! # Analyzing
//!
//! ```
//! let mut program = tx3_lang::parsing::parse_string("tx swap() {}").unwrap();
//! tx3_lang::analyzing::analyze(&mut program).ok().unwrap();
//! ```
//!
//! # Lowering
//!
//! ```
//! let mut program = tx3_lang::parsing::parse_string("tx swap() {}").unwrap();
//! tx3_lang::analyzing::analyze(&mut program).ok().unwrap();
//! let ir = tx3_lang::lowering::lower(&program, "swap").unwrap();
//! ```

pub mod analyzing;
pub mod ast;
pub mod backend;
pub mod loading;
pub mod lowering;
pub mod parsing;

// chain specific
pub mod cardano;

#[macro_export]
macro_rules! include_tx3_build {
    ($package: tt) => {
        include!(concat!(env!("OUT_DIR"), concat!("/", $package, ".rs")));
    };
}

pub struct Protocol {
    pub(crate) ast: ast::Program,
    pub(crate) env_args: std::collections::HashMap<String, tir::reduce::ArgValue>,
}

impl Protocol {
    pub fn from_file(path: impl AsRef<std::path::Path>) -> loading::ProtocolLoader {
        loading::ProtocolLoader::from_file(path)
    }

    pub fn from_string(code: String) -> loading::ProtocolLoader {
        loading::ProtocolLoader::from_string(code)
    }

    pub fn new_tx(&self, template: &str) -> Result<ProtoTx, lowering::Error> {
        let ir = lowering::lower(&self.ast, template)?;
        let mut tx = ProtoTx::from(ir);

        if !self.env_args.is_empty() {
            for (k, v) in &self.env_args {
                tx.set_arg(k, v.clone());
            }
        }

        // TODO: merge lower and apply errors?
        let tx = tx.apply().unwrap();

        Ok(tx)
    }

    pub fn ast(&self) -> &ast::Program {
        &self.ast
    }

    pub fn txs(&self) -> impl Iterator<Item = &ast::TxDef> {
        self.ast.txs.iter()
    }
}

use tx3_tir as tir;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ProtoTx {
    ir: tir::model::v1beta0::Tx,
    args: std::collections::BTreeMap<String, tir::reduce::ArgValue>,
    inputs: std::collections::BTreeMap<String, tir::model::v1beta0::UtxoSet>,
    fees: Option<u64>,
}

impl From<tir::model::v1beta0::Tx> for ProtoTx {
    fn from(ir: tir::model::v1beta0::Tx) -> Self {
        Self {
            ir,
            args: std::collections::BTreeMap::new(),
            inputs: std::collections::BTreeMap::new(),
            fees: None,
        }
    }
}

impl From<ProtoTx> for tir::model::v1beta0::Tx {
    fn from(tx: ProtoTx) -> Self {
        tx.ir
    }
}

impl ProtoTx {
    pub fn find_params(&self) -> std::collections::BTreeMap<String, tir::model::v1beta0::Type> {
        tx3_tir::reduce::find_params(&self.ir)
    }

    pub fn find_queries(
        &self,
    ) -> std::collections::BTreeMap<String, tir::model::v1beta0::InputQuery> {
        tx3_tir::reduce::find_queries(&self.ir)
    }

    pub fn set_arg(&mut self, name: &str, value: tir::reduce::ArgValue) {
        self.args.insert(name.to_lowercase().to_string(), value);
    }

    pub fn with_arg(mut self, name: &str, value: tir::reduce::ArgValue) -> Self {
        self.args.insert(name.to_lowercase().to_string(), value);
        self
    }

    pub fn set_input(&mut self, name: &str, value: tir::model::v1beta0::UtxoSet) {
        self.inputs.insert(name.to_lowercase().to_string(), value);
    }

    pub fn set_fees(&mut self, value: u64) {
        self.fees = Some(value);
    }

    pub fn apply(self) -> Result<Self, tir::reduce::Error> {
        let tx = tir::reduce::apply_args(self.ir, &self.args)?;

        let tx = if let Some(fees) = self.fees {
            tir::reduce::apply_fees(tx, fees)?
        } else {
            tx
        };

        let tx = tir::reduce::apply_inputs(tx, &self.inputs)?;

        let tx = tir::reduce::reduce(tx)?;

        Ok(tx.into())
    }

    pub fn ir_bytes(&self) -> Vec<u8> {
        tir::interop::to_vec(&self.ir)
    }

    pub fn from_ir_bytes(bytes: &[u8]) -> Result<Self, tir::interop::Error> {
        let ir: tir::model::v1beta0::Tx = tir::interop::from_bytes(bytes)?;
        Ok(Self::from(ir))
    }
}

impl AsRef<tir::model::v1beta0::Tx> for ProtoTx {
    fn as_ref(&self) -> &tir::model::v1beta0::Tx {
        &self.ir
    }
}

#[cfg(test)]
mod tests {
    use std::collections::HashSet;

    use super::*;

    #[test]
    fn happy_path() {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let code = format!("{manifest_dir}/../../examples/transfer.tx3");

        let protocol = Protocol::from_file(&code)
            .with_env_arg("sender", tir::reduce::ArgValue::Address(b"sender".to_vec()))
            .load()
            .unwrap();

        let tx = protocol.new_tx("transfer").unwrap();

        dbg!(&tx.find_params());
        dbg!(&tx.find_queries());

        let mut tx = tx
            .with_arg("quantity", tir::reduce::ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

        dbg!(&tx.find_params());
        dbg!(&tx.find_queries());

        tx.set_input(
            "source",
            HashSet::from([tir::model::v1beta0::Utxo {
                r#ref: tir::model::v1beta0::UtxoRef {
                    txid: b"fafafafafafafafafafafafafafafafafafafafafafafafafafafafafafafafa"
                        .to_vec(),
                    index: 0,
                },
                address: b"abababa".to_vec(),
                datum: None,
                assets: tir::model::assets::CanonicalAssets::from_defined_asset(
                    b"abababa", b"asset", 100,
                ),
                script: Some(tir::model::v1beta0::Expression::Bytes(b"abce".to_vec())),
            }]),
        );

        let tx = tx.apply().unwrap();

        dbg!(&tx.find_params());
        dbg!(&tx.find_queries());
    }
}
