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
pub mod applying;
pub mod ast;
pub mod backend;
pub mod ir;
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

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, Hash, PartialEq, Eq)]
pub struct UtxoRef {
    pub txid: Vec<u8>,
    pub index: u32,
}

impl UtxoRef {
    pub fn new(txid: &[u8], index: u32) -> Self {
        Self {
            txid: txid.to_vec(),
            index,
        }
    }
}

pub type AssetPolicy = Vec<u8>;
pub type AssetName = Vec<u8>;

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssetClass {
    Naked,
    Named(AssetName),
    Defined(AssetPolicy, AssetName),
}

impl AssetClass {
    pub fn is_defined(&self) -> bool {
        matches!(self, AssetClass::Defined(_, _))
    }

    pub fn is_named(&self) -> bool {
        matches!(self, AssetClass::Named(_))
    }

    pub fn is_naked(&self) -> bool {
        matches!(self, AssetClass::Naked)
    }

    pub fn policy(&self) -> Option<&[u8]> {
        match self {
            AssetClass::Defined(policy, _) => Some(policy),
            _ => None,
        }
    }

    pub fn name(&self) -> Option<&[u8]> {
        match self {
            AssetClass::Defined(_, name) => Some(name),
            AssetClass::Named(name) => Some(name),
            _ => None,
        }
    }
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct CanonicalAssets(HashMap<AssetClass, i128>);

impl std::ops::Deref for CanonicalAssets {
    type Target = HashMap<AssetClass, i128>;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

impl CanonicalAssets {
    pub fn empty() -> Self {
        Self(HashMap::new())
    }

    pub fn from_naked_amount(amount: i128) -> Self {
        Self(HashMap::from([(AssetClass::Naked, amount)]))
    }

    pub fn from_named_asset(asset_name: &[u8], amount: i128) -> Self {
        Self(HashMap::from([(
            AssetClass::Named(asset_name.to_vec()),
            amount,
        )]))
    }

    pub fn from_defined_asset(policy: &[u8], asset_name: &[u8], amount: i128) -> Self {
        Self(HashMap::from([(
            AssetClass::Defined(policy.to_vec(), asset_name.to_vec()),
            amount,
        )]))
    }

    pub fn from_asset(policy: Option<&[u8]>, name: Option<&[u8]>, amount: i128) -> Self {
        match (policy, name) {
            (Some(policy), Some(name)) => Self::from_defined_asset(policy, name, amount),
            (Some(policy), None) => Self::from_defined_asset(policy, &[], amount),
            (None, Some(name)) => Self::from_named_asset(name, amount),
            (None, None) => Self::from_naked_amount(amount),
        }
    }

    pub fn naked_amount(&self) -> Option<i128> {
        self.get(&AssetClass::Naked).cloned()
    }

    pub fn contains_total(&self, other: &Self) -> bool {
        for (class, amount) in other.iter() {
            let Some(self_amount) = self.get(class) else {
                return false;
            };

            if self_amount < amount {
                return false;
            }
        }

        true
    }

    pub fn contains_some(&self, other: &Self) -> bool {
        for (class, _) in other.iter() {
            if let Some(self_amount) = self.get(class) {
                if *self_amount > 0 {
                    return true;
                }
            }
        }

        false
    }

    pub fn is_empty_or_negative(&self) -> bool {
        for (_, value) in self.iter() {
            if *value > 0 {
                return false;
            }
        }

        true
    }
}

impl std::ops::Neg for CanonicalAssets {
    type Output = Self;

    fn neg(self) -> Self {
        let mut negated = self.0;

        for (_, value) in negated.iter_mut() {
            *value = -*value;
        }

        Self(negated)
    }
}

impl std::ops::Add for CanonicalAssets {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let mut aggregated = self.0;

        for (key, value) in other.0 {
            *aggregated.entry(key).or_default() += value;
        }

        Self(aggregated)
    }
}

impl std::ops::Sub for CanonicalAssets {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        let mut aggregated = self.0;

        for (key, value) in other.0 {
            *aggregated.entry(key).or_default() -= value;
        }

        Self(aggregated)
    }
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Utxo {
    pub r#ref: UtxoRef,
    pub address: Vec<u8>,
    pub datum: Option<ir::Expression>,
    pub assets: CanonicalAssets,
    pub script: Option<ir::Expression>,
}

impl std::hash::Hash for Utxo {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.r#ref.hash(state);
    }
}

impl PartialEq for Utxo {
    fn eq(&self, other: &Self) -> bool {
        self.r#ref == other.r#ref
    }
}

impl Eq for Utxo {}

pub type UtxoSet = HashSet<Utxo>;

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum ArgValue {
    Int(i128),
    Bool(bool),
    String(String),
    Bytes(Vec<u8>),
    Address(Vec<u8>),
    UtxoSet(UtxoSet),
    UtxoRef(UtxoRef),
}

impl From<Vec<u8>> for ArgValue {
    fn from(value: Vec<u8>) -> Self {
        Self::Bytes(value)
    }
}

impl From<String> for ArgValue {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for ArgValue {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<bool> for ArgValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

macro_rules! impl_from_int_for_arg_value {
    ($($t:ty),*) => {
        $(
            impl From<$t> for ArgValue {
                fn from(value: $t) -> Self {
                    Self::Int(value as i128)
                }
            }
        )*
    };
}

impl_from_int_for_arg_value!(i8, i16, i32, i64, i128, u8, u16, u32, u64, u128);

pub struct Protocol {
    pub(crate) ast: ast::Program,
    pub(crate) env_args: std::collections::HashMap<String, ArgValue>,
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

use std::collections::{HashMap, HashSet};

pub use applying::{apply_args, apply_fees, apply_inputs, find_params, find_queries, reduce};
use bincode::{Decode, Encode};
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct ProtoTx {
    ir: ir::Tx,
    args: std::collections::BTreeMap<String, ArgValue>,
    inputs: std::collections::BTreeMap<String, UtxoSet>,
    fees: Option<u64>,
}

impl From<ir::Tx> for ProtoTx {
    fn from(ir: ir::Tx) -> Self {
        Self {
            ir,
            args: std::collections::BTreeMap::new(),
            inputs: std::collections::BTreeMap::new(),
            fees: None,
        }
    }
}

impl From<ProtoTx> for ir::Tx {
    fn from(tx: ProtoTx) -> Self {
        tx.ir
    }
}

impl ProtoTx {
    pub fn find_params(&self) -> std::collections::BTreeMap<String, ir::Type> {
        find_params(&self.ir)
    }

    pub fn find_queries(&self) -> std::collections::BTreeMap<String, ir::InputQuery> {
        find_queries(&self.ir)
    }

    pub fn set_arg(&mut self, name: &str, value: ArgValue) {
        self.args.insert(name.to_lowercase().to_string(), value);
    }

    pub fn with_arg(mut self, name: &str, value: ArgValue) -> Self {
        self.args.insert(name.to_lowercase().to_string(), value);
        self
    }

    pub fn set_input(&mut self, name: &str, value: UtxoSet) {
        self.inputs.insert(name.to_lowercase().to_string(), value);
    }

    pub fn set_fees(&mut self, value: u64) {
        self.fees = Some(value);
    }

    pub fn apply(self) -> Result<Self, applying::Error> {
        let tx = apply_args(self.ir, &self.args)?;

        let tx = if let Some(fees) = self.fees {
            apply_fees(tx, fees)?
        } else {
            tx
        };

        let tx = apply_inputs(tx, &self.inputs)?;

        let tx = reduce(tx)?;

        Ok(tx.into())
    }

    pub fn ir_bytes(&self) -> Vec<u8> {
        let config = bincode::config::standard();
        bincode::encode_to_vec(&self.ir, config).unwrap()
    }

    pub fn from_ir_bytes(bytes: &[u8]) -> Result<Self, bincode::error::DecodeError> {
        let config = bincode::config::standard();
        let (ir, _) = bincode::decode_from_slice::<ir::Tx, _>(bytes, config)?;
        Ok(Self::from(ir))
    }

    pub fn inputs(&self) -> impl Iterator<Item = (&String, &UtxoSet)> {
        self.inputs.iter()
    }
}

impl AsRef<ir::Tx> for ProtoTx {
    fn as_ref(&self) -> &ir::Tx {
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
            .with_env_arg("sender", ArgValue::Address(b"sender".to_vec()))
            .load()
            .unwrap();

        let tx = protocol.new_tx("transfer").unwrap();

        dbg!(&tx.find_params());
        dbg!(&tx.find_queries());

        let mut tx = tx
            .with_arg("quantity", ArgValue::Int(100_000_000))
            .apply()
            .unwrap();

        dbg!(&tx.find_params());
        dbg!(&tx.find_queries());

        tx.set_input(
            "source",
            HashSet::from([Utxo {
                r#ref: UtxoRef {
                    txid: b"fafafafafafafafafafafafafafafafafafafafafafafafafafafafafafafafa"
                        .to_vec(),
                    index: 0,
                },
                address: b"abababa".to_vec(),
                datum: None,
                assets: CanonicalAssets::from_defined_asset(b"abababa", b"asset", 100),
                script: Some(ir::Expression::Bytes(b"abce".to_vec())),
            }]),
        );

        let tx = tx.apply().unwrap();

        dbg!(&tx.find_params());
        dbg!(&tx.find_queries());
    }
}
