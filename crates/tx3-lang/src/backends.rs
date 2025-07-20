use std::collections::HashSet;

use crate::{ir, UtxoRef, UtxoSet};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("transient error: {0}")]
    TransientError(String),

    #[error("invalid pattern: {0}")]
    InvalidPattern(String),

    #[error("utxo not found: {0:?}")]
    UtxoNotFound(UtxoRef),

    #[error("error coercing {0} into {1}")]
    CoerceError(String, String),

    #[error("consistency error: {0}")]
    ConsistencyError(String),

    #[error("arg '{0}' not assigned")]
    ArgNotAssigned(String),

    #[error("format error {0}")]
    FormatError(String),

    #[error("missing expression: {0}")]
    MissingExpression(String),

    #[error("value overflow: {0}")]
    ValueOverflow(String),

    #[error("no AST analysis performed")]
    NoAstAnalysis,

    #[error("can't resolve symbol '{0}'")]
    CantResolveSymbol(String),
}

#[derive(Debug, PartialEq)]
pub struct TxEval {
    pub payload: Vec<u8>,
    pub hash: Vec<u8>,
    pub fee: u64,
    pub ex_units: u64,
}

pub trait Compiler {
    fn compile(&self, tx: &ir::Tx) -> Result<TxEval, Error>;
}

pub enum UtxoPattern<'a> {
    ByAddress(&'a [u8]),
    ByAssetPolicy(&'a [u8]),
    ByAsset(&'a [u8], &'a [u8]),
}

impl<'a> UtxoPattern<'a> {
    pub fn by_address(address: &'a [u8]) -> Self {
        Self::ByAddress(address)
    }

    pub fn by_asset_policy(policy: &'a [u8]) -> Self {
        Self::ByAssetPolicy(policy)
    }

    pub fn by_asset(policy: &'a [u8], name: &'a [u8]) -> Self {
        Self::ByAsset(policy, name)
    }
}

#[trait_variant::make(Send)]
pub trait UtxoStore {
    async fn narrow_refs(&self, pattern: UtxoPattern) -> Result<HashSet<UtxoRef>, Error>;
    async fn fetch_utxos(&self, refs: HashSet<UtxoRef>) -> Result<UtxoSet, Error>;
}
