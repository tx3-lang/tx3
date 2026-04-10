use std::collections::HashSet;

use crate::inputs::CanonicalQuery;

pub mod inputs;
pub mod interop;
pub mod job;
pub mod trp;

pub use job::resolve_tx;
pub use tx3_tir::model::assets::CanonicalAssets;
pub use tx3_tir::model::core::{Type, Utxo, UtxoRef, UtxoSet};

// TODO: we need to re-export this because some of the UtxoStore interface depends ond them, but this is tech debt. We should remove any dependency to versioned IR artifacts.
pub use tx3_tir::model::v1beta0::{Expression, StructExpr};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("can't compile non-constant tir")]
    CantCompileNonConstantTir,

    #[error(transparent)]
    CompileError(#[from] tx3_tir::compile::Error),

    #[error(transparent)]
    InteropError(#[from] interop::Error),

    #[error(transparent)]
    ReduceError(#[from] tx3_tir::reduce::Error),

    #[error("expected {0}, got {1:?}")]
    ExpectedData(String, tx3_tir::model::v1beta0::Expression),

    #[error("input query too broad")]
    InputQueryTooBroad,

    #[error("input not resolved: {0}")]
    InputNotResolved(String, CanonicalQuery, Vec<UtxoRef>),

    #[error("missing argument `{key}` of type {ty:?}")]
    MissingTxArg {
        key: String,
        ty: tx3_tir::model::core::Type,
    },

    #[error("transient error: {0}")]
    TransientError(String),

    #[error("store error: {0}")]
    StoreError(String),

    #[error("TIR encode / decode error: {0}")]
    TirEncodingError(#[from] tx3_tir::encoding::Error),

    #[error("tx was not accepted: {0}")]
    TxNotAccepted(String),

    #[error("tx script returned failure")]
    TxScriptFailure(Vec<String>),
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
    async fn narrow_refs(&self, pattern: UtxoPattern<'_>) -> Result<HashSet<UtxoRef>, Error>;
    async fn fetch_utxos(&self, refs: HashSet<UtxoRef>) -> Result<UtxoSet, Error>;
}
