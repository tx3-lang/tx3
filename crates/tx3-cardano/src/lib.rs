use std::collections::HashMap;

pub mod coercion;
pub mod compile;
pub mod ledgers;
pub mod resolve;

// Re-export pallas for upstream users
pub use pallas;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("error coercing {0} into {1}")]
    CoerceError(String, String),

    #[error("invalid asset expression '{0}'")]
    InvalidAssetExpression(String),

    #[error("missing address")]
    MissingAddress,

    #[error("invalid address expression '{0}'")]
    InvalidAddressExpression(String),

    #[error("ledger internal error: {0}")]
    LedgerInternalError(String),

    #[error("tx '{0}' not found")]
    TxNotFound(String),

    #[error("definition '{0}' not found")]
    DefinitionNotFound(String),

    #[error("party '{0}' not assigned")]
    PartyNotAssigned(String),

    #[error("arg '{0}' not assigned")]
    ArgNotAssigned(String),

    #[error("invalid address")]
    InvalidAddress(#[from] pallas::ledger::addresses::Error),

    #[error("no stake account found")]
    NoStakeAccount,

    #[error("mapping error {0}")]
    MappingError(String),

    #[error("missing input utxo")]
    MissingInputUtxo,

    #[error("missing amount")]
    MissingAmount,

    #[error("missing asset name")]
    MissingAssetName,

    #[error("missing asset")]
    MissingAsset,

    #[error("missing minting policy")]
    MissingMintingPolicy,

    #[error("missing withdrawal")]
    MissingWithdrawal,

    #[error("missing redeemer")]
    MissingRedeemer,

    #[error("input query too broad")]
    InputQueryTooBroad,

    #[error("asset value too high")]
    AssetValueTooHigh,

    #[error("outputs too high")]
    OutputsTooHigh,

    #[error("error applying tx values")]
    ApplyError(#[from] tx3_lang::applying::Error),

    #[error("no AST analysis performed")]
    NoAstAnalysis,

    #[error("inputs for '{0}' not resolved, query: {1:?}")]
    InputsNotResolved(String, Box<tx3_lang::ir::InputQuery>),

    #[error("can't resolve symbol '{0:?}'")]
    CantResolveSymbol(Box<tx3_lang::ast::Symbol>),

    #[error("max optimize rounds reached")]
    MaxOptimizeRoundsReached,

    #[error("can't compile non-constant TIR")]
    CantCompileNonConstantTir,
}

pub type Network = pallas::ledger::primitives::NetworkId;
pub type PlutusVersion = u8;
pub type CostModel = Vec<i64>;

pub struct PParams {
    pub network: Network,
    pub min_fee_coefficient: u64,
    pub min_fee_constant: u64,
    pub coins_per_utxo_byte: u64,
    pub cost_models: HashMap<PlutusVersion, CostModel>,
}

pub use compile::compile_tx;
use pallas::ledger::primitives;
pub use resolve::resolve_tx;
pub use resolve::Ledger;

pub const EXECUTION_UNITS: primitives::ExUnits = primitives::ExUnits {
    mem: 2000000,
    steps: 2000000000,
};
