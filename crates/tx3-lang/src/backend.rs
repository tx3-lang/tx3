use std::collections::HashSet;

use tx3_tir as ir;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("transient error: {0}")]
    TransientError(String),

    #[error("store error: {0}")]
    StoreError(String),

    #[error("invalid pattern: {0}")]
    InvalidPattern(String),

    #[error("utxo not found: {0:?}")]
    UtxoNotFound(ir::model::v1beta0::UtxoRef),

    #[error("arg '{0}' not assigned")]
    ArgNotAssigned(String),

    #[error("value overflow: {0}")]
    ValueOverflow(String),

    #[error("no AST analysis performed")]
    NoAstAnalysis,

    #[error("can't resolve symbol '{0}'")]
    CantResolveSymbol(String),

    #[error("can't reduce {0:?}")]
    CantReduce(ir::model::v1beta0::CompilerOp),
}
