use crate::{model::v1beta0, Visitor};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("error coercing {0} into {1}")]
    CoerceError(String, String),

    #[error("format error {0}")]
    FormatError(String),

    #[error("missing expression: {0}")]
    MissingExpression(String),

    #[error("consistency error: {0}")]
    ConsistencyError(String),
}

#[derive(Debug, PartialEq)]
pub struct CompiledTx {
    pub payload: Vec<u8>,
    pub hash: Vec<u8>,
    pub fee: u64,
    pub ex_units: u64,
}

pub trait Compiler {
    type EntryPoint;
    type CompilerOp;
    type Expression;

    fn compile(&mut self, tx: &Self::EntryPoint) -> Result<CompiledTx, Error>;
    fn reduce_op(&self, op: Self::CompilerOp) -> Result<Self::Expression, crate::reduce::Error>;
}

impl<C> Visitor for C
where
    C: Compiler<Expression = v1beta0::Expression, CompilerOp = v1beta0::CompilerOp>,
{
    fn reduce(
        &mut self,
        expr: v1beta0::Expression,
    ) -> Result<v1beta0::Expression, crate::reduce::Error> {
        match expr {
            v1beta0::Expression::EvalCompiler(op) => Ok(self.reduce_op(*op)?),
            _ => Ok(expr),
        }
    }
}
