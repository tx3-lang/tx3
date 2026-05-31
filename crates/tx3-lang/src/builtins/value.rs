//! Value- and asset-related built-ins.

use super::{Builtin, Signature};
use crate::ast::{self, BuiltinFn, Type};
use crate::lowering::{Context, Error as LowerError, IntoLower};

use tx3_tir::model::v1beta0 as ir;

/// `min_utxo(output) -> AnyAsset` — the minimum value the named output needs to
/// satisfy the chain's min-UTxO rule.
pub struct MinUtxo;

impl Builtin for MinUtxo {
    fn kind(&self) -> BuiltinFn {
        BuiltinFn::MinUtxo
    }

    fn name(&self) -> &'static str {
        "min_utxo"
    }

    fn signature(&self) -> Signature {
        Signature {
            params: vec![("output", Type::Int)],
            returns: Type::AnyAsset,
        }
    }

    fn lower_call(
        &self,
        args: &[ast::DataExpr],
        ctx: &Context,
    ) -> Result<ir::Expression, LowerError> {
        Ok(ir::Expression::EvalCompiler(Box::new(
            ir::CompilerOp::ComputeMinUtxo(args[0].into_lower(ctx)?),
        )))
    }
}
