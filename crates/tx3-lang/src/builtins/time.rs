//! Slot- and time-related built-ins.

use super::{Builtin, Signature};
use crate::ast::{self, BuiltinFn, Type};
use crate::lowering::{Context, Error as LowerError, IntoLower};

use tx3_tir::model::v1beta0 as ir;

/// `tip_slot() -> Int` — the chain tip slot at resolution time.
pub struct TipSlot;

impl Builtin for TipSlot {
    fn kind(&self) -> BuiltinFn {
        BuiltinFn::TipSlot
    }

    fn name(&self) -> &'static str {
        "tip_slot"
    }

    fn signature(&self) -> Signature {
        Signature {
            params: vec![],
            returns: Type::Int,
        }
    }

    fn lower_call(
        &self,
        _args: &[ast::DataExpr],
        _ctx: &Context,
    ) -> Result<ir::Expression, LowerError> {
        Ok(ir::Expression::EvalCompiler(Box::new(
            ir::CompilerOp::ComputeTipSlot,
        )))
    }
}

/// `slot_to_time(slot) -> Int` — convert a slot number to a POSIX time.
pub struct SlotToTime;

impl Builtin for SlotToTime {
    fn kind(&self) -> BuiltinFn {
        BuiltinFn::SlotToTime
    }

    fn name(&self) -> &'static str {
        "slot_to_time"
    }

    fn signature(&self) -> Signature {
        Signature {
            params: vec![("slot", Type::Int)],
            returns: Type::Int,
        }
    }

    fn lower_call(
        &self,
        args: &[ast::DataExpr],
        ctx: &Context,
    ) -> Result<ir::Expression, LowerError> {
        Ok(ir::Expression::EvalCompiler(Box::new(
            ir::CompilerOp::ComputeSlotToTime(args[0].into_lower(ctx)?),
        )))
    }
}

/// `time_to_slot(time) -> Int` — convert a POSIX time to a slot number.
pub struct TimeToSlot;

impl Builtin for TimeToSlot {
    fn kind(&self) -> BuiltinFn {
        BuiltinFn::TimeToSlot
    }

    fn name(&self) -> &'static str {
        "time_to_slot"
    }

    fn signature(&self) -> Signature {
        Signature {
            params: vec![("time", Type::Int)],
            returns: Type::Int,
        }
    }

    fn lower_call(
        &self,
        args: &[ast::DataExpr],
        ctx: &Context,
    ) -> Result<ir::Expression, LowerError> {
        Ok(ir::Expression::EvalCompiler(Box::new(
            ir::CompilerOp::ComputeTimeToSlot(args[0].into_lower(ctx)?),
        )))
    }
}
