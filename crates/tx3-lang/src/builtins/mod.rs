//! Compiler-provided built-in functions.
//!
//! Each built-in is a zero-sized type implementing [`Builtin`], which gathers
//! its signature, its synthetic [`ast::FnDef`], and its lowering in one place.
//! [`ast::BuiltinFn`] is the serializable key carried on `FnDef`; [`resolve`]
//! maps a key to its implementation and is exhaustive, so a new `BuiltinFn`
//! variant will not compile until it is registered here.
//!
//! Built-ins whose lowering is "lower the arguments and feed them to a
//! [`ir::CompilerOp`]" are declared with the [`builtin_boilerplate!`] table
//! below. A built-in that needs custom analysis or lowering can instead be a
//! hand-written `impl Builtin`; both kinds coexist behind [`resolve`].

use crate::ast::{self, BuiltinFn, FnDef, Identifier, ParamDef, ParameterList, Span, Type};
use crate::lowering::{Context, Error as LowerError, IntoLower};

use tx3_tir::model::v1beta0 as ir;

/// The type signature of a built-in: named parameters and a return type.
pub struct Signature {
    pub params: Vec<(&'static str, Type)>,
    pub returns: Type,
}

/// A compiler-provided function. One implementor per [`BuiltinFn`] variant.
pub trait Builtin: Sync {
    /// The serializable key this built-in is registered and resolved under.
    fn kind(&self) -> BuiltinFn;

    /// The call-site name (e.g. `"min_utxo"`).
    fn name(&self) -> &'static str;

    /// Parameter and return types.
    fn signature(&self) -> Signature;

    /// Lower a call to this built-in into its compiler operation. Built-in
    /// calls are evaluated by the resolver, not inlined like user functions.
    fn lower_call(
        &self,
        args: &[ast::DataExpr],
        ctx: &Context,
    ) -> Result<ir::Expression, LowerError>;

    /// The synthetic, body-less `FnDef` registered into the program scope so
    /// that calls resolve against a real signature.
    fn definition(&self) -> FnDef {
        let sig = self.signature();

        FnDef {
            name: Identifier::new(self.name()),
            parameters: ParameterList {
                parameters: sig
                    .params
                    .into_iter()
                    .map(|(name, r#type)| ParamDef {
                        name: Identifier::new(name),
                        r#type,
                        docstring: None,
                    })
                    .collect(),
                span: Span::DUMMY,
            },
            return_type: sig.returns,
            body: None,
            builtin: Some(self.kind()),
            span: Span::DUMMY,
            scope: None,
        }
    }
}

/// Declare a built-in whose lowering is "bind each named parameter to its
/// lowered argument, then construct an [`ir::CompilerOp`]".
///
/// ```text
/// builtin_boilerplate!(Variant, "name", (param: Type, …) -> ReturnType => CompilerOp(param, …));
/// ```
///
/// The operation references the parameters by name, so the names are checked
/// against the signature at compile time. Parameter and return types must be
/// bare [`ast::Type`] variant idents (e.g. `Int`, `AnyAsset`); a built-in that
/// needs a richer type or non-`CompilerOp` lowering is written by hand instead.
macro_rules! builtin_boilerplate {
    (
        $variant:ident,
        $name:literal,
        ( $( $param:ident : $pty:ident ),* $(,)? ) -> $ret:ident
        => $op:ident $(( $( $oparg:ident ),* ))?
    ) => {
        pub struct $variant;

        impl Builtin for $variant {
            fn kind(&self) -> BuiltinFn {
                BuiltinFn::$variant
            }

            fn name(&self) -> &'static str {
                $name
            }

            fn signature(&self) -> Signature {
                Signature {
                    params: vec![ $( (stringify!($param), Type::$pty) ),* ],
                    returns: Type::$ret,
                }
            }

            // A nullary built-in (e.g. `tip_slot`) uses neither `args` nor
            // `ctx`; allow that so every invocation expands uniformly.
            #[allow(unused_mut, unused_variables)]
            fn lower_call(
                &self,
                args: &[ast::DataExpr],
                ctx: &Context,
            ) -> Result<ir::Expression, LowerError> {
                let mut args = args.iter();
                $(
                    let $param = args
                        .next()
                        .ok_or_else(|| LowerError::InvalidAst(format!(
                            "built-in '{}' expects argument '{}'",
                            $name,
                            stringify!($param),
                        )))?
                        .lower(ctx)?;
                )*
                Ok(ir::Expression::EvalCompiler(Box::new(
                    ir::CompilerOp::$op $(( $( $oparg ),* ))?
                )))
            }
        }
    };
}

builtin_boilerplate!(MinUtxo, "min_utxo", (output: Int) -> AnyAsset => ComputeMinUtxo(output));
builtin_boilerplate!(TipSlot, "tip_slot", () -> Int => ComputeTipSlot);
builtin_boilerplate!(SlotToTime, "slot_to_time", (slot: Int) -> Int => ComputeSlotToTime(slot));
builtin_boilerplate!(TimeToSlot, "time_to_slot", (time: Int) -> Int => ComputeTimeToSlot(time));

/// Map a built-in key to its implementation. Exhaustive by construction.
pub fn resolve(kind: BuiltinFn) -> &'static dyn Builtin {
    match kind {
        BuiltinFn::MinUtxo => &MinUtxo,
        BuiltinFn::TipSlot => &TipSlot,
        BuiltinFn::SlotToTime => &SlotToTime,
        BuiltinFn::TimeToSlot => &TimeToSlot,
    }
}

/// Every built-in, in [`BuiltinFn::ALL`] order.
pub fn all() -> impl Iterator<Item = &'static dyn Builtin> {
    BuiltinFn::ALL.into_iter().map(resolve)
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashSet;

    #[test]
    fn registry_is_consistent() {
        let mut names = HashSet::new();
        for kind in BuiltinFn::ALL {
            let b = resolve(kind);
            // `resolve` returns the implementation for the requested key.
            assert_eq!(b.kind(), kind);
            // The synthetic definition is tagged with the same key and matches
            // the declared signature.
            let def = b.definition();
            assert_eq!(def.builtin, Some(kind));
            assert_eq!(def.name.value, b.name());
            assert_eq!(def.parameters.parameters.len(), b.signature().params.len());
            // Names are unique across the set.
            assert!(
                names.insert(b.name()),
                "duplicate built-in name: {}",
                b.name()
            );
        }
    }
}
