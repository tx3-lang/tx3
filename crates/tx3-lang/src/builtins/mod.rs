//! Compiler-provided built-in functions.
//!
//! Each built-in is a zero-sized type implementing [`Builtin`], which gathers
//! its signature, its synthetic [`ast::FnDef`], and its lowering in one place,
//! so adding a built-in is a localized change rather than edits scattered
//! across the analyzer and lowerer.
//!
//! [`ast::BuiltinFn`] is the serializable key carried on `FnDef`; [`resolve`]
//! maps a key to its implementation and is exhaustive, so a new `BuiltinFn`
//! variant will not compile until it is registered here.

use crate::ast::{self, BuiltinFn, FnDef, Identifier, ParamDef, ParameterList, Span, Type};
use crate::lowering::{Context, Error as LowerError};

use tx3_tir::model::v1beta0 as ir;

mod time;
mod value;

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

/// Map a built-in key to its implementation. Exhaustive by construction.
pub fn resolve(kind: BuiltinFn) -> &'static dyn Builtin {
    match kind {
        BuiltinFn::MinUtxo => &value::MinUtxo,
        BuiltinFn::TipSlot => &time::TipSlot,
        BuiltinFn::SlotToTime => &time::SlotToTime,
        BuiltinFn::TimeToSlot => &time::TimeToSlot,
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
            assert!(names.insert(b.name()), "duplicate built-in name: {}", b.name());
        }
    }
}
