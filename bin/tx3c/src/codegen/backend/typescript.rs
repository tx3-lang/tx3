use convert_case::Case;

use super::{tuple_as_fallback_list, Backend, FieldOrder, Placement};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration},
    schema::{Builtin, Scalar, Shape},
};

pub struct TypeScript;

impl Backend for TypeScript {
    fn language(&self) -> &'static str {
        "typescript"
    }

    fn display_name(&self) -> &'static str {
        "TypeScript"
    }

    fn scalar(&self, scalar: Scalar) -> &'static str {
        match scalar {
            Scalar::Boolean => "boolean",
            Scalar::Integer => "number",
            Scalar::String => "string",
            Scalar::Null => "null",
        }
    }

    fn builtin(&self, builtin: Builtin) -> &'static str {
        match builtin {
            Builtin::Bytes => "Uint8Array",
            Builtin::Address | Builtin::UtxoRef | Builtin::AnyAsset => "string",
            Builtin::Utxo => "unknown",
        }
    }

    fn fallback(&self) -> &'static str {
        "any"
    }

    fn list(&self, item: &str) -> String {
        format!("Array<{item}>")
    }

    fn map(&self, value: &str) -> String {
        format!("Record<string, {value}>")
    }

    fn undeclared(&self, shape: &Shape) -> String {
        tuple_as_fallback_list(self, shape)
    }

    /// Record fields keep their wire names because records are sent as-is.
    fn naming(&self, role: Role) -> Option<Case> {
        match role {
            Role::Type => Some(Case::Pascal),
            Role::Field => None,
            Role::Param => Some(Case::Camel),
            Role::Case => Some(Case::Pascal),
        }
    }

    fn placement(&self) -> Placement {
        Placement::None
    }

    fn field_order(&self) -> FieldOrder {
        FieldOrder::Alphabetical
    }

    fn declaration(&self, declaration: &Declaration) -> String {
        let name = &declaration.name;
        match &declaration.kind {
            DeclKind::Record(fields) => {
                let body: String = fields
                    .iter()
                    .map(|field| format!("    {}: {};\n", field.name, field.ty))
                    .collect();
                format!("export type {name} = {{\n{body}}};\n")
            }
            DeclKind::Variant(_) => format!(
                "// TODO: tagged-union codegen pending the variant arg encoder\nexport type {name} = unknown;\n"
            ),
            DeclKind::Tuple(_) | DeclKind::Alias(_) => format!("export type {name} = {{\n}};\n"),
        }
    }
}
