use convert_case::Case;

use super::{tuple_as_fallback_list, Backend, FieldOrder, Placement};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration},
    schema::{Builtin, Scalar, Shape},
};

pub struct Rust;

impl Backend for Rust {
    fn language(&self) -> &'static str {
        "rust"
    }

    fn display_name(&self) -> &'static str {
        "Rust"
    }

    fn scalar(&self, scalar: Scalar) -> &'static str {
        match scalar {
            Scalar::Boolean => "bool",
            Scalar::Integer => "i64",
            Scalar::String => "String",
            Scalar::Null => "()",
        }
    }

    fn builtin(&self, builtin: Builtin) -> &'static str {
        match builtin {
            Builtin::Bytes => "Vec<u8>",
            Builtin::Address => "Address",
            Builtin::UtxoRef => "UtxoRef",
            Builtin::AnyAsset => "String",
            Builtin::Utxo => "serde_json::Value",
        }
    }

    fn fallback(&self) -> &'static str {
        "serde_json::Value"
    }

    fn list(&self, item: &str) -> String {
        format!("Vec<{item}>")
    }

    fn map(&self, value: &str) -> String {
        format!("std::collections::HashMap<String, {value}>")
    }

    fn undeclared(&self, shape: &Shape) -> String {
        tuple_as_fallback_list(self, shape)
    }

    fn naming(&self, role: Role) -> Option<Case> {
        Some(match role {
            Role::Type => Case::Pascal,
            Role::Field | Role::Param => Case::Snake,
            Role::Case => Case::Pascal,
        })
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
                let mut body = String::new();
                for field in fields {
                    if field.name != field.source {
                        body.push_str(&format!("    #[serde(rename = \"{}\")]\n", field.source));
                    }
                    body.push_str(&format!("    pub {}: {},\n", field.name, field.ty));
                }
                format!("#[derive(Debug, Clone, Serialize)]\npub struct {name} {{\n{body}}}\n")
            }
            DeclKind::Variant(_) => format!(
                "// TODO: tagged-union codegen pending the variant arg encoder\npub type {name} = serde_json::Value;\n"
            ),
            DeclKind::Tuple(_) | DeclKind::Alias(_) => {
                format!("#[derive(Debug, Clone, Serialize)]\npub struct {name} {{\n}}\n")
            }
        }
    }
}
