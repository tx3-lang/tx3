use convert_case::Case;

use super::{tuple_as_fallback_list, Backend, FieldOrder, Placement};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration},
    schema::{Builtin, Scalar, Shape},
};

pub struct Go;

impl Backend for Go {
    fn language(&self) -> &'static str {
        "go"
    }

    fn display_name(&self) -> &'static str {
        "Go"
    }

    fn scalar(&self, scalar: Scalar) -> &'static str {
        match scalar {
            Scalar::Boolean => "bool",
            Scalar::Integer => "int64",
            Scalar::String => "string",
            Scalar::Null => "interface{}",
        }
    }

    fn builtin(&self, builtin: Builtin) -> &'static str {
        match builtin {
            Builtin::Bytes => "[]byte",
            Builtin::Address | Builtin::UtxoRef | Builtin::AnyAsset => "string",
            Builtin::Utxo => "interface{}",
        }
    }

    fn fallback(&self) -> &'static str {
        "interface{}"
    }

    fn list(&self, item: &str) -> String {
        format!("[]{item}")
    }

    fn map(&self, value: &str) -> String {
        format!("map[string]{value}")
    }

    fn undeclared(&self, shape: &Shape) -> String {
        tuple_as_fallback_list(self, shape)
    }

    fn naming(&self, role: Role) -> Option<Case> {
        Some(match role {
            Role::Type | Role::Field | Role::Param | Role::Case | Role::Method => Case::Pascal,
            Role::Constant => Case::UpperSnake,
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
                let body: String = fields
                    .iter()
                    .map(|field| {
                        format!("\t{} {} `json:\"{}\"`\n", field.name, field.ty, field.source)
                    })
                    .collect();
                let doc = declaration
                    .params_of
                    .as_deref()
                    .map(|transaction| {
                        format!("// {name} holds the arguments for the {transaction} transaction.\n")
                    })
                    .unwrap_or_default();
                format!("{doc}type {name} struct {{\n{body}}}\n")
            }
            DeclKind::Variant(_) => format!(
                "// TODO: tagged-union codegen pending the variant arg encoder\ntype {name} = interface{{}}\n"
            ),
            DeclKind::Tuple(_) | DeclKind::Alias(_) => format!("type {name} struct {{\n}}\n"),
        }
    }
}
