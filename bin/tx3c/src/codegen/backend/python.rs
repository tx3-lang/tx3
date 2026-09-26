use convert_case::Case;

use super::{tuple_as_fallback_list, Backend, FieldOrder, Placement, TemplateFile};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration},
    schema::{Builtin, Scalar, Shape},
};

pub struct Python;

impl Backend for Python {
    fn language(&self) -> &'static str {
        "python"
    }

    fn display_name(&self) -> &'static str {
        "Python"
    }

    fn scalar(&self, scalar: Scalar) -> &'static str {
        match scalar {
            Scalar::Boolean => "bool",
            Scalar::Integer => "int",
            Scalar::String => "str",
            Scalar::Null => "None",
        }
    }

    fn builtin(&self, builtin: Builtin) -> &'static str {
        match builtin {
            Builtin::Bytes => "bytes",
            Builtin::Address | Builtin::UtxoRef | Builtin::AnyAsset => "str",
            Builtin::Utxo => "Any",
        }
    }

    fn fallback(&self) -> &'static str {
        "Any"
    }

    fn list(&self, item: &str) -> String {
        format!("list[{item}]")
    }

    fn map(&self, value: &str) -> String {
        format!("dict[str, {value}]")
    }

    fn undeclared(&self, shape: &Shape) -> String {
        tuple_as_fallback_list(self, shape)
    }

    fn naming(&self, role: Role) -> Option<Case> {
        Some(match role {
            Role::Type | Role::Case => Case::Pascal,
            Role::Field | Role::Param | Role::Method => Case::Snake,
            Role::Constant => Case::UpperSnake,
        })
    }

    fn client_templates(&self) -> &'static [TemplateFile] {
        client_templates!("python": "README.md.hbs", "__init__.py.hbs", "requirements.txt.hbs")
    }

    fn placement(&self) -> Placement {
        Placement::None
    }

    fn field_order(&self) -> FieldOrder {
        FieldOrder::Alphabetical
    }

    fn declaration(&self, declaration: &Declaration) -> String {
        let name = &declaration.name;
        match (&declaration.kind, declaration.params_of.as_deref()) {
            (DeclKind::Record(fields), Some(transaction)) => {
                let body: String = fields
                    .iter()
                    .map(|field| format!("    {}: {}\n", field.name, field.ty))
                    .collect();
                format!(
                    "@dataclass\nclass {name}:\n    \"\"\"Arguments for the {transaction} transaction.\"\"\"\n\n{body}"
                )
            }
            (DeclKind::Record(fields), None) if !fields.is_empty() => {
                let body: String = fields
                    .iter()
                    .map(|field| format!("    {}: {}\n", field.name, field.ty))
                    .collect();
                format!("@dataclass\nclass {name}:\n{body}")
            }
            (DeclKind::Variant(_), _) => format!(
                "# TODO: tagged-union codegen pending the variant arg encoder\n{name} = Any\n"
            ),
            (DeclKind::Record(_) | DeclKind::Tuple(_) | DeclKind::Alias(_), _) => {
                format!("@dataclass\nclass {name}:\n    pass\n")
            }
        }
    }
}
