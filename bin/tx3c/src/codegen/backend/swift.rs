use std::collections::BTreeSet;

use convert_case::Case;

use super::{Backend, Placement};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration, Member, Usage},
    schema::{Builtin, Scalar},
};

pub struct Swift;

const KEYWORDS: &[&str] = &[
    "Any",
    "Self",
    "Type",
    "Protocol",
    "actor",
    "any",
    "as",
    "associatedtype",
    "associativity",
    "async",
    "await",
    "break",
    "case",
    "catch",
    "class",
    "continue",
    "convenience",
    "copy",
    "default",
    "defer",
    "deinit",
    "didSet",
    "do",
    "dynamic",
    "else",
    "enum",
    "extension",
    "fallthrough",
    "false",
    "fileprivate",
    "final",
    "for",
    "func",
    "get",
    "guard",
    "if",
    "import",
    "in",
    "indirect",
    "infix",
    "init",
    "inout",
    "internal",
    "is",
    "isolated",
    "lazy",
    "let",
    "macro",
    "mutating",
    "nil",
    "nonisolated",
    "nonmutating",
    "open",
    "operator",
    "optional",
    "override",
    "package",
    "postfix",
    "precedence",
    "prefix",
    "private",
    "protocol",
    "public",
    "repeat",
    "required",
    "rethrows",
    "return",
    "self",
    "set",
    "some",
    "static",
    "struct",
    "subscript",
    "super",
    "switch",
    "throws",
    "true",
    "try",
    "typealias",
    "unowned",
    "var",
    "weak",
    "where",
    "while",
    "willSet",
];

impl Backend for Swift {
    fn language(&self) -> &'static str {
        "swift"
    }

    fn display_name(&self) -> &'static str {
        "Swift"
    }

    fn scalar(&self, scalar: Scalar) -> &'static str {
        match scalar {
            Scalar::Boolean => "Bool",
            Scalar::Integer => "BigInt",
            Scalar::String => "ArgValue",
            Scalar::Null => "Void",
        }
    }

    fn builtin(&self, builtin: Builtin) -> &'static str {
        match builtin {
            Builtin::Bytes => "Data",
            Builtin::Address => "Address",
            Builtin::UtxoRef => "UtxoRef",
            Builtin::Utxo | Builtin::AnyAsset => "ArgValue",
        }
    }

    fn fallback(&self) -> &'static str {
        "ArgValue"
    }

    fn list(&self, item: &str) -> String {
        format!("[{item}]")
    }

    fn map(&self, value: &str) -> String {
        format!("[String: {value}]")
    }

    fn naming(&self, role: Role) -> Option<Case> {
        Some(match role {
            Role::Type => Case::Pascal,
            Role::Field | Role::Param | Role::Case | Role::Method => Case::Camel,
            Role::Constant => Case::UpperSnake,
        })
    }

    fn keywords(&self) -> &'static [&'static str] {
        KEYWORDS
    }

    fn placement(&self) -> Placement {
        Placement::Hoisted
    }

    fn declaration(&self, declaration: &Declaration) -> String {
        let name = &declaration.name;
        match &declaration.kind {
            DeclKind::Record(members) | DeclKind::Tuple(members) => structure(name, members),
            DeclKind::Alias(target) => format!("public typealias {name} = {target}"),
            DeclKind::Variant(cases) => {
                let body: String = cases
                    .iter()
                    .map(|case| {
                        if case.fields.is_empty() {
                            format!("    case {}\n", case.name)
                        } else {
                            let associated = case
                                .fields
                                .iter()
                                .map(|field| format!("{}: {}", field.name, field.ty))
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!("    case {}({associated})\n", case.name)
                        }
                    })
                    .collect();
                format!("public enum {name}: Sendable {{\n{body}}}")
            }
        }
    }

    fn join(&self, rendered: Vec<String>) -> String {
        rendered.join("\n\n")
    }

    fn imports(&self, usage: &Usage) -> String {
        let mut modules = BTreeSet::new();
        if usage.builtins.contains(&Builtin::Bytes) {
            modules.insert("Foundation");
        }
        if usage.scalars.contains(&Scalar::Integer) {
            modules.insert("BigInt");
        }
        let sdk_builtin = usage
            .builtins
            .iter()
            .any(|builtin| *builtin != Builtin::Bytes);
        if usage.fallback || sdk_builtin || usage.scalars.contains(&Scalar::String) {
            modules.insert("Tx3SDK");
        }
        ["Foundation", "BigInt", "Tx3SDK"]
            .into_iter()
            .filter(|module| modules.contains(module))
            .map(|module| format!("import {module}\n"))
            .collect()
    }
}

fn structure(name: &str, members: &[Member]) -> String {
    let properties: String = members
        .iter()
        .map(|member| format!("    public let {}: {}\n", member.name, member.ty))
        .collect();
    let parameters = members
        .iter()
        .map(|member| format!("{}: {}", member.name, member.ty))
        .collect::<Vec<_>>()
        .join(", ");
    let assignments: String = members
        .iter()
        .map(|member| format!("        self.{0} = {0}\n", member.name))
        .collect();
    format!(
        "public struct {name}: Sendable {{\n{properties}\n    public init({parameters}) {{\n{assignments}    }}\n}}"
    )
}
