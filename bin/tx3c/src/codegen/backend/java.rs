use convert_case::Case;

use super::{indent, Backend, Placement};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration, Member},
    schema::{Builtin, Scalar},
};

pub struct Java;

const KEYWORDS: &[&str] = &[
    "abstract",
    "assert",
    "boolean",
    "break",
    "byte",
    "case",
    "catch",
    "char",
    "class",
    "const",
    "continue",
    "default",
    "do",
    "double",
    "else",
    "enum",
    "extends",
    "final",
    "finally",
    "float",
    "for",
    "goto",
    "if",
    "implements",
    "import",
    "instanceof",
    "int",
    "interface",
    "long",
    "native",
    "new",
    "package",
    "private",
    "protected",
    "public",
    "return",
    "short",
    "static",
    "strictfp",
    "super",
    "switch",
    "synchronized",
    "this",
    "throw",
    "throws",
    "transient",
    "try",
    "void",
    "volatile",
    "while",
    // Reserved literals and restricted identifiers cannot be used as names in
    // the generated positions either.
    "true",
    "false",
    "null",
    "_",
    "exports",
    "module",
    "non-sealed",
    "open",
    "opens",
    "permits",
    "provides",
    "record",
    "requires",
    "sealed",
    "to",
    "transitive",
    "uses",
    "var",
    "when",
    "with",
    "yield",
];

impl Backend for Java {
    fn language(&self) -> &'static str {
        "java"
    }

    fn display_name(&self) -> &'static str {
        "Java"
    }

    fn scalar(&self, scalar: Scalar) -> &'static str {
        match scalar {
            Scalar::Boolean => "Boolean",
            Scalar::Integer => "java.math.BigInteger",
            Scalar::String => "String",
            Scalar::Null => "land.tx3.sdk.ArgValue",
        }
    }

    fn builtin(&self, builtin: Builtin) -> &'static str {
        match builtin {
            Builtin::Bytes => "byte[]",
            Builtin::Address => "land.tx3.sdk.Address",
            Builtin::UtxoRef => "land.tx3.sdk.UtxoRef",
            Builtin::AnyAsset | Builtin::Utxo => "land.tx3.sdk.ArgValue",
        }
    }

    fn fallback(&self) -> &'static str {
        "land.tx3.sdk.ArgValue"
    }

    fn list(&self, item: &str) -> String {
        format!("java.util.List<{item}>")
    }

    fn map(&self, value: &str) -> String {
        format!("java.util.Map<String, {value}>")
    }

    fn naming(&self, role: Role) -> Option<Case> {
        Some(match role {
            Role::Type | Role::Case => Case::Pascal,
            Role::Field | Role::Param | Role::Method => Case::Camel,
            Role::Constant => Case::UpperSnake,
        })
    }

    fn keywords(&self) -> &'static [&'static str] {
        KEYWORDS
    }

    fn needs_leading_underscore(&self, first: char) -> bool {
        !first.is_alphabetic() && first != '_' && first != '$'
    }

    fn placement(&self) -> Placement {
        Placement::Nested
    }

    fn declaration(&self, declaration: &Declaration) -> String {
        render(declaration, 0)
    }
}

fn render(declaration: &Declaration, depth: usize) -> String {
    let name = &declaration.name;
    match &declaration.kind {
        DeclKind::Record(fields) => record(name, fields, None, &declaration.nested, depth),
        DeclKind::Tuple(items) => record(name, items, None, &declaration.nested, depth),
        // Java has no type aliases; an aliased shape keeps an empty record.
        DeclKind::Alias(_) => record(name, &[], None, &[], depth),
        DeclKind::Variant(cases) => {
            let pad = indent(depth);
            let permits = cases
                .iter()
                .map(|case| format!("{name}.{}", case.name))
                .collect::<Vec<_>>()
                .join(", ");
            let mut out = format!("{pad}sealed interface {name} permits {permits} {{\n");
            for case in cases {
                out.push_str(&record(
                    &case.name,
                    &case.fields,
                    Some(name),
                    &case.nested,
                    depth + 4,
                ));
            }
            out.push_str(&format!("{pad}}}\n"));
            out
        }
    }
}

fn record(
    name: &str,
    members: &[Member],
    implements: Option<&str>,
    nested: &[Declaration],
    depth: usize,
) -> String {
    let pad = indent(depth);
    let components = members
        .iter()
        .map(|member| format!("{} {}", member.ty, member.name))
        .collect::<Vec<_>>()
        .join(", ");
    let implements = implements
        .map(|interface| format!(" implements {interface}"))
        .unwrap_or_default();
    let body: String = nested
        .iter()
        .map(|declaration| format!("\n{}", render(declaration, depth + 4)))
        .collect();

    if body.is_empty() {
        format!("{pad}record {name}({components}){implements} {{}}\n")
    } else {
        format!("{pad}record {name}({components}){implements} {{{body}{pad}}}\n")
    }
}
