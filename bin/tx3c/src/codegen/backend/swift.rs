use anyhow::{Context, Result};
use convert_case::Case;
use serde_json::Value;

use super::{Backend, Placement};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration, Encoding, Member, Usage},
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

    /// Every record, tuple and variant carries an `argValue` property that
    /// spells its canonical `ArgValue`, mirroring the SDK's encoder: records
    /// are constructor 0 with their fields in declared order, variants use
    /// their case index, tuples are positional.
    fn declaration(&self, declaration: &Declaration) -> String {
        let name = &declaration.name;
        match &declaration.kind {
            DeclKind::Record(members) => structure(name, members, &structure_value(0, members, 2)),
            DeclKind::Tuple(members) => structure(name, members, &tuple_value(members, 2)),
            DeclKind::Alias { target, .. } => format!("public typealias {name} = {target}"),
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
                let arms: String = cases
                    .iter()
                    .enumerate()
                    .map(|(index, case)| {
                        let pattern = if case.fields.is_empty() {
                            format!(".{}", case.name)
                        } else {
                            let bindings = case
                                .fields
                                .iter()
                                .map(|field| format!("let {}", field.name))
                                .collect::<Vec<_>>()
                                .join(", ");
                            format!(".{}({bindings})", case.name)
                        };
                        format!(
                            "        case {pattern}:\n            return {}\n",
                            structure_value(index, &case.fields, 3)
                        )
                    })
                    .collect();
                format!(
                    "public enum {name}: Sendable {{\n{body}\n    \
                     /// The canonical argument value of this variant.\n    \
                     public var argValue: ArgValue {{\n        \
                     switch self {{\n{arms}        }}\n    }}\n}}"
                )
            }
        }
    }

    fn join(&self, rendered: Vec<String>) -> String {
        rendered.join("\n\n")
    }

    fn imports(&self, usage: &Usage) -> String {
        self.modules(usage)
            .into_iter()
            .map(|module| format!("import {module}\n"))
            .collect()
    }

    /// `Foundation` backs `Data`, `BigInt` backs integers, and `Tx3SDK`
    /// backs every declaration through its `ArgValue` conversion.
    fn modules(&self, usage: &Usage) -> Vec<&'static str> {
        let mut modules = vec!["Tx3SDK"];
        if usage.builtins.contains(&Builtin::Bytes) {
            modules.push("Foundation");
        }
        if usage.scalars.contains(&Scalar::Integer) {
            modules.push("BigInt");
        }
        ["Foundation", "BigInt", "Tx3SDK"]
            .into_iter()
            .filter(|module| modules.contains(module))
            .collect()
    }

    fn argument(&self, encoding: &Encoding, value: &str) -> Result<String> {
        Ok(argument(encoding, value))
    }

    fn profile(&self, profile: &Value) -> Result<String> {
        let environment = profile
            .get("environment")
            .and_then(Value::as_object)
            .into_iter()
            .flatten()
            .map(|(name, value)| {
                format!(
                    "        {}: {},\n",
                    string_literal(name),
                    json_value(value, 2)
                )
            })
            .collect::<String>();
        let parties = profile
            .get("parties")
            .and_then(Value::as_object)
            .into_iter()
            .flatten()
            .map(|(name, address)| {
                let address = address
                    .as_str()
                    .with_context(|| format!("party `{name}` must map to an address string"))?;
                Ok(format!(
                    "        {}: {},\n",
                    string_literal(name),
                    string_literal(address)
                ))
            })
            .collect::<Result<String>>()?;
        Ok(format!(
            "Tx3SDK.Profile(\n    environment: {},\n    parties: {}\n)",
            dictionary(&environment, 1),
            dictionary(&parties, 1)
        ))
    }
}

/// Spells the `ArgValue` of `value`, an expression of the Swift type the
/// backend gives `encoding`. Containers convert their elements through
/// closures; each closure's `$0` is exactly the element being converted.
fn argument(encoding: &Encoding, value: &str) -> String {
    match encoding {
        Encoding::Scalar(Scalar::Boolean) => format!("ArgValue.boolean({value})"),
        Encoding::Scalar(Scalar::Integer) => format!("ArgValue.integer({value})"),
        // Strings, UTxOs, any-assets and unknown shapes are typed as
        // `ArgValue` already.
        Encoding::Scalar(Scalar::String)
        | Encoding::Builtin(Builtin::Utxo | Builtin::AnyAsset)
        | Encoding::Fallback => value.to_string(),
        Encoding::Scalar(Scalar::Null) => {
            "ArgValue.structure(constructor: 0, fields: [])".to_string()
        }
        Encoding::Builtin(Builtin::Bytes) => format!("ArgValue.bytes({value})"),
        Encoding::Builtin(Builtin::Address) => format!("ArgValue.address({value})"),
        Encoding::Builtin(Builtin::UtxoRef) => format!("ArgValue.utxoRef({value})"),
        Encoding::Declared(_) | Encoding::Component(_) => format!("{value}.argValue"),
        Encoding::List(item) => {
            format!("ArgValue.list({value}.map {{ {} }})", argument(item, "$0"))
        }
        Encoding::Map(item) => format!(
            "ArgValue.mapPairs({value}.sorted {{ $0.key < $1.key }}.map {{ \
             ArgMapEntry(key: ArgValue.string($0.key), value: {}) }})",
            argument(item, "$0.value")
        ),
    }
}

/// `ArgValue.structure(constructor:fields:)` over `members`, read from
/// `self`, laid out one field per line at `depth` indentation levels.
fn structure_value(constructor: usize, members: &[Member], depth: usize) -> String {
    if members.is_empty() {
        return format!("ArgValue.structure(constructor: {constructor}, fields: [])");
    }
    let indent = "    ".repeat(depth);
    let fields: String = members
        .iter()
        .map(|member| {
            format!(
                "{indent}        {},\n",
                argument(&member.encoding, &member.name)
            )
        })
        .collect();
    format!(
        "ArgValue.structure(\n{indent}    constructor: {constructor},\n{indent}    fields: [\n\
         {fields}{indent}    ]\n{indent})"
    )
}

/// `ArgValue.tuple([...])` over `members`, one item per line.
fn tuple_value(members: &[Member], depth: usize) -> String {
    if members.is_empty() {
        return "ArgValue.tuple([])".to_string();
    }
    let indent = "    ".repeat(depth);
    let items: String = members
        .iter()
        .map(|member| {
            format!(
                "{indent}    {},\n",
                argument(&member.encoding, &member.name)
            )
        })
        .collect();
    format!("ArgValue.tuple([\n{items}{indent}])")
}

fn structure(name: &str, members: &[Member], arg_value: &str) -> String {
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
        "public struct {name}: Sendable {{\n{properties}\n    public init({parameters}) {{\n{assignments}    }}\n\n    \
         /// The canonical argument value of this record.\n    \
         public var argValue: ArgValue {{\n        {arg_value}\n    }}\n}}"
    )
}

/// A Swift dictionary literal from already-rendered `"key": value,` lines,
/// or `[:]` when there are none.
fn dictionary(entries: &str, depth: usize) -> String {
    if entries.is_empty() {
        "[:]".to_string()
    } else {
        format!("[\n{entries}{}]", "    ".repeat(depth))
    }
}

/// A `JSONValue` expression for `value`, nested containers indented one
/// level deeper than `depth`.
fn json_value(value: &Value, depth: usize) -> String {
    let indent = "    ".repeat(depth);
    match value {
        Value::Null => "JSONValue.null".to_string(),
        Value::Bool(value) => format!("JSONValue.boolean({value})"),
        Value::Number(number) => format!("JSONValue.number({})", number_literal(number)),
        Value::String(value) => format!("JSONValue.string({})", string_literal(value)),
        Value::Array(items) if items.is_empty() => "JSONValue.array([])".to_string(),
        Value::Array(items) => {
            let items: String = items
                .iter()
                .map(|item| format!("{indent}    {},\n", json_value(item, depth + 1)))
                .collect();
            format!("JSONValue.array([\n{items}{indent}])")
        }
        Value::Object(fields) if fields.is_empty() => "JSONValue.object([:])".to_string(),
        Value::Object(fields) => {
            let fields: String = fields
                .iter()
                .map(|(name, value)| {
                    format!(
                        "{indent}    {}: {},\n",
                        string_literal(name),
                        json_value(value, depth + 1)
                    )
                })
                .collect();
            format!("JSONValue.object([\n{fields}{indent}])")
        }
    }
}

/// A `Double` literal with the value the SDK's own JSON decoding would give
/// the number.
fn number_literal(number: &serde_json::Number) -> String {
    let value = number.as_f64().unwrap_or(f64::NAN);
    if value.is_finite() {
        format!("{value:?}")
    } else {
        // Unreachable for JSON numbers; spelled so a reviewer sees it.
        "Double.nan".to_string()
    }
}

/// A Swift string literal for `value`, escaping what a literal cannot hold.
fn string_literal(value: &str) -> String {
    let mut literal = String::with_capacity(value.len() + 2);
    literal.push('"');
    for character in value.chars() {
        match character {
            '\\' => literal.push_str("\\\\"),
            '"' => literal.push_str("\\\""),
            '\n' => literal.push_str("\\n"),
            '\r' => literal.push_str("\\r"),
            '\t' => literal.push_str("\\t"),
            '\0' => literal.push_str("\\0"),
            character if character.is_control() => {
                literal.push_str(&format!("\\u{{{:x}}}", character as u32));
            }
            character => literal.push(character),
        }
    }
    literal.push('"');
    literal
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn string_literals_escape_what_swift_cannot_hold() {
        assert_eq!(string_literal("plain"), "\"plain\"");
        assert_eq!(string_literal("a\"b\\c\n"), "\"a\\\"b\\\\c\\n\"");
        assert_eq!(string_literal("\u{1}"), "\"\\u{1}\"");
    }

    #[test]
    fn number_literals_are_doubles() {
        let cases = [("5000000", "5000000.0"), ("1.5", "1.5"), ("-3", "-3.0")];
        for (json, expected) in cases {
            let number: serde_json::Number = serde_json::from_str(json).unwrap();
            assert_eq!(number_literal(&number), expected, "{json}");
        }
    }

    #[test]
    fn unsupported_backends_reject_static_arguments() {
        let error = super::super::for_language("go")
            .unwrap()
            .argument(&Encoding::Fallback, "x")
            .unwrap_err();
        assert_eq!(
            error.to_string(),
            "Go clients do not construct argument values statically"
        );
    }
}
