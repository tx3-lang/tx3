//! Java backend.
//!
//! Every value declaration is `public` and carries a `toArgValue()` method
//! that builds the runtime SDK's canonical `ArgValue` from the declared
//! fields, so generated clients construct tagged arguments statically without
//! embedding a schema. SDK and library types are spelled fully qualified: a
//! protocol may declare a component named like an SDK type, and generated
//! files nest the declarations next to the client, so imports could be
//! shadowed.

use convert_case::Case;

use super::{indent, Backend, Placement};
use crate::codegen::{
    names::Role,
    plan::{DeclKind, Declaration, Member},
    schema::{Builtin, Scalar, Shape},
};

const ARG_VALUE: &str = "land.tx3.sdk.ArgValue";

/// Largest UTF-8 length of one string constant in a class file.
const MAX_CONSTANT_BYTES: usize = 65535;

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
            Scalar::Null => ARG_VALUE,
        }
    }

    fn builtin(&self, builtin: Builtin) -> &'static str {
        match builtin {
            Builtin::Bytes => "byte[]",
            Builtin::Address => "land.tx3.sdk.Address",
            Builtin::UtxoRef => "land.tx3.sdk.UtxoRef",
            Builtin::AnyAsset | Builtin::Utxo => ARG_VALUE,
        }
    }

    fn fallback(&self) -> &'static str {
        ARG_VALUE
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

    /// Collapses every run of characters that cannot appear in a Java
    /// identifier into one underscore. Case conversion drops the usual
    /// separators but keeps anything else, such as `.` or `@`, verbatim.
    fn sanitize(&self, normalized: String) -> String {
        let mut out = String::with_capacity(normalized.len());
        let mut illegal = false;
        for c in normalized.chars() {
            if c.is_alphanumeric() || c == '_' || c == '$' {
                out.push(c);
                illegal = false;
            } else if !illegal {
                out.push('_');
                illegal = true;
            }
        }
        out
    }

    fn argument(&self, shape: &Shape, expr: &str) -> Option<String> {
        Some(tagged(shape, expr, 0))
    }

    fn accessor(&self, receiver: &str, member: &str) -> String {
        format!("{receiver}.{member}()")
    }

    /// A string literal, or `String.join` over literal chunks when the text
    /// exceeds what one class-file constant can hold. Constant folding would
    /// merge `+`-concatenated literals back into one constant.
    fn string_literal(&self, text: &str) -> String {
        if text.len() <= MAX_CONSTANT_BYTES {
            return literal(text);
        }
        let mut chunks = Vec::new();
        let mut rest = text;
        while !rest.is_empty() {
            let mut end = rest.len().min(MAX_CONSTANT_BYTES);
            while !rest.is_char_boundary(end) {
                end -= 1;
            }
            chunks.push(literal(&rest[..end]));
            rest = &rest[end..];
        }
        format!("String.join(\"\", {})", chunks.join(", "))
    }

    fn placement(&self) -> Placement {
        Placement::Nested
    }

    fn declaration(&self, declaration: &Declaration) -> String {
        render(declaration, 0)
    }
}

/// Escapes `text` as one Java string literal. JSON escaping is valid Java:
/// the short escapes coincide, and `\u00XX` names only non-line-terminator
/// control characters, which may appear in a literal.
fn literal(text: &str) -> String {
    serde_json::to_string(text).expect("strings serialize")
}

/// Spells the `ArgValue` built from `expr`, a value of the Java type of
/// `shape`. Lambda parameters are numbered by nesting depth so nested
/// container conversions never shadow each other.
fn tagged(shape: &Shape, expr: &str, depth: usize) -> String {
    match shape {
        Shape::Scalar(Scalar::Boolean) => format!("{ARG_VALUE}.bool({expr})"),
        Shape::Scalar(Scalar::Integer) => format!("{ARG_VALUE}.integer({expr})"),
        Shape::Scalar(Scalar::String) => format!("{ARG_VALUE}.string({expr})"),
        Shape::Builtin(Builtin::Bytes) => format!("{ARG_VALUE}.bytes({expr})"),
        Shape::Builtin(Builtin::Address) => format!("{ARG_VALUE}.address({expr})"),
        Shape::Builtin(Builtin::UtxoRef) => format!("{ARG_VALUE}.utxoRef({expr})"),
        // Typed as `ArgValue` already: the caller supplies the tagged value.
        Shape::Scalar(Scalar::Null)
        | Shape::Builtin(Builtin::AnyAsset | Builtin::Utxo)
        | Shape::Unknown => expr.to_string(),
        // Declared types convert themselves.
        Shape::Component(_) | Shape::Record(_) | Shape::Tuple(_) | Shape::Variant(_) => {
            format!("{expr}.toArgValue()")
        }
        Shape::List(item) => {
            let element = format!("v{depth}");
            let converted = tagged(item, &element, depth + 1);
            if converted == element {
                format!("{ARG_VALUE}.list({expr})")
            } else {
                format!("{ARG_VALUE}.list({expr}.stream().map({element} -> {converted}).toList())")
            }
        }
        Shape::Map(value) => {
            let entry = format!("v{depth}");
            let converted = tagged(value, &format!("{entry}.getValue()"), depth + 1);
            format!(
                "{ARG_VALUE}.map({expr}.entrySet().stream()\
                 .sorted(java.util.Map.Entry.comparingByKey())\
                 .map({entry} -> new {ARG_VALUE}.MapEntry({ARG_VALUE}.string({entry}.getKey()), {converted}))\
                 .toList())"
            )
        }
    }
}

fn render(declaration: &Declaration, depth: usize) -> String {
    let name = &declaration.name;
    match &declaration.kind {
        // A transaction's params record is an argument bag, not a value: the
        // client sends each member as its own tagged argument.
        DeclKind::Record(fields) if declaration.params_of.is_some() => {
            record(name, fields, None, None, &declaration.nested, depth)
        }
        DeclKind::Record(fields) => record(
            name,
            fields,
            None,
            Some(Value::Struct(0)),
            &declaration.nested,
            depth,
        ),
        DeclKind::Tuple(items) => record(
            name,
            items,
            None,
            Some(Value::Tuple),
            &declaration.nested,
            depth,
        ),
        // Java has no type aliases; an aliased shape becomes a record holding
        // one `value` of the aliased type.
        DeclKind::Alias(value) => record(
            name,
            std::slice::from_ref(value),
            None,
            Some(Value::Alias),
            &declaration.nested,
            depth,
        ),
        DeclKind::Variant(cases) => {
            let pad = indent(depth);
            let permits = cases
                .iter()
                .map(|case| format!("{name}.{}", case.name))
                .collect::<Vec<_>>()
                .join(", ");
            let mut out = format!("{pad}public sealed interface {name} permits {permits} {{\n");
            out.push_str(&format!(
                "{pad}    /** Converts this value to the SDK's canonical tagged argument. */\n\
                 {pad}    {ARG_VALUE} toArgValue();\n"
            ));
            for (index, case) in cases.iter().enumerate() {
                out.push('\n');
                out.push_str(&record(
                    &case.name,
                    &case.fields,
                    Some(name),
                    Some(Value::Struct(index)),
                    &case.nested,
                    depth + 4,
                ));
            }
            out.push_str(&format!("{pad}}}\n"));
            out
        }
    }
}

/// How a record spells its `toArgValue()`.
enum Value {
    /// A record or variant case: `struct` with this constructor index.
    Struct(usize),
    Tuple,
    /// An alias wrapper: its single member's own conversion.
    Alias,
}

fn record(
    name: &str,
    members: &[Member],
    implements: Option<&str>,
    value: Option<Value>,
    nested: &[Declaration],
    depth: usize,
) -> String {
    let pad = indent(depth);
    let components = members
        .iter()
        .map(|member| format!("{} {}", member.ty, member.name))
        .collect::<Vec<_>>()
        .join(", ");
    // Members of an interface are public already.
    let modifier = if implements.is_some() { "" } else { "public " };
    let implements = implements
        .map(|interface| format!(" implements {interface}"))
        .unwrap_or_default();

    let mut sections: Vec<String> = Vec::new();
    if let Some(value) = value {
        sections.push(to_arg_value(members, value, modifier.is_empty(), depth + 4));
    }
    sections.extend(
        nested
            .iter()
            .map(|declaration| render(declaration, depth + 4)),
    );

    if sections.is_empty() {
        format!("{pad}{modifier}record {name}({components}){implements} {{}}\n")
    } else {
        format!(
            "{pad}{modifier}record {name}({components}){implements} {{\n{}{pad}}}\n",
            sections.join("\n")
        )
    }
}

fn to_arg_value(members: &[Member], value: Value, overrides: bool, depth: usize) -> String {
    let pad = indent(depth);
    let arguments: Vec<&str> = members
        .iter()
        .map(|member| {
            member
                .argument
                .as_deref()
                .expect("the Java backend spells every member's argument")
        })
        .collect();
    let expression = match value {
        Value::Alias => arguments[0].to_string(),
        Value::Struct(_) | Value::Tuple => {
            let constructor = match value {
                Value::Struct(index) => format!("{ARG_VALUE}.struct({index}, "),
                _ => format!("{ARG_VALUE}.tuple("),
            };
            if arguments.is_empty() {
                format!("{constructor}java.util.List.of())")
            } else {
                format!(
                    "{constructor}java.util.List.of(\n{pad}        {}))",
                    arguments.join(&format!(",\n{pad}        "))
                )
            }
        }
    };
    let mut out = String::new();
    if overrides {
        out.push_str(&format!("{pad}@Override\n"));
    } else {
        out.push_str(&format!(
            "{pad}/** Converts this value to the SDK's canonical tagged argument. */\n"
        ));
    }
    out.push_str(&format!(
        "{pad}public {ARG_VALUE} toArgValue() {{\n{pad}    return {expression};\n{pad}}}\n"
    ));
    out
}
