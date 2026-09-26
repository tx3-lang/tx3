//! Language-neutral view of the JSON Schema nodes embedded in a TII document.
//!
//! Every renderer used to re-derive the same facts from raw JSON: which `$ref`
//! is a builtin, which object is a record, in what order fields appear, and
//! which key tags a variant case. [`Shape::parse`] answers those questions once
//! so backends only decide how each shape is spelled.

use anyhow::{bail, Context, Result};
use serde_json::{Map, Value};

/// Prefix of a reference to a user-defined type under `components.schemas`.
const COMPONENT_PREFIX: &str = "#/components/schemas/";

/// Canonical and legacy prefixes of the Tx3 builtin scalar references. Both
/// forms resolve identically by their trailing name.
const BUILTIN_PREFIXES: &[&str] = &[
    "https://tx3.land/specs/v1beta0/tii#/$defs/",
    "https://tx3.land/specs/v1beta0/core#",
];

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Scalar {
    Boolean,
    Integer,
    String,
    Null,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum Builtin {
    Bytes,
    Address,
    UtxoRef,
    Utxo,
    AnyAsset,
}

impl Builtin {
    fn from_name(name: &str) -> Option<Self> {
        match name {
            "Bytes" => Some(Self::Bytes),
            "Address" => Some(Self::Address),
            "UtxoRef" => Some(Self::UtxoRef),
            "Utxo" => Some(Self::Utxo),
            "AnyAsset" => Some(Self::AnyAsset),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Shape<'a> {
    Scalar(Scalar),
    Builtin(Builtin),
    /// A reference to `components.schemas/<name>`, holding the source name.
    Component(&'a str),
    List(Box<Shape<'a>>),
    /// A string-keyed map, holding the value shape.
    Map(Box<Shape<'a>>),
    Tuple(Vec<Shape<'a>>),
    Record(Vec<Field<'a>>),
    Variant(Vec<Case<'a>>),
    /// Anything without a native mapping; backends use their SDK fallback.
    Unknown,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<'a> {
    pub name: &'a str,
    pub shape: Shape<'a>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Case<'a> {
    pub tag: &'a str,
    pub fields: Vec<Field<'a>>,
}

impl<'a> Shape<'a> {
    pub fn parse(schema: &'a Value) -> Result<Self> {
        let Some(schema) = schema.as_object() else {
            return Ok(Self::Unknown);
        };

        if let Some(reference) = schema.get("$ref").and_then(Value::as_str) {
            return Ok(classify_ref(reference));
        }

        if let Some(cases) = schema.get("oneOf").and_then(Value::as_array) {
            return parse_variant(cases).map(Self::Variant);
        }

        let shape = match schema.get("type").and_then(Value::as_str) {
            Some("boolean") => Self::Scalar(Scalar::Boolean),
            Some("integer") => Self::Scalar(Scalar::Integer),
            Some("string") => Self::Scalar(Scalar::String),
            Some("null") => Self::Scalar(Scalar::Null),
            Some("array") => match schema.get("prefixItems").and_then(Value::as_array) {
                Some(items) => Self::Tuple(items.iter().map(Self::parse).collect::<Result<_>>()?),
                None => Self::List(Box::new(match schema.get("items") {
                    Some(items) => Self::parse(items)?,
                    None => Self::Unknown,
                })),
            },
            Some("object") => {
                if let Some(value) = schema.get("additionalProperties").filter(|v| v.is_object()) {
                    Self::Map(Box::new(Self::parse(value)?))
                } else if schema.get("properties").is_some_and(Value::is_object) {
                    Self::Record(parse_fields(schema)?)
                } else {
                    // `additionalProperties: false` closes an object; it does
                    // not describe a map value type.
                    Self::Unknown
                }
            }
            _ => Self::Unknown,
        };
        Ok(shape)
    }
}

/// Local component references become generated types. Only the recognized
/// Tx3 builtin forms map to builtins; any other origin is unknown, even when
/// its trailing name looks like a builtin.
fn classify_ref(reference: &str) -> Shape<'_> {
    let single_segment = |name: &&str| !name.is_empty() && !name.contains('/');

    if let Some(name) = reference
        .strip_prefix(COMPONENT_PREFIX)
        .filter(single_segment)
    {
        return Shape::Component(name);
    }

    BUILTIN_PREFIXES
        .iter()
        .find_map(|prefix| reference.strip_prefix(prefix))
        .filter(single_segment)
        .and_then(Builtin::from_name)
        .map_or(Shape::Unknown, Shape::Builtin)
}

/// Record fields in declaration order: `required` first, as the compiler lists
/// every field there in source order, then any optional field by name.
fn parse_fields(schema: &Map<String, Value>) -> Result<Vec<Field<'_>>> {
    let Some(properties) = schema.get("properties").and_then(Value::as_object) else {
        return Ok(Vec::new());
    };

    let mut names: Vec<&str> = Vec::with_capacity(properties.len());
    for name in schema
        .get("required")
        .and_then(Value::as_array)
        .into_iter()
        .flatten()
        .filter_map(Value::as_str)
    {
        if properties.contains_key(name) && !names.contains(&name) {
            names.push(name);
        }
    }
    let mut optional: Vec<&str> = properties
        .keys()
        .map(String::as_str)
        .filter(|name| !names.contains(name))
        .collect();
    optional.sort_unstable();
    names.extend(optional);

    names
        .into_iter()
        .map(|name| {
            Ok(Field {
                name,
                shape: Shape::parse(&properties[name])
                    .with_context(|| format!("field `{name}`"))?,
            })
        })
        .collect()
}

/// Each case of an externally tagged variant is an object with exactly one
/// required key, the tag, whose property holds the case payload record.
fn parse_variant(cases: &[Value]) -> Result<Vec<Case<'_>>> {
    let mut parsed = Vec::with_capacity(cases.len());
    for (index, case) in cases.iter().enumerate() {
        let tag = case
            .get("required")
            .and_then(Value::as_array)
            .filter(|required| required.len() == 1)
            .and_then(|required| required[0].as_str())
            .with_context(|| format!("variant case {index} must name exactly one required tag"))?;
        let Some(payload) = case
            .get("properties")
            .and_then(Value::as_object)
            .and_then(|properties| properties.get(tag))
        else {
            bail!("variant case `{tag}` is missing its payload schema");
        };
        let fields = match payload.as_object() {
            Some(payload) => {
                parse_fields(payload).with_context(|| format!("variant case `{tag}`"))?
            }
            None => Vec::new(),
        };
        parsed.push(Case { tag, fields });
    }
    Ok(parsed)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn parse(schema: Value) -> Shape<'static> {
        let leaked: &'static Value = Box::leak(Box::new(schema));
        Shape::parse(leaked).unwrap()
    }

    #[test]
    fn refs_are_classified_by_origin() {
        for name in ["Bytes", "Address", "UtxoRef", "Utxo", "AnyAsset"] {
            let expected = Shape::Builtin(Builtin::from_name(name).unwrap());
            let canonical = format!("https://tx3.land/specs/v1beta0/tii#/$defs/{name}");
            let legacy = format!("https://tx3.land/specs/v1beta0/core#{name}");
            assert_eq!(parse(json!({ "$ref": canonical })), expected);
            assert_eq!(parse(json!({ "$ref": legacy })), expected);
        }

        assert_eq!(
            parse(json!({ "$ref": "#/components/schemas/Address" })),
            Shape::Component("Address")
        );
        assert_eq!(
            parse(json!({ "$ref": "https://example.com/schema#/$defs/Address" })),
            Shape::Unknown
        );
        assert_eq!(
            parse(json!({ "$ref": "https://tx3.land/specs/v1beta0/tii#/$defs/Future" })),
            Shape::Unknown
        );
        assert_eq!(
            parse(json!({ "$ref": "#/components/schemas/a/b" })),
            Shape::Unknown
        );
    }

    #[test]
    fn containers_and_closed_objects() {
        assert_eq!(
            parse(json!({ "type": "array", "items": { "type": "integer" } })),
            Shape::List(Box::new(Shape::Scalar(Scalar::Integer)))
        );
        assert_eq!(
            parse(json!({ "type": "array" })),
            Shape::List(Box::new(Shape::Unknown))
        );
        assert_eq!(
            parse(json!({ "type": "object", "additionalProperties": { "type": "boolean" } })),
            Shape::Map(Box::new(Shape::Scalar(Scalar::Boolean)))
        );
        assert_eq!(
            parse(json!({ "type": "object", "additionalProperties": false })),
            Shape::Unknown
        );
        assert_eq!(parse(json!({ "type": "object" })), Shape::Unknown);
        assert_eq!(parse(json!(false)), Shape::Unknown);
    }

    #[test]
    fn record_fields_follow_required_order_then_name() {
        let shape = parse(json!({
            "type": "object",
            "properties": {
                "b": { "type": "integer" },
                "z": { "type": "boolean" },
                "a": { "type": "string" },
                "y": { "type": "null" }
            },
            "required": ["z", "b"]
        }));
        let Shape::Record(fields) = shape else {
            panic!("expected a record");
        };
        let names: Vec<_> = fields.iter().map(|field| field.name).collect();
        assert_eq!(names, ["z", "b", "a", "y"]);
    }

    #[test]
    fn variant_tags_are_strict() {
        let two_tags = json!({ "oneOf": [{
            "type": "object",
            "required": ["A", "B"],
            "properties": { "A": { "type": "object", "properties": {} } }
        }]});
        assert_eq!(
            Shape::parse(&two_tags).unwrap_err().to_string(),
            "variant case 0 must name exactly one required tag"
        );

        let no_payload = json!({ "oneOf": [{ "type": "object", "required": ["A"] }] });
        assert_eq!(
            Shape::parse(&no_payload).unwrap_err().to_string(),
            "variant case `A` is missing its payload schema"
        );
    }
}
