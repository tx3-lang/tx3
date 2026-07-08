use base64::Engine as _;
use serde::{Deserialize, Serialize};
use serde_json::{Map, Number, Value};
use thiserror::Error;

use tx3_tir::model::assets::{AssetClass, CanonicalAssets};
use tx3_tir::model::core::{Type, Utxo, UtxoRef};
pub use tx3_tir::reduce::ArgValue;

#[derive(Debug, Error)]
pub enum Error {
    #[error("invalid base64: {0}")]
    InvalidBase64(#[from] base64::DecodeError),

    #[error("invalid hex: {0}")]
    InvalidHex(#[from] hex::FromHexError),

    #[error("invalid bech32: {0}")]
    InvalidBech32(#[from] bech32::DecodeError),

    #[error("value is not a valid number: {0}")]
    InvalidBytesForNumber(String),

    #[error("value is null")]
    ValueIsNull,

    #[error("can't infer type for value: {0}")]
    CantInferTypeForValue(Value),

    #[error("value is not a number: {0}")]
    ValueIsNotANumber(Value),

    #[error("value can't fit: {0}")]
    NumberCantFit(Number),

    #[error("value is not a bool: {0}")]
    ValueIsNotABool(Value),

    #[error("value is not a string")]
    ValueIsNotAString,

    #[error("value is not bytes: {0}")]
    ValueIsNotBytes(Value),

    #[error("value is not a utxo ref: {0}")]
    ValueIsNotUtxoRef(Value),

    #[error("invalid bytes envelope: {0}")]
    InvalidBytesEnvelope(serde_json::Error),

    #[error("value is not an address: {0}")]
    ValueIsNotAnAddress(Value),

    #[error("invalid utxo ref: {0}")]
    InvalidUtxoRef(String),

    #[error("target type not supported: {0:?}")]
    TargetTypeNotSupported(Type),

    #[error("unknown tag: {0}")]
    UnknownTag(String),

    #[error("malformed tagged value (expected a single-key object): {0}")]
    MalformedTaggedArg(Value),

    #[error("malformed tagged map pair (expected a [key, value] array): {0}")]
    MalformedMapPair(Value),

    #[error("malformed tagged struct (expected {{ constructor, fields }}): {0}")]
    MalformedStruct(Value),
}

#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum BytesEncoding {
    Base64,
    Hex,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct BytesEnvelope {
    // Aliases for backward compatibility
    #[serde(alias = "bytecode", alias = "payload")]
    pub content: String,
    #[serde(rename = "contentType", alias = "encoding")]
    pub content_type: BytesEncoding,
}

/// Envelope for serialized TIR (Transaction Intermediate Representation) bytes.
/// Used for serialization/deserialization of TIR across dump mechanism, TRP, etc.
#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct TirEnvelope {
    // Aliases for backward compatibility
    #[serde(alias = "bytecode", alias = "payload")]
    pub content: String,
    pub encoding: BytesEncoding,
    pub version: String,
}

impl BytesEnvelope {
    pub fn from_hex(hex: &str) -> Result<Self, Error> {
        Ok(Self {
            content: hex.to_string(),
            content_type: BytesEncoding::Hex,
        })
    }
}

impl From<BytesEnvelope> for Vec<u8> {
    fn from(envelope: BytesEnvelope) -> Self {
        match envelope.content_type {
            BytesEncoding::Base64 => base64_to_bytes(&envelope.content).unwrap(),
            BytesEncoding::Hex => hex_to_bytes(&envelope.content).unwrap(),
        }
    }
}

impl From<TirEnvelope> for Vec<u8> {
    fn from(envelope: TirEnvelope) -> Self {
        match envelope.encoding {
            BytesEncoding::Base64 => base64_to_bytes(&envelope.content).unwrap(),
            BytesEncoding::Hex => hex_to_bytes(&envelope.content).unwrap(),
        }
    }
}

impl TryFrom<TirEnvelope> for tx3_tir::encoding::AnyTir {
    type Error = crate::Error;

    fn try_from(envelope: TirEnvelope) -> Result<Self, Self::Error> {
        let version = tx3_tir::encoding::TirVersion::try_from(envelope.version.as_str())?;
        let bytes: Vec<u8> = envelope.into();
        let tir = tx3_tir::encoding::from_bytes(&bytes, version)?;
        Ok(tir)
    }
}

impl From<tx3_tir::encoding::AnyTir> for TirEnvelope {
    fn from(tir: tx3_tir::encoding::AnyTir) -> Self {
        let (bytes, version) = match tir {
            tx3_tir::encoding::AnyTir::V1Beta0(tx) => tx3_tir::encoding::to_bytes(&tx),
        };
        Self {
            content: hex::encode(bytes),
            encoding: BytesEncoding::Hex,
            version: version.to_string(),
        }
    }
}

fn has_hex_prefix(s: &str) -> bool {
    s.starts_with("0x")
}

pub fn string_to_bigint(s: String) -> Result<i128, Error> {
    if has_hex_prefix(&s) {
        let bytes = hex_to_bytes(&s)?;
        let bytes = <[u8; 16]>::try_from(bytes)
            .map_err(|x| Error::InvalidBytesForNumber(hex::encode(x)))?;
        Ok(i128::from_be_bytes(bytes))
    } else {
        let i = s
            .parse::<i128>()
            .map_err(|x| Error::InvalidBytesForNumber(x.to_string()))?;
        Ok(i)
    }
}

pub fn hex_to_bytes(s: &str) -> Result<Vec<u8>, Error> {
    let s = if has_hex_prefix(s) {
        s.trim_start_matches("0x")
    } else {
        s
    };

    let out = hex::decode(s)?;

    Ok(out)
}

pub fn base64_to_bytes(s: &str) -> Result<Vec<u8>, Error> {
    let out = base64::engine::general_purpose::STANDARD.decode(s)?;

    Ok(out)
}

pub fn bech32_to_bytes(s: &str) -> Result<Vec<u8>, Error> {
    let (_, data) = bech32::decode(s)?;

    Ok(data)
}

fn number_to_bigint(x: Number) -> Result<i128, Error> {
    x.as_i128().ok_or(Error::NumberCantFit(x))
}

fn value_to_bigint(value: Value) -> Result<i128, Error> {
    let out = match value {
        Value::Number(n) => number_to_bigint(n)?,
        Value::String(s) => string_to_bigint(s)?,
        Value::Null => return Err(Error::ValueIsNull),
        x => return Err(Error::ValueIsNotANumber(x)),
    };

    Ok(out)
}

fn value_to_bool(value: Value) -> Result<bool, Error> {
    match value {
        Value::Bool(b) => Ok(b),
        Value::Number(n) if n == Number::from(0) => Ok(false),
        Value::Number(n) if n == Number::from(1) => Ok(true),
        Value::String(s) if s == "true" => Ok(true),
        Value::String(s) if s == "false" => Ok(false),
        x => Err(Error::ValueIsNotABool(x)),
    }
}

fn value_to_bytes(value: Value) -> Result<Vec<u8>, Error> {
    let out = match value {
        Value::String(s) => hex_to_bytes(&s)?,
        Value::Object(_) => {
            let envelope: BytesEnvelope =
                serde_json::from_value(value).map_err(Error::InvalidBytesEnvelope)?;

            match envelope.content_type {
                BytesEncoding::Base64 => base64_to_bytes(&envelope.content)?,
                BytesEncoding::Hex => hex_to_bytes(&envelope.content)?,
            }
        }
        x => return Err(Error::ValueIsNotBytes(x)),
    };

    Ok(out)
}

fn value_to_address(value: Value) -> Result<Vec<u8>, Error> {
    let out = match value {
        Value::String(s) => match bech32_to_bytes(&s) {
            Ok(data) => data,
            Err(_) => hex_to_bytes(&s)?,
        },
        x => return Err(Error::ValueIsNotAnAddress(x)),
    };

    Ok(out)
}

fn value_to_underfined(value: Value) -> Result<ArgValue, Error> {
    match value {
        Value::Bool(b) => Ok(ArgValue::Bool(b)),
        Value::Number(x) => Ok(ArgValue::Int(number_to_bigint(x)?)),
        Value::String(s) => Ok(ArgValue::String(s)),
        x => Err(Error::CantInferTypeForValue(x)),
    }
}

pub fn string_to_utxo_ref(s: &str) -> Result<UtxoRef, Error> {
    let (txid, index) = s
        .split_once('#')
        .ok_or(Error::InvalidUtxoRef(s.to_string()))?;

    let txid = hex::decode(txid).map_err(|_| Error::InvalidUtxoRef(s.to_string()))?;
    let index = index
        .parse()
        .map_err(|_| Error::InvalidUtxoRef(s.to_string()))?;

    Ok(UtxoRef { txid, index })
}

fn value_to_utxo_ref(value: Value) -> Result<UtxoRef, Error> {
    match value {
        Value::String(s) => string_to_utxo_ref(&s),
        x => Err(Error::ValueIsNotUtxoRef(x)),
    }
}

/// Returns the single `(key, value)` of a one-key object, else `None`.
fn single_key(value: &Value) -> Option<(&str, &Value)> {
    let obj = value.as_object()?;
    if obj.len() != 1 {
        return None;
    }
    obj.iter().next().map(|(k, v)| (k.as_str(), v))
}

/// Whether `value` is a tagged node: a single-key object whose key
/// is one of the wire tags. A tagged value is self-describing and decodes the
/// same way whatever its kind; anything else is a bare (legacy) scalar.
fn is_tagged(value: &Value) -> bool {
    matches!(
        single_key(value),
        Some((
            "int"
                | "bool"
                | "string"
                | "bytes"
                | "address"
                | "utxoRef"
                | "list"
                | "tuple"
                | "map"
                | "struct",
            _,
        ))
    )
}

/// Binds a JSON argument value to its [`ArgValue`].
///
/// One path for every argument: a tagged value decodes from its tags via
/// [`decode_tagged`] regardless of kind, while a bare value (no tag) is coerced
/// via the param's flat TIR [`Type`] — the legacy form a top-level scalar may
/// still take.
pub fn from_json(value: Value, target: &Type) -> Result<ArgValue, Error> {
    if is_tagged(&value) {
        decode_tagged(value)
    } else {
        coerce_bare(value, target)
    }
}

/// Coerces a bare (untagged) value via the param's flat [`Type`]. Only scalars
/// may be sent bare; an aggregate must arrive tagged.
fn coerce_bare(value: Value, target: &Type) -> Result<ArgValue, Error> {
    match target {
        Type::Int => Ok(ArgValue::Int(value_to_bigint(value)?)),
        Type::Bool => Ok(ArgValue::Bool(value_to_bool(value)?)),
        Type::Bytes => Ok(ArgValue::Bytes(value_to_bytes(value)?)),
        Type::Address => Ok(ArgValue::Address(value_to_address(value)?)),
        Type::UtxoRef => Ok(ArgValue::UtxoRef(value_to_utxo_ref(value)?)),
        Type::Undefined => value_to_underfined(value),
        x => Err(Error::TargetTypeNotSupported(x.clone())),
    }
}

/// Decodes a tagged JSON value into an [`ArgValue`], recursively.
/// Every node carries its own tag and struct fields are positional, so no schema
/// is needed; scalar leaves reuse the per-scalar coercions.
fn decode_tagged(value: Value) -> Result<ArgValue, Error> {
    let Some((tag, _)) = single_key(&value) else {
        return Err(Error::MalformedTaggedArg(value));
    };
    let tag = tag.to_string();
    // Take ownership of the inner value out of the one-key object.
    let inner = match value {
        Value::Object(mut m) => m.remove(&tag).expect("single key present"),
        _ => unreachable!("single_key guaranteed an object"),
    };

    match tag.as_str() {
        "int" => Ok(ArgValue::Int(value_to_bigint(inner)?)),
        "bool" => Ok(ArgValue::Bool(value_to_bool(inner)?)),
        "string" => match inner {
            Value::String(s) => Ok(ArgValue::String(s)),
            _ => Err(Error::ValueIsNotAString),
        },
        "bytes" => Ok(ArgValue::Bytes(value_to_bytes(inner)?)),
        "address" => Ok(ArgValue::Address(value_to_address(inner)?)),
        "utxoRef" => Ok(ArgValue::UtxoRef(value_to_utxo_ref(inner)?)),
        "list" => Ok(ArgValue::List(decode_seq(inner)?)),
        "tuple" => Ok(ArgValue::Tuple(decode_seq(inner)?)),
        "map" => Ok(ArgValue::Map(decode_map(inner)?)),
        "struct" => decode_struct(inner),
        other => Err(Error::UnknownTag(other.to_string())),
    }
}

/// Decodes an array of tagged values (the body of a `list`/`tuple`/struct fields).
fn decode_seq(value: Value) -> Result<Vec<ArgValue>, Error> {
    match value {
        Value::Array(items) => items.into_iter().map(decode_tagged).collect(),
        x => Err(Error::MalformedTaggedArg(x)),
    }
}

/// Decodes a `map` body: an array of `[key, value]` tagged pairs.
fn decode_map(value: Value) -> Result<Vec<(ArgValue, ArgValue)>, Error> {
    let arr = match value {
        Value::Array(a) => a,
        x => return Err(Error::MalformedTaggedArg(x)),
    };

    arr.into_iter()
        .map(|pair| match pair {
            Value::Array(mut kv) if kv.len() == 2 => {
                let v = kv.pop().unwrap();
                let k = kv.pop().unwrap();
                Ok((decode_tagged(k)?, decode_tagged(v)?))
            }
            x => Err(Error::MalformedMapPair(x)),
        })
        .collect()
}

/// Decodes a `struct` body: `{ "constructor": <usize>, "fields": [TaggedArg, …] }`.
fn decode_struct(value: Value) -> Result<ArgValue, Error> {
    let mut obj = match value {
        Value::Object(o) => o,
        x => return Err(Error::MalformedStruct(x)),
    };

    let constructor =
        obj.get("constructor")
            .and_then(Value::as_u64)
            .ok_or_else(|| Error::MalformedStruct(Value::Object(obj.clone())))? as usize;

    let fields = obj
        .remove("fields")
        .ok_or_else(|| Error::MalformedStruct(Value::Object(obj.clone())))?;

    Ok(ArgValue::Struct {
        constructor,
        fields: decode_seq(fields)?,
    })
}

// ---------------------------------------------------------------------------
// Rust → JSON marshalling
// ---------------------------------------------------------------------------

pub fn utxo_ref_to_json(r: &UtxoRef) -> Value {
    Value::String(format!("{}#{}", hex::encode(&r.txid), r.index))
}

pub fn arg_to_json(arg: &ArgValue) -> Value {
    match arg {
        ArgValue::Int(i) => serde_json::json!(i),
        ArgValue::Bool(b) => Value::Bool(*b),
        ArgValue::String(s) => Value::String(s.clone()),
        ArgValue::Bytes(v) => Value::String(hex::encode(v)),
        ArgValue::Address(v) => Value::String(hex::encode(v)),
        ArgValue::UtxoRef(r) => utxo_ref_to_json(r),
        ArgValue::UtxoSet(_) => Value::Null,
        // Aggregates round-trip through the tagged form (re-decodable by
        // `decode_tagged`); scalars stay bare above.
        ArgValue::List(_) | ArgValue::Tuple(_) | ArgValue::Map(_) | ArgValue::Struct { .. } => {
            tagged_arg_to_json(arg)
        }
    }
}

/// Serializes any [`ArgValue`] into its fully-tagged JSON form — the
/// inverse of [`decode_tagged`], with every leaf tagged.
pub fn tagged_arg_to_json(arg: &ArgValue) -> Value {
    match arg {
        ArgValue::Int(i) => serde_json::json!({ "int": i }),
        ArgValue::Bool(b) => serde_json::json!({ "bool": b }),
        ArgValue::String(s) => serde_json::json!({ "string": s }),
        ArgValue::Bytes(v) => serde_json::json!({ "bytes": hex::encode(v) }),
        ArgValue::Address(v) => serde_json::json!({ "address": hex::encode(v) }),
        ArgValue::UtxoRef(r) => {
            serde_json::json!({ "utxoRef": format!("{}#{}", hex::encode(&r.txid), r.index) })
        }
        // A resolved UTxO set is not a tagged leaf; surface it as null.
        ArgValue::UtxoSet(_) => Value::Null,
        ArgValue::List(xs) => {
            serde_json::json!({ "list": xs.iter().map(tagged_arg_to_json).collect::<Vec<_>>() })
        }
        ArgValue::Tuple(xs) => {
            serde_json::json!({ "tuple": xs.iter().map(tagged_arg_to_json).collect::<Vec<_>>() })
        }
        ArgValue::Map(pairs) => serde_json::json!({
            "map": pairs
                .iter()
                .map(|(k, v)| vec![tagged_arg_to_json(k), tagged_arg_to_json(v)])
                .collect::<Vec<_>>()
        }),
        ArgValue::Struct {
            constructor,
            fields,
        } => serde_json::json!({
            "struct": {
                "constructor": constructor,
                "fields": fields.iter().map(tagged_arg_to_json).collect::<Vec<_>>(),
            }
        }),
    }
}

pub fn utxo_to_json(utxo: &Utxo) -> Value {
    let assets: Map<String, Value> = utxo
        .assets
        .iter()
        .map(|(class, amount)| (class.to_string(), serde_json::json!(amount)))
        .collect();

    serde_json::json!({
        "ref": utxo_ref_to_json(&utxo.r#ref),
        "address": hex::encode(&utxo.address),
        "assets": assets,
        "datum": utxo.datum,
        "script": utxo.script,
    })
}

fn parse_asset_class(key: &str) -> AssetClass {
    if key == "naked" {
        AssetClass::Naked
    } else if let Some((policy, name)) = key.split_once('.') {
        let policy_bytes = hex::decode(policy).unwrap_or_default();
        let name_bytes = hex::decode(name).unwrap_or_default();
        AssetClass::Defined(policy_bytes, name_bytes)
    } else {
        let name_bytes = hex::decode(key).unwrap_or_default();
        AssetClass::Named(name_bytes)
    }
}

fn assets_from_json(value: &Value) -> Result<CanonicalAssets, Error> {
    let obj = value.as_object().ok_or(Error::ValueIsNotAString)?;

    let mut assets = CanonicalAssets::empty();
    for (key, amount_val) in obj {
        let class = parse_asset_class(key);
        let amount = value_to_bigint(amount_val.clone())?;
        assets = assets + CanonicalAssets::from_class_and_amount(class, amount);
    }

    Ok(assets)
}

pub fn utxo_from_json(value: &Value) -> Result<Utxo, Error> {
    let ref_str = value["ref"].as_str().ok_or(Error::ValueIsNotAString)?;
    let utxo_ref = string_to_utxo_ref(ref_str)?;

    let address = hex_to_bytes(value["address"].as_str().ok_or(Error::ValueIsNotAString)?)?;

    let assets = assets_from_json(&value["assets"])?;

    let datum = value
        .get("datum")
        .filter(|v| !v.is_null())
        .map(|v| serde_json::from_value(v.clone()))
        .transpose()
        .map_err(Error::InvalidBytesEnvelope)?;

    let script = value
        .get("script")
        .filter(|v| !v.is_null())
        .map(|v| serde_json::from_value(v.clone()))
        .transpose()
        .map_err(Error::InvalidBytesEnvelope)?;

    Ok(Utxo {
        r#ref: utxo_ref,
        address,
        assets,
        datum,
        script,
    })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn assert_from_json(provided: Value, target: Type, expected: ArgValue) {
        let value = from_json(provided, &target).unwrap();
        assert_eq!(value, expected);
    }

    // -----------------------------------------------------------------------
    // JSON → Rust (from_json)
    // -----------------------------------------------------------------------

    #[test]
    fn from_json_small_int() {
        assert_from_json(json!(123456789), Type::Int, ArgValue::Int(123456789));
    }

    #[test]
    fn from_json_negative_int() {
        assert_from_json(json!(-123456789), Type::Int, ArgValue::Int(-123456789));
    }

    #[test]
    fn from_json_big_int() {
        assert_from_json(
            json!("12345678901234567890"),
            Type::Int,
            ArgValue::Int(12345678901234567890),
        );
    }

    #[test]
    fn from_json_int_overflow() {
        assert_from_json(
            json!(i128::MIN.to_string()),
            Type::Int,
            ArgValue::Int(i128::MIN),
        );
        assert_from_json(
            json!(i128::MAX.to_string()),
            Type::Int,
            ArgValue::Int(i128::MAX),
        );
    }

    #[test]
    fn from_json_bool() {
        assert_from_json(json!(true), Type::Bool, ArgValue::Bool(true));
        assert_from_json(json!(false), Type::Bool, ArgValue::Bool(false));
    }

    #[test]
    fn from_json_bool_number() {
        assert_from_json(json!(1), Type::Bool, ArgValue::Bool(true));
        assert_from_json(json!(0), Type::Bool, ArgValue::Bool(false));
    }

    #[test]
    fn from_json_bool_string() {
        assert_from_json(json!("true"), Type::Bool, ArgValue::Bool(true));
        assert_from_json(json!("false"), Type::Bool, ArgValue::Bool(false));
    }

    #[test]
    fn from_json_bytes() {
        assert_from_json(
            json!(hex::encode("hello")),
            Type::Bytes,
            ArgValue::Bytes(b"hello".to_vec()),
        );

        assert_from_json(
            json!(format!("0x{}", hex::encode("hello"))),
            Type::Bytes,
            ArgValue::Bytes(b"hello".to_vec()),
        );
    }

    #[test]
    fn from_json_bytes_base64() {
        let json = json!(BytesEnvelope {
            content: "aGVsbG8=".to_string(),
            content_type: BytesEncoding::Base64,
        });

        assert_from_json(json, Type::Bytes, ArgValue::Bytes(b"hello".to_vec()));
    }

    #[test]
    fn from_json_bytes_hex() {
        let json = json!(BytesEnvelope {
            content: "68656c6c6f".to_string(),
            content_type: BytesEncoding::Hex,
        });

        assert_from_json(json, Type::Bytes, ArgValue::Bytes(b"hello".to_vec()));
    }

    #[test]
    fn from_json_address() {
        assert_from_json(
            json!(hex::encode("abc123")),
            Type::Address,
            ArgValue::Address(b"abc123".to_vec()),
        );
    }

    #[test]
    fn from_json_address_bech32() {
        let json = json!("addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8");
        let bytes =
            hex::decode("619493315cd92eb5d8c4304e67b7e16ae36d61d34502694657811a2c8e").unwrap();
        assert_from_json(json, Type::Address, ArgValue::Address(bytes));
    }

    #[test]
    fn from_json_utxo_ref() {
        let json = json!("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef#0");

        let utxo_ref = UtxoRef {
            txid: hex::decode("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
                .unwrap(),
            index: 0,
        };

        assert_from_json(json, Type::UtxoRef, ArgValue::UtxoRef(utxo_ref));
    }

    #[test]
    fn from_json_undefined_infers_from_shape() {
        // The untyped fallback: a bare value binds by its JSON shape.
        assert_from_json(json!(true), Type::Undefined, ArgValue::Bool(true));
        assert_from_json(json!(42), Type::Undefined, ArgValue::Int(42));
        assert_from_json(json!("hi"), Type::Undefined, ArgValue::String("hi".into()));
    }

    #[test]
    fn bytes_envelope_object_is_not_mistaken_for_a_tag() {
        // A two-key BytesEnvelope (the only object-shaped legacy scalar input)
        // must route to bare coercion, never to the tagged decoder.
        let envelope = json!({ "content": "aGVsbG8=", "contentType": "base64" });
        assert_from_json(envelope, Type::Bytes, ArgValue::Bytes(b"hello".to_vec()));
    }

    #[test]
    fn decode_list_of_int() {
        assert_from_json(
            json!({ "list": [{ "int": 1 }, { "int": 2 }, { "int": 3 }] }),
            Type::List,
            ArgValue::List(vec![ArgValue::Int(1), ArgValue::Int(2), ArgValue::Int(3)]),
        );
    }

    #[test]
    fn decode_tuple_int_bytes() {
        assert_from_json(
            json!({ "tuple": [{ "int": 42 }, { "bytes": "cafe" }] }),
            Type::Tuple,
            ArgValue::Tuple(vec![ArgValue::Int(42), ArgValue::Bytes(vec![0xca, 0xfe])]),
        );
    }

    #[test]
    fn decode_map_string_keys() {
        assert_from_json(
            json!({ "map": [[{ "string": "1" }, { "int": 100 }], [{ "string": "2" }, { "int": 200 }]] }),
            Type::Map,
            ArgValue::Map(vec![
                (ArgValue::String("1".into()), ArgValue::Int(100)),
                (ArgValue::String("2".into()), ArgValue::Int(200)),
            ]),
        );
    }

    #[test]
    fn decode_struct_record() {
        // AssetClass { policy: Bytes, name: Bytes } in declared order.
        assert_from_json(
            json!({ "struct": { "constructor": 0, "fields": [{ "bytes": "aabb" }, { "bytes": "0011" }] } }),
            Type::Custom("AssetClass".into()),
            ArgValue::Struct {
                constructor: 0,
                fields: vec![
                    ArgValue::Bytes(vec![0xaa, 0xbb]),
                    ArgValue::Bytes(vec![0x00, 0x11]),
                ],
            },
        );
    }

    #[test]
    fn decode_nested_struct_05_invoke() {
        // The journey-critical shape: Meta { tags: List<Int>, level: Int }.
        assert_from_json(
            json!({
                "struct": {
                    "constructor": 0,
                    "fields": [
                        { "list": [{ "int": 1 }, { "int": 2 }, { "int": 3 }] },
                        { "int": 7 }
                    ]
                }
            }),
            Type::Custom("Meta".into()),
            ArgValue::Struct {
                constructor: 0,
                fields: vec![
                    ArgValue::List(vec![ArgValue::Int(1), ArgValue::Int(2), ArgValue::Int(3)]),
                    ArgValue::Int(7),
                ],
            },
        );
    }

    #[test]
    fn decode_accepts_tagged_scalar_leniently() {
        assert_from_json(json!({ "int": 5 }), Type::Int, ArgValue::Int(5));
        assert_from_json(
            json!({ "bytes": "cafe" }),
            Type::Bytes,
            ArgValue::Bytes(vec![0xca, 0xfe]),
        );
    }

    #[test]
    fn decode_round_trips_through_encoded_json() {
        let encoded = json!({
            "struct": {
                "constructor": 0,
                "fields": [{ "list": [{ "int": 1 }, { "int": 2 }] }, { "int": 7 }]
            }
        });
        let arg = from_json(encoded.clone(), &Type::Custom("Meta".into())).unwrap();
        assert_eq!(tagged_arg_to_json(&arg), encoded);
    }

    #[test]
    fn decode_rejects_unknown_tag() {
        // A single-key object whose key is a known tag but unknown nested tag.
        let err = from_json(json!({ "list": [{ "bogus": 1 }] }), &Type::List).unwrap_err();
        assert!(matches!(err, Error::UnknownTag(_)), "got {err:?}");
    }

    #[test]
    fn bare_aggregate_is_rejected() {
        // A bare array carries no tag, so it can't bind to an aggregate param.
        let err = from_json(json!([1, 2, 3]), &Type::List).unwrap_err();
        assert!(
            matches!(err, Error::TargetTypeNotSupported(_)),
            "got {err:?}"
        );
    }

    #[test]
    fn decode_rejects_malformed_struct() {
        // Missing `fields`.
        let err = from_json(
            json!({ "struct": { "constructor": 0 } }),
            &Type::Custom("Meta".into()),
        )
        .unwrap_err();
        assert!(matches!(err, Error::MalformedStruct(_)), "got {err:?}");
    }

    #[test]
    fn decode_rejects_malformed_map_pair() {
        let err = from_json(json!({ "map": [[{ "int": 1 }]] }), &Type::Map).unwrap_err();
        assert!(matches!(err, Error::MalformedMapPair(_)), "got {err:?}");
    }

    #[test]
    fn decode_rejects_untagged_nested_value() {
        // A bare element inside a tagged list has no tag to decode by.
        let err = from_json(json!({ "list": [5] }), &Type::List).unwrap_err();
        assert!(matches!(err, Error::MalformedTaggedArg(_)), "got {err:?}");
    }

    // -----------------------------------------------------------------------
    // Rust → JSON (to_json)
    // -----------------------------------------------------------------------

    use tx3_tir::model::assets::{AssetClass, CanonicalAssets};
    use tx3_tir::model::core::Utxo;

    fn sample_utxo_ref() -> UtxoRef {
        UtxoRef {
            txid: hex::decode("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
                .unwrap(),
            index: 2,
        }
    }

    #[test]
    fn utxo_ref_to_json_format() {
        let r = sample_utxo_ref();
        let v = utxo_ref_to_json(&r);
        assert_eq!(
            v,
            json!("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef#2")
        );
    }

    #[test]
    fn arg_to_json_int() {
        assert_eq!(arg_to_json(&ArgValue::Int(42)), json!(42));
    }

    #[test]
    fn arg_to_json_bool() {
        assert_eq!(arg_to_json(&ArgValue::Bool(true)), json!(true));
    }

    #[test]
    fn arg_to_json_string() {
        assert_eq!(
            arg_to_json(&ArgValue::String("hello".into())),
            json!("hello")
        );
    }

    #[test]
    fn arg_to_json_bytes() {
        assert_eq!(
            arg_to_json(&ArgValue::Bytes(b"hello".to_vec())),
            json!("68656c6c6f")
        );
    }

    #[test]
    fn arg_to_json_address() {
        assert_eq!(
            arg_to_json(&ArgValue::Address(b"\x01\x02".to_vec())),
            json!("0102")
        );
    }

    #[test]
    fn arg_to_json_utxo_ref() {
        let r = sample_utxo_ref();
        assert_eq!(
            arg_to_json(&ArgValue::UtxoRef(r.clone())),
            utxo_ref_to_json(&r)
        );
    }

    #[test]
    fn utxo_to_json_empty_assets() {
        let utxo = Utxo {
            r#ref: sample_utxo_ref(),
            address: b"\xab\xcd".to_vec(),
            assets: CanonicalAssets::empty(),
            datum: None,
            script: None,
        };

        let v = utxo_to_json(&utxo);
        assert_eq!(v["assets"], json!({}));
        assert_eq!(v["address"], json!("abcd"));
        assert!(v["datum"].is_null());
        assert!(v["script"].is_null());
    }

    #[test]
    fn utxo_to_json_naked_assets() {
        let utxo = Utxo {
            r#ref: sample_utxo_ref(),
            address: b"\x01".to_vec(),
            assets: CanonicalAssets::from_naked_amount(5_000_000),
            datum: None,
            script: None,
        };

        let v = utxo_to_json(&utxo);
        assert_eq!(v["assets"]["naked"], json!(5_000_000));
    }

    #[test]
    fn utxo_to_json_defined_assets() {
        let assets = CanonicalAssets::from_class_and_amount(
            AssetClass::Defined(b"policy1".to_vec(), b"token1".to_vec()),
            100,
        );

        let utxo = Utxo {
            r#ref: sample_utxo_ref(),
            address: b"\x01".to_vec(),
            assets,
            datum: None,
            script: None,
        };

        let v = utxo_to_json(&utxo);
        let key = format!("{}.{}", hex::encode(b"policy1"), hex::encode(b"token1"));
        assert_eq!(v["assets"][&key], json!(100));
    }
}
