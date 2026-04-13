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
        let i = i128::from_str_radix(&s, 10)
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

pub fn from_json(value: Value, target: &Type) -> Result<ArgValue, Error> {
    match target {
        Type::Int => {
            let i = value_to_bigint(value)?;
            Ok(ArgValue::Int(i))
        }
        Type::Bool => {
            let b = value_to_bool(value)?;
            Ok(ArgValue::Bool(b))
        }
        Type::Bytes => {
            let b = value_to_bytes(value)?;
            Ok(ArgValue::Bytes(b))
        }
        Type::Address => {
            let a = value_to_address(value)?;
            Ok(ArgValue::Address(a))
        }
        Type::UtxoRef => {
            let x = value_to_utxo_ref(value)?;
            Ok(ArgValue::UtxoRef(x))
        }
        Type::Undefined => value_to_underfined(value),
        x => Err(Error::TargetTypeNotSupported(x.clone())),
    }
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
        .map_err(|e| Error::InvalidBytesEnvelope(e))?;

    let script = value
        .get("script")
        .filter(|v| !v.is_null())
        .map(|v| serde_json::from_value(v.clone()))
        .transpose()
        .map_err(|e| Error::InvalidBytesEnvelope(e))?;

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

    // TODO: derive PartialEq in upstream tx3-lang
    fn partial_eq(a: ArgValue, b: ArgValue) -> bool {
        match a {
            ArgValue::Int(a) => match b {
                ArgValue::Int(b) => dbg!(a) == dbg!(b),
                _ => false,
            },
            ArgValue::Bool(a) => match b {
                ArgValue::Bool(b) => a == b,
                _ => false,
            },
            ArgValue::String(a) => match b {
                ArgValue::String(b) => a == b,
                _ => false,
            },
            ArgValue::Bytes(a) => match b {
                ArgValue::Bytes(b) => a == b,
                _ => false,
            },
            ArgValue::Address(a) => match b {
                ArgValue::Address(b) => a == b,
                _ => false,
            },
            ArgValue::UtxoSet(hash_set) => match b {
                ArgValue::UtxoSet(b) => hash_set == b,
                _ => false,
            },
            ArgValue::UtxoRef(utxo_ref) => match b {
                ArgValue::UtxoRef(b) => utxo_ref == b,
                _ => false,
            },
        }
    }

    fn assert_from_json(provided: Value, target: Type, expected: ArgValue) {
        let value = from_json(provided, &target).unwrap();
        assert!(partial_eq(value, expected));
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
