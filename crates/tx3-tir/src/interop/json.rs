use base64::Engine as _;
use serde::{Deserialize, Serialize};
use serde_json::{json, Number, Value};
use thiserror::Error;

pub use crate::reduce::ArgValue;
use crate::{
    model::v1beta0::{Type, UtxoRef},
    reduce::ArgMap,
};

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct BytesEnvelope {
    pub content: String,
    pub encoding: BytesEncoding,
}

impl BytesEnvelope {
    pub fn from_hex(hex: &str) -> Result<Self, Error> {
        Ok(Self {
            content: hex.to_string(),
            encoding: BytesEncoding::Hex,
        })
    }
}

impl From<BytesEnvelope> for Vec<u8> {
    fn from(envelope: BytesEnvelope) -> Self {
        match envelope.encoding {
            BytesEncoding::Base64 => base64_to_bytes(&envelope.content).unwrap(),
            BytesEncoding::Hex => hex_to_bytes(&envelope.content).unwrap(),
        }
    }
}

#[derive(Debug, Deserialize, Serialize, Clone)]
#[serde(rename_all = "lowercase")]
pub enum BytesEncoding {
    Base64,
    Hex,
}

#[derive(Debug, Deserialize, Serialize, Clone)]
pub struct TirEnvelope {
    pub content: String,
    pub encoding: BytesEncoding,
    pub version: String,
}

impl From<TirEnvelope> for Vec<u8> {
    fn from(envelope: TirEnvelope) -> Self {
        let bytes = match envelope.encoding {
            BytesEncoding::Base64 => base64_to_bytes(&envelope.content).unwrap(),
            BytesEncoding::Hex => hex_to_bytes(&envelope.content).unwrap(),
        };

        bytes
    }
}

impl TryFrom<TirEnvelope> for crate::model::v1beta0::Tx {
    type Error = super::Error;

    fn try_from(envelope: TirEnvelope) -> Result<Self, Self::Error> {
        let version = super::TirVersion::try_from(envelope.version.as_str())?;

        let bytes: Vec<u8> = envelope.into();

        if version != super::TirVersion::V1Beta0 {
            return Err(Self::Error::InvalidTirVersion(version.to_string()));
        }

        let tx: crate::model::v1beta0::Tx = super::from_bytes(&bytes)?;

        Ok(tx)
    }
}

fn utxoref_to_value(x: UtxoRef) -> Value {
    Value::String(format!("{}#{}", hex::encode(x.txid), x.index))
}

fn bigint_to_value(i: i128) -> Value {
    if i >= i64::MIN as i128 && i <= i64::MAX as i128 {
        Value::Number((i as i64).into())
    } else {
        let ashex = hex::encode(i.to_be_bytes());
        Value::String(format!("0x{ashex}"))
    }
}

fn number_to_bigint(x: Number) -> Result<i128, Error> {
    x.as_i128().ok_or(Error::NumberCantFit(x))
}

fn string_to_bigint(s: String) -> Result<i128, Error> {
    let bytes = hex_to_bytes(&s)?;
    let bytes =
        <[u8; 16]>::try_from(bytes).map_err(|x| Error::InvalidBytesForNumber(hex::encode(x)))?;
    Ok(i128::from_be_bytes(bytes))
}

fn value_to_bigint(value: Value) -> Result<i128, Error> {
    match value {
        Value::Number(n) => number_to_bigint(n),
        Value::String(s) => string_to_bigint(s),
        Value::Null => Err(Error::ValueIsNull),
        x => Err(Error::ValueIsNotANumber(x)),
    }
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

fn hex_to_bytes(s: &str) -> Result<Vec<u8>, Error> {
    let s = if s.starts_with("0x") {
        s.trim_start_matches("0x")
    } else {
        s
    };

    hex::decode(s).map_err(Error::InvalidHex)
}

fn base64_to_bytes(s: &str) -> Result<Vec<u8>, Error> {
    base64::engine::general_purpose::STANDARD
        .decode(s)
        .map_err(Error::InvalidBase64)
}

fn value_to_bytes(value: Value) -> Result<Vec<u8>, Error> {
    match value {
        Value::String(s) => hex_to_bytes(&s),
        Value::Object(_) => {
            let envelope: BytesEnvelope =
                serde_json::from_value(value).map_err(Error::InvalidBytesEnvelope)?;

            match envelope.encoding {
                BytesEncoding::Base64 => base64_to_bytes(&envelope.content),
                BytesEncoding::Hex => hex_to_bytes(&envelope.content),
            }
        }
        x => Err(Error::ValueIsNotBytes(x)),
    }
}

fn bech32_to_bytes(s: &str) -> Result<Vec<u8>, Error> {
    let (_, data) = bech32::decode(s).map_err(Error::InvalidBech32)?;
    Ok(data)
}

fn value_to_address(value: Value) -> Result<Vec<u8>, Error> {
    match value {
        Value::String(s) => match bech32_to_bytes(&s) {
            Ok(data) => Ok(data),
            Err(_) => hex_to_bytes(&s),
        },
        x => Err(Error::ValueIsNotAnAddress(x)),
    }
}

fn value_to_underfined(value: Value) -> Result<ArgValue, Error> {
    match value {
        Value::Bool(b) => Ok(ArgValue::Bool(b)),
        Value::Number(x) => Ok(ArgValue::Int(number_to_bigint(x)?)),
        Value::String(s) => Ok(ArgValue::String(s)),
        x => Err(Error::CantInferTypeForValue(x)),
    }
}

fn string_to_utxo_ref(s: &str) -> Result<UtxoRef, Error> {
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

#[derive(Debug, Error)]
pub enum Error {
    #[error("value is null")]
    ValueIsNull,

    #[error("can't infer type for value: {0}")]
    CantInferTypeForValue(Value),

    #[error("value is not a number: {0}")]
    ValueIsNotANumber(Value),

    #[error("value can't fit: {0}")]
    NumberCantFit(Number),

    #[error("value is not a valid number: {0}")]
    InvalidBytesForNumber(String),

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

    #[error("invalid base64: {0}")]
    InvalidBase64(base64::DecodeError),

    #[error("invalid hex: {0}")]
    InvalidHex(hex::FromHexError),

    #[error("invalid bech32: {0}")]
    InvalidBech32(bech32::DecodeError),

    #[error("value is not an address: {0}")]
    ValueIsNotAnAddress(Value),

    #[error("invalid utxo ref: {0}")]
    InvalidUtxoRef(String),

    #[error("target type not supported: {0:?}")]
    TargetTypeNotSupported(Type),
}

pub fn to_json(value: ArgValue) -> Value {
    match value {
        ArgValue::Int(i) => bigint_to_value(i),
        ArgValue::Bool(b) => Value::Bool(b),
        ArgValue::String(s) => Value::String(s),
        ArgValue::Bytes(b) => Value::String(format!("0x{}", hex::encode(b))),
        ArgValue::Address(a) => Value::String(hex::encode(a)),
        ArgValue::UtxoSet(x) => {
            let v = x.into_iter().map(|x| json!(x)).collect();
            Value::Array(v)
        }
        ArgValue::UtxoRef(x) => utxoref_to_value(x),
    }
}

pub fn to_json_object(map: ArgMap) -> Value {
    let v = map.into_iter().map(|(k, v)| (k, to_json(v))).collect();
    Value::Object(v)
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

#[cfg(test)]
mod tests {
    use super::*;

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

    fn json_to_value_test(provided: Value, target: Type, expected: ArgValue) {
        let value = from_json(provided, &target).unwrap();
        assert!(partial_eq(value, expected));
    }

    fn round_trip_test(value: ArgValue, target: Type) {
        let json = to_json(value.clone());
        dbg!(&json);
        let value2 = from_json(json, &target).unwrap();
        assert!(partial_eq(value, value2));
    }

    #[test]
    fn test_round_trip_small_int() {
        round_trip_test(ArgValue::Int(123456789), Type::Int);
    }

    #[test]
    fn test_round_trip_negative_int() {
        round_trip_test(ArgValue::Int(-123456789), Type::Int);
    }

    #[test]
    fn test_round_trip_big_int() {
        round_trip_test(ArgValue::Int(12345678901234567890), Type::Int);
    }

    #[test]
    fn test_round_trip_int_overflow() {
        round_trip_test(ArgValue::Int(i128::MIN), Type::Int);
        round_trip_test(ArgValue::Int(i128::MAX), Type::Int);
    }

    #[test]
    fn test_round_trip_bool() {
        round_trip_test(ArgValue::Bool(true), Type::Bool);
        round_trip_test(ArgValue::Bool(false), Type::Bool);
    }

    #[test]
    fn test_round_trip_bool_number() {
        json_to_value_test(json!(1), Type::Bool, ArgValue::Bool(true));
        json_to_value_test(json!(0), Type::Bool, ArgValue::Bool(false));
    }

    #[test]
    fn test_round_trip_bool_string() {
        json_to_value_test(json!("true"), Type::Bool, ArgValue::Bool(true));
        json_to_value_test(json!("false"), Type::Bool, ArgValue::Bool(false));
    }

    #[test]
    fn test_round_trip_bytes() {
        round_trip_test(ArgValue::Bytes(b"hello".to_vec()), Type::Bytes);
    }

    #[test]
    fn test_round_trip_bytes_base64() {
        let json = json!(BytesEnvelope {
            content: "aGVsbG8=".to_string(),
            encoding: BytesEncoding::Base64,
        });

        json_to_value_test(json, Type::Bytes, ArgValue::Bytes(b"hello".to_vec()));
    }

    #[test]
    fn test_round_trip_bytes_hex() {
        let json = json!(BytesEnvelope {
            content: "68656c6c6f".to_string(),
            encoding: BytesEncoding::Hex,
        });

        json_to_value_test(json, Type::Bytes, ArgValue::Bytes(b"hello".to_vec()));
    }

    #[test]
    fn test_round_trip_address() {
        round_trip_test(ArgValue::Address(b"abc123".to_vec()), Type::Address);
    }

    #[test]
    fn test_round_trip_address_bech32() {
        let json = json!("addr1vx2fxv2umyhttkxyxp8x0dlpdt3k6cwng5pxj3jhsydzers66hrl8");
        let bytes =
            hex::decode("619493315cd92eb5d8c4304e67b7e16ae36d61d34502694657811a2c8e").unwrap();
        json_to_value_test(json, Type::Address, ArgValue::Address(bytes));
    }

    #[test]
    fn test_round_trip_utxo_ref() {
        let json = json!("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef#0");

        let utxo_ref = UtxoRef {
            txid: hex::decode("0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef")
                .unwrap(),
            index: 0,
        };

        json_to_value_test(json, Type::UtxoRef, ArgValue::UtxoRef(utxo_ref));
    }
}
