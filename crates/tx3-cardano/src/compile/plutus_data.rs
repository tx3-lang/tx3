pub use pallas::codec::utils::Int;
use pallas::{codec::utils::KeyValuePairs, ledger::addresses};
pub use pallas::ledger::primitives::{BigInt, BoundedBytes, Constr, MaybeIndefArray, PlutusData};
use tx3_lang::ir;
use std::collections::HashMap;

pub trait IntoData {
    fn as_data(&self) -> PlutusData;
}

pub trait TryIntoData {
    fn try_as_data(&self) -> Result<PlutusData, super::Error>;
}

macro_rules! constr {
    ($index:expr, $($field:expr),*) => {
        {
            let fields = vec![$($field.into_data()),*];
            $crate::compile::plutus_data::constr($index as u64, fields)
        }
    };
}

pub fn constr(index: u64, fields: Vec<PlutusData>) -> PlutusData {
    PlutusData::Constr(Constr {
        tag: 121 + index,
        any_constructor: None,
        fields: MaybeIndefArray::Def(fields),
    })
}

impl IntoData for () {
    fn as_data(&self) -> PlutusData {
        constr!(0,)
    }
}

impl IntoData for PlutusData {
    fn as_data(&self) -> PlutusData {
        self.clone()
    }
}

impl IntoData for bool {
    fn as_data(&self) -> PlutusData {
        constr(*self as u64, vec![])
    }
}

impl IntoData for &str {
    fn as_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(BoundedBytes::from(self.as_bytes().to_vec()))
    }
}

impl IntoData for &[u8] {
    fn as_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(BoundedBytes::from(self.to_vec()))
    }
}

impl<const N: usize> IntoData for [u8; N] {
    fn as_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(BoundedBytes::from(self.to_vec()))
    }
}

impl IntoData for Vec<u8> {
    fn as_data(&self) -> PlutusData {
        PlutusData::BoundedBytes(BoundedBytes::from(self.clone()))
    }
}

impl IntoData for u64 {
    fn as_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(Int::from(*self as i64)))
    }
}

impl IntoData for i64 {
    fn as_data(&self) -> PlutusData {
        PlutusData::BigInt(BigInt::Int(Int::from(*self)))
    }
}

impl IntoData for i128 {
    fn as_data(&self) -> PlutusData {
        let int = Int::try_from(*self).unwrap();
        PlutusData::BigInt(BigInt::Int(int))
    }
}

impl TryIntoData for Vec<ir::Expression> {
    fn try_as_data(&self) -> Result<PlutusData, super::Error> {
        let items = self
            .iter()
            .map(TryIntoData::try_as_data)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(PlutusData::Array(MaybeIndefArray::Def(items)))
    }
}

impl TryIntoData for Vec<(ir::Expression, ir::Expression)> {
    fn try_as_data(&self) -> Result<PlutusData, super::Error> {
        let items = self
            .iter()
            .map(|(k, v)| Ok((k.try_as_data()?, v.try_as_data()?)))
            .collect::<Result<Vec<_>, super::Error>>()?;

        Ok(PlutusData::Map(KeyValuePairs::Def(items)))
    }
}

impl TryIntoData for ir::StructExpr {
    fn try_as_data(&self) -> Result<PlutusData, super::Error> {
        let fields = self
            .fields
            .iter()
            .map(TryIntoData::try_as_data)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(constr(self.constructor as u64, fields))
    }
}

impl<T> IntoData for Option<T>
where
    T: IntoData,
{
    fn as_data(&self) -> PlutusData {
        match self {
            Some(value) => value.as_data(),
            None => ().as_data(),
        }
    }
}

impl TryIntoData for ir::Expression {
    fn try_as_data(&self) -> Result<PlutusData, super::Error> {
        match self {
            ir::Expression::None => Ok(().as_data()),
            ir::Expression::Struct(x) => x.try_as_data(),
            ir::Expression::Bytes(x) => Ok(x.as_data()),
            ir::Expression::Number(x) => Ok(x.as_data()),
            ir::Expression::Bool(x) => Ok(x.as_data()),
            ir::Expression::String(x) => Ok(x.as_bytes().as_data()),
            ir::Expression::Address(x) => Ok(x.as_data()),
            ir::Expression::Hash(x) => Ok(x.as_data()),
            ir::Expression::List(x) => x.try_as_data(),
            ir::Expression::Map(x) => x.try_as_data(),
            ir::Expression::AdHocDirective(x) => match x.name.as_str() {
                "cardano_address_payment_part" => address_payment_part(&x.data),
                "cardano_address_staking_part" => address_staking_part(&x.data),
                _ => Ok(().as_data()),
            },
            _ => Err(super::Error::CoerceError(
                format!("{self:?}"),
                "PlutusData".to_string(),
            )),
        }
    }
}

fn extract_address(data: &HashMap<String, ir::Expression>) -> Result<addresses::Address, super::Error> {
    data.get("address")
        .ok_or_else(|| super::Error::CoerceError(
            "Address field not found in data".to_string(),
            "Address".to_string(),
        ))
        .and_then(|expr| match expr {
            ir::Expression::Address(bytes) => {
                addresses::Address::from_bytes(bytes)
                    .map_err(|_| super::Error::CoerceError(
                        "Invalid address bytes".to_string(),
                        "Address".to_string(),
                    ))
            }
            _ => Err(super::Error::CoerceError(
                format!("Expected Address expression, found {:?}", expr),
                "Address".to_string(),
            )),
        })
}

fn address_payment_part(data: &HashMap<String, ir::Expression>) -> Result<PlutusData, super::Error> {
    let addr = extract_address(data)?;

    match addr {
        addresses::Address::Shelley(shelley_addr) => {
            // Extract the payment credential from the Shelley address
            let payment_bytes = shelley_addr.payment().to_vec();
            Ok(payment_bytes.as_data())
        }
        addresses::Address::Byron(byron_addr) => {
            // Extract the payload from the Byron address
            let payload_bytes = byron_addr.to_vec();
            Ok(payload_bytes.as_data())
        }
        _ => Err(super::Error::CoerceError(
            "Address type does not support payment parts".to_string(),
            "Payment credential".to_string(),
        )),
    }
}

fn address_staking_part(data: &HashMap<String, ir::Expression>) -> Result<PlutusData, super::Error> {
    let addr = extract_address(data)?;

    match addr {
        addresses::Address::Shelley(shelley_addr) => {
            Ok(shelley_addr.delegation().to_vec().as_data())
        }
        addresses::Address::Stake(stake_addr) => {
            Ok(stake_addr.to_vec().as_data())
        }
        _ => Err(super::Error::CoerceError(
            "Address type does not support staking parts".to_string(),
            "Staking credential".to_string(),
        )),
    }
}