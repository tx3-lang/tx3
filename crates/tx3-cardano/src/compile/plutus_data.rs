pub use pallas::codec::utils::Int;
use pallas::codec::utils::KeyValuePairs;
pub use pallas::ledger::primitives::{BigInt, BoundedBytes, Constr, MaybeIndefArray, PlutusData};
use tx3_lang::ir;

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
            x => Err(super::Error::CoerceError(
                format!("{:?}", x),
                "PlutusData".to_string(),
            )),
        }
    }
}
