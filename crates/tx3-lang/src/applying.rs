use std::{
    collections::{BTreeMap, HashMap, HashSet},
    ops::Neg,
};

use crate::{ir, ArgValue, Utxo};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("invalid built-in operation {0:?}")]
    InvalidBuiltInOp(Box<ir::BuiltInOp>),

    #[error("invalid argument {0:?} for {1}")]
    InvalidArgument(ArgValue, String),

    #[error("property {0} not found in {1}")]
    PropertyNotFound(String, String),

    #[error("property index {0} not found in {1}")]
    PropertyIndexNotFound(usize, String),

    #[error("invalid {0} operation over {1:?} and {2:?}")]
    InvalidBinaryOp(String, String, String),

    #[error("invalid {0} operation over {1:?}")]
    InvalidUnaryOp(String, String),

    #[error("cannot coerce {0:?} into assets")]
    CannotCoerceIntoAssets(ir::Expression),

    #[error("cannot coerce {0:?} into datum")]
    CannotCoerceIntoDatum(ir::Expression),
}

pub trait Indexable: std::fmt::Debug {
    fn index(&self, index: usize) -> Option<ir::Expression>;

    fn index_or_err(&self, index: usize) -> Result<ir::Expression, Error> {
        self.index(index)
            .ok_or(Error::PropertyIndexNotFound(index, format!("{:?}", self)))
    }
}

impl Indexable for ir::StructExpr {
    fn index(&self, index: usize) -> Option<ir::Expression> {
        self.fields.get(index).cloned()
    }
}

impl Indexable for ir::Expression {
    fn index(&self, index: usize) -> Option<ir::Expression> {
        match self {
            ir::Expression::None => None,
            ir::Expression::List(x) => x.get(index).cloned(),
            ir::Expression::Tuple(x) => match index {
                0 => Some(x.0.clone()),
                1 => Some(x.1.clone()),
                _ => None,
            },
            ir::Expression::Struct(x) => x.index(index),
            _ => None,
        }
    }
}

pub trait Arithmetic {
    fn add(self, other: ir::Expression) -> Result<ir::Expression, Error>;
    fn sub(self, other: ir::Expression) -> Result<ir::Expression, Error>;
    fn neg(self) -> Result<ir::Expression, Error>;
}

impl<T> Arithmetic for T
where
    T: Into<CanonicalAssets> + std::fmt::Debug,
{
    fn add(self, other: ir::Expression) -> Result<ir::Expression, Error> {
        let y = match other {
            ir::Expression::Assets(x) => CanonicalAssets::from(x),
            ir::Expression::None => CanonicalAssets::new(),
            other => {
                return Err(Error::InvalidBinaryOp(
                    "add".to_string(),
                    format!("{:?}", self),
                    format!("{:?}", other),
                ))
            }
        };

        let x = self.into();
        let total = x + y;
        Ok(ir::Expression::Assets(total.into()))
    }

    fn sub(self, other: ir::Expression) -> Result<ir::Expression, Error> {
        let other_neg = other.neg()?;
        self.add(other_neg)
    }

    fn neg(self) -> Result<ir::Expression, Error> {
        let negated = self.into().neg();
        Ok(ir::Expression::Assets(negated.into()))
    }
}

impl Arithmetic for i128 {
    fn add(self, other: ir::Expression) -> Result<ir::Expression, Error> {
        match other {
            ir::Expression::Number(y) => Ok(ir::Expression::Number(self + y)),
            ir::Expression::None => Ok(ir::Expression::Number(self)),
            _ => Err(Error::InvalidBinaryOp(
                "add".to_string(),
                format!("{:?}", self),
                format!("{:?}", other),
            )),
        }
    }

    fn sub(self, other: ir::Expression) -> Result<ir::Expression, Error> {
        let other_neg = other.neg()?;
        self.add(other_neg)
    }

    fn neg(self) -> Result<ir::Expression, Error> {
        Ok(ir::Expression::Number(-self))
    }
}

impl Arithmetic for ir::Expression {
    fn add(self, other: ir::Expression) -> Result<ir::Expression, Error> {
        match self {
            ir::Expression::None => Ok(other),
            ir::Expression::Number(x) => Arithmetic::add(x, other),
            ir::Expression::Assets(x) => Arithmetic::add(x, other),
            x => Err(Error::InvalidBinaryOp(
                "add".to_string(),
                format!("{:?}", x),
                format!("{:?}", other),
            )),
        }
    }

    fn sub(self, other: ir::Expression) -> Result<ir::Expression, Error> {
        match self {
            ir::Expression::None => Ok(other),
            ir::Expression::Number(x) => Arithmetic::sub(x, other),
            ir::Expression::Assets(x) => Arithmetic::sub(x, other),
            x => Err(Error::InvalidBinaryOp(
                "sub".to_string(),
                format!("{:?}", x),
                format!("{:?}", other),
            )),
        }
    }

    fn neg(self) -> Result<ir::Expression, Error> {
        match self {
            ir::Expression::None => Ok(ir::Expression::None),
            ir::Expression::Number(x) => Arithmetic::neg(x),
            ir::Expression::Assets(x) => Arithmetic::neg(x),
            x => Err(Error::InvalidUnaryOp("neg".to_string(), format!("{:?}", x))),
        }
    }
}

pub trait Coerceable {
    fn into_assets(self) -> Result<ir::Expression, Error>;
    fn into_datum(self) -> Result<ir::Expression, Error>;
}

impl Coerceable for ir::Expression {
    fn into_assets(self) -> Result<ir::Expression, Error> {
        match self {
            ir::Expression::None => Ok(ir::Expression::None),
            ir::Expression::Assets(x) => Ok(ir::Expression::Assets(x)),
            ir::Expression::UtxoSet(x) => Ok(ir::Expression::Assets(
                x.into_iter().flat_map(|x| x.assets).collect(),
            )),
            _ => Err(Error::CannotCoerceIntoAssets(self)),
        }
    }

    fn into_datum(self) -> Result<ir::Expression, Error> {
        match self {
            ir::Expression::None => Ok(ir::Expression::None),
            ir::Expression::UtxoSet(x) => Ok(x
                .into_iter()
                .next()
                .and_then(|x| x.datum)
                .unwrap_or(ir::Expression::None)),
            ir::Expression::List(x) => Ok(ir::Expression::List(x)),
            ir::Expression::Tuple(x) => Ok(ir::Expression::Tuple(x)),
            ir::Expression::Struct(x) => Ok(ir::Expression::Struct(x)),
            ir::Expression::Bytes(x) => Ok(ir::Expression::Bytes(x)),
            ir::Expression::Number(x) => Ok(ir::Expression::Number(x)),
            ir::Expression::String(x) => Ok(ir::Expression::String(x)),
            ir::Expression::Address(x) => Ok(ir::Expression::Bytes(x)),
            ir::Expression::Hash(x) => Ok(ir::Expression::Bytes(x)),
            _ => Err(Error::CannotCoerceIntoDatum(self)),
        }
    }
}

fn arg_value_into_expr(arg: ArgValue) -> ir::Expression {
    match arg {
        ArgValue::Address(x) => ir::Expression::Address(x),
        ArgValue::Int(x) => ir::Expression::Number(x),
        ArgValue::Bool(x) => ir::Expression::Bool(x),
        ArgValue::String(x) => ir::Expression::String(x),
        ArgValue::Bytes(x) => ir::Expression::Bytes(x),
        ArgValue::UtxoSet(x) => ir::Expression::UtxoSet(x),
        ArgValue::UtxoRef(x) => ir::Expression::UtxoRefs(vec![x]),
    }
}

pub trait Apply: Sized + std::fmt::Debug {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error>;
    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error>;
    fn apply_fees(self, fees: u64) -> Result<Self, Error>;

    fn is_constant(&self) -> bool;

    fn params(&self) -> BTreeMap<String, ir::Type>;
    fn queries(&self) -> BTreeMap<String, ir::InputQuery>;

    fn reduce(self) -> Result<Self, Error>;
    fn distribute(self) -> Result<Self, Error>;
}

trait Composite: Sized {
    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn distribute_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn components(&self) -> Vec<&ir::Expression>;

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone;

    fn reduce_nested(self) -> Result<Self, Error> {
        self.try_map_components(|x| x.reduce())
    }

    fn distribute_nested(self) -> Result<Self, Error> {
        self.try_map_components(|x| x.distribute())
    }
}

impl<T> Apply for T
where
    T: Composite + std::fmt::Debug,
{
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        self.try_map_components(|x| x.apply_args(args))
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        self.try_map_components(|x| x.apply_inputs(args))
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        self.try_map_components(|x| x.apply_fees(fees))
    }

    fn is_constant(&self) -> bool {
        self.components().iter().all(|x| x.is_constant())
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        self.components().iter().flat_map(|x| x.params()).collect()
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        self.components().iter().flat_map(|x| x.queries()).collect()
    }

    fn distribute(self) -> Result<Self, Error> {
        let x = self.distribute_self()?;
        x.distribute_nested()
    }

    fn reduce(self) -> Result<Self, Error> {
        let x = self.reduce_nested()?;

        if x.is_constant() {
            x.reduce_self()
        } else {
            Ok(x)
        }
    }
}

impl<T> Apply for Option<T>
where
    T: Apply,
{
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        self.map(|x| x.apply_args(args)).transpose()
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        self.map(|x| x.apply_inputs(args)).transpose()
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        self.map(|x| x.apply_fees(fees)).transpose()
    }

    fn is_constant(&self) -> bool {
        match self {
            Some(x) => x.is_constant(),
            None => true,
        }
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        match self {
            Some(x) => x.params(),
            None => BTreeMap::new(),
        }
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        match self {
            Some(x) => x.queries(),
            None => BTreeMap::new(),
        }
    }

    fn reduce(self) -> Result<Self, Error> {
        self.map(|x| x.reduce()).transpose()
    }

    fn distribute(self) -> Result<Self, Error> {
        self.map(|x| x.distribute()).transpose()
    }
}

impl<T> Apply for Vec<T>
where
    T: Apply,
{
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        self.into_iter().map(|x| x.apply_args(args)).collect()
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        self.into_iter().map(|x| x.apply_inputs(args)).collect()
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        self.into_iter().map(|x| x.apply_fees(fees)).collect()
    }

    fn is_constant(&self) -> bool {
        self.iter().all(|x| x.is_constant())
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        self.iter().flat_map(|x| x.params()).collect()
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        self.iter().flat_map(|x| x.queries()).collect()
    }

    fn reduce(self) -> Result<Self, Error> {
        self.into_iter().map(|x| x.reduce()).collect()
    }

    fn distribute(self) -> Result<Self, Error> {
        self.into_iter().map(|x| x.distribute()).collect()
    }
}

impl<T> Apply for HashMap<String, T>
where
    T: Apply,
{
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        self.into_iter()
            .map(|(k, v)| v.apply_args(args).map(|v| (k, v)))
            .collect()
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        self.into_iter()
            .map(|(k, v)| v.apply_inputs(args).map(|v| (k, v)))
            .collect()
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        self.into_iter()
            .map(|(k, v)| v.apply_fees(fees).map(|v| (k, v)))
            .collect()
    }

    fn is_constant(&self) -> bool {
        self.values().all(|x| x.is_constant())
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        self.values().flat_map(|x| x.params()).collect()
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        self.values().flat_map(|x| x.queries()).collect()
    }

    fn reduce(self) -> Result<Self, Error> {
        self.into_iter()
            .map(|(k, v)| v.reduce().map(|v| (k, v)))
            .collect()
    }

    fn distribute(self) -> Result<Self, Error> {
        self.into_iter()
            .map(|(k, v)| v.distribute().map(|v| (k, v)))
            .collect()
    }
}

impl Composite for ir::ScriptSource {
    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn distribute_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn components(&self) -> Vec<&ir::Expression> {
        match self {
            ir::ScriptSource::Embedded(x) => vec![x],
            ir::ScriptSource::UtxoRef { r#ref, source } => {
                std::iter::once(r#ref).chain(source.as_ref()).collect()
            }
        }
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        match self {
            ir::ScriptSource::Embedded(x) => Ok(ir::ScriptSource::Embedded(f(x)?)),
            ir::ScriptSource::UtxoRef { r#ref, source } => Ok(ir::ScriptSource::UtxoRef {
                r#ref: f(r#ref)?,
                source: source.map(&f).transpose()?,
            }),
        }
    }
}

impl TryFrom<&ArgValue> for ir::ScriptSource {
    type Error = Error;

    fn try_from(value: &ArgValue) -> Result<Self, Self::Error> {
        match value {
            ArgValue::Bytes(x) => Ok(ir::ScriptSource::Embedded(ir::Expression::Bytes(x.clone()))),
            ArgValue::UtxoRef(x) => Ok(ir::ScriptSource::UtxoRef {
                r#ref: ir::Expression::UtxoRefs(vec![x.clone()]),
                source: None,
            }),
            _ => Err(Error::InvalidArgument(value.clone(), "script".to_string())),
        }
    }
}

impl Composite for ir::PolicyExpr {
    fn components(&self) -> Vec<&ir::Expression> {
        let script = self.script.components();
        std::iter::once(&self.hash).chain(script).collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            name: self.name,
            hash: f(self.hash)?,
            script: self.script.try_map_components(f)?,
        })
    }
}

impl Composite for ir::StructExpr {
    fn components(&self) -> Vec<&ir::Expression> {
        self.fields.iter().collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            constructor: self.constructor,
            fields: self
                .fields
                .into_iter()
                .map(&f)
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl Composite for ir::AssetExpr {
    fn components(&self) -> Vec<&ir::Expression> {
        vec![&self.policy, &self.asset_name, &self.amount]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            policy: f(self.policy)?,
            asset_name: f(self.asset_name)?,
            amount: f(self.amount)?,
        })
    }
}

impl Composite for ir::Coerce {
    fn components(&self) -> Vec<&ir::Expression> {
        match self {
            Self::IntoAssets(x) => vec![x],
            Self::IntoDatum(x) => vec![x],
            Self::IntoScript(x) => vec![x],
            Self::NoOp(x) => vec![x],
        }
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        match self {
            Self::IntoAssets(x) => Ok(Self::IntoAssets(f(x)?)),
            Self::IntoDatum(x) => Ok(Self::IntoDatum(f(x)?)),
            Self::IntoScript(x) => Ok(Self::IntoScript(f(x)?)),
            Self::NoOp(x) => Ok(Self::NoOp(f(x)?)),
        }
    }

    fn distribute_self(self) -> Result<Self, Error> {
        match self {
            Self::IntoAssets(inner) => match inner {
                ir::Expression::EvalBuiltIn(op) => {
                    let wrapped = op.try_map_components(|x| Ok(Self::IntoAssets(x).into()))?;
                    Ok(Self::NoOp(wrapped.into()))
                }
                x => Ok(Self::IntoAssets(x)),
            },
            Self::IntoDatum(inner) => match inner {
                ir::Expression::EvalBuiltIn(op) => {
                    let wrapped = op.try_map_components(|x| Ok(Self::IntoDatum(x).into()))?;
                    Ok(Self::NoOp(wrapped.into()))
                }
                x => Ok(Self::IntoDatum(x)),
            },
            x => Ok(x),
        }
    }

    fn reduce_self(self) -> Result<Self, Error> {
        match self {
            Self::NoOp(x) => Ok(Self::NoOp(x)),
            Self::IntoAssets(x) => Ok(Self::NoOp(x.into_assets()?)),
            Self::IntoDatum(x) => Ok(Self::NoOp(x.into_datum()?)),
            Self::IntoScript(x) => todo!(),
        }
    }
}

impl Composite for ir::BuiltInOp {
    fn components(&self) -> Vec<&ir::Expression> {
        match self {
            Self::NoOp(x) => vec![x],
            Self::Add(x, y) => vec![x, y],
            Self::Sub(x, y) => vec![x, y],
            Self::Negate(x) => vec![x],
            Self::Property(x, _) => vec![x],
        }
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        match self {
            Self::NoOp(x) => Ok(Self::NoOp(f(x)?)),
            Self::Add(x, y) => Ok(Self::Add(f(x)?, f(y)?)),
            Self::Sub(x, y) => Ok(Self::Sub(f(x)?, f(y)?)),
            Self::Negate(x) => Ok(Self::Negate(f(x)?)),
            Self::Property(x, prop) => Ok(Self::Property(f(x)?, prop)),
        }
    }

    fn reduce_self(self) -> Result<Self, Error> {
        match self {
            Self::Add(x, y) => Ok(Self::NoOp(x.add(y)?)),
            Self::Sub(x, y) => Ok(Self::NoOp(x.sub(y)?)),
            Self::Negate(x) => Ok(Self::NoOp(x.neg()?)),
            Self::Property(x, prop) => Ok(Self::NoOp(x.index_or_err(prop)?)),
            Self::NoOp(x) => Ok(Self::NoOp(x)),
        }
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        match self {
            Self::Add(x, y) => Ok(Self::Add(x.reduce()?, y.reduce()?)),
            Self::Sub(x, y) => Ok(Self::Sub(x.reduce()?, y.reduce()?)),
            Self::Negate(x) => Ok(Self::Negate(x.reduce()?)),
            Self::Property(x, y) => Ok(Self::Property(x.reduce()?, y)),
            Self::NoOp(x) => Ok(Self::NoOp(x.reduce()?)),
        }
    }
}

type AssetClass = (Option<Vec<u8>>, Option<Vec<u8>>);

struct CanonicalAssets(HashMap<AssetClass, i128>);

impl CanonicalAssets {
    fn new() -> Self {
        Self(HashMap::new())
    }
}

impl std::ops::Neg for CanonicalAssets {
    type Output = Self;

    fn neg(self) -> Self {
        let mut negated = self.0;

        for (_, value) in negated.iter_mut() {
            *value = -*value;
        }

        Self(negated)
    }
}

impl std::ops::Add for CanonicalAssets {
    type Output = Self;

    fn add(self, other: Self) -> Self {
        let mut aggregated = self.0;

        for (key, value) in other.0 {
            *aggregated.entry(key).or_default() += value;
        }

        Self(aggregated)
    }
}

impl std::ops::Sub for CanonicalAssets {
    type Output = Self;

    fn sub(self, other: Self) -> Self {
        let mut aggregated = self.0;

        for (key, value) in other.0 {
            *aggregated.entry(key).or_default() -= value;
        }

        Self(aggregated)
    }
}

impl From<ir::AssetExpr> for CanonicalAssets {
    fn from(asset: ir::AssetExpr) -> Self {
        let policy = asset.expect_constant_policy().map(|x| x.to_vec());
        let asset_name = asset.expect_constant_name().map(|x| x.to_vec());
        let amount = asset.expect_constant_amount();

        Self(HashMap::from([((policy, asset_name), amount)]))
    }
}

impl From<Vec<ir::AssetExpr>> for CanonicalAssets {
    fn from(assets: Vec<ir::AssetExpr>) -> Self {
        let mut result = CanonicalAssets::new();

        for asset in assets {
            let asset = asset.into();
            result = result + asset;
        }

        result
    }
}

impl From<CanonicalAssets> for Vec<ir::AssetExpr> {
    fn from(assets: CanonicalAssets) -> Self {
        let mut result = Vec::new();

        for ((policy, asset_name), amount) in assets.0.into_iter() {
            result.push(ir::AssetExpr {
                policy: policy
                    .map(ir::Expression::Bytes)
                    .unwrap_or(ir::Expression::None),
                asset_name: asset_name
                    .map(ir::Expression::Bytes)
                    .unwrap_or(ir::Expression::None),
                amount: ir::Expression::Number(amount),
            });
        }

        result
    }
}

impl ir::AssetExpr {
    fn expect_constant_policy(&self) -> Option<&[u8]> {
        match &self.policy {
            ir::Expression::None => None,
            ir::Expression::Bytes(x) => Some(x.as_slice()),
            _ => None,
        }
    }

    fn expect_constant_name(&self) -> Option<&[u8]> {
        match &self.asset_name {
            ir::Expression::None => None,
            ir::Expression::Bytes(x) => Some(x.as_slice()),
            ir::Expression::String(x) => Some(x.as_bytes()),
            _ => None,
        }
    }

    fn expect_constant_amount(&self) -> i128 {
        match &self.amount {
            ir::Expression::Number(x) => *x,
            _ => unreachable!("amount expected to be Number"),
        }
    }
}

impl Composite for ir::Input {
    fn components(&self) -> Vec<&ir::Expression> {
        self.query
            .components()
            .into_iter()
            .chain(self.redeemer.iter())
            .chain(self.policy.iter().flat_map(|x| x.components()))
            .collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            name: self.name,
            query: self.query.try_map_components(&f)?,
            refs: self.refs,
            redeemer: self.redeemer.map(&f).transpose()?,
            policy: self.policy.map(|x| x.try_map_components(f)).transpose()?,
        })
    }
}

impl Composite for ir::InputQuery {
    fn components(&self) -> Vec<&ir::Expression> {
        vec![&self.address, &self.min_amount, &self.r#ref]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            address: f(self.address)?,
            min_amount: f(self.min_amount)?,
            r#ref: f(self.r#ref)?,
        })
    }
}

impl Apply for ir::Param {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        match self {
            ir::Param::ExpectValue(name, ty) => {
                let defined = args.get(&name).cloned();

                match defined {
                    Some(x) => Ok(ir::Param::Set(arg_value_into_expr(x))),
                    None => Ok(Self::ExpectValue(name, ty)),
                }
            }
            // queries can have nested params
            ir::Param::ExpectInput(name, query) => {
                Ok(ir::Param::ExpectInput(name, query.apply_args(args)?))
            }
            x => Ok(x),
        }
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        match self {
            ir::Param::ExpectInput(name, query) => {
                let defined = args.get(&name).cloned();

                match defined {
                    Some(x) => Ok(ir::Param::Set(ir::Expression::UtxoSet(x))),
                    None => Ok(Self::ExpectInput(name, query)),
                }
            }
            x => Ok(x),
        }
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        match self {
            ir::Param::ExpectFees => Ok(ir::Param::Set(ir::Expression::Assets(vec![
                ir::AssetExpr {
                    policy: ir::Expression::None,
                    asset_name: ir::Expression::None,
                    amount: ir::Expression::Number(fees as i128),
                },
            ]))),
            x => Ok(x),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            ir::Param::Set(x) => x.is_constant(),
            _ => false,
        }
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        match self {
            ir::Param::ExpectValue(name, ty) => BTreeMap::from([(name.clone(), ty.clone())]),
            // queries can have nested params
            ir::Param::ExpectInput(_, x) => x.params(),
            _ => BTreeMap::new(),
        }
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        match self {
            ir::Param::ExpectInput(name, query) => BTreeMap::from([(name.clone(), query.clone())]),
            _ => BTreeMap::new(),
        }
    }

    fn reduce(self) -> Result<Self, Error> {
        match self {
            // queries can have nested expressions that need to be reduced
            ir::Param::ExpectInput(name, query) => {
                Ok(ir::Param::ExpectInput(name, query.reduce()?))
            }
            x => Ok(x),
        }
    }

    fn distribute(self) -> Result<Self, Error> {
        match self {
            // queries can have nested expressions that need to be distributed
            ir::Param::ExpectInput(name, query) => {
                Ok(ir::Param::ExpectInput(name, query.distribute()?))
            }
            x => Ok(x),
        }
    }
}

impl Apply for ir::Expression {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        match self {
            Self::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.apply_args(args))
                    .collect::<Result<_, _>>()?,
            )),
            Self::Tuple(x) => Ok(Self::Tuple(Box::new((
                x.0.apply_args(args)?,
                x.1.apply_args(args)?,
            )))),
            Self::Struct(x) => Ok(Self::Struct(x.apply_args(args)?)),
            Self::Assets(x) => Ok(Self::Assets(
                x.into_iter()
                    .map(|x| x.apply_args(args))
                    .collect::<Result<_, _>>()?,
            )),
            Self::EvalParam(x) => Ok(Self::EvalParam(Box::new(x.apply_args(args)?))),
            Self::EvalBuiltIn(x) => Ok(Self::EvalBuiltIn(Box::new(x.apply_args(args)?))),
            Self::EvalCoerce(x) => Ok(Self::EvalCoerce(Box::new(x.apply_args(args)?))),
            Self::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.apply_args(args)?))),

            // it's safe to skip the remaining expressions as they are constant
            _ => Ok(self),
        }
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        match self {
            Self::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.apply_inputs(args))
                    .collect::<Result<_, _>>()?,
            )),
            Self::Tuple(x) => Ok(Self::Tuple(Box::new((
                x.0.apply_inputs(args)?,
                x.1.apply_inputs(args)?,
            )))),
            Self::Struct(x) => Ok(Self::Struct(x.apply_inputs(args)?)),
            Self::Assets(x) => Ok(Self::Assets(
                x.into_iter()
                    .map(|x| x.apply_inputs(args))
                    .collect::<Result<_, _>>()?,
            )),
            Self::EvalParam(x) => Ok(Self::EvalParam(Box::new(x.apply_inputs(args)?))),
            Self::EvalBuiltIn(x) => Ok(Self::EvalBuiltIn(Box::new(x.apply_inputs(args)?))),
            Self::EvalCoerce(x) => Ok(Self::EvalCoerce(Box::new(x.apply_inputs(args)?))),
            Self::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.apply_inputs(args)?))),

            // it's safe to skip the remaining expressions as they are constant
            _ => Ok(self),
        }
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        match self {
            Self::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.apply_fees(fees))
                    .collect::<Result<_, _>>()?,
            )),
            Self::Tuple(x) => Ok(Self::Tuple(Box::new((
                x.0.apply_fees(fees)?,
                x.1.apply_fees(fees)?,
            )))),
            Self::Struct(x) => Ok(Self::Struct(x.apply_fees(fees)?)),
            Self::Assets(x) => Ok(Self::Assets(
                x.into_iter()
                    .map(|x| x.apply_fees(fees))
                    .collect::<Result<_, _>>()?,
            )),
            Self::EvalParam(x) => Ok(Self::EvalParam(Box::new(x.apply_fees(fees)?))),
            Self::EvalBuiltIn(x) => Ok(Self::EvalBuiltIn(Box::new(x.apply_fees(fees)?))),
            Self::EvalCoerce(x) => Ok(Self::EvalCoerce(Box::new(x.apply_fees(fees)?))),
            Self::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.apply_fees(fees)?))),

            // it's safe to skip the remaining expressions as they are constant
            _ => Ok(self),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            Self::List(x) => x.iter().all(|x| x.is_constant()),
            Self::Tuple(x) => x.0.is_constant() && x.1.is_constant(),
            Self::Struct(x) => x.is_constant(),
            Self::Assets(x) => x.iter().all(|x| x.is_constant()),
            Self::EvalParam(x) => x.is_constant(),
            Self::EvalBuiltIn(x) => x.is_constant(),
            Self::EvalCoerce(x) => x.is_constant(),
            Self::AdHocDirective(x) => x.is_constant(),

            // it's safe to skip the remaining expressions as they are constant
            _ => true,
        }
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        match self {
            Self::List(x) => x.iter().flat_map(|x| x.params()).collect(),
            Self::Tuple(x) => [x.0.params(), x.1.params()].into_iter().flatten().collect(),
            Self::Struct(x) => x.params(),
            Self::Assets(x) => x.iter().flat_map(|x| x.params()).collect(),
            Self::EvalParam(x) => x.params(),
            Self::EvalBuiltIn(x) => x.params(),
            Self::EvalCoerce(x) => x.params(),
            Self::AdHocDirective(x) => x.params(),

            // it's safe to skip the remaining expressions as they are constant
            _ => BTreeMap::new(),
        }
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        match self {
            Self::List(x) => x.iter().flat_map(|x| x.queries()).collect(),
            Self::Tuple(x) => [x.0.queries(), x.1.queries()]
                .into_iter()
                .flatten()
                .collect(),
            Self::Struct(x) => x.queries(),
            Self::Assets(x) => x.iter().flat_map(|x| x.queries()).collect(),
            Self::EvalParam(x) => x.queries(),
            Self::EvalBuiltIn(x) => x.queries(),
            Self::EvalCoerce(x) => x.queries(),
            Self::AdHocDirective(x) => x.queries(),

            // it's safe to skip the remaining expressions as they are constant
            _ => BTreeMap::new(),
        }
    }

    fn distribute(self) -> Result<Self, Error> {
        match self {
            // the following expressions can only be reduced internally
            ir::Expression::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.distribute())
                    .collect::<Result<_, _>>()?,
            )),
            ir::Expression::Tuple(x) => Ok(Self::Tuple(Box::new((
                x.0.distribute()?,
                x.1.distribute()?,
            )))),
            ir::Expression::Struct(x) => Ok(Self::Struct(x.distribute()?)),
            ir::Expression::Assets(x) => Ok(Self::Assets(
                x.into_iter()
                    .map(|x| x.distribute())
                    .collect::<Result<_, _>>()?,
            )),
            ir::Expression::AdHocDirective(x) => {
                Ok(Self::AdHocDirective(Box::new(x.distribute()?)))
            }

            // the following ones can be turned into simpler expressions
            ir::Expression::EvalBuiltIn(op) => match *op {
                ir::BuiltInOp::NoOp(x) => Ok(x),
                x => Ok(ir::Expression::EvalBuiltIn(Box::new(x.distribute()?))),
            },
            ir::Expression::EvalCoerce(x) => match *x {
                ir::Coerce::NoOp(x) => Ok(x),
                x => Ok(ir::Expression::EvalCoerce(Box::new(x.distribute()?))),
            },
            ir::Expression::EvalParam(x) => match *x {
                ir::Param::Set(x) => Ok(x),
                x => Ok(ir::Expression::EvalParam(Box::new(x.distribute()?))),
            },
            _ => Ok(self),
        }
    }

    fn reduce(self) -> Result<Self, Error> {
        match self {
            // the following expressions can only be reduced internally
            ir::Expression::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.reduce())
                    .collect::<Result<_, _>>()?,
            )),
            ir::Expression::Tuple(x) => Ok(Self::Tuple(Box::new((x.0.reduce()?, x.1.reduce()?)))),
            ir::Expression::Struct(x) => Ok(Self::Struct(x.reduce()?)),
            ir::Expression::Assets(x) => Ok(Self::Assets(
                x.into_iter()
                    .map(|x| x.reduce())
                    .collect::<Result<_, _>>()?,
            )),
            ir::Expression::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.reduce()?))),

            // the following ones can be turned into simpler expressions
            ir::Expression::EvalBuiltIn(x) => match x.reduce()? {
                ir::BuiltInOp::NoOp(x) => Ok(x),
                x => Ok(ir::Expression::EvalBuiltIn(Box::new(x.reduce()?))),
            },
            ir::Expression::EvalCoerce(x) => match x.reduce()? {
                ir::Coerce::NoOp(x) => Ok(x),
                x => Ok(ir::Expression::EvalCoerce(Box::new(x.reduce()?))),
            },
            ir::Expression::EvalParam(x) => match x.reduce()? {
                ir::Param::Set(x) => Ok(x),
                x => Ok(ir::Expression::EvalParam(Box::new(x.reduce()?))),
            },
            _ => Ok(self),
        }
    }
}

impl Composite for ir::Output {
    fn components(&self) -> Vec<&ir::Expression> {
        vec![&self.address, &self.datum, &self.amount]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            address: f(self.address)?,
            datum: f(self.datum)?,
            amount: f(self.amount)?,
        })
    }
}

impl Composite for ir::Mint {
    fn components(&self) -> Vec<&ir::Expression> {
        vec![&self.amount, &self.redeemer]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            amount: f(self.amount)?,
            redeemer: f(self.redeemer)?,
        })
    }
}

impl Composite for ir::AdHocDirective {
    fn components(&self) -> Vec<&ir::Expression> {
        self.data.values().collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            name: self.name,
            data: self
                .data
                .into_iter()
                .map(|(k, v)| f(v).map(|v| (k, v)))
                .collect::<Result<_, _>>()?,
        })
    }
}

impl Composite for ir::Signers {
    fn components(&self) -> Vec<&ir::Expression> {
        self.signers.iter().collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            signers: self.signers.into_iter().map(f).collect::<Result<_, _>>()?,
        })
    }
}

impl Composite for ir::Collateral {
    fn components(&self) -> Vec<&ir::Expression> {
        self.query.components()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            query: self.query.try_map_components(f)?,
        })
    }
}

impl Composite for ir::Validity {
    fn components(&self) -> Vec<&ir::Expression> {
        vec![&self.since, &self.until]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            since: f(self.since)?,
            until: f(self.until)?,
        })
    }
}

impl Composite for ir::Metadata {
    fn components(&self) -> Vec<&ir::Expression> {
        vec![&self.key, &self.value]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(ir::Expression) -> Result<ir::Expression, Error> + Clone,
    {
        Ok(Self {
            key: f(self.key)?,
            value: f(self.value)?,
        })
    }
}

impl Apply for ir::Tx {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        let tx = ir::Tx {
            references: self.references.apply_args(args)?,
            inputs: self.inputs.apply_args(args)?,
            outputs: self.outputs.apply_args(args)?,
            validity: self.validity.apply_args(args)?,
            mints: self.mints.apply_args(args)?,
            fees: self.fees.apply_args(args)?,
            adhoc: self.adhoc.apply_args(args)?,
            collateral: self.collateral.apply_args(args)?,
            signers: self.signers.apply_args(args)?,
            metadata: self.metadata.apply_args(args)?,
        };

        Ok(tx)
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            references: self.references.apply_inputs(args)?,
            inputs: self.inputs.apply_inputs(args)?,
            outputs: self.outputs.apply_inputs(args)?,
            validity: self.validity.apply_inputs(args)?,
            mints: self.mints.apply_inputs(args)?,
            fees: self.fees.apply_inputs(args)?,
            adhoc: self.adhoc.apply_inputs(args)?,
            collateral: self.collateral.apply_inputs(args)?,
            signers: self.signers.apply_inputs(args)?,
            metadata: self.metadata.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            references: self.references.apply_fees(fees)?,
            inputs: self.inputs.apply_fees(fees)?,
            outputs: self.outputs.apply_fees(fees)?,
            validity: self.validity.apply_fees(fees)?,
            mints: self.mints.apply_fees(fees)?,
            fees: self.fees.apply_fees(fees)?,
            adhoc: self.adhoc.apply_fees(fees)?,
            collateral: self.collateral.apply_fees(fees)?,
            signers: self.signers.apply_fees(fees)?,
            metadata: self.metadata.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.inputs.iter().all(|x| x.is_constant())
            && self.outputs.iter().all(|x| x.is_constant())
            && self.mints.iter().all(|x| x.is_constant())
            && self.fees.is_constant()
            && self.metadata.is_constant()
            && self.validity.is_constant()
            && self.references.is_constant()
            && self.collateral.is_constant()
            && self.adhoc.iter().all(|x| x.is_constant())
            && self.signers.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        // TODO: analyze if necessary to add ref_inputs
        let mut params = BTreeMap::new();
        params.extend(self.inputs.params());
        params.extend(self.outputs.params());
        params.extend(self.mints.params());
        params.extend(self.fees.params());
        params.extend(self.adhoc.params());
        params.extend(self.signers.params());
        params.extend(self.validity.params());
        params.extend(self.metadata.params());
        params.extend(self.references.params());
        params.extend(self.collateral.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        let mut queries = BTreeMap::new();
        queries.extend(self.inputs.queries());
        queries.extend(self.outputs.queries());
        queries.extend(self.mints.queries());
        queries.extend(self.fees.queries());
        queries.extend(self.adhoc.queries());
        queries.extend(self.signers.queries());
        queries.extend(self.validity.queries());
        queries.extend(self.metadata.queries());
        queries.extend(self.collateral.queries());
        queries.extend(self.references.queries());
        queries
    }

    fn distribute(self) -> Result<Self, Error> {
        Ok(Self {
            references: self.references.distribute()?,
            inputs: self.inputs.distribute()?,
            outputs: self.outputs.distribute()?,
            validity: self.validity.distribute()?,
            mints: self.mints.distribute()?,
            fees: self.fees.distribute()?,
            adhoc: self.adhoc.distribute()?,
            collateral: self.collateral.distribute()?,
            signers: self.signers.distribute()?,
            metadata: self.metadata.distribute()?,
        })
    }

    fn reduce(self) -> Result<Self, Error> {
        Ok(Self {
            references: self.references.reduce()?,
            inputs: self.inputs.reduce()?,
            outputs: self.outputs.reduce()?,
            validity: self.validity.reduce()?,
            mints: self.mints.reduce()?,
            fees: self.fees.reduce()?,
            adhoc: self.adhoc.reduce()?,
            collateral: self.collateral.reduce()?,
            signers: self.signers.reduce()?,
            metadata: self.metadata.reduce()?,
        })
    }
}

pub fn apply_args(template: ir::Tx, args: &BTreeMap<String, ArgValue>) -> Result<ir::Tx, Error> {
    template.apply_args(args)
}

pub fn apply_inputs(
    template: ir::Tx,
    args: &BTreeMap<String, HashSet<Utxo>>,
) -> Result<ir::Tx, Error> {
    template.apply_inputs(args)
}

pub fn apply_fees(template: ir::Tx, fees: u64) -> Result<ir::Tx, Error> {
    template.apply_fees(fees)
}

pub fn reduce(template: ir::Tx) -> Result<ir::Tx, Error> {
    let x = template.distribute()?;
    x.reduce()
}

pub fn find_params(template: &ir::Tx) -> BTreeMap<String, ir::Type> {
    template.params()
}

pub fn find_queries(template: &ir::Tx) -> BTreeMap<String, ir::InputQuery> {
    template.queries()
}

#[cfg(test)]
mod tests {

    use crate::UtxoRef;

    use super::*;

    const SUBJECT_PROTOCOL: &str = r#"
    party Sender;

    tx swap(a: Int, b: Int) {
        input source {
            from: Sender,
            min_amount: Ada(a) + Ada(b),
        }
    }
    "#;

    #[test]
    fn param_expression_is_applied() {
        let ir = ir::Expression::EvalParam(Box::new(ir::Param::ExpectValue(
            "a".to_string(),
            ir::Type::Int,
        )));

        let params = ir.params();
        assert_eq!(params.len(), 1);
        assert_eq!(params.get("a"), Some(&ir::Type::Int));

        let args = BTreeMap::from([("a".to_string(), ArgValue::Int(100))]);

        let after = ir.apply_args(&args).unwrap();

        assert_eq!(
            after,
            ir::Expression::EvalParam(Box::new(ir::Param::Set(ir::Expression::Number(100),)))
        );
    }

    #[test]
    fn nested_param_expression_is_applied() {
        let ir = ir::Expression::EvalParam(Box::new(ir::Param::ExpectInput(
            "out".to_string(),
            ir::InputQuery {
                address: ir::Expression::None,
                min_amount: ir::Expression::None,
                r#ref: ir::Expression::EvalParam(Box::new(ir::Param::ExpectValue(
                    "in".to_string(),
                    ir::Type::Int,
                ))),
            },
        )));

        let params = ir.params();
        assert_eq!(params.len(), 1);
        assert_eq!(params.get("in"), Some(&ir::Type::Int));

        let args = BTreeMap::from([("in".to_string(), ArgValue::Int(100))]);
        let after = ir.apply_args(&args).unwrap();

        let after = after.reduce().unwrap();

        let queries = after.queries();
        assert_eq!(queries.len(), 1);
        assert_eq!(
            queries.get("out"),
            Some(&ir::InputQuery {
                address: ir::Expression::None,
                min_amount: ir::Expression::None,
                r#ref: ir::Expression::Number(100),
            })
        );
    }

    #[test]
    fn param_expression_is_reduced() {
        let ir = ir::Expression::EvalParam(Box::new(ir::Param::Set(ir::Expression::Number(3))));

        let after = ir.reduce().unwrap();

        assert_eq!(after, ir::Expression::Number(3));
    }

    #[test]
    fn test_apply_args() {
        let mut ast = crate::parsing::parse_string(SUBJECT_PROTOCOL).unwrap();
        crate::analyzing::analyze(&mut ast).ok().unwrap();

        let before = crate::lowering::lower(&ast, "swap").unwrap();

        dbg!(&before);

        let params = find_params(&before);
        assert_eq!(params.len(), 3);
        assert_eq!(params.get("sender"), Some(&ir::Type::Address));
        assert_eq!(params.get("a"), Some(&ir::Type::Int));
        assert_eq!(params.get("b"), Some(&ir::Type::Int));

        let args = BTreeMap::from([
            ("sender".to_string(), ArgValue::Address(b"abc".to_vec())),
            ("a".to_string(), ArgValue::Int(100)),
            ("b".to_string(), ArgValue::Int(200)),
        ]);

        let after = apply_args(before, &args).unwrap();

        let params = find_params(&after);
        assert_eq!(params.len(), 0);
    }

    #[test]
    fn built_in_expression_is_reduced() {
        let op =
            ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::NoOp(ir::Expression::Number(5))));

        let after = op.reduce().unwrap();

        assert_eq!(after, ir::Expression::Number(5))
    }

    #[test]
    fn numeric_add_is_reduced() {
        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Add(
            ir::Expression::Number(1),
            ir::Expression::Number(5),
        )));

        let after = op.reduce().unwrap();

        assert_eq!(after, ir::Expression::Number(6));
    }

    #[test]
    fn numeric_sub_is_reduced() {
        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Sub(
            ir::Expression::Number(8),
            ir::Expression::Number(5),
        )));

        let after = op.reduce().unwrap();

        assert_eq!(after, ir::Expression::Number(3));
    }

    #[test]
    fn nested_numeric_binary_op_is_reduced() {
        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Add(
            ir::Expression::Number(1),
            ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Sub(
                ir::Expression::Number(5),
                ir::Expression::Number(3),
            ))),
        )));

        let after = op.reduce().unwrap();

        assert_eq!(after, ir::Expression::Number(3));
    }

    #[test]
    fn test_reduce_single_custom_asset_binary_op() {
        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Add(
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::Bytes(b"abc".to_vec()),
                asset_name: ir::Expression::Bytes(b"111".to_vec()),
                amount: ir::Expression::Number(100),
            }]),
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::Bytes(b"abc".to_vec()),
                asset_name: ir::Expression::Bytes(b"111".to_vec()),
                amount: ir::Expression::Number(200),
            }]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            ir::Expression::Assets(assets) => {
                assert_eq!(assets.len(), 1);
                assert_eq!(assets[0].policy, ir::Expression::Bytes(b"abc".to_vec()));
                assert_eq!(assets[0].asset_name, ir::Expression::Bytes(b"111".to_vec()));
                assert_eq!(assets[0].amount, ir::Expression::Number(300));
            }
            _ => panic!("Expected assets"),
        };
    }

    #[test]
    fn test_reduce_native_asset_binary_op() {
        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Add(
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(100),
            }]),
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(200),
            }]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            ir::Expression::Assets(assets) => {
                assert_eq!(assets.len(), 1);
                assert_eq!(assets[0].policy, ir::Expression::None);
                assert_eq!(assets[0].asset_name, ir::Expression::None);
                assert_eq!(assets[0].amount, ir::Expression::Number(300));
            }
            _ => panic!("Expected assets"),
        };
    }

    #[test]
    fn test_reduce_mixed_asset_binary_op() {
        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Add(
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(100),
            }]),
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::Bytes(b"abc".to_vec()),
                asset_name: ir::Expression::Bytes(b"111".to_vec()),
                amount: ir::Expression::Number(200),
            }]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            ir::Expression::Assets(assets) => {
                assert_eq!(assets.len(), 2);

                for asset in assets {
                    if asset.policy == ir::Expression::None {
                        assert_eq!(asset.asset_name, ir::Expression::None);
                        assert_eq!(asset.amount, ir::Expression::Number(100));
                    } else {
                        assert_eq!(asset.policy, ir::Expression::Bytes(b"abc".to_vec()));
                        assert_eq!(asset.asset_name, ir::Expression::Bytes(b"111".to_vec()));
                        assert_eq!(asset.amount, ir::Expression::Number(200));
                    }
                }
            }
            _ => panic!("Expected assets"),
        };
    }

    #[test]
    fn test_reduce_coerce_noop() {
        let op = ir::Expression::EvalCoerce(Box::new(ir::Coerce::NoOp(ir::Expression::Number(5))));

        let reduced = op.reduce().unwrap();

        match reduced {
            ir::Expression::Number(5) => (),
            _ => panic!("Expected number 5"),
        };
    }

    #[test]
    fn test_coerce_utxo_set_into_assets() {
        let utxos = vec![Utxo {
            r#ref: UtxoRef::new(b"abc", 1),
            address: b"abc".into(),
            datum: Some(ir::Expression::Number(1)),
            assets: vec![ir::AssetExpr {
                policy: ir::Expression::Bytes(b"abc".to_vec()),
                asset_name: ir::Expression::Bytes(b"111".to_vec()),
                amount: ir::Expression::Number(1),
            }],
            script: None,
        }];

        let op = ir::Coerce::IntoAssets(ir::Expression::UtxoSet(HashSet::from_iter(
            utxos.clone().into_iter(),
        )));

        let reduced = op.reduce().unwrap();

        assert_eq!(
            reduced,
            ir::Coerce::NoOp(ir::Expression::Assets(utxos[0].assets.clone()))
        );
    }

    #[test]
    fn test_coerce_utxo_set_into_datum() {
        let utxos = vec![Utxo {
            r#ref: UtxoRef::new(b"abc", 1),
            address: b"abc".into(),
            datum: Some(ir::Expression::Number(1)),
            assets: vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(1),
            }],
            script: None,
        }];

        let op = ir::Coerce::IntoDatum(ir::Expression::UtxoSet(HashSet::from_iter(
            utxos.clone().into_iter(),
        )));

        let reduced = op.reduce().unwrap();

        assert_eq!(reduced, ir::Coerce::NoOp(utxos[0].datum.clone().unwrap()));
    }

    #[test]
    fn test_coerce_distributes_itself() {
        let utxos = vec![Utxo {
            r#ref: UtxoRef::new(b"abc", 1),
            address: b"abc".into(),
            datum: None,
            assets: vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(2),
            }],
            script: None,
        }];

        let op = ir::BuiltInOp::Add(
            ir::Expression::UtxoSet(HashSet::from_iter(utxos.clone().into_iter())),
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(1),
            }]),
        );

        let expr = ir::Expression::from(ir::Coerce::IntoAssets(op.into()));

        let after = expr.distribute().unwrap().reduce().unwrap();

        assert_eq!(
            after,
            ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(3),
            }])
        );
    }

    #[test]
    fn test_reduce_struct_property_access() {
        let object = ir::Expression::Struct(ir::StructExpr {
            constructor: 0,
            fields: vec![
                ir::Expression::Number(1),
                ir::Expression::Number(2),
                ir::Expression::Number(3),
            ],
        });

        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Property(object.clone(), 1)));

        let reduced = op.reduce();

        match reduced {
            Ok(ir::Expression::Number(2)) => (),
            _ => panic!("Expected number 2"),
        };

        let op =
            ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Property(object.clone(), 100)));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(100, _)) => (),
            _ => panic!("Expected property index not found"),
        };
    }

    #[test]
    fn test_reduce_list_property_access() {
        let object = ir::Expression::List(vec![
            ir::Expression::Number(1),
            ir::Expression::Number(2),
            ir::Expression::Number(3),
        ]);

        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Property(object.clone(), 1)));

        let reduced = op.reduce();

        match reduced {
            Ok(ir::Expression::Number(2)) => (),
            _ => panic!("Expected number 2"),
        };

        let op =
            ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Property(object.clone(), 100)));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(100, _)) => (),
            _ => panic!("Expected property index not found"),
        };
    }

    #[test]
    fn test_reduce_tuple_property_access() {
        let object = ir::Expression::Tuple(Box::new((
            ir::Expression::Number(1),
            ir::Expression::Number(2),
        )));

        let op = ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Property(object.clone(), 1)));

        let reduced = op.reduce();

        match reduced {
            Ok(ir::Expression::Number(2)) => (),
            _ => panic!("Expected number 2"),
        };

        let op =
            ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Property(object.clone(), 100)));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(100, _)) => (),
            _ => panic!("Expected property index not found"),
        };
    }
}
