use std::collections::{BTreeMap, HashMap, HashSet};

use crate::model::assets::CanonicalAssets;
use crate::model::v1beta0::*;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("invalid built-in operation {0:?}")]
    InvalidBuiltInOp(Box<BuiltInOp>),

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
    CannotCoerceIntoAssets(Expression),

    #[error("cannot coerce {0:?} into datum")]
    CannotCoerceIntoDatum(Expression),

    #[error("compiler op failed: {0}")]
    CompilerOpFailed(Box<crate::compile::Error>),
}

impl From<crate::compile::Error> for Error {
    fn from(error: crate::compile::Error) -> Self {
        Error::CompilerOpFailed(Box::new(error))
    }
}

pub trait Indexable: std::fmt::Debug {
    fn index(&self, index: Expression) -> Option<Expression>;

    fn index_or_err(&self, index: Expression) -> Result<Expression, Error> {
        let index_value = index.as_number().unwrap_or(0) as usize;
        self.index(index).ok_or(Error::PropertyIndexNotFound(
            index_value,
            format!("{self:?}"),
        ))
    }
}

impl Indexable for StructExpr {
    fn index(&self, index: Expression) -> Option<Expression> {
        // numeric indices represent the index of the field of the struct
        match index {
            Expression::Number(n) => self.fields.get(n as usize).cloned(),
            _ => return None,
        }
    }
}

impl Indexable for Expression {
    fn index(&self, index: Expression) -> Option<Expression> {
        match self {
            Expression::None => None,
            Expression::Map(x) => x
                .iter()
                .find(|(k, _)| *k == index)
                .map(|(k, v)| Expression::Tuple(Box::new((k.clone(), v.clone())))),
            Expression::List(x) => x.get(index.as_number()? as usize).cloned(),
            Expression::Tuple(x) => match index.as_number()? {
                0 => Some(x.0.clone()),
                1 => Some(x.1.clone()),
                _ => None,
            },
            Expression::Struct(x) => x.index(index.clone()),
            _ => None,
        }
    }
}

pub trait Concatenable {
    fn concat(self, other: Expression) -> Result<Expression, Error>;
}

pub trait Arithmetic {
    fn add(self, other: Expression) -> Result<Expression, Error>;
    fn sub(self, other: Expression) -> Result<Expression, Error>;
    fn neg(self) -> Result<Expression, Error>;
}

impl<T> Arithmetic for T
where
    T: Into<CanonicalAssets> + std::fmt::Debug,
{
    fn add(self, other: Expression) -> Result<Expression, Error> {
        let y = match other {
            Expression::Assets(x) => CanonicalAssets::from(x),
            Expression::None => CanonicalAssets::empty(),
            other => {
                return Err(Error::InvalidBinaryOp(
                    "add".to_string(),
                    format!("{self:?}"),
                    format!("{other:?}"),
                ))
            }
        };

        let x = self.into();
        let total = x + y;
        Ok(Expression::Assets(total.into()))
    }

    fn sub(self, other: Expression) -> Result<Expression, Error> {
        let other_neg = other.neg()?;
        self.add(other_neg)
    }

    fn neg(self) -> Result<Expression, Error> {
        let negated = std::ops::Neg::neg(self.into());
        Ok(Expression::Assets(negated.into()))
    }
}

impl Arithmetic for i128 {
    fn add(self, other: Expression) -> Result<Expression, Error> {
        match other {
            Expression::Number(y) => Ok(Expression::Number(self + y)),
            Expression::None => Ok(Expression::Number(self)),
            _ => Err(Error::InvalidBinaryOp(
                "add".to_string(),
                format!("{self:?}"),
                format!("{other:?}"),
            )),
        }
    }

    fn sub(self, other: Expression) -> Result<Expression, Error> {
        let other_neg = other.neg()?;
        self.add(other_neg)
    }

    fn neg(self) -> Result<Expression, Error> {
        Ok(Expression::Number(-self))
    }
}

impl Arithmetic for Expression {
    fn add(self, other: Expression) -> Result<Expression, Error> {
        match self {
            Expression::None => Ok(other),
            Expression::Number(x) => Arithmetic::add(x, other),
            Expression::Assets(x) => Arithmetic::add(x, other),
            x => Err(Error::InvalidBinaryOp(
                "add".to_string(),
                format!("{x:?}"),
                format!("{other:?}"),
            )),
        }
    }

    fn sub(self, other: Expression) -> Result<Expression, Error> {
        match self {
            Expression::None => Ok(other),
            Expression::Number(x) => Arithmetic::sub(x, other),
            Expression::Assets(x) => Arithmetic::sub(x, other),
            x => Err(Error::InvalidBinaryOp(
                "sub".to_string(),
                format!("{x:?}"),
                format!("{other:?}"),
            )),
        }
    }

    fn neg(self) -> Result<Expression, Error> {
        match self {
            Expression::None => Ok(Expression::None),
            Expression::Number(x) => Arithmetic::neg(x),
            Expression::Assets(x) => Arithmetic::neg(x),
            x => Err(Error::InvalidUnaryOp("neg".to_string(), format!("{x:?}"))),
        }
    }
}

impl Concatenable for String {
    fn concat(self, other: Expression) -> Result<Expression, Error> {
        match other {
            Expression::String(y) => Ok(Expression::String(self + &y)),
            Expression::None => Ok(Expression::String(self)),
            _ => Err(Error::InvalidBinaryOp(
                "concat".to_string(),
                format!("String({self:?})"),
                format!("{other:?}"),
            )),
        }
    }
}

impl Concatenable for Vec<Expression> {
    fn concat(self, other: Expression) -> Result<Expression, Error> {
        match other {
            Expression::List(expressions) => {
                Ok(Expression::List([&self[..], &expressions[..]].concat()))
            }
            _ => Err(Error::InvalidBinaryOp(
                "concat".to_string(),
                format!("List({:?})", self),
                format!("{:?}", other),
            )),
        }
    }
}

impl Concatenable for Vec<u8> {
    fn concat(self, other: Expression) -> Result<Expression, Error> {
        match other {
            Expression::Bytes(y) => {
                let mut result = self;
                result.extend(y);
                Ok(Expression::Bytes(result))
            }
            Expression::None => Ok(Expression::Bytes(self)),
            _ => Err(Error::InvalidBinaryOp(
                "concat".to_string(),
                format!("Bytes({self:?})"),
                format!("{other:?}"),
            )),
        }
    }
}

impl Concatenable for Expression {
    fn concat(self, other: Expression) -> Result<Expression, Error> {
        match self {
            Expression::None => Ok(other),
            Expression::String(x) => Concatenable::concat(x, other),
            Expression::Bytes(x) => Concatenable::concat(x, other),
            Expression::List(x) => Concatenable::concat(x, other),
            x => Err(Error::InvalidBinaryOp(
                "concat".to_string(),
                format!("{x:?}"),
                format!("{other:?}"),
            )),
        }
    }
}

pub trait Coerceable {
    fn into_assets(self) -> Result<Expression, Error>;
    fn into_datum(self) -> Result<Expression, Error>;
}

impl Coerceable for Expression {
    fn into_assets(self) -> Result<Expression, Error> {
        match self {
            Expression::None => Ok(Expression::None),
            Expression::Assets(x) => Ok(Expression::Assets(x)),
            Expression::UtxoSet(x) => {
                let all = x
                    .into_iter()
                    .map(|x| x.assets)
                    .fold(CanonicalAssets::empty(), |acc, x| acc + x);

                Ok(Expression::Assets(all.into()))
            }
            _ => Err(Error::CannotCoerceIntoAssets(self)),
        }
    }

    fn into_datum(self) -> Result<Expression, Error> {
        match self {
            Expression::None => Ok(Expression::None),
            Expression::UtxoSet(x) => Ok(x
                .into_iter()
                .next()
                .and_then(|x| x.datum)
                .unwrap_or(Expression::None)),
            Expression::List(x) => Ok(Expression::List(x)),
            Expression::Map(x) => Ok(Expression::Map(x)),
            Expression::Tuple(x) => Ok(Expression::Tuple(x)),
            Expression::Struct(x) => Ok(Expression::Struct(x)),
            Expression::Bytes(x) => Ok(Expression::Bytes(x)),
            Expression::Number(x) => Ok(Expression::Number(x)),
            Expression::String(x) => Ok(Expression::String(x)),
            Expression::Address(x) => Ok(Expression::Bytes(x)),
            Expression::Hash(x) => Ok(Expression::Bytes(x)),
            _ => Err(Error::CannotCoerceIntoDatum(self)),
        }
    }
}

fn arg_value_into_expr(arg: ArgValue) -> Expression {
    match arg {
        ArgValue::Address(x) => Expression::Address(x),
        ArgValue::Int(x) => Expression::Number(x),
        ArgValue::Bool(x) => Expression::Bool(x),
        ArgValue::String(x) => Expression::String(x),
        ArgValue::Bytes(x) => Expression::Bytes(x),
        ArgValue::UtxoSet(x) => Expression::UtxoSet(x),
        ArgValue::UtxoRef(x) => Expression::UtxoRefs(vec![x]),
    }
}

pub trait Apply: Sized + std::fmt::Debug {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error>;
    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error>;
    fn apply_fees(self, fees: u64) -> Result<Self, Error>;

    fn is_constant(&self) -> bool;

    fn params(&self) -> BTreeMap<String, Type>;
    fn queries(&self) -> BTreeMap<String, InputQuery>;

    fn reduce(self) -> Result<Self, Error>;
}

pub trait Composite: Sized {
    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn components(&self) -> Vec<&Expression>;

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone;

    fn reduce_nested(self) -> Result<Self, Error> {
        self.try_map_components(|x| x.reduce())
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

    fn params(&self) -> BTreeMap<String, Type> {
        self.components().iter().flat_map(|x| x.params()).collect()
    }

    fn queries(&self) -> BTreeMap<String, InputQuery> {
        self.components().iter().flat_map(|x| x.queries()).collect()
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

    fn params(&self) -> BTreeMap<String, Type> {
        match self {
            Some(x) => x.params(),
            None => BTreeMap::new(),
        }
    }

    fn queries(&self) -> BTreeMap<String, InputQuery> {
        match self {
            Some(x) => x.queries(),
            None => BTreeMap::new(),
        }
    }

    fn reduce(self) -> Result<Self, Error> {
        self.map(|x| x.reduce()).transpose()
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

    fn params(&self) -> BTreeMap<String, Type> {
        self.iter().flat_map(|x| x.params()).collect()
    }

    fn queries(&self) -> BTreeMap<String, InputQuery> {
        self.iter().flat_map(|x| x.queries()).collect()
    }

    fn reduce(self) -> Result<Self, Error> {
        self.into_iter().map(|x| x.reduce()).collect()
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

    fn params(&self) -> BTreeMap<String, Type> {
        self.values().flat_map(|x| x.params()).collect()
    }

    fn queries(&self) -> BTreeMap<String, InputQuery> {
        self.values().flat_map(|x| x.queries()).collect()
    }

    fn reduce(self) -> Result<Self, Error> {
        self.into_iter()
            .map(|(k, v)| v.reduce().map(|v| (k, v)))
            .collect()
    }
}

impl Composite for ScriptSource {
    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn components(&self) -> Vec<&Expression> {
        match self {
            ScriptSource::Embedded(x) => vec![x],
            ScriptSource::UtxoRef { r#ref, source } => {
                std::iter::once(r#ref).chain(source.as_ref()).collect()
            }
        }
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        match self {
            ScriptSource::Embedded(x) => Ok(ScriptSource::Embedded(f(x)?)),
            ScriptSource::UtxoRef { r#ref, source } => Ok(ScriptSource::UtxoRef {
                r#ref: f(r#ref)?,
                source: source.map(&f).transpose()?,
            }),
        }
    }
}

impl TryFrom<&ArgValue> for ScriptSource {
    type Error = Error;

    fn try_from(value: &ArgValue) -> Result<Self, Self::Error> {
        match value {
            ArgValue::Bytes(x) => Ok(ScriptSource::Embedded(Expression::Bytes(x.clone()))),
            ArgValue::UtxoRef(x) => Ok(ScriptSource::UtxoRef {
                r#ref: Expression::UtxoRefs(vec![x.clone()]),
                source: None,
            }),
            _ => Err(Error::InvalidArgument(value.clone(), "script".to_string())),
        }
    }
}

impl Composite for PolicyExpr {
    fn components(&self) -> Vec<&Expression> {
        let script = self.script.components();
        std::iter::once(&self.hash).chain(script).collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            name: self.name,
            hash: f(self.hash)?,
            script: self.script.try_map_components(f)?,
        })
    }
}

impl Composite for StructExpr {
    fn components(&self) -> Vec<&Expression> {
        self.fields.iter().collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
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

impl Composite for AssetExpr {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.policy, &self.asset_name, &self.amount]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            policy: f(self.policy)?,
            asset_name: f(self.asset_name)?,
            amount: f(self.amount)?,
        })
    }
}

impl Composite for Coerce {
    fn components(&self) -> Vec<&Expression> {
        match self {
            Self::IntoAssets(x) => vec![x],
            Self::IntoDatum(x) => vec![x],
            Self::IntoScript(x) => vec![x],
            Self::NoOp(x) => vec![x],
        }
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        match self {
            Self::IntoAssets(x) => Ok(Self::IntoAssets(f(x)?)),
            Self::IntoDatum(x) => Ok(Self::IntoDatum(f(x)?)),
            Self::IntoScript(x) => Ok(Self::IntoScript(f(x)?)),
            Self::NoOp(x) => Ok(Self::NoOp(f(x)?)),
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

impl Composite for BuiltInOp {
    fn components(&self) -> Vec<&Expression> {
        match self {
            Self::NoOp(x) => vec![x],
            Self::Add(x, y) => vec![x, y],
            Self::Sub(x, y) => vec![x, y],
            Self::Concat(x, y) => vec![x, y],
            Self::Negate(x) => vec![x],
            Self::Property(x, _) => vec![x],
        }
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        match self {
            Self::NoOp(x) => Ok(Self::NoOp(f(x)?)),
            Self::Add(x, y) => Ok(Self::Add(f(x)?, f(y)?)),
            Self::Sub(x, y) => Ok(Self::Sub(f(x)?, f(y)?)),
            Self::Concat(x, y) => Ok(Self::Concat(f(x)?, f(y)?)),
            Self::Negate(x) => Ok(Self::Negate(f(x)?)),
            Self::Property(x, prop) => Ok(Self::Property(f(x)?, prop)),
        }
    }

    fn reduce_self(self) -> Result<Self, Error> {
        match self {
            Self::Add(x, y) => Ok(Self::NoOp(x.add(y)?)),
            Self::Sub(x, y) => Ok(Self::NoOp(x.sub(y)?)),
            Self::Concat(x, y) => Ok(Self::NoOp(x.concat(y)?)),
            Self::Negate(x) => Ok(Self::NoOp(x.neg()?)),
            Self::Property(x, prop) => Ok(Self::NoOp(x.index_or_err(prop)?)),
            Self::NoOp(x) => Ok(Self::NoOp(x)),
        }
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        match self {
            Self::Add(x, y) => Ok(Self::Add(x.reduce()?, y.reduce()?)),
            Self::Sub(x, y) => Ok(Self::Sub(x.reduce()?, y.reduce()?)),
            Self::Concat(x, y) => Ok(Self::Concat(x.reduce()?, y.reduce()?)),
            Self::Negate(x) => Ok(Self::Negate(x.reduce()?)),
            Self::Property(x, y) => Ok(Self::Property(x.reduce()?, y.reduce()?)),
            Self::NoOp(x) => Ok(Self::NoOp(x.reduce()?)),
        }
    }
}

impl From<AssetExpr> for CanonicalAssets {
    fn from(asset: AssetExpr) -> Self {
        let policy = asset.expect_constant_policy();
        let name = asset.expect_constant_name();
        let amount = asset.expect_constant_amount();

        Self::from_asset(policy, name, amount)
    }
}

impl From<Vec<AssetExpr>> for CanonicalAssets {
    fn from(assets: Vec<AssetExpr>) -> Self {
        let mut result = CanonicalAssets::empty();

        for asset in assets {
            let asset = asset.into();
            result = result + asset;
        }

        result
    }
}

impl From<CanonicalAssets> for Vec<AssetExpr> {
    fn from(assets: CanonicalAssets) -> Self {
        let mut result = Vec::new();

        for (class, amount) in assets.into_iter() {
            result.push(AssetExpr {
                policy: class
                    .policy()
                    .map(|x| Expression::Bytes(x.to_vec()))
                    .unwrap_or(Expression::None),
                asset_name: class
                    .name()
                    .map(|x| Expression::Bytes(x.to_vec()))
                    .unwrap_or(Expression::None),
                amount: Expression::Number(amount),
            });
        }

        result
    }
}

impl AssetExpr {
    fn expect_constant_policy(&self) -> Option<&[u8]> {
        match &self.policy {
            Expression::None => None,
            Expression::Bytes(x) => Some(x.as_slice()),
            _ => None,
        }
    }

    fn expect_constant_name(&self) -> Option<&[u8]> {
        match &self.asset_name {
            Expression::None => None,
            Expression::Bytes(x) => Some(x.as_slice()),
            Expression::String(x) => Some(x.as_bytes()),
            _ => None,
        }
    }

    fn expect_constant_amount(&self) -> i128 {
        match &self.amount {
            Expression::Number(x) => *x,
            _ => unreachable!("amount expected to be Number"),
        }
    }
}

impl Composite for Input {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.utxos, &self.redeemer]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            name: self.name,
            utxos: f(self.utxos)?,
            redeemer: f(self.redeemer)?,
        })
    }
}

impl Composite for InputQuery {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.address, &self.min_amount, &self.r#ref]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            address: f(self.address)?,
            min_amount: f(self.min_amount)?,
            r#ref: f(self.r#ref)?,
            ..self
        })
    }
}

impl Apply for Param {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        match self {
            Param::ExpectValue(name, ty) => {
                let defined = args.get(&name).cloned();

                match defined {
                    Some(x) => Ok(Param::Set(arg_value_into_expr(x))),
                    None => Ok(Self::ExpectValue(name, ty)),
                }
            }
            // queries can have nested params
            Param::ExpectInput(name, query) => {
                Ok(Param::ExpectInput(name, query.apply_args(args)?))
            }
            x => Ok(x),
        }
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        match self {
            Param::ExpectInput(name, query) => {
                let defined = args.get(&name).cloned();

                match defined {
                    Some(x) => Ok(Param::Set(Expression::UtxoSet(x))),
                    None => Ok(Self::ExpectInput(name, query)),
                }
            }
            x => Ok(x),
        }
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        match self {
            Param::ExpectFees => Ok(Param::Set(Expression::Assets(vec![AssetExpr {
                policy: Expression::None,
                asset_name: Expression::None,
                amount: Expression::Number(fees as i128),
            }]))),
            // queries can have nested params
            Param::ExpectInput(name, query) => {
                Ok(Param::ExpectInput(name, query.apply_fees(fees)?))
            }
            x => Ok(x),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            Param::Set(x) => x.is_constant(),
            _ => false,
        }
    }

    fn params(&self) -> BTreeMap<String, Type> {
        match self {
            Param::ExpectValue(name, ty) => BTreeMap::from([(name.clone(), ty.clone())]),
            // queries can have nested params
            Param::ExpectInput(_, x) => x.params(),
            _ => BTreeMap::new(),
        }
    }

    fn queries(&self) -> BTreeMap<String, InputQuery> {
        match self {
            Param::ExpectInput(name, query) => BTreeMap::from([(name.clone(), query.clone())]),
            _ => BTreeMap::new(),
        }
    }

    fn reduce(self) -> Result<Self, Error> {
        match self {
            // queries can have nested expressions that need to be reduced
            Param::ExpectInput(name, query) => Ok(Param::ExpectInput(name, query.reduce()?)),
            x => Ok(x),
        }
    }
}

impl Apply for Expression {
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
            Self::Map(x) => Ok(Self::Map(
                x.into_iter()
                    .map(|(k, v)| {
                        Ok::<(Expression, Expression), Error>((
                            k.apply_args(args)?,
                            v.apply_args(args)?,
                        ))
                    })
                    .collect::<Result<_, _>>()?,
            )),
            Self::Struct(x) => Ok(Self::Struct(x.apply_args(args)?)),
            Self::Assets(x) => Ok(Self::Assets(
                x.into_iter()
                    .map(|x| x.apply_args(args))
                    .collect::<Result<_, _>>()?,
            )),
            Self::EvalParam(x) => Ok(Self::EvalParam(Box::new(x.apply_args(args)?))),
            Self::EvalBuiltIn(x) => Ok(Self::EvalBuiltIn(Box::new(x.apply_args(args)?))),
            Self::EvalCoerce(x) => Ok(Self::EvalCoerce(Box::new(x.apply_args(args)?))),
            Self::EvalCompiler(x) => Ok(Self::EvalCompiler(Box::new(x.apply_args(args)?))),
            Self::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.apply_args(args)?))),

            // Don't fall into the temptation of simplifying the following cases under a single
            // wildcard with a default implementation, it makes it really hard to detect missing
            // implementation when adding new `Expression` variants.
            Self::None => Ok(self),
            Self::Bytes(_) => Ok(self),
            Self::Number(_) => Ok(self),
            Self::Bool(_) => Ok(self),
            Self::String(_) => Ok(self),
            Self::Address(_) => Ok(self),
            Self::Hash(_) => Ok(self),
            Self::UtxoRefs(_) => Ok(self),
            Self::UtxoSet(_) => Ok(self),
        }
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        match self {
            Self::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.apply_inputs(args))
                    .collect::<Result<_, _>>()?,
            )),
            Self::Map(x) => Ok(Self::Map(
                x.into_iter()
                    .map(|(k, v)| {
                        Ok::<(Expression, Expression), Error>((
                            k.apply_inputs(args)?,
                            v.apply_inputs(args)?,
                        ))
                    })
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
            Self::EvalCompiler(x) => Ok(Self::EvalCompiler(Box::new(x.apply_inputs(args)?))),
            Self::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.apply_inputs(args)?))),

            // Don't fall into the temptation of simplifying the following cases under a single
            // wildcard with a default implementation, it makes it really hard to detect missing
            // implementation when adding new `Expression` variants.
            Self::None => Ok(self),
            Self::Bytes(_) => Ok(self),
            Self::Number(_) => Ok(self),
            Self::Bool(_) => Ok(self),
            Self::String(_) => Ok(self),
            Self::Address(_) => Ok(self),
            Self::Hash(_) => Ok(self),
            Self::UtxoRefs(_) => Ok(self),
            Self::UtxoSet(_) => Ok(self),
        }
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        match self {
            Self::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.apply_fees(fees))
                    .collect::<Result<_, _>>()?,
            )),
            Self::Map(x) => Ok(Self::Map(
                x.into_iter()
                    .map(|(k, v)| {
                        Ok::<(Expression, Expression), Error>((
                            k.apply_fees(fees)?,
                            v.apply_fees(fees)?,
                        ))
                    })
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
            Self::EvalCompiler(x) => Ok(Self::EvalCompiler(Box::new(x.apply_fees(fees)?))),
            Self::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.apply_fees(fees)?))),

            // Don't fall into the temptation of simplifying the following cases under a single
            // wildcard with a default implementation, it makes it really hard to detect missing
            // implementation when adding new `Expression` variants.
            Self::None => Ok(self),
            Self::Bytes(_) => Ok(self),
            Self::Number(_) => Ok(self),
            Self::Bool(_) => Ok(self),
            Self::String(_) => Ok(self),
            Self::Address(_) => Ok(self),
            Self::Hash(_) => Ok(self),
            Self::UtxoRefs(_) => Ok(self),
            Self::UtxoSet(_) => Ok(self),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            Self::List(x) => x.iter().all(|x| x.is_constant()),
            Self::Map(x) => x.iter().all(|(k, v)| k.is_constant() && v.is_constant()),
            Self::Tuple(x) => x.0.is_constant() && x.1.is_constant(),
            Self::Struct(x) => x.is_constant(),
            Self::Assets(x) => x.iter().all(|x| x.is_constant()),
            Self::EvalParam(x) => x.is_constant(),
            Self::EvalBuiltIn(x) => x.is_constant(),
            Self::EvalCoerce(x) => x.is_constant(),
            Self::EvalCompiler(_) => false,
            Self::AdHocDirective(x) => x.is_constant(),

            // Don't fall into the temptation of simplifying the following cases under a single
            // wildcard with a default implementation, it makes it really hard to detect missing
            // implementation when adding new `Expression` variants.
            Self::None => true,
            Self::Bytes(_) => true,
            Self::Number(_) => true,
            Self::Bool(_) => true,
            Self::String(_) => true,
            Self::Address(_) => true,
            Self::Hash(_) => true,
            Self::UtxoRefs(_) => true,
            Self::UtxoSet(_) => true,
        }
    }

    fn params(&self) -> BTreeMap<String, Type> {
        match self {
            Self::List(x) => x.iter().flat_map(|x| x.params()).collect(),
            Self::Map(x) => x
                .iter()
                .flat_map(|(k, v)| [k.params(), v.params()].into_iter().flatten())
                .collect(),
            Self::Tuple(x) => [x.0.params(), x.1.params()].into_iter().flatten().collect(),
            Self::Struct(x) => x.params(),
            Self::Assets(x) => x.iter().flat_map(|x| x.params()).collect(),
            Self::EvalParam(x) => x.params(),
            Self::EvalBuiltIn(x) => x.params(),
            Self::EvalCoerce(x) => x.params(),
            Self::EvalCompiler(x) => x.params(),
            Self::AdHocDirective(x) => x.params(),

            // Don't fall into the temptation of simplifying the following cases under a single
            // wildcard with a default implementation, it makes it really hard to detect missing
            // implementation when adding new `Expression` variants.
            Self::None => BTreeMap::new(),
            Self::Bytes(_) => BTreeMap::new(),
            Self::Number(_) => BTreeMap::new(),
            Self::Bool(_) => BTreeMap::new(),
            Self::String(_) => BTreeMap::new(),
            Self::Address(_) => BTreeMap::new(),
            Self::Hash(_) => BTreeMap::new(),
            Self::UtxoRefs(_) => BTreeMap::new(),
            Self::UtxoSet(_) => BTreeMap::new(),
        }
    }

    fn queries(&self) -> BTreeMap<String, InputQuery> {
        match self {
            Self::List(x) => x.iter().flat_map(|x| x.queries()).collect(),
            Self::Map(x) => x
                .iter()
                .flat_map(|(k, v)| [k.queries(), v.queries()].into_iter().flatten())
                .collect(),
            Self::Tuple(x) => [x.0.queries(), x.1.queries()]
                .into_iter()
                .flatten()
                .collect(),
            Self::Struct(x) => x.queries(),
            Self::Assets(x) => x.iter().flat_map(|x| x.queries()).collect(),
            Self::EvalParam(x) => x.queries(),
            Self::EvalBuiltIn(x) => x.queries(),
            Self::EvalCoerce(x) => x.queries(),
            Self::EvalCompiler(x) => x.queries(),
            Self::AdHocDirective(x) => x.queries(),

            // Don't fall into the temptation of simplifying the following cases under a single
            // wildcard with a default implementation, it makes it really hard to detect missing
            // implementation when adding new `Expression` variants.
            Self::None => BTreeMap::new(),
            Self::Bytes(_) => BTreeMap::new(),
            Self::Number(_) => BTreeMap::new(),
            Self::Bool(_) => BTreeMap::new(),
            Self::String(_) => BTreeMap::new(),
            Self::Address(_) => BTreeMap::new(),
            Self::Hash(_) => BTreeMap::new(),
            Self::UtxoRefs(_) => BTreeMap::new(),
            Self::UtxoSet(_) => BTreeMap::new(),
        }
    }

    fn reduce(self) -> Result<Self, Error> {
        match self {
            // the following expressions can only be reduced internally
            Expression::List(x) => Ok(Self::List(
                x.into_iter()
                    .map(|x| x.reduce())
                    .collect::<Result<_, _>>()?,
            )),
            Expression::Map(x) => Ok(Self::Map(
                x.into_iter()
                    .map(|(k, v)| Ok::<(Expression, Expression), Error>((k.reduce()?, v.reduce()?)))
                    .collect::<Result<_, _>>()?,
            )),
            Expression::Tuple(x) => Ok(Self::Tuple(Box::new((x.0.reduce()?, x.1.reduce()?)))),
            Expression::Struct(x) => Ok(Self::Struct(x.reduce()?)),
            Expression::Assets(x) => Ok(Self::Assets(
                x.into_iter()
                    .map(|x| x.reduce())
                    .collect::<Result<_, _>>()?,
            )),
            Expression::EvalCompiler(x) => Ok(Self::EvalCompiler(Box::new(x.reduce()?))),
            Expression::AdHocDirective(x) => Ok(Self::AdHocDirective(Box::new(x.reduce()?))),

            // the following ones can be turned into simpler expressions
            Expression::EvalBuiltIn(x) => match x.reduce()? {
                BuiltInOp::NoOp(x) => Ok(x),
                x => Ok(Expression::EvalBuiltIn(Box::new(x.reduce()?))),
            },
            Expression::EvalCoerce(x) => match x.reduce()? {
                Coerce::NoOp(x) => Ok(x),
                x => Ok(Expression::EvalCoerce(Box::new(x.reduce()?))),
            },
            Expression::EvalParam(x) => match x.reduce()? {
                Param::Set(x) => Ok(x),
                x => Ok(Expression::EvalParam(Box::new(x.reduce()?))),
            },

            // Don't fall into the temptation of simplifying the following cases under a single
            // wildcard with a default implementation, it makes it really hard to detect missing
            // implementation when adding new `Expression` variants.
            Self::None => Ok(self),
            Self::Bytes(_) => Ok(self),
            Self::Number(_) => Ok(self),
            Self::Bool(_) => Ok(self),
            Self::String(_) => Ok(self),
            Self::Address(_) => Ok(self),
            Self::Hash(_) => Ok(self),
            Self::UtxoRefs(_) => Ok(self),
            Self::UtxoSet(_) => Ok(self),
        }
    }
}

impl Composite for Output {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.address, &self.datum, &self.amount]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            address: f(self.address)?,
            datum: f(self.datum)?,
            amount: f(self.amount)?,
            optional: self.optional,
        })
    }
}

impl Composite for Mint {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.amount, &self.redeemer]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            amount: f(self.amount)?,
            redeemer: f(self.redeemer)?,
        })
    }
}

impl Composite for AdHocDirective {
    fn components(&self) -> Vec<&Expression> {
        self.data.values().collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
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

impl Composite for CompilerOp {
    fn components(&self) -> Vec<&Expression> {
        match self {
            CompilerOp::BuildScriptAddress(x) => vec![x],
            CompilerOp::ComputeMinUtxo(x) => vec![x],
            CompilerOp::ComputeTipSlot => vec![],
            CompilerOp::ComputeSlotToTime(x) => vec![x],
            CompilerOp::ComputeTimeToSlot(x) => vec![x],
        }
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        match self {
            CompilerOp::BuildScriptAddress(x) => Ok(CompilerOp::BuildScriptAddress(f(x)?)),
            CompilerOp::ComputeMinUtxo(x) => Ok(CompilerOp::ComputeMinUtxo(f(x)?)),
            CompilerOp::ComputeTipSlot => Ok(CompilerOp::ComputeTipSlot),
            CompilerOp::ComputeSlotToTime(x) => Ok(CompilerOp::ComputeSlotToTime(f(x)?)),
            CompilerOp::ComputeTimeToSlot(x) => Ok(CompilerOp::ComputeTimeToSlot(f(x)?)),
        }
    }
}

impl Composite for Signers {
    fn components(&self) -> Vec<&Expression> {
        self.signers.iter().collect()
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            signers: self.signers.into_iter().map(f).collect::<Result<_, _>>()?,
        })
    }
}

impl Composite for Collateral {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.utxos]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            utxos: f(self.utxos)?,
        })
    }
}

impl Composite for Validity {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.since, &self.until]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            since: f(self.since)?,
            until: f(self.until)?,
        })
    }
}

impl Composite for Metadata {
    fn components(&self) -> Vec<&Expression> {
        vec![&self.key, &self.value]
    }

    fn try_map_components<F>(self, f: F) -> Result<Self, Error>
    where
        F: Fn(Expression) -> Result<Expression, Error> + Clone,
    {
        Ok(Self {
            key: f(self.key)?,
            value: f(self.value)?,
        })
    }
}

impl Apply for Tx {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        let tx = Tx {
            references: self.references.apply_args(args)?,
            inputs: self.inputs.apply_args(args)?,
            outputs: self.outputs.apply_args(args)?,
            validity: self.validity.apply_args(args)?,
            mints: self.mints.apply_args(args)?,
            burns: self.burns.apply_args(args)?,
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
            burns: self.burns.apply_inputs(args)?,
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
            burns: self.burns.apply_fees(fees)?,
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
            && self.burns.iter().all(|x| x.is_constant())
            && self.fees.is_constant()
            && self.metadata.is_constant()
            && self.validity.is_constant()
            && self.references.is_constant()
            && self.collateral.is_constant()
            && self.adhoc.iter().all(|x| x.is_constant())
            && self.signers.is_constant()
    }

    fn params(&self) -> BTreeMap<String, Type> {
        // TODO: analyze if necessary to add ref_inputs
        let mut params = BTreeMap::new();
        params.extend(self.inputs.params());
        params.extend(self.outputs.params());
        params.extend(self.mints.params());
        params.extend(self.burns.params());
        params.extend(self.fees.params());
        params.extend(self.adhoc.params());
        params.extend(self.signers.params());
        params.extend(self.validity.params());
        params.extend(self.metadata.params());
        params.extend(self.references.params());
        params.extend(self.collateral.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, InputQuery> {
        let mut queries = BTreeMap::new();
        queries.extend(self.inputs.queries());
        queries.extend(self.outputs.queries());
        queries.extend(self.mints.queries());
        queries.extend(self.burns.queries());
        queries.extend(self.fees.queries());
        queries.extend(self.adhoc.queries());
        queries.extend(self.signers.queries());
        queries.extend(self.validity.queries());
        queries.extend(self.metadata.queries());
        queries.extend(self.collateral.queries());
        queries.extend(self.references.queries());
        queries
    }

    fn reduce(self) -> Result<Self, Error> {
        Ok(Self {
            references: self.references.reduce()?,
            inputs: self.inputs.reduce()?,
            outputs: self.outputs.reduce()?,
            validity: self.validity.reduce()?,
            mints: self.mints.reduce()?,
            burns: self.burns.reduce()?,
            fees: self.fees.reduce()?,
            adhoc: self.adhoc.reduce()?,
            collateral: self.collateral.reduce()?,
            signers: self.signers.reduce()?,
            metadata: self.metadata.reduce()?,
        })
    }
}

#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub enum ArgValue {
    Int(i128),
    Bool(bool),
    String(String),
    Bytes(Vec<u8>),
    Address(Vec<u8>),
    UtxoSet(UtxoSet),
    UtxoRef(UtxoRef),
}

impl From<Vec<u8>> for ArgValue {
    fn from(value: Vec<u8>) -> Self {
        Self::Bytes(value)
    }
}

impl From<String> for ArgValue {
    fn from(value: String) -> Self {
        Self::String(value)
    }
}

impl From<&str> for ArgValue {
    fn from(value: &str) -> Self {
        Self::String(value.to_string())
    }
}

impl From<bool> for ArgValue {
    fn from(value: bool) -> Self {
        Self::Bool(value)
    }
}

macro_rules! impl_from_int_for_arg_value {
    ($($t:ty),*) => {
        $(
            impl From<$t> for ArgValue {
                fn from(value: $t) -> Self {
                    Self::Int(value as i128)
                }
            }
        )*
    };
}

impl_from_int_for_arg_value!(i8, i16, i32, i64, i128, u8, u16, u32, u64, u128);

pub fn apply_args(template: Tx, args: &BTreeMap<String, ArgValue>) -> Result<Tx, Error> {
    template.apply_args(args)
}

pub fn apply_inputs(template: Tx, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Tx, Error> {
    template.apply_inputs(args)
}

pub fn apply_fees(template: Tx, fees: u64) -> Result<Tx, Error> {
    template.apply_fees(fees)
}

pub fn reduce(template: Tx) -> Result<Tx, Error> {
    template.reduce()
}

pub fn find_params(template: &Tx) -> BTreeMap<String, Type> {
    template.params()
}

pub fn find_queries(template: &Tx) -> BTreeMap<String, InputQuery> {
    template.queries()
}

#[cfg(test)]
mod tests {
    use super::*;

    const SUBJECT_TIR: &str = include_str!("test_subject.tir");

    fn get_subject_tx() -> Tx {
        serde_json::from_str::<Tx>(SUBJECT_TIR).unwrap()
    }

    #[test]
    fn param_expression_is_applied() {
        let ir = Expression::EvalParam(Box::new(Param::ExpectValue("a".to_string(), Type::Int)));

        let params = ir.params();
        assert_eq!(params.len(), 1);
        assert_eq!(params.get("a"), Some(&Type::Int));

        let args = BTreeMap::from([("a".to_string(), ArgValue::Int(100))]);

        let after = ir.apply_args(&args).unwrap();

        assert_eq!(
            after,
            Expression::EvalParam(Box::new(Param::Set(Expression::Number(100),)))
        );
    }

    #[test]
    fn nested_param_expression_is_applied() {
        let ir = Expression::EvalParam(Box::new(Param::ExpectInput(
            "out".to_string(),
            InputQuery {
                address: Expression::None,
                min_amount: Expression::None,
                r#ref: Expression::EvalParam(Box::new(Param::ExpectValue(
                    "in".to_string(),
                    Type::Int,
                ))),
                many: false,
                collateral: false,
            },
        )));

        let params = ir.params();
        assert_eq!(params.len(), 1);
        assert_eq!(params.get("in"), Some(&Type::Int));

        let args = BTreeMap::from([("in".to_string(), ArgValue::Int(100))]);
        let after = ir.apply_args(&args).unwrap();

        let after = after.reduce().unwrap();

        let queries = after.queries();
        assert_eq!(queries.len(), 1);
        assert_eq!(
            queries.get("out"),
            Some(&InputQuery {
                address: Expression::None,
                min_amount: Expression::None,
                r#ref: Expression::Number(100),
                many: false,
                collateral: false,
            })
        );
    }

    #[test]
    fn param_expression_is_reduced() {
        let ir = Expression::EvalParam(Box::new(Param::Set(Expression::Number(3))));

        let after = ir.reduce().unwrap();

        assert_eq!(after, Expression::Number(3));
    }

    #[test]
    fn test_apply_args() {
        let before = get_subject_tx();

        let params = find_params(&before);
        assert_eq!(params.len(), 3);
        assert_eq!(params.get("sender"), Some(&Type::Address));
        assert_eq!(params.get("a"), Some(&Type::Int));
        assert_eq!(params.get("b"), Some(&Type::Int));

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
    fn test_apply_inputs() {
        let before = get_subject_tx();

        let args = BTreeMap::from([
            ("sender".to_string(), ArgValue::Address(b"abc".to_vec())),
            ("a".to_string(), ArgValue::Int(100)),
            ("b".to_string(), ArgValue::Int(200)),
        ]);

        let before = apply_args(before, &args).unwrap();

        let before = before.reduce().unwrap();

        let queries = find_queries(&before);
        dbg!(&queries);

        assert_eq!(queries.len(), 1);
        assert!(queries.contains_key("source"));

        let inputs = BTreeMap::from([(
            "source".to_string(),
            HashSet::from([Utxo {
                r#ref: UtxoRef::new(b"abc", 0),
                address: b"abc".to_vec(),
                datum: None,
                assets: CanonicalAssets::from_naked_amount(300),
                script: None,
            }]),
        )]);

        let after = apply_inputs(before, &inputs).unwrap();

        let queries = find_queries(&after);
        dbg!(&queries);

        assert_eq!(queries.len(), 0);
    }

    #[test]
    fn test_apply_fees() {
        let before = get_subject_tx();

        let args = BTreeMap::from([
            ("sender".to_string(), ArgValue::Address(b"abc".to_vec())),
            ("a".to_string(), ArgValue::Int(100)),
            ("b".to_string(), ArgValue::Int(200)),
        ]);

        let before = apply_args(before, &args).unwrap().reduce().unwrap();

        let after = before.apply_fees(100).unwrap().reduce().unwrap();

        let queries = find_queries(&after);

        let query = queries.get("source").unwrap();

        assert_eq!(
            query,
            &InputQuery {
                address: Expression::Address(b"abc".to_vec()),
                min_amount: Expression::Assets(vec![AssetExpr {
                    policy: Expression::None,
                    asset_name: Expression::None,
                    amount: Expression::Number(400),
                }]),
                r#ref: Expression::None,
                many: false,
                collateral: false,
            }
        );
    }

    #[test]
    fn built_in_expression_is_reduced() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::NoOp(Expression::Number(5))));

        let after = op.reduce().unwrap();

        assert_eq!(after, Expression::Number(5))
    }

    #[test]
    fn numeric_add_is_reduced() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Add(
            Expression::Number(1),
            Expression::Number(5),
        )));

        let after = op.reduce().unwrap();

        assert_eq!(after, Expression::Number(6));
    }

    #[test]
    fn numeric_sub_is_reduced() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Sub(
            Expression::Number(8),
            Expression::Number(5),
        )));

        let after = op.reduce().unwrap();

        assert_eq!(after, Expression::Number(3));
    }

    #[test]
    fn nested_numeric_binary_op_is_reduced() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Add(
            Expression::Number(1),
            Expression::EvalBuiltIn(Box::new(BuiltInOp::Sub(
                Expression::Number(5),
                Expression::Number(3),
            ))),
        )));

        let after = op.reduce().unwrap();

        assert_eq!(after, Expression::Number(3));
    }

    #[test]
    fn test_reduce_single_custom_asset_binary_op() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Add(
            Expression::Assets(vec![AssetExpr {
                policy: Expression::Bytes(b"abc".to_vec()),
                asset_name: Expression::Bytes(b"111".to_vec()),
                amount: Expression::Number(100),
            }]),
            Expression::Assets(vec![AssetExpr {
                policy: Expression::Bytes(b"abc".to_vec()),
                asset_name: Expression::Bytes(b"111".to_vec()),
                amount: Expression::Number(200),
            }]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::Assets(assets) => {
                assert_eq!(assets.len(), 1);
                assert_eq!(assets[0].policy, Expression::Bytes(b"abc".to_vec()));
                assert_eq!(assets[0].asset_name, Expression::Bytes(b"111".to_vec()));
                assert_eq!(assets[0].amount, Expression::Number(300));
            }
            _ => panic!("Expected assets"),
        };
    }

    #[test]
    fn test_reduce_native_asset_binary_op() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Add(
            Expression::Assets(vec![AssetExpr {
                policy: Expression::None,
                asset_name: Expression::None,
                amount: Expression::Number(100),
            }]),
            Expression::Assets(vec![AssetExpr {
                policy: Expression::None,
                asset_name: Expression::None,
                amount: Expression::Number(200),
            }]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::Assets(assets) => {
                assert_eq!(assets.len(), 1);
                assert_eq!(assets[0].policy, Expression::None);
                assert_eq!(assets[0].asset_name, Expression::None);
                assert_eq!(assets[0].amount, Expression::Number(300));
            }
            _ => panic!("Expected assets"),
        };
    }

    #[test]
    fn test_reduce_mixed_asset_binary_op() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Add(
            Expression::Assets(vec![AssetExpr {
                policy: Expression::None,
                asset_name: Expression::None,
                amount: Expression::Number(100),
            }]),
            Expression::Assets(vec![AssetExpr {
                policy: Expression::Bytes(b"abc".to_vec()),
                asset_name: Expression::Bytes(b"111".to_vec()),
                amount: Expression::Number(200),
            }]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::Assets(assets) => {
                assert_eq!(assets.len(), 2);

                for asset in assets {
                    if asset.policy == Expression::None {
                        assert_eq!(asset.asset_name, Expression::None);
                        assert_eq!(asset.amount, Expression::Number(100));
                    } else {
                        assert_eq!(asset.policy, Expression::Bytes(b"abc".to_vec()));
                        assert_eq!(asset.asset_name, Expression::Bytes(b"111".to_vec()));
                        assert_eq!(asset.amount, Expression::Number(200));
                    }
                }
            }
            _ => panic!("Expected assets"),
        };
    }

    #[test]
    fn test_reduce_coerce_noop() {
        let op = Expression::EvalCoerce(Box::new(Coerce::NoOp(Expression::Number(5))));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::Number(5) => (),
            _ => panic!("Expected number 5"),
        };
    }

    #[test]
    fn test_coerce_utxo_set_into_assets() {
        let utxos = vec![Utxo {
            r#ref: UtxoRef::new(b"abc", 1),
            address: b"abc".into(),
            datum: Some(Expression::Number(1)),
            assets: CanonicalAssets::from_defined_asset(b"abc", b"111", 1),
            script: None,
        }];

        let op = Coerce::IntoAssets(Expression::UtxoSet(HashSet::from_iter(
            utxos.clone().into_iter(),
        )));

        let reduced = op.reduce().unwrap();

        assert_eq!(
            reduced,
            Coerce::NoOp(Expression::Assets(utxos[0].assets.clone().into()))
        );
    }

    #[test]
    fn test_coerce_utxo_set_into_datum() {
        let utxos = vec![Utxo {
            r#ref: UtxoRef::new(b"abc", 1),
            address: b"abc".into(),
            datum: Some(Expression::Number(1)),
            assets: CanonicalAssets::from_naked_amount(1),
            script: None,
        }];

        let op = Coerce::IntoDatum(Expression::UtxoSet(HashSet::from_iter(
            utxos.clone().into_iter(),
        )));

        let reduced = op.reduce().unwrap();

        assert_eq!(reduced, Coerce::NoOp(utxos[0].datum.clone().unwrap()));
    }

    #[test]
    fn test_reduce_struct_property_access() {
        let object = Expression::Struct(StructExpr {
            constructor: 0,
            fields: vec![
                Expression::Number(1),
                Expression::Number(2),
                Expression::Number(3),
            ],
        });

        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            object.clone(),
            Expression::Number(1),
        )));

        let reduced = op.reduce().unwrap();

        assert_eq!(reduced, Expression::Number(2));

        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            object.clone(),
            Expression::Number(100),
        )));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(100, _)) => (),
            _ => panic!("Expected property index not found"),
        };
    }

    #[test]
    fn test_reduce_list_property_access() {
        let object = Expression::List(vec![
            Expression::Number(1),
            Expression::Number(2),
            Expression::Number(3),
        ]);

        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            object.clone(),
            Expression::Number(1),
        )));

        let reduced = op.reduce();

        match reduced {
            Ok(Expression::Number(2)) => (),
            _ => panic!("Expected number 2"),
        };

        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            object.clone(),
            Expression::Number(100),
        )));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(100, _)) => (),
            _ => panic!("Expected property index not found"),
        };
    }

    #[test]
    fn test_reduce_tuple_property_access() {
        let object = Expression::Tuple(Box::new((Expression::Number(1), Expression::Number(2))));

        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            object.clone(),
            Expression::Number(1),
        )));

        let reduced = op.reduce();

        match reduced {
            Ok(Expression::Number(2)) => (),
            _ => panic!("Expected number 2"),
        };

        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            object.clone(),
            Expression::Number(100),
        )));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(100, _)) => (),
            _ => panic!("Expected property index not found"),
        };
    }

    #[test]
    fn test_string_concat_is_reduced() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Concat(
            Expression::String("hello".to_string()),
            Expression::String("world".to_string()),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::String(s) => assert_eq!(s, "helloworld"),
            _ => panic!("Expected string 'helloworld'"),
        }
    }

    #[test]
    fn test_bytes_concat_is_reduced() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Concat(
            Expression::Bytes(vec![1, 2, 3]),
            Expression::Bytes(vec![4, 5, 6]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::Bytes(b) => assert_eq!(b, vec![1, 2, 3, 4, 5, 6]),
            _ => panic!("Expected bytes [1, 2, 3, 4, 5, 6]"),
        }
    }

    #[test]
    fn test_list_concat_is_reduced() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Concat(
            Expression::List(vec![Expression::Number(1)]),
            Expression::List(vec![Expression::Number(2)]),
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::List(b) => {
                assert_eq!(b, vec![Expression::Number(1), Expression::Number(2)])
            }
            _ => panic!("Expected List [Number(1), Number(2)"),
        }
    }

    #[test]
    fn test_concat_type_mismatch_error() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Concat(
            Expression::String("hello".to_string()),
            Expression::Bytes(vec![1, 2, 3]),
        )));

        let reduced = op.reduce();

        match reduced {
            Err(Error::InvalidBinaryOp(op, _, _)) => assert_eq!(op, "concat"),
            _ => panic!("Expected InvalidBinaryOp error"),
        }
    }

    #[test]
    fn test_concat_with_none() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Concat(
            Expression::String("hello".to_string()),
            Expression::None,
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::String(s) => assert_eq!(s, "hello"),
            _ => panic!("Expected string 'hello'"),
        }
    }

    #[test]
    fn test_min_utxo_add_non_reduction() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Add(
            Expression::Assets(vec![AssetExpr {
                policy: Expression::None,
                asset_name: Expression::None,
                amount: Expression::Number(29),
            }]),
            Expression::EvalCompiler(Box::new(CompilerOp::ComputeMinUtxo(Expression::Number(20)))),
        )));

        let reduced = op.clone().reduce().unwrap();

        assert!(op == reduced)
    }

    #[test]
    fn test_min_utxo_sub_non_reduction() {
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Sub(
            Expression::Assets(vec![AssetExpr {
                policy: Expression::None,
                asset_name: Expression::None,
                amount: Expression::Number(29),
            }]),
            Expression::EvalCompiler(Box::new(CompilerOp::ComputeMinUtxo(Expression::Number(20)))),
        )));

        let reduced = op.clone().reduce().unwrap();

        assert!(op == reduced)
    }

    #[test]
    fn test_index_list_with_expression() {
        let list = Expression::List(vec![
            Expression::String("first".to_string()),
            Expression::String("second".to_string()),
            Expression::String("third".to_string()),
        ]);

        let index_expr = Expression::Number(1);
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(list.clone(), index_expr)));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::String(s) => assert_eq!(s, "second"),
            _ => panic!("Expected string 'second'"),
        }
    }

    #[test]
    fn test_index_list_out_of_bounds() {
        let list = Expression::List(vec![Expression::Number(1), Expression::Number(2)]);

        let index_expr = Expression::Number(5);
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(list.clone(), index_expr)));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(5, _)) => (),
            _ => panic!("Expected PropertyIndexNotFound error"),
        }
    }

    #[test]
    fn test_index_tuple_with_expression() {
        let tuple = Expression::Tuple(Box::new((
            Expression::String("left".to_string()),
            Expression::String("right".to_string()),
        )));

        let index_expr = Expression::Number(0);
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(tuple.clone(), index_expr)));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::String(s) => assert_eq!(s, "left"),
            _ => panic!("Expected string 'left'"),
        }

        let index_expr = Expression::Number(1);
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(tuple.clone(), index_expr)));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::String(s) => assert_eq!(s, "right"),
            _ => panic!("Expected string 'right'"),
        }
    }

    #[test]
    fn test_index_struct_with_expression() {
        let struct_expr = Expression::Struct(StructExpr {
            constructor: 0,
            fields: vec![
                Expression::String("field0".to_string()),
                Expression::String("field1".to_string()),
                Expression::String("field2".to_string()),
            ],
        });

        let index_expr = Expression::Number(1);
        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            struct_expr.clone(),
            index_expr,
        )));

        let reduced = op.reduce().unwrap();

        match reduced {
            Expression::String(s) => assert_eq!(s, "field1"),
            _ => panic!("Expected string 'field1'"),
        }
    }

    #[test]
    fn test_index_none_expression() {
        let none_expr = Expression::None;
        let index_expr = Expression::Number(0);

        let op = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(none_expr, index_expr)));

        let reduced = op.reduce();

        match reduced {
            Err(Error::PropertyIndexNotFound(0, _)) => (),
            _ => panic!("Expected PropertyIndexNotFound error for None expression"),
        }
    }

    #[test]
    fn test_indexable_trait_on_expression() {
        let list_expr = Expression::List(vec![Expression::Number(10), Expression::Number(20)]);

        let result = list_expr.index(Expression::Number(0));
        assert_eq!(result, Some(Expression::Number(10)));

        let result = list_expr.index(Expression::Number(1));
        assert_eq!(result, Some(Expression::Number(20)));

        let result = list_expr.index(Expression::Number(2));
        assert_eq!(result, None);

        let tuple_expr =
            Expression::Tuple(Box::new((Expression::Number(100), Expression::Number(200))));

        let result = tuple_expr.index(Expression::Number(0));
        assert_eq!(result, Some(Expression::Number(100)));

        let result = tuple_expr.index(Expression::Number(1));
        assert_eq!(result, Some(Expression::Number(200)));

        let result = tuple_expr.index(Expression::Number(2));
        assert_eq!(result, None);
    }

    #[test]
    fn test_nested_property_access() {
        let nested_expr = Expression::List(vec![
            Expression::Tuple(Box::new((Expression::Number(1), Expression::Number(2)))),
            Expression::Tuple(Box::new((Expression::Number(3), Expression::Number(4)))),
        ]);

        // Access nested_expr[1][0] (should be 3)
        let first_access = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            nested_expr,
            Expression::Number(1),
        )));

        let second_access = Expression::EvalBuiltIn(Box::new(BuiltInOp::Property(
            first_access,
            Expression::Number(0),
        )));

        let reduced = second_access.reduce().unwrap();

        match reduced {
            Expression::Number(n) => assert_eq!(n, 3),
            _ => panic!("Expected number 3"),
        }
    }
}
