use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{
    ir::{self, BinaryOp, PropertyAccess},
    ArgValue, Utxo,
};

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("invalid binary operation {0:?}")]
    InvalidBinaryOp(Box<BinaryOp>),

    #[error("invalid argument {0:?} for {1}")]
    InvalidArgument(ArgValue, String),

    #[error("property {0} not found in {1}")]
    PropertyNotFound(String, String),

    #[error("invalid eval property")]
    InvalidEvalProperty,
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

    fn reduce_self(self) -> Result<Self, Error>;
    fn reduce_nested(self) -> Result<Self, Error>;

    fn reduce(self) -> Result<Self, Error> {
        let reduced = self.reduce_nested()?;

        if reduced.is_constant() {
            reduced.reduce_self()
        } else {
            Ok(reduced)
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

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        self.map(|x| x.reduce()).transpose()
    }
}

impl<T> Apply for Box<T>
where
    T: Apply,
{
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        let x = *self;
        Ok(Box::new(x.apply_args(args)?))
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        let x = *self;
        Ok(Box::new(x.apply_inputs(args)?))
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        let x = *self;
        Ok(Box::new(x.apply_fees(fees)?))
    }

    fn is_constant(&self) -> bool {
        self.as_ref().is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        self.as_ref().params()
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        self.as_ref().queries()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        let x = (*self).reduce()?;
        Ok(Box::new(x))
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
        // TODO: what happens if there's a conflict on types?

        self.iter()
            .map(|x| x.params())
            .fold(BTreeMap::new(), |mut acc, map| {
                acc.extend(map);
                acc
            })
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        self.iter()
            .map(|x| x.queries())
            .fold(BTreeMap::new(), |mut acc, map| {
                acc.extend(map);
                acc
            })
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        let reduced = self
            .into_iter()
            .map(|x| x.reduce())
            .collect::<Result<Vec<_>, _>>()?;

        Ok(reduced)
    }
}

impl<T> Apply for HashMap<String, T>
where
    T: Apply,
{
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        let items = self
            .into_iter()
            .map(|(k, v)| v.apply_args(args).map(|v| (k, v)))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .collect();

        Ok(items)
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        let items = self
            .into_iter()
            .map(|(k, v)| v.apply_inputs(args).map(|v| (k, v)))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .collect();

        Ok(items)
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        let items = self
            .into_iter()
            .map(|(k, v)| v.apply_fees(fees).map(|v| (k, v)))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .collect();

        Ok(items)
    }

    fn is_constant(&self) -> bool {
        self.iter().all(|(_, v)| v.is_constant())
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        self.iter()
            .map(|(_, v)| v.params())
            .fold(BTreeMap::new(), |mut acc, map| {
                acc.extend(map);
                acc
            })
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        self.iter()
            .map(|(_, v)| v.queries())
            .fold(BTreeMap::new(), |mut acc, map| {
                acc.extend(map);
                acc
            })
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        let reduced = self
            .into_iter()
            .map(|(k, v)| v.reduce().map(|v| (k, v)))
            .collect::<Result<Vec<_>, _>>()?
            .into_iter()
            .collect();

        Ok(reduced)
    }
}

impl Apply for ir::ScriptSource {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        match self {
            ir::ScriptSource::Embedded(x) => Ok(ir::ScriptSource::Embedded(x.apply_args(args)?)),
            ir::ScriptSource::UtxoRef { r#ref, source } => Ok(ir::ScriptSource::UtxoRef {
                r#ref: r#ref.apply_args(args)?,
                source: source.apply_args(args)?,
            }),
        }
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        match self {
            ir::ScriptSource::Embedded(x) => Ok(ir::ScriptSource::Embedded(x.apply_inputs(args)?)),
            ir::ScriptSource::UtxoRef { r#ref, source } => Ok(ir::ScriptSource::UtxoRef {
                r#ref: r#ref.apply_inputs(args)?,
                source: source.apply_inputs(args)?,
            }),
        }
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        match self {
            ir::ScriptSource::Embedded(x) => Ok(ir::ScriptSource::Embedded(x.apply_fees(fees)?)),
            ir::ScriptSource::UtxoRef { r#ref, source } => Ok(ir::ScriptSource::UtxoRef {
                r#ref: r#ref.apply_fees(fees)?,
                source: source.apply_fees(fees)?,
            }),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            ir::ScriptSource::Embedded(x) => x.is_constant(),
            ir::ScriptSource::UtxoRef { r#ref, source } => {
                r#ref.is_constant() && source.is_constant()
            }
        }
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();

        match self {
            ir::ScriptSource::Embedded(x) => params.extend(x.params()),
            ir::ScriptSource::UtxoRef { r#ref, source } => {
                params.extend(r#ref.params());
                params.extend(source.params());
            }
        }

        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        let mut queries = BTreeMap::new();

        match self {
            ir::ScriptSource::Embedded(x) => queries.extend(x.queries()),
            ir::ScriptSource::UtxoRef { r#ref, source } => {
                queries.extend(r#ref.queries());
                queries.extend(source.queries());
            }
        }

        queries
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        match self {
            ir::ScriptSource::Embedded(x) => Ok(ir::ScriptSource::Embedded(x.reduce_nested()?)),
            ir::ScriptSource::UtxoRef { r#ref, source } => Ok(ir::ScriptSource::UtxoRef {
                r#ref: r#ref.reduce_nested()?,
                source: source.reduce_nested()?,
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

impl Apply for ir::PolicyExpr {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        let name = self.name;
        let hash = self.hash.apply_args(args)?;

        let script = if self.script.is_some() {
            self.script.apply_args(args)?
        } else {
            let defined = args.get(&format!("{}_script", name.to_lowercase()));
            defined.map(TryFrom::try_from).transpose()?
        };

        Ok(Self { name, hash, script })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        let name = self.name;
        let hash = self.hash.apply_inputs(args)?;

        let script = self
            .script
            .map(|x| match x {
                ir::ScriptSource::UtxoRef { r#ref, .. } => {
                    let source = args
                        .get(&name.to_lowercase())
                        .and_then(|x| x.iter().next())
                        .and_then(|x| x.script.clone());

                    Ok(ir::ScriptSource::UtxoRef { r#ref, source })
                }
                x => Ok(x),
            })
            .transpose()?;

        Ok(Self { name, hash, script })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            name: self.name,
            hash: self.hash.apply_fees(fees)?,
            script: self.script.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.hash.is_constant() && self.script.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.hash.params());
        params.extend(self.script.params());

        if self.script.is_none() {
            params.insert(
                format!("{}_script", self.name.to_lowercase()),
                ir::Type::UtxoRef,
            );
        }

        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        let mut queries = BTreeMap::new();

        if let Some(ir::ScriptSource::UtxoRef { r#ref, source }) = &self.script {
            if source.is_none() {
                queries.insert(
                    self.name.to_lowercase(),
                    ir::InputQuery {
                        r#ref: Some(r#ref.clone()),
                        ..Default::default()
                    },
                );
            }
        }

        queries
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            name: self.name,
            hash: self.hash.reduce_nested()?,
            script: self.script.reduce_nested()?,
        })
    }
}

impl Apply for ir::StructExpr {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            constructor: self.constructor,
            fields: self
                .fields
                .into_iter()
                .map(|x| x.apply_args(args))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            constructor: self.constructor,
            fields: self
                .fields
                .into_iter()
                .map(|x| x.apply_inputs(args))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            constructor: self.constructor,
            fields: self
                .fields
                .into_iter()
                .map(|x| x.apply_fees(fees))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }

    fn is_constant(&self) -> bool {
        self.fields.iter().all(|x| x.is_constant())
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        self.fields
            .iter()
            .map(|x| x.params())
            .fold(BTreeMap::new(), |mut acc, map| {
                acc.extend(map);
                acc
            })
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        // structs don't have queries
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            constructor: self.constructor,
            fields: self
                .fields
                .into_iter()
                .map(|x| x.reduce())
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl Apply for ir::PropertyAccess {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            object: self.object.apply_args(args)?,
            field: self.field,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            object: self.object.apply_inputs(args)?,
            field: self.field,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            object: self.object.apply_fees(fees)?,
            field: self.field,
        })
    }

    fn is_constant(&self) -> bool {
        self.object.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        self.object.params()
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        self.object.queries()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            object: self.object.reduce()?,
            field: self.field,
        })
    }
}

impl Apply for ir::AssetExpr {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            policy: self.policy.apply_args(args)?,
            asset_name: self.asset_name.apply_args(args)?,
            amount: self.amount.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            policy: self.policy.apply_inputs(args)?,
            asset_name: self.asset_name.apply_inputs(args)?,
            amount: self.amount.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            policy: self.policy.apply_fees(fees)?,
            asset_name: self.asset_name.apply_fees(fees)?,
            amount: self.amount.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.policy.is_constant() && self.asset_name.is_constant() && self.amount.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.policy.params());
        params.extend(self.asset_name.params());
        params.extend(self.amount.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        // assets don't have queries
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            policy: self.policy.reduce()?,
            asset_name: self.asset_name.reduce()?,
            amount: self.amount.reduce()?,
        })
    }
}

impl Apply for ir::BinaryOp {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        let left = self.left.apply_args(args)?;
        let right = self.right.apply_args(args)?;

        let op = Self {
            left,
            right,
            op: self.op,
        };

        // TODO: reduce if both sides are constants

        Ok(op)
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        let left = self.left.apply_inputs(args)?;
        let right = self.right.apply_inputs(args)?;

        Ok(Self {
            left,
            right,
            op: self.op,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            left: self.left.apply_fees(fees)?,
            right: self.right.apply_fees(fees)?,
            op: self.op,
        })
    }

    fn is_constant(&self) -> bool {
        self.left.is_constant() && self.right.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.left.params());
        params.extend(self.right.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        // binary ops don't have queries
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            left: self.left.reduce()?,
            right: self.right.reduce()?,
            op: self.op,
        })
    }
}

fn build_assets(aggregated: HashMap<AssetClass, i128>) -> Vec<ir::AssetExpr> {
    // Convert back to Vec<AssetExpr>
    aggregated
        .into_iter()
        .map(|((policy, asset_name), amount)| ir::AssetExpr {
            policy: match policy {
                Some(policy) => ir::Expression::Bytes(policy.to_vec()),
                None => ir::Expression::None,
            },
            asset_name: match asset_name {
                Some(asset_name) => ir::Expression::Bytes(asset_name.to_vec()),
                None => ir::Expression::None,
            },
            amount: ir::Expression::Number(amount),
        })
        .collect()
}

type AssetClass<'a> = (Option<&'a [u8]>, Option<&'a [u8]>);

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

    fn aggregate(items: &[Self]) -> HashMap<AssetClass, i128> {
        let mut aggregated: HashMap<AssetClass, i128> = HashMap::new();

        // Group assets by policy and asset_name, summing their amounts
        for asset in items.iter() {
            let policy = asset.expect_constant_policy();
            let asset_name = asset.expect_constant_name();
            let amount = asset.expect_constant_amount();

            let key = (policy, asset_name);
            *aggregated.entry(key).or_default() += amount;
        }

        aggregated
    }

    fn sum<'a>(
        a: HashMap<AssetClass<'a>, i128>,
        b: HashMap<AssetClass<'a>, i128>,
    ) -> HashMap<AssetClass<'a>, i128> {
        let mut aggregated = a;

        for (key, value) in b {
            *aggregated.entry(key).or_default() += value;
        }

        aggregated
    }

    fn sub<'a>(
        a: HashMap<AssetClass<'a>, i128>,
        b: HashMap<AssetClass<'a>, i128>,
    ) -> HashMap<AssetClass<'a>, i128> {
        let mut aggregated = a;

        for (key, value) in b {
            *aggregated.entry(key).or_default() -= value;
        }

        aggregated
    }
}

impl Apply for ir::InputQuery {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.apply_args(args)?,
            min_amount: self.min_amount.apply_args(args)?,
            r#ref: self.r#ref.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.apply_inputs(args)?,
            min_amount: self.min_amount.apply_inputs(args)?,
            r#ref: self.r#ref.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.apply_fees(fees)?,
            min_amount: self.min_amount.apply_fees(fees)?,
            r#ref: self.r#ref.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.address.is_constant() && self.min_amount.is_constant() && self.r#ref.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.address.params());
        params.extend(self.min_amount.params());
        params.extend(self.r#ref.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        // input queries itself don't have inner queries. This is assuming that an
        // expression higher up the tree will return this as a required query.
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.reduce()?,
            min_amount: self.min_amount.reduce()?,
            r#ref: self.r#ref.reduce()?,
        })
    }
}

impl Apply for ir::Input {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            query: self.query.apply_args(args)?,
            redeemer: self.redeemer.apply_args(args)?,
            policy: self.policy.apply_args(args)?,
            ..self
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        let defined = args.get(&self.name).cloned();

        if let Some(refs) = defined {
            return Ok(Self {
                query: None,
                refs: refs.into_iter().map(|x| x.r#ref).collect(),
                ..self
            });
        }

        Ok(self)
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            query: self.query.apply_fees(fees)?,
            redeemer: self.redeemer.apply_fees(fees)?,
            policy: self.policy.apply_fees(fees)?,
            ..self
        })
    }

    fn is_constant(&self) -> bool {
        self.query.is_constant() && self.redeemer.is_constant() && self.policy.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.query.params());
        params.extend(self.redeemer.params());
        params.extend(self.policy.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        let input = self.query.iter().map(|x| (self.name.clone(), x.clone()));
        let policy = self.policy.queries();

        BTreeMap::from_iter(input.chain(policy))
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            query: self.query.reduce()?,
            redeemer: self.redeemer.reduce()?,
            policy: self.policy.reduce()?,
            ..self
        })
    }
}

impl Apply for ir::Expression {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        match self {
            ir::Expression::Struct(x) => Ok(ir::Expression::Struct(x.apply_args(args)?)),
            ir::Expression::List(x) => Ok(ir::Expression::List(x.apply_args(args)?)),
            ir::Expression::Assets(x) => Ok(ir::Expression::Assets(x.apply_args(args)?)),
            ir::Expression::EvalCustom(x) => Ok(ir::Expression::EvalCustom(x.apply_args(args)?)),
            ir::Expression::EvalParameter(name, ty) => {
                let defined = args.get(&name).cloned();

                match defined {
                    Some(x) => Ok(arg_value_into_expr(x)),
                    None => Ok(ir::Expression::EvalParameter(name, ty)),
                }
            }
            ir::Expression::EvalProperty(x) => {
                Ok(ir::Expression::EvalProperty(x.apply_args(args)?))
            }
            // the remaining cases are constants, so we can just return them
            x => Ok(x),
        }
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        match self {
            ir::Expression::EvalInputDatum(x) => {
                let defined = args.get(&x).cloned();

                match defined {
                    Some(x) => {
                        let datum = x
                            .into_iter()
                            .flat_map(|x| x.datum)
                            .next()
                            .unwrap_or(ir::Expression::None);

                        Ok(datum)
                    }
                    None => Ok(ir::Expression::EvalInputDatum(x)),
                }
            }
            ir::Expression::EvalInputAssets(x) => {
                let defined = args.get(&x).cloned();

                match defined {
                    Some(x) => {
                        let assets = x.into_iter().flat_map(|x| x.assets).collect();

                        Ok(ir::Expression::Assets(assets))
                    }
                    None => Ok(ir::Expression::EvalInputAssets(x)),
                }
            }
            ir::Expression::Struct(x) => Ok(ir::Expression::Struct(x.apply_inputs(args)?)),
            ir::Expression::List(x) => Ok(ir::Expression::List(x.apply_inputs(args)?)),
            ir::Expression::Assets(x) => Ok(ir::Expression::Assets(x.apply_inputs(args)?)),
            ir::Expression::EvalCustom(x) => Ok(ir::Expression::EvalCustom(x.apply_inputs(args)?)),
            ir::Expression::EvalProperty(x) => {
                Ok(ir::Expression::EvalProperty(x.apply_inputs(args)?))
            }
            _ => Ok(self),
        }
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        match self {
            ir::Expression::FeeQuery => Ok(ir::Expression::Assets(vec![ir::AssetExpr {
                policy: ir::Expression::None,
                asset_name: ir::Expression::None,
                amount: ir::Expression::Number(fees as i128),
            }])),
            ir::Expression::Struct(x) => Ok(ir::Expression::Struct(x.apply_fees(fees)?)),
            ir::Expression::List(x) => Ok(ir::Expression::List(x.apply_fees(fees)?)),
            ir::Expression::Assets(x) => Ok(ir::Expression::Assets(x.apply_fees(fees)?)),
            ir::Expression::EvalCustom(x) => Ok(ir::Expression::EvalCustom(x.apply_fees(fees)?)),
            _ => Ok(self),
        }
    }

    fn is_constant(&self) -> bool {
        match self {
            Self::None => true,
            Self::Bytes(_) => true,
            Self::Number(_) => true,
            Self::Bool(_) => true,
            Self::String(_) => true,
            Self::Address(_) => true,
            Self::Hash(_) => true,
            Self::UtxoRefs(_) => true,
            Self::UtxoSet(_) => true,
            Self::List(x) => x.is_constant(),
            Self::Struct(x) => x.is_constant(),
            Self::Assets(x) => x.is_constant(),
            Self::EvalCustom(x) => x.is_constant(),
            Self::EvalProperty(x) => x.is_constant(),
            Self::AdHocDirective(x) => x.is_constant(),
            Self::EvalInputDatum(..) => false,
            Self::EvalInputAssets(..) => false,
            Self::FeeQuery => false,
            Self::EvalParameter(..) => false,
            Self::Tuple(b) => {
                let (x, y) = &**b;
                x.is_constant() && y.is_constant()
            }
        }
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        match self {
            ir::Expression::Struct(x) => x.params(),
            ir::Expression::Assets(x) => x.params(),
            ir::Expression::EvalCustom(x) => x.params(),
            ir::Expression::EvalParameter(x, ty) => BTreeMap::from([(x.to_string(), ty.clone())]),

            // the remaining cases are constants, so we can just return them
            _ => BTreeMap::new(),
        }
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        match self {
            ir::Expression::EvalCustom(op) => match (&op.left, &op.right) {
                (ir::Expression::Number(x), ir::Expression::Number(y)) => {
                    let result = match &op.op {
                        ir::BinaryOpKind::Add => x + y,
                        ir::BinaryOpKind::Sub => x - y,
                    };

                    Ok(ir::Expression::Number(result))
                }
                (ir::Expression::Assets(x), ir::Expression::Assets(y)) => {
                    let x = ir::AssetExpr::aggregate(x);
                    let y = ir::AssetExpr::aggregate(y);

                    let result = match &op.op {
                        ir::BinaryOpKind::Add => ir::AssetExpr::sum(x, y),
                        ir::BinaryOpKind::Sub => ir::AssetExpr::sub(x, y),
                    };

                    Ok(ir::Expression::Assets(build_assets(result)))
                }
                _ => Err(Error::InvalidBinaryOp(Box::new(*op))),
            },
            ir::Expression::EvalProperty(property_access) => {
                let object = property_access.object.reduce()?;
                match *object {
                    ir::Expression::Struct(struct_expr) => {
                        Ok(struct_expr.fields[property_access.field as usize].clone())
                    }
                    _ => Err(Error::InvalidEvalProperty),
                }
            }
            _ => Ok(self),
        }
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        match self {
            ir::Expression::Struct(x) => Ok(ir::Expression::Struct(x.reduce()?)),
            ir::Expression::Assets(x) => Ok(ir::Expression::Assets(x.reduce()?)),
            ir::Expression::EvalCustom(x) => Ok(ir::Expression::EvalCustom(x.reduce()?)),
            ir::Expression::EvalProperty(x) => Ok(ir::Expression::EvalProperty(x.reduce()?)),
            _ => Ok(self),
        }
    }
}

impl Apply for ir::Output {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.apply_args(args)?,
            datum: self.datum.apply_args(args)?,
            amount: self.amount.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.apply_inputs(args)?,
            datum: self.datum.apply_inputs(args)?,
            amount: self.amount.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.apply_fees(fees)?,
            datum: self.datum.apply_fees(fees)?,
            amount: self.amount.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.address.is_constant() && self.datum.is_constant() && self.amount.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.address.params());
        params.extend(self.datum.params());
        params.extend(self.amount.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        // outputs don't have queries
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            address: self.address.reduce()?,
            datum: self.datum.reduce()?,
            amount: self.amount.reduce()?,
        })
    }
}

impl Apply for ir::Mint {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            amount: self.amount.apply_args(args)?,
            redeemer: self.redeemer.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            amount: self.amount.apply_inputs(args)?,
            redeemer: self.redeemer.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            amount: self.amount.apply_fees(fees)?,
            redeemer: self.redeemer.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.amount.is_constant() && self.redeemer.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.amount.params());
        params.extend(self.redeemer.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        // mints don't have queries
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            amount: self.amount.reduce()?,
            redeemer: self.redeemer.reduce()?,
        })
    }
}

impl Apply for ir::AdHocDirective {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            name: self.name,
            data: self.data.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            name: self.name,
            data: self.data.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            name: self.name,
            data: self.data.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.data.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.data.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            name: self.name,
            data: self.data.reduce()?,
        })
    }
}

impl Apply for ir::Signers {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            signers: self.signers.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            signers: self.signers.apply_inputs(args)?,
        })
    }
    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            signers: self.signers.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.signers.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.signers.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            signers: self.signers.reduce()?,
        })
    }
}

impl Apply for ir::Collateral {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            query: self.query.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            query: self.query.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            query: self.query.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.query.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.query.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        let mut b_tree = BTreeMap::new();
        b_tree.insert("collateral".to_string(), self.query.clone());
        b_tree
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            query: self.query.reduce()?,
        })
    }
}

impl Apply for ir::Validity {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            since: self.since.apply_args(args)?,
            until: self.until.apply_args(args)?,
        })
    }

    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            since: self.since.apply_inputs(args)?,
            until: self.until.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            since: self.since.apply_fees(fees)?,
            until: self.until.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.since.is_constant() && self.until.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.since.params());
        params.extend(self.until.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        // validity range does not have queries
        BTreeMap::new()
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            since: self.since.reduce()?,
            until: self.until.reduce()?,
        })
    }
}

impl Apply for ir::Metadata {
    fn apply_args(self, args: &BTreeMap<String, ArgValue>) -> Result<Self, Error> {
        Ok(Self {
            key: self.key.apply_args(args)?,
            value: self.value.apply_args(args)?,
        })
    }
    fn apply_inputs(self, args: &BTreeMap<String, HashSet<Utxo>>) -> Result<Self, Error> {
        Ok(Self {
            key: self.key.apply_inputs(args)?,
            value: self.value.apply_inputs(args)?,
        })
    }

    fn apply_fees(self, fees: u64) -> Result<Self, Error> {
        Ok(Self {
            key: self.key.apply_fees(fees)?,
            value: self.value.apply_fees(fees)?,
        })
    }

    fn is_constant(&self) -> bool {
        self.key.is_constant() && self.value.is_constant()
    }

    fn params(&self) -> BTreeMap<String, ir::Type> {
        let mut params = BTreeMap::new();
        params.extend(self.key.params());
        params.extend(self.value.params());
        params
    }

    fn queries(&self) -> BTreeMap<String, ir::InputQuery> {
        let mut queries = BTreeMap::new();
        queries.extend(self.key.queries());
        queries.extend(self.value.queries());
        queries
    }

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
        Ok(Self {
            key: self.key.reduce()?,
            value: self.value.reduce()?,
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

    fn reduce_self(self) -> Result<Self, Error> {
        Ok(self)
    }

    fn reduce_nested(self) -> Result<Self, Error> {
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
    template.reduce()
}

pub fn find_params(template: &ir::Tx) -> BTreeMap<String, ir::Type> {
    template.params()
}

pub fn find_queries(template: &ir::Tx) -> BTreeMap<String, ir::InputQuery> {
    template.queries()
}

#[cfg(test)]
mod tests {

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
    fn test_apply_args() {
        let mut ast = crate::parsing::parse_string(SUBJECT_PROTOCOL).unwrap();
        crate::analyzing::analyze(&mut ast).ok().unwrap();

        let before = crate::lowering::lower(&ast, "swap").unwrap();

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
    fn test_reduce_numeric_binary_op() {
        let op = ir::Expression::EvalCustom(
            ir::BinaryOp {
                op: ir::BinaryOpKind::Add,
                left: ir::Expression::Number(1),
                right: ir::Expression::EvalCustom(
                    ir::BinaryOp {
                        op: ir::BinaryOpKind::Sub,
                        left: ir::Expression::Number(5),
                        right: ir::Expression::Number(3),
                    }
                    .into(),
                ),
            }
            .into(),
        );

        let reduced = op.reduce().unwrap();

        match reduced {
            ir::Expression::Number(3) => (),
            _ => panic!("Expected number 3"),
        };
    }

    #[test]
    fn test_reduce_single_custom_asset_binary_op() {
        let op = ir::Expression::EvalCustom(
            ir::BinaryOp {
                op: ir::BinaryOpKind::Add,
                left: ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::Bytes(b"abc".to_vec()),
                    asset_name: ir::Expression::Bytes(b"111".to_vec()),
                    amount: ir::Expression::Number(100),
                }]),
                right: ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::Bytes(b"abc".to_vec()),
                    asset_name: ir::Expression::Bytes(b"111".to_vec()),
                    amount: ir::Expression::Number(200),
                }]),
            }
            .into(),
        );

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
        let op = ir::Expression::EvalCustom(
            ir::BinaryOp {
                op: ir::BinaryOpKind::Add,
                left: ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::None,
                    asset_name: ir::Expression::None,
                    amount: ir::Expression::Number(100),
                }]),
                right: ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::None,
                    asset_name: ir::Expression::None,
                    amount: ir::Expression::Number(200),
                }]),
            }
            .into(),
        );

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
        let op = ir::Expression::EvalCustom(
            ir::BinaryOp {
                op: ir::BinaryOpKind::Add,
                left: ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::None,
                    asset_name: ir::Expression::None,
                    amount: ir::Expression::Number(100),
                }]),
                right: ir::Expression::Assets(vec![ir::AssetExpr {
                    policy: ir::Expression::Bytes(b"abc".to_vec()),
                    asset_name: ir::Expression::Bytes(b"111".to_vec()),
                    amount: ir::Expression::Number(200),
                }]),
            }
            .into(),
        );

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
}
