//! The Tx3 language intermediate representation (IR).
//!
//! This module defines the intermediate representation (IR) for the Tx3
//! language. It provides the structure for representing Tx3 programs in a more
//! abstract form, suitable for further processing or execution.
//!
//! This module is not intended to be used directly by end-users. See
//! [`lower`](crate::lower) for lowering an AST to the intermediate
//! representation.

use std::collections::{HashMap, HashSet};

use bincode::{Decode, Encode};
use serde::{Deserialize, Serialize};

use crate::{Utxo, UtxoRef};

pub const IR_VERSION: &str = "v1alpha8";

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct StructExpr {
    pub constructor: usize,
    pub fields: Vec<Expression>,
}

impl StructExpr {
    pub fn unit() -> Self {
        Self {
            constructor: 0,
            fields: vec![],
        }
    }
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Coerce {
    NoOp(Expression),
    IntoAssets(Expression),
    IntoDatum(Expression),
    IntoScript(Expression),
}

/// Operations that are executed during the "apply" phase.
///
/// These are operations that are executed during the "apply" phase, as opposed
/// to the compiler operations that are executed during the "compile" phase.
///
/// These ops can be executed (aka "reduced") very early in the process. As long
/// as they underlying expressions are "constant" (aka: don't rely on external
/// data), the will be simplified directly during the "apply" phase.
#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum BuiltInOp {
    NoOp(Expression),
    Add(Expression, Expression),
    Sub(Expression, Expression),
    Concat(Expression, Expression),
    Negate(Expression),
    Property(Expression, Expression),
}

/// Operations that are performed by the compiler.
///
/// These are operations that are performed by the compiler, as opposed to the
/// built-in operations that are executed (aka "reduced") during the "apply"
/// phase.
///
/// These ops can't be executed earlier because they are either: chain-specific
/// or rely on data that is only available to the compiler.
#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum CompilerOp {
    BuildScriptAddress(Expression),
    ComputeMinUtxo(Expression),
    ComputeTipSlot,
    ComputeSlotToTime(Expression),
    ComputeTimeToSlot(Expression),
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AssetExpr {
    pub policy: Expression,
    pub asset_name: Expression,
    pub amount: Expression,
}

impl AssetExpr {
    pub fn class_matches(&self, other: &Self) -> bool {
        self.policy.as_bytes() == other.policy.as_bytes()
            && self.asset_name.as_bytes() == other.asset_name.as_bytes()
    }
}

/// An ad-hoc compile directive.
///
/// It's a generic, pass-through structure that the final chain-specific
/// compiler can use to compile custom structures. Tx3 won't attempt to process
/// this IR structure for anything other than trying to apply / reduce its
/// expressions.
#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AdHocDirective {
    pub name: String,
    pub data: HashMap<String, Expression>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum ScriptSource {
    Embedded(Expression),
    UtxoRef {
        r#ref: Expression,
        source: Option<Expression>,
    },
}

impl ScriptSource {
    pub fn new_ref(r#ref: Expression, source: Expression) -> Self {
        Self::UtxoRef {
            r#ref,
            source: Some(source),
        }
    }

    pub fn new_embedded(source: Expression) -> Self {
        Self::Embedded(source)
    }

    pub fn expect_parameter(policy_name: String) -> Self {
        Self::Embedded(
            Param::ExpectValue(
                format!("{}_script", policy_name.to_lowercase()),
                Type::Bytes,
            )
            .into(),
        )
    }

    pub fn expect_ref_input(policy_name: String, r#ref: Expression) -> Self {
        Self::UtxoRef {
            r#ref: r#ref.clone(),
            source: Some(
                Coerce::IntoScript(
                    Param::ExpectInput(
                        format!("{}_script", policy_name.to_lowercase()),
                        InputQuery {
                            address: Expression::None,
                            min_amount: Expression::None,
                            many: false,
                            r#ref,
                            collateral: false,
                        },
                    )
                    .into(),
                )
                .into(),
            ),
        }
    }

    pub fn as_utxo_ref(&self) -> Option<Expression> {
        match self {
            Self::UtxoRef { r#ref, .. } => Some(r#ref.clone()),
            Self::Embedded(Expression::UtxoRefs(x)) => Some(Expression::UtxoRefs(x.clone())),
            _ => None,
        }
    }
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct PolicyExpr {
    pub name: String,
    pub hash: Expression,
    pub script: ScriptSource,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Type {
    Undefined,
    Unit,
    Int,
    Bool,
    Bytes,
    Address,
    Utxo,
    UtxoRef,
    AnyAsset,
    List,
    Map,
    Custom(String),
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Param {
    Set(Expression),
    ExpectValue(String, Type),
    ExpectInput(String, InputQuery),
    ExpectFees,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum Expression {
    None,
    List(Vec<Expression>),
    Map(Vec<(Expression, Expression)>),
    Tuple(Box<(Expression, Expression)>),
    Struct(StructExpr),
    Bytes(Vec<u8>),
    Number(i128),
    Bool(bool),
    String(String),
    Address(Vec<u8>),
    Hash(Vec<u8>),
    UtxoRefs(Vec<UtxoRef>),
    UtxoSet(HashSet<Utxo>),
    Assets(Vec<AssetExpr>),

    EvalParam(Box<Param>),
    EvalBuiltIn(Box<BuiltInOp>),
    EvalCompiler(Box<CompilerOp>),
    EvalCoerce(Box<Coerce>),

    // pass-through
    AdHocDirective(Box<AdHocDirective>),
}

impl Default for Expression {
    fn default() -> Self {
        Self::None
    }
}

impl Expression {
    pub fn is_none(&self) -> bool {
        matches!(self, Self::None)
    }

    pub fn as_option(&self) -> Option<&Self> {
        match self {
            Self::None => None,
            _ => Some(self),
        }
    }

    pub fn into_option(self) -> Option<Self> {
        match self {
            Self::None => None,
            _ => Some(self),
        }
    }

    pub fn as_bytes(&self) -> Option<&[u8]> {
        match self {
            Self::Bytes(bytes) => Some(bytes),
            Self::String(s) => Some(s.as_bytes()),
            Self::Address(x) => Some(x),
            Self::Hash(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_number(&self) -> Option<i128> {
        match self {
            Self::Number(x) => Some(*x),
            _ => None,
        }
    }

    pub fn as_assets(&self) -> Option<&[AssetExpr]> {
        match self {
            Self::Assets(assets) => Some(assets),
            _ => None,
        }
    }

    pub fn as_utxo_refs(&self) -> Option<&[UtxoRef]> {
        match self {
            Self::UtxoRefs(refs) => Some(refs),
            _ => None,
        }
    }
}

impl From<BuiltInOp> for Expression {
    fn from(op: BuiltInOp) -> Self {
        Self::EvalBuiltIn(Box::new(op))
    }
}

impl From<CompilerOp> for Expression {
    fn from(op: CompilerOp) -> Self {
        Self::EvalCompiler(Box::new(op))
    }
}

impl From<Coerce> for Expression {
    fn from(coerce: Coerce) -> Self {
        Self::EvalCoerce(Box::new(coerce))
    }
}

impl From<Param> for Expression {
    fn from(param: Param) -> Self {
        Self::EvalParam(Box::new(param))
    }
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct InputQuery {
    pub address: Expression,
    pub min_amount: Expression,
    pub r#ref: Expression,
    pub many: bool,
    pub collateral: bool,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Input {
    pub name: String,
    pub utxos: Expression,
    pub redeemer: Expression,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Output {
    pub address: Expression,
    pub datum: Expression,
    pub amount: Expression,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Validity {
    pub since: Expression,
    pub until: Expression,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Mint {
    pub amount: Expression,
    pub redeemer: Expression,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Collateral {
    pub utxos: Expression,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Metadata {
    pub key: Expression,
    pub value: Expression,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Signers {
    pub signers: Vec<Expression>,
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone)]
pub struct Tx {
    pub fees: Expression,
    pub references: Vec<Expression>,
    pub inputs: Vec<Input>,
    pub outputs: Vec<Output>,
    pub validity: Option<Validity>,
    pub mints: Vec<Mint>,
    pub burns: Vec<Mint>,
    pub adhoc: Vec<AdHocDirective>,
    pub collateral: Vec<Collateral>,
    pub signers: Option<Signers>,
    pub metadata: Vec<Metadata>,
}

pub trait Visitor {
    fn reduce(&mut self, op: Expression) -> Result<Expression, crate::applying::Error>;
}

pub trait Node: Sized {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error>;
}

impl<T: Node> Node for Option<T> {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        self.map(|x| x.apply(visitor)).transpose()
    }
}

impl<T: Node> Node for Box<T> {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = (*self).apply(visitor)?;
        Ok(Box::new(visited))
    }
}

impl Node for (Expression, Expression) {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let (a, b) = self;
        Ok((a.apply(visitor)?, b.apply(visitor)?))
    }
}

impl<T: Node> Node for Vec<T> {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        self.into_iter().map(|x| x.apply(visitor)).collect()
    }
}

impl Node for StructExpr {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            constructor: self.constructor,
            fields: self.fields.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for AssetExpr {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            policy: self.policy.apply(visitor)?,
            asset_name: self.asset_name.apply(visitor)?,
            amount: self.amount.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for InputQuery {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            address: self.address.apply(visitor)?,
            min_amount: self.min_amount.apply(visitor)?,
            r#ref: self.r#ref.apply(visitor)?,
            ..self
        };

        Ok(visited)
    }
}

impl Node for Param {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = match self {
            Param::Set(x) => Param::Set(x.apply(visitor)?),
            Param::ExpectValue(name, ty) => Param::ExpectValue(name, ty),
            Param::ExpectInput(name, query) => Param::ExpectInput(name, query.apply(visitor)?),
            Param::ExpectFees => Param::ExpectFees,
        };

        Ok(visited)
    }
}

impl Node for BuiltInOp {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = match self {
            BuiltInOp::NoOp(x) => BuiltInOp::NoOp(x.apply(visitor)?),
            BuiltInOp::Add(a, b) => BuiltInOp::Add(a.apply(visitor)?, b.apply(visitor)?),
            BuiltInOp::Sub(a, b) => BuiltInOp::Sub(a.apply(visitor)?, b.apply(visitor)?),
            BuiltInOp::Concat(a, b) => BuiltInOp::Concat(a.apply(visitor)?, b.apply(visitor)?),
            BuiltInOp::Negate(x) => BuiltInOp::Negate(x.apply(visitor)?),
            BuiltInOp::Property(x, i) => BuiltInOp::Property(x.apply(visitor)?, i),
        };

        Ok(visited)
    }
}

impl Node for CompilerOp {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = match self {
            CompilerOp::BuildScriptAddress(x) => CompilerOp::BuildScriptAddress(x.apply(visitor)?),
            CompilerOp::ComputeMinUtxo(x) => CompilerOp::ComputeMinUtxo(x.apply(visitor)?),
            CompilerOp::ComputeTipSlot => CompilerOp::ComputeTipSlot,
            CompilerOp::ComputeSlotToTime(x) => CompilerOp::ComputeSlotToTime(x.apply(visitor)?),
            CompilerOp::ComputeTimeToSlot(x) => CompilerOp::ComputeTimeToSlot(x.apply(visitor)?),
        };

        Ok(visited)
    }
}

impl Node for Coerce {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = match self {
            Coerce::NoOp(x) => Coerce::NoOp(x.apply(visitor)?),
            Coerce::IntoAssets(x) => Coerce::IntoAssets(x.apply(visitor)?),
            Coerce::IntoDatum(x) => Coerce::IntoDatum(x.apply(visitor)?),
            Coerce::IntoScript(x) => Coerce::IntoScript(x.apply(visitor)?),
        };

        Ok(visited)
    }
}

impl Node for Expression {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        // first we visit the nested expressions
        let visited = match self {
            Expression::List(x) => Expression::List(x.apply(visitor)?),
            Expression::Map(x) => Expression::Map(x.apply(visitor)?),
            Expression::Tuple(x) => Expression::Tuple(x.apply(visitor)?),
            Expression::Struct(x) => Expression::Struct(x.apply(visitor)?),
            Expression::Assets(x) => Expression::Assets(x.apply(visitor)?),
            Expression::EvalParam(x) => Expression::EvalParam(x.apply(visitor)?),
            Expression::AdHocDirective(x) => Expression::AdHocDirective(x.apply(visitor)?),
            Expression::EvalBuiltIn(x) => Expression::EvalBuiltIn(x.apply(visitor)?),
            Expression::EvalCompiler(x) => Expression::EvalCompiler(x.apply(visitor)?),
            Expression::EvalCoerce(x) => Expression::EvalCoerce(x.apply(visitor)?),

            // leaf expressions don't need to be visited
            Expression::Bytes(x) => Expression::Bytes(x),
            Expression::None => Expression::None,
            Expression::Number(x) => Expression::Number(x),
            Expression::Bool(x) => Expression::Bool(x),
            Expression::String(x) => Expression::String(x),
            Expression::Address(x) => Expression::Address(x),
            Expression::Hash(x) => Expression::Hash(x),
            Expression::UtxoRefs(x) => Expression::UtxoRefs(x),
            Expression::UtxoSet(x) => Expression::UtxoSet(x),
        };

        // then we reduce the visited expression
        visitor.reduce(visited)
    }
}

impl Node for Input {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            utxos: self.utxos.apply(visitor)?,
            redeemer: self.redeemer.apply(visitor)?,
            ..self
        };

        Ok(visited)
    }
}

impl Node for Output {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            address: self.address.apply(visitor)?,
            datum: self.datum.apply(visitor)?,
            amount: self.amount.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for Validity {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            since: self.since.apply(visitor)?,
            until: self.until.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for Mint {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            amount: self.amount.apply(visitor)?,
            redeemer: self.redeemer.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for Collateral {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            utxos: self.utxos.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for Metadata {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            key: self.key.apply(visitor)?,
            value: self.value.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for Signers {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            signers: self.signers.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for HashMap<String, Expression> {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited: Vec<_> = self
            .into_iter()
            .map(|(k, v)| visitor.reduce(v).map(|v| (k, v)))
            .collect::<Result<_, _>>()?;

        Ok(visited.into_iter().collect())
    }
}

impl Node for AdHocDirective {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            name: self.name,
            data: self.data.apply(visitor)?,
        };

        Ok(visited)
    }
}

impl Node for Tx {
    fn apply<V: Visitor>(self, visitor: &mut V) -> Result<Self, crate::applying::Error> {
        let visited = Self {
            fees: self.fees.apply(visitor)?,
            references: self.references.apply(visitor)?,
            inputs: self.inputs.apply(visitor)?,
            outputs: self.outputs.apply(visitor)?,
            validity: self.validity.apply(visitor)?,
            mints: self.mints.apply(visitor)?,
            burns: self.burns.apply(visitor)?,
            adhoc: self.adhoc.apply(visitor)?,
            collateral: self.collateral.apply(visitor)?,
            signers: self.signers.apply(visitor)?,
            metadata: self.metadata.apply(visitor)?,
        };

        Ok(visited)
    }
}
