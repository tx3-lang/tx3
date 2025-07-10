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

pub const IR_VERSION: &str = "v1alpha6";

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

pub type PropertyIndex = usize;

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub enum BuiltInOp {
    NoOp(Expression),
    Add(Expression, Expression),
    Sub(Expression, Expression),
    Negate(Expression),
    Property(Expression, PropertyIndex),
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct AssetExpr {
    pub policy: Expression,
    pub asset_name: Expression,
    pub amount: Expression,
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
                            r#ref,
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
    Custom(String),
    Tuple,
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
}

impl From<BuiltInOp> for Expression {
    fn from(op: BuiltInOp) -> Self {
        Self::EvalBuiltIn(Box::new(op))
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
}

#[derive(Encode, Decode, Serialize, Deserialize, Debug, Clone, PartialEq, Eq)]
pub struct Input {
    pub name: String,
    pub query: InputQuery,
    pub refs: HashSet<UtxoRef>,
    pub redeemer: Option<Expression>,
    pub policy: Option<PolicyExpr>,
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
    pub query: InputQuery,
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
    pub adhoc: Vec<AdHocDirective>,
    pub collateral: Vec<Collateral>,
    pub signers: Option<Signers>,
    pub metadata: Vec<Metadata>,
}
