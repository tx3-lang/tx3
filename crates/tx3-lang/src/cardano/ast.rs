use serde::{Deserialize, Serialize};

use crate::ast::{DataExpr, Identifier, Span};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum WithdrawalField {
    From(Box<DataExpr>),
    Amount(Box<DataExpr>),
    Redeemer(Box<DataExpr>),
}

impl WithdrawalField {
    pub(crate) fn key(&self) -> &str {
        match self {
            WithdrawalField::From(_) => "from",
            WithdrawalField::Amount(_) => "amount",
            WithdrawalField::Redeemer(_) => "redeemer",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct WithdrawalBlock {
    pub fields: Vec<WithdrawalField>,
    pub span: Span,
}

impl WithdrawalBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&WithdrawalField> {
        self.fields.iter().find(|x| x.key() == key)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VoteDelegationCertificate {
    pub drep: DataExpr,
    pub stake: DataExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StakeDelegationCertificate {
    pub pool: DataExpr,
    pub stake: DataExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PlutusWitnessField {
    Version(DataExpr, Span),
    Script(DataExpr, Span),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PlutusWitnessBlock {
    pub fields: Vec<PlutusWitnessField>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum NativeWitnessField {
    Script(DataExpr, Span),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NativeWitnessBlock {
    pub fields: Vec<NativeWitnessField>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TreasuryDonationBlock {
    pub coin: DataExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CardanoPublishBlockField {
    To(Box<DataExpr>),
    Amount(Box<DataExpr>),
    Datum(Box<DataExpr>),
    Version(Box<DataExpr>),
    Script(Box<DataExpr>),
}

impl CardanoPublishBlockField {
    pub(crate) fn key(&self) -> &str {
        match self {
            CardanoPublishBlockField::To(_) => "to",
            CardanoPublishBlockField::Amount(_) => "amount",
            CardanoPublishBlockField::Datum(_) => "datum",
            CardanoPublishBlockField::Version(_) => "version",
            CardanoPublishBlockField::Script(_) => "script",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CardanoPublishBlock {
    pub name: Option<Identifier>,
    pub fields: Vec<CardanoPublishBlockField>,
    pub span: Span,
}

impl CardanoPublishBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&CardanoPublishBlockField> {
        self.fields.iter().find(|x| x.key() == key)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CardanoBlock {
    VoteDelegationCertificate(VoteDelegationCertificate),
    StakeDelegationCertificate(StakeDelegationCertificate),
    Withdrawal(WithdrawalBlock),
    PlutusWitness(PlutusWitnessBlock),
    NativeWitness(NativeWitnessBlock),
    TreasuryDonation(TreasuryDonationBlock),
    Publish(CardanoPublishBlock),
}
