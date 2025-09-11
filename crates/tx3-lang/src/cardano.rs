use std::{collections::HashMap, rc::Rc};

use pest::iterators::Pair;
use serde::{Deserialize, Serialize};

use crate::{
    analyzing::{Analyzable, AnalyzeReport},
    ast::{DataExpr, Scope, Span, Type},
    ir,
    lowering::IntoLower,
    parsing::{AstNode, Error, Rule},
};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum WithdrawalField {
    From(Box<DataExpr>),
    Amount(Box<DataExpr>),
    Redeemer(Box<DataExpr>),
}

impl WithdrawalField {
    fn key(&self) -> &str {
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

impl AstNode for WithdrawalField {
    const RULE: Rule = Rule::cardano_withdrawal_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::cardano_withdrawal_from => {
                let pair = pair.into_inner().next().unwrap();
                Ok(WithdrawalField::From(DataExpr::parse(pair)?.into()))
            }
            Rule::cardano_withdrawal_amount => {
                let pair = pair.into_inner().next().unwrap();
                Ok(WithdrawalField::Amount(DataExpr::parse(pair)?.into()))
            }
            Rule::cardano_withdrawal_redeemer => {
                let pair = pair.into_inner().next().unwrap();
                Ok(WithdrawalField::Redeemer(DataExpr::parse(pair)?.into()))
            }
            x => unreachable!("Unexpected rule in cardano_withdrawal_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::From(x) => x.span(),
            Self::Amount(x) => x.span(),
            Self::Redeemer(x) => x.span(),
        }
    }
}

impl AstNode for WithdrawalBlock {
    const RULE: Rule = Rule::cardano_withdrawal_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| WithdrawalField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(WithdrawalBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Analyzable for WithdrawalField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            WithdrawalField::From(x) => x.analyze(parent),
            WithdrawalField::Amount(x) => {
                let amount = x.analyze(parent.clone());
                let amount_type = AnalyzeReport::expect_data_expr_type(x, &Type::Int);
                amount + amount_type
            }
            WithdrawalField::Redeemer(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            WithdrawalField::From(x) => x.is_resolved(),
            WithdrawalField::Amount(x) => x.is_resolved(),
            WithdrawalField::Redeemer(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for WithdrawalBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl IntoLower for WithdrawalField {
    type Output = ir::Expression;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            WithdrawalField::From(x) => x.into_lower(ctx),
            WithdrawalField::Amount(x) => x.into_lower(ctx),
            WithdrawalField::Redeemer(x) => x.into_lower(ctx),
        }
    }
}

impl IntoLower for WithdrawalBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        let credential = self
            .find("from")
            .ok_or_else(|| {
                crate::lowering::Error::MissingRequiredField("from".to_string(), "WithdrawalBlock")
            })?
            .into_lower(ctx)?;

        let amount = self
            .find("amount")
            .ok_or_else(|| {
                crate::lowering::Error::MissingRequiredField(
                    "amount".to_string(),
                    "WithdrawalBlock",
                )
            })?
            .into_lower(ctx)?;

        let redeemer = self
            .find("redeemer")
            .map(|r| r.into_lower(ctx))
            .transpose()?
            .unwrap_or(ir::Expression::None);

        Ok(ir::AdHocDirective {
            name: "withdrawal".to_string(),
            data: std::collections::HashMap::from([
                ("credential".to_string(), credential),
                ("amount".to_string(), amount),
                ("redeemer".to_string(), redeemer),
            ]),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VoteDelegationCertificate {
    pub drep: DataExpr,
    pub stake: DataExpr,
    pub span: Span,
}

impl AstNode for VoteDelegationCertificate {
    const RULE: Rule = Rule::cardano_vote_delegation_certificate;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        Ok(VoteDelegationCertificate {
            drep: DataExpr::parse(inner.next().unwrap())?,
            stake: DataExpr::parse(inner.next().unwrap())?,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Analyzable for VoteDelegationCertificate {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let drep = self.drep.analyze(parent.clone());
        let stake = self.stake.analyze(parent.clone());

        drep + stake
    }

    fn is_resolved(&self) -> bool {
        self.drep.is_resolved() && self.stake.is_resolved()
    }
}

impl IntoLower for VoteDelegationCertificate {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        Ok(ir::AdHocDirective {
            name: "vote_delegation_certificate".to_string(),
            data: HashMap::from([
                ("drep".to_string(), self.drep.into_lower(ctx)?),
                ("stake".to_string(), self.stake.into_lower(ctx)?),
            ]),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StakeDelegationCertificate {
    pub pool: DataExpr,
    pub stake: DataExpr,
    pub span: Span,
}

impl AstNode for StakeDelegationCertificate {
    const RULE: Rule = Rule::cardano_stake_delegation_certificate;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        Ok(StakeDelegationCertificate {
            pool: DataExpr::parse(inner.next().unwrap())?,
            stake: DataExpr::parse(inner.next().unwrap())?,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Analyzable for StakeDelegationCertificate {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let pool = self.pool.analyze(parent.clone());
        let stake = self.stake.analyze(parent.clone());

        pool + stake
    }

    fn is_resolved(&self) -> bool {
        self.pool.is_resolved() && self.stake.is_resolved()
    }
}

impl IntoLower for StakeDelegationCertificate {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        _ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        todo!("StakeDelegationCertificate lowering not implemented")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PlutusWitnessField {
    Version(DataExpr, Span),
    Script(DataExpr, Span),
}

impl IntoLower for PlutusWitnessField {
    type Output = (String, ir::Expression);

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            PlutusWitnessField::Version(x, _) => Ok(("version".to_string(), x.into_lower(ctx)?)),
            PlutusWitnessField::Script(x, _) => Ok(("script".to_string(), x.into_lower(ctx)?)),
        }
    }
}

impl AstNode for PlutusWitnessField {
    const RULE: Rule = Rule::cardano_plutus_witness_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();

        match pair.as_rule() {
            Rule::cardano_plutus_witness_version => {
                Ok(PlutusWitnessField::Version(DataExpr::parse(pair)?, span))
            }
            Rule::cardano_plutus_witness_script => {
                Ok(PlutusWitnessField::Script(DataExpr::parse(pair)?, span))
            }
            x => unreachable!("Unexpected rule in cardano_plutus_witness_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            PlutusWitnessField::Version(_, span) => span,
            PlutusWitnessField::Script(_, span) => span,
        }
    }
}

impl Analyzable for PlutusWitnessField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            PlutusWitnessField::Version(x, _) => x.analyze(parent),
            PlutusWitnessField::Script(x, _) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            PlutusWitnessField::Version(x, _) => x.is_resolved(),
            PlutusWitnessField::Script(x, _) => x.is_resolved(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PlutusWitnessBlock {
    pub fields: Vec<PlutusWitnessField>,
    pub span: Span,
}

impl AstNode for PlutusWitnessBlock {
    const RULE: Rule = Rule::cardano_plutus_witness_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| PlutusWitnessField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(PlutusWitnessBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Analyzable for PlutusWitnessBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl IntoLower for PlutusWitnessBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        let data = self
            .fields
            .iter()
            .map(|x| x.into_lower(ctx))
            .collect::<Result<_, _>>()?;

        Ok(ir::AdHocDirective {
            name: "plutus_witness".to_string(),
            data,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum NativeWitnessField {
    Script(DataExpr, Span),
}

impl IntoLower for NativeWitnessField {
    type Output = (String, ir::Expression);

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            NativeWitnessField::Script(x, _) => Ok(("script".to_string(), x.into_lower(ctx)?)),
        }
    }
}

impl AstNode for NativeWitnessField {
    const RULE: Rule = Rule::cardano_native_witness_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();

        match pair.as_rule() {
            Rule::cardano_native_witness_script => {
                Ok(NativeWitnessField::Script(DataExpr::parse(pair)?, span))
            }
            x => unreachable!("Unexpected rule in cardano_native_witness_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            NativeWitnessField::Script(_, span) => span,
        }
    }
}

impl Analyzable for NativeWitnessField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            NativeWitnessField::Script(x, _) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            NativeWitnessField::Script(x, _) => x.is_resolved(),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NativeWitnessBlock {
    pub fields: Vec<NativeWitnessField>,
    pub span: Span,
}

impl AstNode for NativeWitnessBlock {
    const RULE: Rule = Rule::cardano_native_witness_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| NativeWitnessField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(NativeWitnessBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Analyzable for NativeWitnessBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl IntoLower for NativeWitnessBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        let data = self
            .fields
            .iter()
            .map(|x| x.into_lower(ctx))
            .collect::<Result<_, _>>()?;

        Ok(ir::AdHocDirective {
            name: "native_witness".to_string(),
            data,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TreasuryDonationBlock {
    pub coin: DataExpr,
    pub span: Span,
}

impl AstNode for TreasuryDonationBlock {
    const RULE: Rule = Rule::cardano_treasury_donation_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();

        let mut inner = pair.into_inner();
        let coin = DataExpr::parse(inner.next().unwrap())?;

        Ok(TreasuryDonationBlock { coin, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Analyzable for TreasuryDonationBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let coin = self.coin.analyze(parent);
        let coin_type = AnalyzeReport::expect_data_expr_type(&self.coin, &Type::Int);

        coin + coin_type
    }

    fn is_resolved(&self) -> bool {
        self.coin.is_resolved()
    }
}

impl IntoLower for TreasuryDonationBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        let coin = self.coin.into_lower(ctx)?;

        Ok(ir::AdHocDirective {
            name: "treasury_donation".to_string(),
            data: std::collections::HashMap::from([("coin".to_string(), coin)]),
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CardanoOutputBlockField {
    To(Box<DataExpr>),
    Amount(Box<DataExpr>),
    Datum(Box<DataExpr>),
    ReferenceScript(PlutusWitnessBlock),
}

impl CardanoOutputBlockField {
    fn key(&self) -> &str {
        match self {
            CardanoOutputBlockField::To(_) => "to",
            CardanoOutputBlockField::Amount(_) => "amount",
            CardanoOutputBlockField::Datum(_) => "datum",
            CardanoOutputBlockField::ReferenceScript(_) => "ref_script",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CardanoOutputBlock {
    pub fields: Vec<CardanoOutputBlockField>,
    pub span: Span,
}

impl CardanoOutputBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&CardanoOutputBlockField> {
        self.fields.iter().find(|x| x.key() == key)
    }
}

impl AstNode for CardanoOutputBlockField {
    const RULE: Rule = Rule::cardano_output_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::cardano_output_block_to => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoOutputBlockField::To(DataExpr::parse(pair)?.into()))
            }
            Rule::cardano_output_block_amount => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoOutputBlockField::Amount(
                    DataExpr::parse(pair)?.into(),
                ))
            }
            Rule::cardano_output_block_datum => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoOutputBlockField::Datum(
                    DataExpr::parse(pair)?.into(),
                ))
            }
            Rule::cardano_output_block_reference_script => {
                let span = pair.as_span().into();
                let inner = pair.into_inner();
                let fields = inner
                    .map(|x| PlutusWitnessField::parse(x))
                    .collect::<Result<Vec<_>, _>>()?;
                let witness_block = PlutusWitnessBlock { fields, span };
                Ok(CardanoOutputBlockField::ReferenceScript(witness_block))
            }
            x => unreachable!("Unexpected rule in cardano_output_block_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::To(x) => x.span(),
            Self::Amount(x) => x.span(),
            Self::Datum(x) => x.span(),
            Self::ReferenceScript(x) => x.span(),
        }
    }
}

impl AstNode for CardanoOutputBlock {
    const RULE: Rule = Rule::cardano_output_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| CardanoOutputBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(CardanoOutputBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl Analyzable for CardanoOutputBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            CardanoOutputBlockField::To(x) => x.analyze(parent),
            CardanoOutputBlockField::Amount(x) => x.analyze(parent),
            CardanoOutputBlockField::Datum(x) => x.analyze(parent),
            CardanoOutputBlockField::ReferenceScript(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            CardanoOutputBlockField::To(x) => x.is_resolved(),
            CardanoOutputBlockField::Amount(x) => x.is_resolved(),
            CardanoOutputBlockField::Datum(x) => x.is_resolved(),
            CardanoOutputBlockField::ReferenceScript(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for CardanoOutputBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl IntoLower for CardanoOutputBlockField {
    type Output = (String, ir::Expression);

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            CardanoOutputBlockField::To(x) => Ok(("to".to_string(), x.into_lower(ctx)?)),
            CardanoOutputBlockField::Amount(x) => Ok(("amount".to_string(), x.into_lower(ctx)?)),
            CardanoOutputBlockField::Datum(x) => Ok(("datum".to_string(), x.into_lower(ctx)?)),
            CardanoOutputBlockField::ReferenceScript(x) => {
                let witness_data = x.into_lower(ctx)?;
                Ok((
                    "ref_script".to_string(),
                    ir::Expression::AdHocDirective(Box::new(witness_data)),
                ))
            }
        }
    }
}

impl IntoLower for CardanoOutputBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        let data = self
            .fields
            .iter()
            .map(|x| x.into_lower(ctx))
            .collect::<Result<_, _>>()?;

        Ok(ir::AdHocDirective {
            name: "cardano_output".to_string(),
            data,
        })
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
    Output(CardanoOutputBlock),
}

impl AstNode for CardanoBlock {
    const RULE: Rule = Rule::cardano_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let mut inner = pair.into_inner();
        let item = inner.next().unwrap();

        match item.as_rule() {
            Rule::cardano_vote_delegation_certificate => Ok(
                CardanoBlock::VoteDelegationCertificate(VoteDelegationCertificate::parse(item)?),
            ),
            Rule::cardano_stake_delegation_certificate => Ok(
                CardanoBlock::StakeDelegationCertificate(StakeDelegationCertificate::parse(item)?),
            ),
            Rule::cardano_withdrawal_block => {
                Ok(CardanoBlock::Withdrawal(WithdrawalBlock::parse(item)?))
            }
            Rule::cardano_plutus_witness_block => Ok(CardanoBlock::PlutusWitness(
                PlutusWitnessBlock::parse(item)?,
            )),
            Rule::cardano_native_witness_block => Ok(CardanoBlock::NativeWitness(
                NativeWitnessBlock::parse(item)?,
            )),
            Rule::cardano_treasury_donation_block => Ok(CardanoBlock::TreasuryDonation(
                TreasuryDonationBlock::parse(item)?,
            )),
            Rule::cardano_output_block => {
                Ok(CardanoBlock::Output(CardanoOutputBlock::parse(item)?))
            }
            x => unreachable!("Unexpected rule in cardano_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.span(),
            CardanoBlock::StakeDelegationCertificate(x) => x.span(),
            CardanoBlock::Withdrawal(x) => x.span(),
            CardanoBlock::PlutusWitness(x) => x.span(),
            CardanoBlock::NativeWitness(x) => x.span(),
            CardanoBlock::TreasuryDonation(x) => x.span(),
            CardanoBlock::Output(x) => x.span(),
        }
    }
}

impl Analyzable for CardanoBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.analyze(parent),
            CardanoBlock::StakeDelegationCertificate(x) => x.analyze(parent),
            CardanoBlock::Withdrawal(x) => x.analyze(parent),
            CardanoBlock::PlutusWitness(x) => x.analyze(parent),
            CardanoBlock::NativeWitness(x) => x.analyze(parent),
            CardanoBlock::TreasuryDonation(x) => x.analyze(parent),
            CardanoBlock::Output(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.is_resolved(),
            CardanoBlock::StakeDelegationCertificate(x) => x.is_resolved(),
            CardanoBlock::Withdrawal(x) => x.is_resolved(),
            CardanoBlock::PlutusWitness(x) => x.is_resolved(),
            CardanoBlock::NativeWitness(x) => x.is_resolved(),
            Self::TreasuryDonation(x) => x.is_resolved(),
            CardanoBlock::Output(x) => x.is_resolved(),
        }
    }
}

impl IntoLower for CardanoBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<<CardanoBlock as IntoLower>::Output, crate::lowering::Error> {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.into_lower(ctx),
            CardanoBlock::StakeDelegationCertificate(x) => x.into_lower(ctx),
            CardanoBlock::Withdrawal(x) => x.into_lower(ctx),
            CardanoBlock::PlutusWitness(x) => x.into_lower(ctx),
            CardanoBlock::NativeWitness(x) => x.into_lower(ctx),
            CardanoBlock::TreasuryDonation(x) => x.into_lower(ctx),
            CardanoBlock::Output(x) => x.into_lower(ctx),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{
        analyzing::analyze,
        ast::{self, *},
    };
    use pest::Parser;

    macro_rules! input_to_ast_check {
        ($ast:ty, $name:expr, $input:expr, $expected:expr) => {
            paste::paste! {
                #[test]
                fn [<test_parse_ $ast:snake _ $name>]() {
                    let pairs = crate::parsing::Tx3Grammar::parse(<$ast>::RULE, $input).unwrap();
                    let single_match = pairs.into_iter().next().unwrap();
                    let result = <$ast>::parse(single_match).unwrap();

                    assert_eq!(result, $expected);
                }
            }
        };
    }

    input_to_ast_check!(
        PlutusWitnessBlock,
        "basic",
        "plutus_witness {
            version: 3,
            script: 0xABCDEF,
        }",
        PlutusWitnessBlock {
            fields: vec![
                PlutusWitnessField::Version(DataExpr::Number(3), Span::DUMMY),
                PlutusWitnessField::Script(
                    DataExpr::HexString(HexStringLiteral::new("ABCDEF".to_string())),
                    Span::DUMMY
                )
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        NativeWitnessBlock,
        "basic",
        "native_witness {
            script: 0xABCDEF,
        }",
        NativeWitnessBlock {
            fields: vec![NativeWitnessField::Script(
                DataExpr::HexString(HexStringLiteral::new("ABCDEF".to_string())),
                Span::DUMMY
            )],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        TreasuryDonationBlock,
        "basic",
        "treasury_donation {
            coin: 2020,
        }",
        TreasuryDonationBlock {
            coin: DataExpr::Number(2020),
            span: Span::DUMMY,
        }
    );

    #[test]
    fn test_treasury_donation_type() {
        let mut ast = crate::parsing::parse_string(
            r#"
            tx test(quantity: Int) {
                cardano::treasury_donation {
                  coin: quantity,
                }
            }
            "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_treasury_donation_type_not_ok() {
        let mut ast = crate::parsing::parse_string(
            r#"
            tx test(quantity: Bytes) {
                cardano::treasury_donation {
                  coin: quantity,
                }
            }
            "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(!result.errors.is_empty());
    }
}
