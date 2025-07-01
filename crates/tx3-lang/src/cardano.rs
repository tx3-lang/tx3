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

    fn into_lower(&self) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            WithdrawalField::From(x) => x.into_lower(),
            WithdrawalField::Amount(x) => x.into_lower(),
            WithdrawalField::Redeemer(x) => x.into_lower(),
        }
    }
}

impl IntoLower for WithdrawalBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(&self) -> Result<Self::Output, crate::lowering::Error> {
        let credential = self
            .find("from")
            .ok_or_else(|| {
                crate::lowering::Error::MissingRequiredField("from".to_string(), "WithdrawalBlock")
            })?
            .into_lower()?;

        let amount = self
            .find("amount")
            .ok_or_else(|| {
                crate::lowering::Error::MissingRequiredField(
                    "amount".to_string(),
                    "WithdrawalBlock",
                )
            })?
            .into_lower()?;

        let redeemer = self
            .find("redeemer")
            .map(|r| r.into_lower())
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

    fn into_lower(&self) -> Result<Self::Output, crate::lowering::Error> {
        Ok(ir::AdHocDirective {
            name: "vote_delegation_certificate".to_string(),
            data: HashMap::from([
                ("drep".to_string(), self.drep.into_lower()?),
                ("stake".to_string(), self.stake.into_lower()?),
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

    fn into_lower(&self) -> Result<Self::Output, crate::lowering::Error> {
        todo!("StakeDelegationCertificate lowering not implemented")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CardanoBlock {
    VoteDelegationCertificate(VoteDelegationCertificate),
    StakeDelegationCertificate(StakeDelegationCertificate),
    Withdrawal(WithdrawalBlock),
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
            x => unreachable!("Unexpected rule in cardano_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.span(),
            CardanoBlock::StakeDelegationCertificate(x) => x.span(),
            CardanoBlock::Withdrawal(x) => x.span(),
        }
    }
}

impl Analyzable for CardanoBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.analyze(parent),
            CardanoBlock::StakeDelegationCertificate(x) => x.analyze(parent),
            CardanoBlock::Withdrawal(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.is_resolved(),
            CardanoBlock::StakeDelegationCertificate(x) => x.is_resolved(),
            CardanoBlock::Withdrawal(x) => x.is_resolved(),
        }
    }
}

impl IntoLower for CardanoBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(&self) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.into_lower(),
            CardanoBlock::StakeDelegationCertificate(x) => x.into_lower(),
            CardanoBlock::Withdrawal(x) => x.into_lower(),
        }
    }
}
