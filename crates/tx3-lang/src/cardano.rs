use std::{collections::HashMap, rc::Rc};

use pest::iterators::Pair;
use serde::{Deserialize, Serialize};

use crate::{
    analyzing::{Analyzable, AnalyzeReport},
    ast::{DataExpr, Scope, Span, WithdrawBlock},
    ir,
    lowering::IntoLower,
    parsing::{AstNode, Error, Rule},
};

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

impl IntoLower for WithdrawBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(&self) -> Result<Self::Output, crate::lowering::Error> {
        let fields = self
            .fields
            .iter()
            .map(|withdraw_field| withdraw_field.into_lower())
            .collect::<Result<Vec<_>, _>>()?;

        let mut data = HashMap::new();

        for (index, withdraw) in fields.into_iter().enumerate() {
            data.insert(format!("withdraw_{}", index), withdraw.key);
            data.insert(format!("amount_{}", index), withdraw.value);
        }

        Ok(ir::AdHocDirective {
            name: "withdraw".to_string(),
            data,
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
        // TODO: Implement StakeDelegationCertificate lowering
        todo!("StakeDelegationCertificate lowering not implemented")
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CardanoBlock {
    VoteDelegationCertificate(VoteDelegationCertificate),
    StakeDelegationCertificate(StakeDelegationCertificate),
    Withdraw(WithdrawBlock),
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
            Rule::withdraw_block => Ok(CardanoBlock::Withdraw(WithdrawBlock::parse(item)?)),
            x => unreachable!("Unexpected rule in cardano_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.span(),
            CardanoBlock::StakeDelegationCertificate(x) => x.span(),
            CardanoBlock::Withdraw(x) => x.span(),
        }
    }
}

impl Analyzable for CardanoBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.analyze(parent),
            CardanoBlock::StakeDelegationCertificate(x) => x.analyze(parent),
            CardanoBlock::Withdraw(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.is_resolved(),
            CardanoBlock::StakeDelegationCertificate(x) => x.is_resolved(),
            CardanoBlock::Withdraw(x) => x.is_resolved(),
        }
    }
}

impl IntoLower for CardanoBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(&self) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            CardanoBlock::VoteDelegationCertificate(x) => x.into_lower(),
            CardanoBlock::StakeDelegationCertificate(x) => x.into_lower(),
            CardanoBlock::Withdraw(x) => x.into_lower(),
        }
    }
}
