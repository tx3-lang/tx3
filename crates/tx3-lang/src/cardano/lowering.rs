use std::collections::HashMap;

use crate::{ir, lowering::IntoLower};

use super::ast::*;

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

impl IntoLower for StakeDelegationCertificate {
    type Output = ir::AdHocDirective;

    fn into_lower(
        &self,
        _ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        todo!("StakeDelegationCertificate lowering not implemented")
    }
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

impl IntoLower for CardanoPublishBlockField {
    type Output = (String, ir::Expression);

    fn into_lower(
        &self,
        ctx: &crate::lowering::Context,
    ) -> Result<Self::Output, crate::lowering::Error> {
        match self {
            CardanoPublishBlockField::To(x) => Ok(("to".to_string(), x.into_lower(ctx)?)),
            CardanoPublishBlockField::Amount(x) => Ok(("amount".to_string(), x.into_lower(ctx)?)),
            CardanoPublishBlockField::Datum(x) => Ok(("datum".to_string(), x.into_lower(ctx)?)),
            CardanoPublishBlockField::Version(x) => Ok(("version".to_string(), x.into_lower(ctx)?)),
            CardanoPublishBlockField::Script(x) => Ok(("script".to_string(), x.into_lower(ctx)?)),
        }
    }
}

impl IntoLower for CardanoPublishBlock {
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
            name: "cardano_publish".to_string(),
            data,
        })
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
            CardanoBlock::Publish(x) => x.into_lower(ctx),
        }
    }
}
