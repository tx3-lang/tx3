use std::rc::Rc;

use crate::{
    analyzing::{Analyzable, AnalyzeReport},
    ast::{Scope, Type},
};

use super::ast::*;

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

impl Analyzable for PlutusWitnessBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
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

impl Analyzable for NativeWitnessBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
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

impl Analyzable for CardanoPublishBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            CardanoPublishBlockField::To(x) => x.analyze(parent),
            CardanoPublishBlockField::Amount(x) => x.analyze(parent),
            CardanoPublishBlockField::Datum(x) => x.analyze(parent),
            CardanoPublishBlockField::Version(x) => x.analyze(parent),
            CardanoPublishBlockField::Script(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            CardanoPublishBlockField::To(x) => x.is_resolved(),
            CardanoPublishBlockField::Amount(x) => x.is_resolved(),
            CardanoPublishBlockField::Datum(x) => x.is_resolved(),
            CardanoPublishBlockField::Version(x) => x.is_resolved(),
            CardanoPublishBlockField::Script(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for CardanoPublishBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
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
            CardanoBlock::Publish(x) => x.analyze(parent),
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
            CardanoBlock::Publish(x) => x.is_resolved(),
        }
    }
}

#[cfg(test)]
mod tests {

    use crate::analyzing::analyze;

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

    #[test]
    fn test_publish_type_ok() {
        let mut ast = crate::parsing::parse_string(
            r#"
            party Receiver;

            tx test(quantity: Int) {
                cardano::publish {
                    to: Receiver,
                    amount: Ada(quantity),
                    version: 3,
                    script: 0xABCDEF,
                }
            }
            "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_publish_type_with_name_ok() {
        let mut ast = crate::parsing::parse_string(
            r#"
            party Receiver;

            tx test(quantity: Int) {
                cardano::publish deploy {
                    to: Receiver,
                    amount: Ada(quantity),
                    version: 3,
                    script: 0xABCDEF,
                }
            }
            "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(result.errors.is_empty());
    }
}
