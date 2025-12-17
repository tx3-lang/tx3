use std::{collections::HashMap, fs};

use pest::iterators::Pair;

use crate::{
    ast::{DataExpr, Identifier, RecordField, Span, Symbol, Type, TypeDef, VariantCase},
    parsing::{AstNode, Error, Rule},
};

use super::{ast::*, blueprint};

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

impl AstNode for CardanoPublishBlockField {
    const RULE: Rule = Rule::cardano_publish_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::cardano_publish_block_to => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoPublishBlockField::To(DataExpr::parse(pair)?.into()))
            }
            Rule::cardano_publish_block_amount => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoPublishBlockField::Amount(
                    DataExpr::parse(pair)?.into(),
                ))
            }
            Rule::cardano_publish_block_datum => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoPublishBlockField::Datum(
                    DataExpr::parse(pair)?.into(),
                ))
            }
            Rule::cardano_publish_block_version => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoPublishBlockField::Version(
                    DataExpr::parse(pair)?.into(),
                ))
            }
            Rule::cardano_publish_block_script => {
                let pair = pair.into_inner().next().unwrap();
                Ok(CardanoPublishBlockField::Script(
                    DataExpr::parse(pair)?.into(),
                ))
            }
            x => unreachable!("Unexpected rule in cardano_publish_block_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::To(x) => x.span(),
            Self::Amount(x) => x.span(),
            Self::Datum(x) => x.span(),
            Self::Version(x) => x.span(),
            Self::Script(x) => x.span(),
        }
    }
}

impl AstNode for CardanoPublishBlock {
    const RULE: Rule = Rule::cardano_publish_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();
        let has_name = inner
            .peek()
            .map(|x| x.as_rule() == Rule::identifier)
            .unwrap_or_default();

        let name = if has_name {
            Some(Identifier::parse(inner.next().unwrap())?)
        } else {
            None
        };

        let fields = inner
            .map(|x| CardanoPublishBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(CardanoPublishBlock { name, fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
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
            Rule::cardano_publish_block => {
                Ok(CardanoBlock::Publish(CardanoPublishBlock::parse(item)?))
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
            CardanoBlock::Publish(x) => x.span(),
        }
    }
}

pub fn load_externals(
    path: &str,
) -> Result<HashMap<String, crate::ast::Symbol>, crate::parsing::Error> {
    let json = fs::read_to_string(path).map_err(|e| crate::parsing::Error {
        message: format!("Failed to read import file: {}", e),
        src: path.to_string(),
        span: crate::ast::Span::DUMMY,
    })?;
    let bp =
        serde_json::from_str::<cip_57::Blueprint>(&json).map_err(|e| crate::parsing::Error {
            message: format!("Failed to parse blueprint JSON: {}", e),
            src: "".to_string(),
            span: crate::ast::Span::DUMMY,
        })?;

    let is_aiken = bp
        .preamble
        .compiler
        .as_ref()
        .is_some_and(|c| c.name.to_lowercase() == "aiken");

    let name_mapping: HashMap<String, String> = if is_aiken {
        let keys: Vec<&str> = bp
            .definitions
            .as_ref()
            .map(|d| d.inner.keys().map(|s| s.as_str()).collect())
            .unwrap_or_default();
        blueprint::build_aiken_name_mapping(&keys)
    } else {
        bp.definitions
            .as_ref()
            .map(|d| {
                d.inner
                    .keys()
                    .map(|k| (k.clone(), blueprint::generic_sanitizer(k)))
                    .collect()
            })
            .unwrap_or_default()
    };

    let ref_to_type = |r: &str| -> Type {
        let key = r.strip_prefix("#/definitions/").unwrap_or(r);
        let decoded_key = key.replace("~1", "/");
        let sanitized = name_mapping.get(&decoded_key).cloned().unwrap_or_else(|| {
            if is_aiken {
                blueprint::aiken_prettify_name(key)
            } else {
                blueprint::generic_sanitizer(key)
            }
        });
        Type::Custom(Identifier::new(&sanitized))
    };

    let mut symbols = HashMap::new();
    for (key, def) in bp
        .definitions
        .as_ref()
        .map(|d| d.inner.iter())
        .into_iter()
        .flatten()
    {
        let name = name_mapping.get(key).cloned().unwrap_or_else(|| {
            if is_aiken {
                blueprint::aiken_prettify_name(key)
            } else {
                blueprint::generic_sanitizer(key)
            }
        });

        let new = match def.data_type {
            Some(cip_57::DataType::Integer) => Some(Symbol::AliasDef(Box::new(
                crate::ast::AliasDef::new(&name, Type::Int),
            ))),
            Some(cip_57::DataType::Bytes) => Some(Symbol::AliasDef(Box::new(
                crate::ast::AliasDef::new(&name, Type::Bytes),
            ))),
            Some(cip_57::DataType::Map) => {
                let key_ty = def
                    .keys
                    .as_ref()
                    .and_then(|r| r.reference.as_ref())
                    .map(|r| ref_to_type(r));
                let value = def
                    .values
                    .as_ref()
                    .and_then(|r| r.reference.as_ref())
                    .map(|r| ref_to_type(r));

                if let (Some(key_ty), Some(value)) = (key_ty, value) {
                    Some(Symbol::AliasDef(Box::new(crate::ast::AliasDef::new(
                        &name,
                        Type::Map(Box::new(key_ty), Box::new(value)),
                    ))))
                } else {
                    None
                }
            }
            Some(cip_57::DataType::List) => match &def.items {
                Some(cip_57::ReferencesArray::Single(r)) => {
                    let name = name.clone();
                    r.reference.as_ref().map(|r| {
                        Symbol::AliasDef(Box::new(crate::ast::AliasDef::new(
                            &name,
                            Type::List(Box::new(ref_to_type(r))),
                        )))
                    })
                }
                _ => None,
            },
            Some(cip_57::DataType::Constructor) => {
                let mut cases = vec![];
                if let Some(any_of) = &def.any_of {
                    let single = any_of.len() == 1;
                    for schema in any_of {
                        let original_case_name = schema
                            .title
                            .clone()
                            .unwrap_or_else(|| format!("Constructor{}", schema.index));
                        let case_name = if single && original_case_name == name {
                            "Default".to_string()
                        } else {
                            original_case_name
                        };
                        let mut fields = vec![];
                        for (i, field) in schema.fields.iter().enumerate() {
                            let field_name = field
                                .title
                                .clone()
                                .unwrap_or_else(|| format!("field_{}", i));
                            let field_ty = ref_to_type(&field.reference);
                            fields.push(RecordField::new(&field_name, field_ty));
                        }
                        cases.push(VariantCase {
                            name: Identifier::new(case_name),
                            fields,
                            span: Span::default(),
                        });
                    }
                }
                Some(Symbol::TypeDef(Box::new(TypeDef {
                    name: Identifier::new(&name),
                    cases,
                    span: Span::default(),
                })))
            }
            None if def.any_of.is_some() => {
                let mut cases = vec![];
                if let Some(any_of) = &def.any_of {
                    let single = any_of.len() == 1;
                    for schema in any_of {
                        let original_case_name = schema
                            .title
                            .clone()
                            .unwrap_or_else(|| format!("Constructor{}", schema.index));
                        let case_name = if single && original_case_name == name {
                            "Default".to_string()
                        } else {
                            original_case_name
                        };
                        let mut fields = vec![];
                        for (i, field) in schema.fields.iter().enumerate() {
                            let field_name = field
                                .title
                                .clone()
                                .unwrap_or_else(|| format!("field_{}", i));
                            let field_ty = ref_to_type(&field.reference);
                            fields.push(RecordField::new(&field_name, field_ty));
                        }
                        cases.push(VariantCase {
                            name: Identifier::new(case_name),
                            fields,
                            span: Span::default(),
                        });
                    }
                }
                Some(Symbol::TypeDef(Box::new(TypeDef {
                    name: Identifier::new(&name),
                    cases,
                    span: Span::default(),
                })))
            }
            None => None,
        };
        if let Some(symbol) = new {
            symbols.insert(name, symbol);
        }
    }
    Ok(symbols)
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::ast::*;
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

    input_to_ast_check!(
        CardanoPublishBlock,
        "basic",
        "publish {
            to: Receiver,
            amount: Ada(quantity),
            version: 3,
            script: 0xABCDEF,
        }",
        CardanoPublishBlock {
            name: None,
            fields: vec![
                CardanoPublishBlockField::To(Box::new(DataExpr::Identifier(Identifier::new(
                    "Receiver"
                )))),
                CardanoPublishBlockField::Amount(Box::new(DataExpr::StaticAssetConstructor(
                    StaticAssetConstructor {
                        r#type: Identifier::new("Ada"),
                        amount: Box::new(DataExpr::Identifier(Identifier::new("quantity"))),
                        span: Span::DUMMY,
                    }
                ))),
                CardanoPublishBlockField::Version(Box::new(DataExpr::Number(3))),
                CardanoPublishBlockField::Script(Box::new(DataExpr::HexString(
                    HexStringLiteral::new("ABCDEF".to_string())
                ))),
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        CardanoPublishBlock,
        "basic_with_name",
        "publish test_publish {
            to: Receiver,
            amount: Ada(quantity),
            version: 3,
            script: 0xABCDEF,
        }",
        CardanoPublishBlock {
            name: Some(Identifier::new("test_publish")),
            fields: vec![
                CardanoPublishBlockField::To(Box::new(DataExpr::Identifier(Identifier::new(
                    "Receiver"
                )))),
                CardanoPublishBlockField::Amount(Box::new(DataExpr::StaticAssetConstructor(
                    StaticAssetConstructor {
                        r#type: Identifier::new("Ada"),
                        amount: Box::new(DataExpr::Identifier(Identifier::new("quantity"))),
                        span: Span::DUMMY,
                    }
                ))),
                CardanoPublishBlockField::Version(Box::new(DataExpr::Number(3))),
                CardanoPublishBlockField::Script(Box::new(DataExpr::HexString(
                    HexStringLiteral::new("ABCDEF".to_string())
                ))),
            ],
            span: Span::DUMMY,
        }
    );

    #[test]
    fn test_single_constructor_alias() {
        let json = r##"{
            "preamble": {
                "title": "Test",
                "description": "Test",
                "version": "1.0.0",
                "plutusVersion": "v2",
                "compiler": {
                    "name": "Aiken",
                    "version": "1.0.0"
                },
                "license": "Apache-2.0"
            },
            "validators": [],
            "definitions": {
                "ticketer/types/TicketerDatum": {
                    "title": "TicketerDatum",
                    "anyOf": [
                        {
                            "title": "TicketerDatum",
                            "dataType": "constructor",
                            "index": 0,
                            "fields": [
                                {
                                    "title": "ticket_counter",
                                    "$ref": "#/definitions/Int"
                                }
                            ]
                        }
                    ]
                },
                "ticketer/types/TicketerRedeemer": {
                    "title": "TicketerRedeemer",
                    "anyOf": [
                        {
                            "title": "BuyTicket",
                            "dataType": "constructor",
                            "index": 0,
                            "fields": []
                        }
                    ]
                },
                "Int": {
                    "dataType": "integer"
                }
            }
        }"##;

        use std::io::Write;

        let mut path = std::env::temp_dir();
        path.push(format!("tx3_test_{}.json", std::process::id()));

        {
            let mut file = std::fs::File::create(&path).unwrap();
            file.write_all(json.as_bytes()).unwrap();
        }

        let symbols = load_externals(path.to_str().unwrap()).unwrap();

        // Cleanup
        let _ = std::fs::remove_file(&path);

        let datum = symbols.get("TicketerDatum").unwrap();

        if let Symbol::TypeDef(def) = datum {
            assert_eq!(def.cases.len(), 1);
            // Matches type name -> Default
            assert_eq!(def.cases[0].name.value, "Default");
        } else {
            panic!("Expected TypeDef for TicketerDatum, got {:?}", datum);
        }

        let redeemer = symbols.get("TicketerRedeemer").unwrap();
        if let Symbol::TypeDef(def) = redeemer {
            assert_eq!(def.cases.len(), 1);
            // Does NOT match type name -> Keep original name
            assert_eq!(def.cases[0].name.value, "BuyTicket");
        } else {
            panic!("Expected TypeDef for TicketerRedeemer, got {:?}", redeemer);
        }
    }
    #[test]
    fn test_load_externals_generic_compiler() {
        let json = r##"{
            "preamble": {
                "title": "Test",
                "description": "Test",
                "version": "1.0.0",
                "plutusVersion": "v1",
                "compiler": {
                    "name": "Test",
                    "version": "0.0.0"
                },
                "license": "MIT"
            },
            "validators": [],
            "definitions": {
                "Option<Int>": {
                    "dataType": "integer"
                }
            }
        }"##;

        use std::io::Write;

        let mut path = std::env::temp_dir();
        path.push(format!("tx3_test_generic_{}.json", std::process::id()));

        {
            let mut file = std::fs::File::create(&path).unwrap();
            file.write_all(json.as_bytes()).unwrap();
        }

        let symbols = load_externals(path.to_str().unwrap()).unwrap();

        let _ = std::fs::remove_file(&path);

        assert!(symbols.contains_key("Option_Int"));
        assert!(!symbols.contains_key("OptionInt"));
    }
}
