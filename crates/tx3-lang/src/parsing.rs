//! Parses the Tx3 language.
//!
//! This module takes a string and parses it into Tx3 AST.

use std::sync::LazyLock;

use miette::SourceOffset;
use pest::{
    iterators::Pair,
    pratt_parser::{Assoc, Op, PrattParser},
    Parser,
};
use pest_derive::Parser;

use crate::ast::*;

#[derive(Parser)]
#[grammar = "tx3.pest"]
pub(crate) struct Tx3Grammar;

#[derive(Debug, thiserror::Error, miette::Diagnostic)]
#[error("Parsing error: {message}")]
#[diagnostic(code(tx3::parsing))]
pub struct Error {
    pub message: String,

    #[source_code]
    pub src: String,

    #[label]
    pub span: Span,
}

impl From<pest::error::Error<Rule>> for Error {
    fn from(error: pest::error::Error<Rule>) -> Self {
        match &error.variant {
            pest::error::ErrorVariant::ParsingError { positives, .. } => Error {
                message: format!("expected {:?}", positives),
                src: error.line().to_string(),
                span: error.location.into(),
            },
            pest::error::ErrorVariant::CustomError { message } => Error {
                message: message.clone(),
                src: error.line().to_string(),
                span: error.location.into(),
            },
        }
    }
}

impl From<pest::error::InputLocation> for Span {
    fn from(value: pest::error::InputLocation) -> Self {
        match value {
            pest::error::InputLocation::Pos(pos) => Self::new(pos, pos),
            pest::error::InputLocation::Span((start, end)) => Self::new(start, end),
        }
    }
}

impl From<pest::Span<'_>> for Span {
    fn from(span: pest::Span<'_>) -> Self {
        Self::new(span.start(), span.end())
    }
}

impl From<Span> for miette::SourceSpan {
    fn from(span: Span) -> Self {
        miette::SourceSpan::new(SourceOffset::from(span.start), span.end - span.start)
    }
}

pub trait AstNode: Sized {
    const RULE: Rule;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error>;

    fn span(&self) -> &Span;
}

impl AstNode for Program {
    const RULE: Rule = Rule::program;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let mut program = Self {
            env: None,
            txs: Vec::new(),
            assets: Vec::new(),
            types: Vec::new(),
            parties: Vec::new(),
            policies: Vec::new(),
            scope: None,
            span,
        };

        for pair in inner {
            match pair.as_rule() {
                Rule::env_def => program.env = Some(EnvDef::parse(pair)?),
                Rule::tx_def => program.txs.push(TxDef::parse(pair)?),
                Rule::asset_def => program.assets.push(AssetDef::parse(pair)?),
                Rule::record_def => program.types.push(TypeDef::parse(pair)?),
                Rule::variant_def => program.types.push(TypeDef::parse(pair)?),
                Rule::party_def => program.parties.push(PartyDef::parse(pair)?),
                Rule::policy_def => program.policies.push(PolicyDef::parse(pair)?),
                Rule::EOI => break,
                x => unreachable!("Unexpected rule in program: {:?}", x),
            }
        }

        Ok(program)
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for EnvField {
    const RULE: Rule = Rule::env_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();
        let identifier = inner.next().unwrap().as_str().to_string();
        let r#type = Type::parse(inner.next().unwrap())?;

        Ok(EnvField {
            name: identifier,
            r#type,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for EnvDef {
    const RULE: Rule = Rule::env_def;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| EnvField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(EnvDef { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for ParameterList {
    const RULE: Rule = Rule::parameter_list;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let mut parameters = Vec::new();

        for param in inner {
            let mut inner = param.into_inner();
            let name = Identifier::parse(inner.next().unwrap())?;
            let r#type = Type::parse(inner.next().unwrap())?;

            parameters.push(ParamDef { name, r#type });
        }

        Ok(ParameterList { parameters, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for TxDef {
    const RULE: Rule = Rule::tx_def;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let name = Identifier::parse(inner.next().unwrap())?;
        let parameters = ParameterList::parse(inner.next().unwrap())?;

        let mut locals = None;
        let mut references = Vec::new();
        let mut inputs = Vec::new();
        let mut outputs = Vec::new();
        let mut validity = None;
        let mut burn = None;
        let mut mints = Vec::new();
        let mut adhoc = Vec::new();
        let mut collateral = Vec::new();
        let mut signers = None;
        let mut metadata = None;

        for item in inner {
            match item.as_rule() {
                Rule::locals_block => locals = Some(LocalsBlock::parse(item)?),
                Rule::reference_block => references.push(ReferenceBlock::parse(item)?),
                Rule::input_block => inputs.push(InputBlock::parse(item)?),
                Rule::output_block => outputs.push(OutputBlock::parse(item)?),
                Rule::validity_block => validity = Some(ValidityBlock::parse(item)?),
                Rule::burn_block => burn = Some(BurnBlock::parse(item)?),
                Rule::mint_block => mints.push(MintBlock::parse(item)?),
                Rule::chain_specific_block => adhoc.push(ChainSpecificBlock::parse(item)?),
                Rule::collateral_block => collateral.push(CollateralBlock::parse(item)?),
                Rule::signers_block => signers = Some(SignersBlock::parse(item)?),
                Rule::metadata_block => metadata = Some(MetadataBlock::parse(item)?),
                x => unreachable!("Unexpected rule in tx_def: {:?}", x),
            }
        }

        Ok(TxDef {
            name,
            parameters,
            locals,
            references,
            inputs,
            outputs,
            validity,
            burn,
            mints,
            signers,
            adhoc,
            scope: None,
            span,
            collateral,
            metadata,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for Identifier {
    const RULE: Rule = Rule::identifier;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(Identifier {
            value: pair.as_str().to_string(),
            symbol: None,
            span: pair.as_span().into(),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for StringLiteral {
    const RULE: Rule = Rule::string;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(StringLiteral {
            value: pair.as_str()[1..pair.as_str().len() - 1].to_string(),
            span: pair.as_span().into(),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for HexStringLiteral {
    const RULE: Rule = Rule::hex_string;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(HexStringLiteral {
            value: pair.as_str()[2..].to_string(),
            span: pair.as_span().into(),
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for PartyDef {
    const RULE: Rule = Rule::party_def;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();
        let identifier = Identifier::parse(inner.next().unwrap())?;

        Ok(PartyDef {
            name: identifier,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for LocalsAssign {
    const RULE: Rule = Rule::locals_assign;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let name = Identifier::parse(inner.next().unwrap())?;
        let value = DataExpr::parse(inner.next().unwrap())?;

        Ok(LocalsAssign { name, value, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for LocalsBlock {
    const RULE: Rule = Rule::locals_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let assigns = inner
            .map(|x| LocalsAssign::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(LocalsBlock { assigns, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for ReferenceBlock {
    const RULE: Rule = Rule::reference_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let name = inner.next().unwrap().as_str().to_string();

        let pair = inner.next().unwrap();
        match pair.as_rule() {
            Rule::input_block_ref => {
                let pair = pair.into_inner().next().unwrap();
                let r#ref = DataExpr::parse(pair)?;
                Ok(ReferenceBlock { name, r#ref, span })
            }
            x => unreachable!("Unexpected rule in ref_input_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for CollateralBlockField {
    const RULE: Rule = Rule::collateral_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::input_block_from => {
                let pair = pair.into_inner().next().unwrap();
                let x = CollateralBlockField::From(DataExpr::parse(pair)?);
                Ok(x)
            }
            Rule::input_block_min_amount => {
                let pair = pair.into_inner().next().unwrap();
                let x = CollateralBlockField::MinAmount(DataExpr::parse(pair)?);
                Ok(x)
            }
            Rule::input_block_ref => {
                let pair = pair.into_inner().next().unwrap();
                let x = CollateralBlockField::Ref(DataExpr::parse(pair)?);
                Ok(x)
            }
            x => unreachable!("Unexpected rule in collateral_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::From(x) => x.span(),
            Self::MinAmount(x) => x.span(),
            Self::Ref(x) => x.span(),
        }
    }
}

impl AstNode for CollateralBlock {
    const RULE: Rule = Rule::collateral_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| CollateralBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(CollateralBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for MetadataBlockField {
    const RULE: Rule = Rule::metadata_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        match pair.as_rule() {
            Rule::metadata_block_field => {
                let mut inner = pair.into_inner();
                let key = inner.next().unwrap();
                let value = inner.next().unwrap();
                Ok(MetadataBlockField {
                    key: DataExpr::parse(key)?,
                    value: DataExpr::parse(value)?,
                    span,
                })
            }
            x => unreachable!("Unexpected rule in metadata_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for MetadataBlock {
    const RULE: Rule = Rule::metadata_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| MetadataBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(MetadataBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for InputBlockField {
    const RULE: Rule = Rule::input_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::input_block_from => {
                let pair = pair.into_inner().next().unwrap();
                let x = InputBlockField::From(DataExpr::parse(pair)?);
                Ok(x)
            }
            Rule::input_block_datum_is => {
                let pair = pair.into_inner().next().unwrap();
                let x = InputBlockField::DatumIs(Type::parse(pair)?);
                Ok(x)
            }
            Rule::input_block_min_amount => {
                let pair = pair.into_inner().next().unwrap();
                let x = InputBlockField::MinAmount(DataExpr::parse(pair)?);
                Ok(x)
            }
            Rule::input_block_redeemer => {
                let pair = pair.into_inner().next().unwrap();
                let x = InputBlockField::Redeemer(DataExpr::parse(pair)?);
                Ok(x)
            }
            Rule::input_block_ref => {
                let pair = pair.into_inner().next().unwrap();
                let x = InputBlockField::Ref(DataExpr::parse(pair)?);
                Ok(x)
            }
            x => unreachable!("Unexpected rule in input_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::From(x) => x.span(),
            Self::DatumIs(x) => x.span(),
            Self::MinAmount(x) => x.span(),
            Self::Redeemer(x) => x.span(),
            Self::Ref(x) => x.span(),
        }
    }
}

impl AstNode for InputBlock {
    const RULE: Rule = Rule::input_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let name = inner.next().unwrap().as_str().to_string();

        let fields = inner
            .map(|x| InputBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(InputBlock {
            name,
            is_many: false,
            fields,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for OutputBlockField {
    const RULE: Rule = Rule::output_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::output_block_to => {
                let pair = pair.into_inner().next().unwrap();
                let x = OutputBlockField::To(Box::new(DataExpr::parse(pair)?));
                Ok(x)
            }
            Rule::output_block_amount => {
                let pair = pair.into_inner().next().unwrap();
                let x = OutputBlockField::Amount(DataExpr::parse(pair)?.into());
                Ok(x)
            }
            Rule::output_block_datum => {
                let pair = pair.into_inner().next().unwrap();
                let x = OutputBlockField::Datum(DataExpr::parse(pair)?.into());
                Ok(x)
            }
            x => unreachable!("Unexpected rule in output_block_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::To(x) => x.span(),
            Self::Amount(x) => x.span(),
            Self::Datum(x) => x.span(),
        }
    }
}

impl AstNode for OutputBlock {
    const RULE: Rule = Rule::output_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let has_name = inner
            .peek()
            .map(|x| x.as_rule() == Rule::identifier)
            .unwrap_or_default();

        let name = has_name.then(|| inner.next().unwrap().as_str().to_string());

        let fields = inner
            .map(|x| OutputBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(OutputBlock { name, fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for ValidityBlockField {
    const RULE: Rule = Rule::validity_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::validity_since_slot => {
                let pair = pair.into_inner().next().unwrap();
                let x = ValidityBlockField::SinceSlot(DataExpr::parse(pair)?.into());
                Ok(x)
            }
            Rule::validity_until_slot => {
                let pair = pair.into_inner().next().unwrap();
                let x = ValidityBlockField::UntilSlot(DataExpr::parse(pair)?.into());
                Ok(x)
            }
            x => unreachable!("Unexpected rule in validity_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::UntilSlot(x) => x.span(),
            Self::SinceSlot(x) => x.span(),
        }
    }
}

impl AstNode for ValidityBlock {
    const RULE: Rule = Rule::validity_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| ValidityBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ValidityBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for MintBlockField {
    const RULE: Rule = Rule::mint_block_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::mint_block_amount => {
                let pair = pair.into_inner().next().unwrap();
                let x = MintBlockField::Amount(DataExpr::parse(pair)?.into());
                Ok(x)
            }
            Rule::mint_block_redeemer => {
                let pair = pair.into_inner().next().unwrap();
                let x = MintBlockField::Redeemer(DataExpr::parse(pair)?.into());
                Ok(x)
            }
            x => unreachable!("Unexpected rule in output_block_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::Amount(x) => x.span(),
            Self::Redeemer(x) => x.span(),
        }
    }
}

impl AstNode for SignersBlock {
    const RULE: Rule = Rule::signers_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let signers = inner
            .map(|x| DataExpr::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(SignersBlock { signers, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for MintBlock {
    const RULE: Rule = Rule::mint_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| MintBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(MintBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for BurnBlock {
    const RULE: Rule = Rule::burn_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| MintBlockField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(BurnBlock { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for RecordField {
    const RULE: Rule = Rule::record_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();
        let identifier = Identifier::parse(inner.next().unwrap())?;
        let r#type = Type::parse(inner.next().unwrap())?;

        Ok(RecordField {
            name: identifier,
            r#type,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for PolicyField {
    const RULE: Rule = Rule::policy_def_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::policy_def_hash => Ok(PolicyField::Hash(DataExpr::parse(
                pair.into_inner().next().unwrap(),
            )?)),
            Rule::policy_def_script => Ok(PolicyField::Script(DataExpr::parse(
                pair.into_inner().next().unwrap(),
            )?)),
            Rule::policy_def_ref => Ok(PolicyField::Ref(DataExpr::parse(
                pair.into_inner().next().unwrap(),
            )?)),
            x => unreachable!("Unexpected rule in policy_field: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::Hash(x) => x.span(),
            Self::Script(x) => x.span(),
            Self::Ref(x) => x.span(),
        }
    }
}

impl AstNode for PolicyConstructor {
    const RULE: Rule = Rule::policy_def_constructor;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let fields = inner
            .map(|x| PolicyField::parse(x))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(PolicyConstructor { fields, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for PolicyValue {
    const RULE: Rule = Rule::policy_def_value;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::policy_def_constructor => {
                Ok(PolicyValue::Constructor(PolicyConstructor::parse(pair)?))
            }
            Rule::policy_def_assign => Ok(PolicyValue::Assign(HexStringLiteral::parse(
                pair.into_inner().next().unwrap(),
            )?)),
            x => unreachable!("Unexpected rule in policy_value: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::Constructor(x) => x.span(),
            Self::Assign(x) => x.span(),
        }
    }
}

impl AstNode for PolicyDef {
    const RULE: Rule = Rule::policy_def;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();
        let name = Identifier::parse(inner.next().unwrap())?;
        let value = PolicyValue::parse(inner.next().unwrap())?;

        Ok(PolicyDef { name, value, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for StaticAssetConstructor {
    const RULE: Rule = Rule::static_asset_constructor;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let r#type = Identifier::parse(inner.next().unwrap())?;
        let amount = DataExpr::parse(inner.next().unwrap())?;

        Ok(StaticAssetConstructor {
            r#type,
            amount: Box::new(amount),
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for AnyAssetConstructor {
    const RULE: Rule = Rule::any_asset_constructor;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let policy = DataExpr::parse(inner.next().unwrap())?;
        let asset_name = DataExpr::parse(inner.next().unwrap())?;
        let amount = DataExpr::parse(inner.next().unwrap())?;

        Ok(AnyAssetConstructor {
            policy: Box::new(policy),
            asset_name: Box::new(asset_name),
            amount: Box::new(amount),
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for RecordConstructorField {
    const RULE: Rule = Rule::record_constructor_field;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let name = Identifier::parse(inner.next().unwrap())?;
        let value = DataExpr::parse(inner.next().unwrap())?;

        Ok(RecordConstructorField {
            name,
            value: Box::new(value),
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for UtxoRef {
    const RULE: Rule = Rule::utxo_ref;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let raw_ref = pair.as_span().as_str()[2..].to_string();
        let (raw_txid, raw_output_ix) = raw_ref.split_once("#").expect("Invalid utxo ref");

        Ok(UtxoRef {
            txid: hex::decode(raw_txid).expect("Invalid hex txid"),
            index: raw_output_ix.parse().expect("Invalid output index"),
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for StructConstructor {
    const RULE: Rule = Rule::struct_constructor;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let r#type = Identifier::parse(inner.next().unwrap())?;
        let case = VariantCaseConstructor::parse(inner.next().unwrap())?;

        Ok(StructConstructor {
            r#type,
            case,
            scope: None,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl VariantCaseConstructor {
    fn implicit_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let mut fields = Vec::new();
        let mut spread = None;

        for pair in inner {
            match pair.as_rule() {
                Rule::record_constructor_field => {
                    fields.push(RecordConstructorField::parse(pair)?);
                }
                Rule::spread_expression => {
                    spread = Some(DataExpr::parse(pair.into_inner().next().unwrap())?);
                }
                x => unreachable!("Unexpected rule in datum_constructor: {:?}", x),
            }
        }

        Ok(VariantCaseConstructor {
            name: Identifier::new("Default"),
            fields,
            spread: spread.map(Box::new),
            scope: None,
            span,
        })
    }

    fn explicit_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let name = Identifier::parse(inner.next().unwrap())?;

        let mut fields = Vec::new();
        let mut spread = None;

        for pair in inner {
            match pair.as_rule() {
                Rule::record_constructor_field => {
                    fields.push(RecordConstructorField::parse(pair)?);
                }
                Rule::spread_expression => {
                    spread = Some(DataExpr::parse(pair.into_inner().next().unwrap())?);
                }
                x => unreachable!("Unexpected rule in datum_constructor: {:?}", x),
            }
        }

        Ok(VariantCaseConstructor {
            name,
            fields,
            spread: spread.map(Box::new),
            scope: None,
            span,
        })
    }
}

impl AstNode for VariantCaseConstructor {
    const RULE: Rule = Rule::variant_case_constructor;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::implicit_variant_case_constructor => Self::implicit_parse(pair),
            Rule::explicit_variant_case_constructor => Self::explicit_parse(pair),
            x => unreachable!("Unexpected rule in datum_constructor: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for ListConstructor {
    const RULE: Rule = Rule::list_constructor;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let inner = pair.into_inner();

        let elements = inner.map(DataExpr::parse).collect::<Result<Vec<_>, _>>()?;

        Ok(ListConstructor { elements, span })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl DataExpr {
    fn number_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::Number(pair.as_str().parse().unwrap()))
    }

    fn bool_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::Bool(pair.as_str().parse().unwrap()))
    }

    fn identifier_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::Identifier(Identifier::parse(pair)?))
    }

    fn struct_constructor_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::StructConstructor(StructConstructor::parse(pair)?))
    }

    fn list_constructor_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::ListConstructor(ListConstructor::parse(pair)?))
    }

    fn utxo_ref_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::UtxoRef(UtxoRef::parse(pair)?))
    }

    fn static_asset_constructor_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::StaticAssetConstructor(
            StaticAssetConstructor::parse(pair)?,
        ))
    }

    fn any_asset_constructor_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        Ok(DataExpr::AnyAssetConstructor(AnyAssetConstructor::parse(
            pair,
        )?))
    }

    fn min_utxo_parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let inner = pair.into_inner().next().unwrap();
        Ok(DataExpr::MinUtxo(Identifier::parse(inner)?))
    }

    fn negate_op_parse(pair: Pair<Rule>, right: DataExpr) -> Result<Self, Error> {
        Ok(DataExpr::NegateOp(NegateOp {
            operand: Box::new(right),
            span: pair.as_span().into(),
        }))
    }

    fn property_op_parse(pair: Pair<Rule>, left: DataExpr) -> Result<Self, Error> {
        let span: Span = pair.as_span().into();
        let mut inner = pair.into_inner();

        Ok(DataExpr::PropertyOp(PropertyOp {
            operand: Box::new(left),
            property: Box::new(Identifier::parse(inner.next().unwrap())?),
            span,
            scope: None,
        }))
    }

    fn add_op_parse(left: DataExpr, pair: Pair<Rule>, right: DataExpr) -> Result<Self, Error> {
        let span = pair.as_span().into();

        Ok(DataExpr::AddOp(AddOp {
            lhs: Box::new(left),
            rhs: Box::new(right),
            span,
        }))
    }

    fn sub_op_parse(left: DataExpr, pair: Pair<Rule>, right: DataExpr) -> Result<Self, Error> {
        let span = pair.as_span().into();

        Ok(DataExpr::SubOp(SubOp {
            lhs: Box::new(left),
            rhs: Box::new(right),
            span,
        }))
    }
}

static DATA_EXPR_PRATT_PARSER: LazyLock<PrattParser<Rule>> = LazyLock::new(|| {
    PrattParser::new()
        .op(Op::infix(Rule::data_add, Assoc::Left) | Op::infix(Rule::data_sub, Assoc::Left))
        .op(Op::prefix(Rule::data_negate))
        .op(Op::postfix(Rule::data_property))
});

impl AstNode for DataExpr {
    const RULE: Rule = Rule::data_expr;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let inner = pair.into_inner();

        DATA_EXPR_PRATT_PARSER
            .map_primary(|x| match x.as_rule() {
                Rule::number => DataExpr::number_parse(x),
                Rule::string => Ok(DataExpr::String(StringLiteral::parse(x)?)),
                Rule::bool => DataExpr::bool_parse(x),
                Rule::hex_string => Ok(DataExpr::HexString(HexStringLiteral::parse(x)?)),
                Rule::struct_constructor => DataExpr::struct_constructor_parse(x),
                Rule::list_constructor => DataExpr::list_constructor_parse(x),
                Rule::unit => Ok(DataExpr::Unit),
                Rule::identifier => DataExpr::identifier_parse(x),
                Rule::utxo_ref => DataExpr::utxo_ref_parse(x),
                Rule::static_asset_constructor => DataExpr::static_asset_constructor_parse(x),
                Rule::any_asset_constructor => DataExpr::any_asset_constructor_parse(x),
                Rule::min_utxo => DataExpr::min_utxo_parse(x),
                Rule::data_expr => DataExpr::parse(x),
                x => unreachable!("unexpected rule as data primary: {:?}", x),
            })
            .map_prefix(|op, right| match op.as_rule() {
                Rule::data_negate => DataExpr::negate_op_parse(op, right?),
                x => unreachable!("Unexpected rule as data prefix: {:?}", x),
            })
            .map_postfix(|left, op| match op.as_rule() {
                Rule::data_property => DataExpr::property_op_parse(op, left?),
                x => unreachable!("Unexpected rule as data postfix: {:?}", x),
            })
            .map_infix(|left, op, right| match op.as_rule() {
                Rule::data_add => DataExpr::add_op_parse(left?, op, right?),
                Rule::data_sub => DataExpr::sub_op_parse(left?, op, right?),
                x => unreachable!("Unexpected rule as data infix: {:?}", x),
            })
            .parse(inner)
    }

    fn span(&self) -> &Span {
        match self {
            DataExpr::None => &Span::DUMMY,      // TODO
            DataExpr::Unit => &Span::DUMMY,      // TODO
            DataExpr::Number(_) => &Span::DUMMY, // TODO
            DataExpr::Bool(_) => &Span::DUMMY,   // TODO
            DataExpr::String(x) => x.span(),
            DataExpr::HexString(x) => x.span(),
            DataExpr::StructConstructor(x) => x.span(),
            DataExpr::ListConstructor(x) => x.span(),
            DataExpr::StaticAssetConstructor(x) => x.span(),
            DataExpr::AnyAssetConstructor(x) => x.span(),
            DataExpr::Identifier(x) => x.span(),
            DataExpr::AddOp(x) => &x.span,
            DataExpr::SubOp(x) => &x.span,
            DataExpr::NegateOp(x) => &x.span,
            DataExpr::PropertyOp(x) => &x.span,
            DataExpr::UtxoRef(x) => x.span(),
            DataExpr::MinUtxo(x) => x.span(),
        }
    }
}

impl AstNode for Type {
    const RULE: Rule = Rule::r#type;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let inner = pair.into_inner().next().unwrap();

        match inner.as_rule() {
            Rule::primitive_type => match inner.as_str() {
                "Int" => Ok(Type::Int),
                "Bool" => Ok(Type::Bool),
                "Bytes" => Ok(Type::Bytes),
                "Address" => Ok(Type::Address),
                "UtxoRef" => Ok(Type::UtxoRef),
                "AnyAsset" => Ok(Type::AnyAsset),
                _ => unreachable!("Unexpected string in primitive_type: {:?}", inner.as_str()),
            },
            Rule::list_type => {
                let inner = inner.into_inner().next().unwrap();
                Ok(Type::List(Box::new(Type::parse(inner)?)))
            }
            Rule::custom_type => Ok(Type::Custom(Identifier::new(inner.as_str().to_owned()))),
            x => unreachable!("Unexpected rule in type: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        &Span::DUMMY // TODO
    }
}

impl TypeDef {
    fn parse_variant_format(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let identifier = Identifier::parse(inner.next().unwrap())?;

        let cases = inner
            .map(VariantCase::parse)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(TypeDef {
            name: identifier,
            cases,
            span,
        })
    }

    fn parse_record_format(pair: Pair<Rule>) -> Result<Self, Error> {
        let span: Span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let identifier = Identifier::parse(inner.next().unwrap())?;

        let fields = inner
            .map(RecordField::parse)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(TypeDef {
            name: identifier.clone(),
            cases: vec![VariantCase {
                name: Identifier::new("Default"),
                fields,
                span: span.clone(),
            }],
            span,
        })
    }
}

impl AstNode for TypeDef {
    const RULE: Rule = Rule::type_def;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        match pair.as_rule() {
            Rule::variant_def => Ok(Self::parse_variant_format(pair)?),
            Rule::record_def => Ok(Self::parse_record_format(pair)?),
            x => unreachable!("Unexpected rule in type_def: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl VariantCase {
    fn struct_case_parse(pair: pest::iterators::Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let identifier = Identifier::parse(inner.next().unwrap())?;

        let fields = inner
            .map(RecordField::parse)
            .collect::<Result<Vec<_>, _>>()?;

        Ok(Self {
            name: identifier,
            fields,
            span,
        })
    }

    fn unit_case_parse(pair: pest::iterators::Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let identifier = Identifier::parse(inner.next().unwrap())?;

        Ok(Self {
            name: identifier,
            fields: vec![],
            span,
        })
    }
}

impl AstNode for VariantCase {
    const RULE: Rule = Rule::variant_case;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let case = match pair.as_rule() {
            Rule::variant_case_struct => Self::struct_case_parse(pair),
            Rule::variant_case_tuple => todo!("parse variant case tuple"),
            Rule::variant_case_unit => Self::unit_case_parse(pair),
            x => unreachable!("Unexpected rule in datum_variant: {:?}", x),
        }?;

        Ok(case)
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for AssetDef {
    const RULE: Rule = Rule::asset_def;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let span = pair.as_span().into();
        let mut inner = pair.into_inner();

        let identifier = Identifier::parse(inner.next().unwrap())?;
        let policy = DataExpr::parse(inner.next().unwrap())?;
        let asset_name = DataExpr::parse(inner.next().unwrap())?;

        Ok(AssetDef {
            name: identifier,
            policy,
            asset_name,
            span,
        })
    }

    fn span(&self) -> &Span {
        &self.span
    }
}

impl AstNode for ChainSpecificBlock {
    const RULE: Rule = Rule::chain_specific_block;

    fn parse(pair: Pair<Rule>) -> Result<Self, Error> {
        let mut inner = pair.into_inner();

        let block = inner.next().unwrap();

        match block.as_rule() {
            Rule::cardano_block => {
                let block = crate::cardano::CardanoBlock::parse(block)?;
                Ok(ChainSpecificBlock::Cardano(block))
            }
            x => unreachable!("Unexpected rule in chain_specific_block: {:?}", x),
        }
    }

    fn span(&self) -> &Span {
        match self {
            Self::Cardano(x) => x.span(),
        }
    }
}

/// Parses a Tx3 source string into a Program AST.
///
/// # Arguments
///
/// * `input` - String containing Tx3 source code
///
/// # Returns
///
/// * `Result<Program, Error>` - The parsed Program AST or an error
///
/// # Errors
///
/// Returns an error if:
/// - The input string is not valid Tx3 syntax
/// - The AST construction fails
///
/// # Example
///
/// ```
/// use tx3_lang::parsing::parse_string;
/// let program = parse_string("tx swap() {}").unwrap();
/// ```
pub fn parse_string(input: &str) -> Result<Program, Error> {
    let pairs = Tx3Grammar::parse(Rule::program, input)?;
    Program::parse(pairs.into_iter().next().unwrap())
}

#[cfg(test)]
pub fn parse_well_known_example(example: &str) -> Program {
    let manifest_dir = env!("CARGO_MANIFEST_DIR");
    let test_file = format!("{}/../../examples/{}.tx3", manifest_dir, example);
    let input = std::fs::read_to_string(&test_file).unwrap();
    parse_string(&input).unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;
    use assert_json_diff::assert_json_eq;
    use paste::paste;
    use pest::Parser;

    #[test]
    fn smoke_test_parse_string() {
        let _ = parse_string("tx swap() {}").unwrap();
    }

    macro_rules! input_to_ast_check {
        ($ast:ty, $name:expr, $input:expr, $expected:expr) => {
            paste::paste! {
                #[test]
                fn [<test_parse_ $ast:snake _ $name>]() {
                    let pairs = super::Tx3Grammar::parse(<$ast>::RULE, $input).unwrap();
                    let single_match = pairs.into_iter().next().unwrap();
                    let result = <$ast>::parse(single_match).unwrap();

                    assert_eq!(result, $expected);
                }
            }
        };
    }

    input_to_ast_check!(Type, "int", "Int", Type::Int);

    input_to_ast_check!(Type, "bool", "Bool", Type::Bool);

    input_to_ast_check!(Type, "bytes", "Bytes", Type::Bytes);

    input_to_ast_check!(Type, "address", "Address", Type::Address);

    input_to_ast_check!(Type, "utxo_ref", "UtxoRef", Type::UtxoRef);

    input_to_ast_check!(Type, "any_asset", "AnyAsset", Type::AnyAsset);

    input_to_ast_check!(Type, "list", "List<Int>", Type::List(Box::new(Type::Int)));

    input_to_ast_check!(
        Type,
        "identifier",
        "MyType",
        Type::Custom(Identifier::new("MyType".to_string()))
    );

    input_to_ast_check!(
        Type,
        "other_type",
        "List<Bytes>",
        Type::List(Box::new(Type::Bytes))
    );

    input_to_ast_check!(
        Type,
        "within_list",
        "List<List<Int>>",
        Type::List(Box::new(Type::List(Box::new(Type::Int))))
    );

    input_to_ast_check!(
        TypeDef,
        "type_def_record",
        "type MyRecord {
            field1: Int,
            field2: Bytes,
        }",
        TypeDef {
            name: Identifier::new("MyRecord"),
            cases: vec![VariantCase {
                name: Identifier::new("Default"),
                fields: vec![
                    RecordField::new("field1", Type::Int),
                    RecordField::new("field2", Type::Bytes)
                ],
                span: Span::DUMMY,
            }],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        TypeDef,
        "type_def_variant",
        "type MyVariant {
            Case1 {
                field1: Int,
                field2: Bytes,
            },
            Case2,
        }",
        TypeDef {
            name: Identifier::new("MyVariant"),
            cases: vec![
                VariantCase {
                    name: Identifier::new("Case1"),
                    fields: vec![
                        RecordField::new("field1", Type::Int),
                        RecordField::new("field2", Type::Bytes)
                    ],
                    span: Span::DUMMY,
                },
                VariantCase {
                    name: Identifier::new("Case2"),
                    fields: vec![],
                    span: Span::DUMMY,
                },
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        StringLiteral,
        "literal_string",
        "\"Hello, world!\"",
        StringLiteral::new("Hello, world!".to_string())
    );

    input_to_ast_check!(
        HexStringLiteral,
        "hex_string",
        "0xAFAFAF",
        HexStringLiteral::new("AFAFAF".to_string())
    );

    input_to_ast_check!(
        StringLiteral,
        "literal_string_address",
        "\"addr1qx234567890abcdefghijklmnopqrstuvwxyz\"",
        StringLiteral::new("addr1qx234567890abcdefghijklmnopqrstuvwxyz".to_string())
    );

    input_to_ast_check!(
        ListConstructor,
        "empty_list",
        "[]",
        ListConstructor {
            elements: vec![],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        ListConstructor,
        "trailing_comma",
        "[1, 2,]",
        ListConstructor {
            elements: vec![DataExpr::Number(1), DataExpr::Number(2),],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        ListConstructor,
        "int_list",
        "[1, 2]",
        ListConstructor {
            elements: vec![DataExpr::Number(1), DataExpr::Number(2),],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        ListConstructor,
        "string_list",
        "[\"Hello\", \"World\"]",
        ListConstructor {
            elements: vec![
                DataExpr::String(StringLiteral::new("Hello".to_string())),
                DataExpr::String(StringLiteral::new("World".to_string()))
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        ListConstructor,
        "mixed_list",
        "[1, \"Hello\", true]",
        ListConstructor {
            elements: vec![
                DataExpr::Number(1),
                DataExpr::String(StringLiteral::new("Hello".to_string())),
                DataExpr::Bool(true)
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        ListConstructor,
        "list_within_list",
        "[[1, 2], [3, 4]]",
        ListConstructor {
            elements: vec![
                DataExpr::ListConstructor(ListConstructor {
                    elements: vec![DataExpr::Number(1), DataExpr::Number(2),],
                    span: Span::DUMMY,
                }),
                DataExpr::ListConstructor(ListConstructor {
                    elements: vec![DataExpr::Number(3), DataExpr::Number(4),],
                    span: Span::DUMMY,
                }),
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(DataExpr, "literal_bool_true", "true", DataExpr::Bool(true));

    input_to_ast_check!(
        DataExpr,
        "literal_bool_false",
        "false",
        DataExpr::Bool(false)
    );

    input_to_ast_check!(DataExpr, "unit_value", "())", DataExpr::Unit);

    input_to_ast_check!(DataExpr, "number_value", "123", DataExpr::Number(123));

    input_to_ast_check!(
        PolicyDef,
        "policy_def_assign",
        "policy MyPolicy = 0xAFAFAF;",
        PolicyDef {
            name: Identifier::new("MyPolicy"),
            value: PolicyValue::Assign(HexStringLiteral::new("AFAFAF".to_string())),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        PolicyDef,
        "policy_def_constructor",
        "policy MyPolicy {
            hash: 0x1234567890,
            script: 0x1234567890,
            ref: 0x1234567890,
        };",
        PolicyDef {
            name: Identifier::new("MyPolicy"),
            value: PolicyValue::Constructor(PolicyConstructor {
                fields: vec![
                    PolicyField::Hash(DataExpr::HexString(HexStringLiteral::new(
                        "1234567890".to_string()
                    ))),
                    PolicyField::Script(DataExpr::HexString(HexStringLiteral::new(
                        "1234567890".to_string()
                    ))),
                    PolicyField::Ref(DataExpr::HexString(HexStringLiteral::new(
                        "1234567890".to_string()
                    ))),
                ],
                span: Span::DUMMY,
            }),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        AssetDef,
        "hex_hex",
        "asset MyToken = 0xef7a1cebb2dc7de884ddf82f8fcbc91fe9750dcd8c12ec7643a99bbe.0xef7a1ceb;",
        AssetDef {
            name: Identifier::new("MyToken"),
            policy: DataExpr::HexString(HexStringLiteral::new(
                "ef7a1cebb2dc7de884ddf82f8fcbc91fe9750dcd8c12ec7643a99bbe".to_string()
            )),
            asset_name: DataExpr::HexString(HexStringLiteral::new("ef7a1ceb".to_string())),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        AssetDef,
        "hex_string",
        "asset MyToken = 0xef7a1cebb2dc7de884ddf82f8fcbc91fe9750dcd8c12ec7643a99bbe.\"MY TOKEN\";",
        AssetDef {
            name: Identifier::new("MyToken"),
            policy: DataExpr::HexString(HexStringLiteral::new(
                "ef7a1cebb2dc7de884ddf82f8fcbc91fe9750dcd8c12ec7643a99bbe".to_string()
            )),
            asset_name: DataExpr::String(StringLiteral::new("MY TOKEN".to_string())),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        StaticAssetConstructor,
        "type_and_literal",
        "MyToken(15)",
        StaticAssetConstructor {
            r#type: Identifier::new("MyToken"),
            amount: Box::new(DataExpr::Number(15)),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        AnyAssetConstructor,
        "any_asset_constructor",
        "AnyAsset(0x1234567890, \"MyToken\", 15)",
        AnyAssetConstructor {
            policy: Box::new(DataExpr::HexString(HexStringLiteral::new(
                "1234567890".to_string()
            ))),
            asset_name: Box::new(DataExpr::String(StringLiteral::new("MyToken".to_string()))),
            amount: Box::new(DataExpr::Number(15)),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        AnyAssetConstructor,
        "any_asset_identifiers",
        "AnyAsset(my_policy, my_token, my_amount)",
        AnyAssetConstructor {
            policy: Box::new(DataExpr::Identifier(Identifier::new("my_policy"))),
            asset_name: Box::new(DataExpr::Identifier(Identifier::new("my_token"))),
            amount: Box::new(DataExpr::Identifier(Identifier::new("my_amount"))),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        AnyAssetConstructor,
        "any_asset_property_access",
        "AnyAsset(input1.policy, input1.asset_name, input1.amount)",
        AnyAssetConstructor {
            policy: Box::new(DataExpr::PropertyOp(PropertyOp {
                operand: Box::new(DataExpr::Identifier(Identifier::new("input1"))),
                property: Box::new(Identifier::new("policy")),
                span: Span::DUMMY,
                scope: None,
            })),
            asset_name: Box::new(DataExpr::PropertyOp(PropertyOp {
                operand: Box::new(DataExpr::Identifier(Identifier::new("input1"))),
                property: Box::new(Identifier::new("asset_name")),
                span: Span::DUMMY,
                scope: None,
            })),
            amount: Box::new(DataExpr::PropertyOp(PropertyOp {
                operand: Box::new(DataExpr::Identifier(Identifier::new("input1"))),
                property: Box::new(Identifier::new("amount")),
                span: Span::DUMMY,
                scope: None,
            })),
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(DataExpr, "literal", "5", DataExpr::Number(5));

    input_to_ast_check!(
        DataExpr,
        "add_op",
        "5 + var1",
        DataExpr::AddOp(AddOp {
            lhs: Box::new(DataExpr::Number(5)),
            rhs: Box::new(DataExpr::Identifier(Identifier::new("var1"))),
            span: Span::DUMMY,
        })
    );

    input_to_ast_check!(
        DataExpr,
        "property_access",
        "subject.property",
        DataExpr::PropertyOp(PropertyOp {
            operand: Box::new(DataExpr::Identifier(Identifier::new("subject"))),
            property: Box::new(Identifier::new("property")),
            span: Span::DUMMY,
            scope: None,
        })
    );

    input_to_ast_check!(
        DataExpr,
        "multiple_properties",
        "subject.property.subproperty",
        DataExpr::PropertyOp(PropertyOp {
            operand: Box::new(DataExpr::PropertyOp(PropertyOp {
                operand: Box::new(DataExpr::Identifier(Identifier::new("subject"))),
                property: Box::new(Identifier::new("property")),
                span: Span::DUMMY,
                scope: None,
            })),
            property: Box::new(Identifier::new("subproperty")),
            span: Span::DUMMY,
            scope: None,
        })
    );

    input_to_ast_check!(DataExpr, "empty_parentheses", "()", DataExpr::Unit);

    input_to_ast_check!(DataExpr, "nested_parentheses", "((()))", DataExpr::Unit);

    input_to_ast_check!(
        DataExpr,
        "nested_arithmetic_expression",
        "(1 + ((6 - 3) + 4))",
        DataExpr::AddOp(AddOp {
            lhs: Box::new(DataExpr::Number(1)),
            rhs: Box::new(DataExpr::AddOp(AddOp {
                lhs: Box::new(DataExpr::SubOp(SubOp {
                    lhs: Box::new(DataExpr::Number(6)),
                    rhs: Box::new(DataExpr::Number(3)),
                    span: Span::DUMMY,
                })),
                rhs: Box::new(DataExpr::Number(4)),
                span: Span::DUMMY,
            })),
            span: Span::DUMMY,
        })
    );

    input_to_ast_check!(
        DataExpr,
        "negate_op",
        "!a",
        DataExpr::NegateOp(NegateOp {
            operand: Box::new(DataExpr::Identifier(Identifier::new("a"))),
            span: Span::DUMMY,
        })
    );

    input_to_ast_check!(
        DataExpr,
        "negate_precedence",
        "!a.b",
        DataExpr::NegateOp(NegateOp {
            operand: Box::new(DataExpr::PropertyOp(PropertyOp {
                operand: Box::new(DataExpr::Identifier(Identifier::new("a"))),
                property: Box::new(Identifier::new("b")),
                span: Span::DUMMY,
                scope: None,
            })),
            span: Span::DUMMY,
        })
    );

    input_to_ast_check!(
        DataExpr,
        "negate_override_precedence",
        "(!a).b",
        DataExpr::PropertyOp(PropertyOp {
            operand: Box::new(DataExpr::NegateOp(NegateOp {
                operand: Box::new(DataExpr::Identifier(Identifier::new("a"))),
                span: Span::DUMMY,
            })),
            property: Box::new(Identifier::new("b")),
            span: Span::DUMMY,
            scope: None,
        })
    );

    input_to_ast_check!(
        DataExpr,
        "overly_complex",
        "(1 + 5) - ((a.b.c - 3) + !d.f)",
        DataExpr::SubOp(SubOp {
            lhs: Box::new(DataExpr::AddOp(AddOp {
                lhs: Box::new(DataExpr::Number(1)),
                rhs: Box::new(DataExpr::Number(5)),
                span: Span::DUMMY,
            })),
            rhs: Box::new(DataExpr::AddOp(AddOp {
                lhs: Box::new(DataExpr::SubOp(SubOp {
                    lhs: Box::new(DataExpr::PropertyOp(PropertyOp {
                        operand: Box::new(DataExpr::PropertyOp(PropertyOp {
                            operand: Box::new(DataExpr::Identifier(Identifier::new("a"))),
                            property: Box::new(Identifier::new("b")),
                            span: Span::DUMMY,
                            scope: None,
                        })),
                        property: Box::new(Identifier::new("c")),
                        span: Span::DUMMY,
                        scope: None,
                    })),
                    rhs: Box::new(DataExpr::Number(3)),
                    span: Span::DUMMY,
                })),
                rhs: Box::new(DataExpr::NegateOp(NegateOp {
                    operand: Box::new(DataExpr::PropertyOp(PropertyOp {
                        operand: Box::new(DataExpr::Identifier(Identifier::new("d"))),
                        property: Box::new(Identifier::new("f")),
                        span: Span::DUMMY,
                        scope: None,
                    })),
                    span: Span::DUMMY,
                })),

                span: Span::DUMMY,
            })),
            span: Span::DUMMY,
        })
    );

    input_to_ast_check!(
        StructConstructor,
        "struct_constructor_record",
        "MyRecord {
            field1: 10,
            field2: abc,
        }",
        StructConstructor {
            r#type: Identifier::new("MyRecord"),
            case: VariantCaseConstructor {
                name: Identifier::new("Default"),
                fields: vec![
                    RecordConstructorField {
                        name: Identifier::new("field1"),
                        value: Box::new(DataExpr::Number(10)),
                        span: Span::DUMMY,
                    },
                    RecordConstructorField {
                        name: Identifier::new("field2"),
                        value: Box::new(DataExpr::Identifier(Identifier::new("abc"))),
                        span: Span::DUMMY,
                    },
                ],
                spread: None,
                scope: None,
                span: Span::DUMMY,
            },
            scope: None,
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        StructConstructor,
        "struct_constructor_variant",
        "ShipCommand::MoveShip {
            delta_x: delta_x,
            delta_y: delta_y,
        }",
        StructConstructor {
            r#type: Identifier::new("ShipCommand"),
            case: VariantCaseConstructor {
                name: Identifier::new("MoveShip"),
                fields: vec![
                    RecordConstructorField {
                        name: Identifier::new("delta_x"),
                        value: Box::new(DataExpr::Identifier(Identifier::new("delta_x"))),
                        span: Span::DUMMY,
                    },
                    RecordConstructorField {
                        name: Identifier::new("delta_y"),
                        value: Box::new(DataExpr::Identifier(Identifier::new("delta_y"))),
                        span: Span::DUMMY,
                    },
                ],
                spread: None,
                scope: None,
                span: Span::DUMMY,
            },
            scope: None,
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        StructConstructor,
        "struct_constructor_variant_with_spread",
        "ShipCommand::MoveShip {
            delta_x: delta_x,
            delta_y: delta_y,
            ...abc
        }",
        StructConstructor {
            r#type: Identifier::new("ShipCommand"),
            case: VariantCaseConstructor {
                name: Identifier::new("MoveShip"),
                fields: vec![
                    RecordConstructorField {
                        name: Identifier::new("delta_x"),
                        value: Box::new(DataExpr::Identifier(Identifier::new("delta_x"))),
                        span: Span::DUMMY,
                    },
                    RecordConstructorField {
                        name: Identifier::new("delta_y"),
                        value: Box::new(DataExpr::Identifier(Identifier::new("delta_y"))),
                        span: Span::DUMMY,
                    },
                ],
                spread: Some(Box::new(DataExpr::Identifier(Identifier::new(
                    "abc".to_string()
                )))),
                scope: None,
                span: Span::DUMMY,
            },
            scope: None,
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        LocalsBlock,
        "basic",
        "locals {
            a: 10,
        }",
        LocalsBlock {
            assigns: vec![LocalsAssign {
                name: Identifier::new("a"),
                value: DataExpr::Number(10),
                span: Span::DUMMY,
            },],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        LocalsBlock,
        "multiple",
        "locals {
            a: 10,
            b: 20,
        }",
        LocalsBlock {
            assigns: vec![
                LocalsAssign {
                    name: Identifier::new("a"),
                    value: DataExpr::Number(10),
                    span: Span::DUMMY,
                },
                LocalsAssign {
                    name: Identifier::new("b"),
                    value: DataExpr::Number(20),
                    span: Span::DUMMY,
                },
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        LocalsBlock,
        "complex_expression",
        "locals {
            a: (10 + 20) - 8,
            b: a.b.c + (5 - d),
        }",
        LocalsBlock {
            assigns: vec![
                LocalsAssign {
                    name: Identifier::new("a"),
                    value: DataExpr::SubOp(SubOp {
                        lhs: Box::new(DataExpr::AddOp(AddOp {
                            lhs: Box::new(DataExpr::Number(10)),
                            rhs: Box::new(DataExpr::Number(20)),
                            span: Span::DUMMY,
                        })),
                        rhs: Box::new(DataExpr::Number(8)),
                        span: Span::DUMMY,
                    }),
                    span: Span::DUMMY,
                },
                LocalsAssign {
                    name: Identifier::new("b"),
                    value: DataExpr::AddOp(AddOp {
                        lhs: Box::new(DataExpr::PropertyOp(PropertyOp {
                            operand: Box::new(DataExpr::PropertyOp(PropertyOp {
                                operand: Box::new(DataExpr::Identifier(Identifier::new("a"))),
                                property: Box::new(Identifier::new("b")),
                                span: Span::DUMMY,
                                scope: None,
                            })),
                            property: Box::new(Identifier::new("c")),
                            span: Span::DUMMY,
                            scope: None,
                        })),
                        rhs: Box::new(DataExpr::SubOp(SubOp {
                            lhs: Box::new(DataExpr::Number(5)),
                            rhs: Box::new(DataExpr::Identifier(Identifier::new("d"))),
                            span: Span::DUMMY,
                        })),
                        span: Span::DUMMY,
                    }),
                    span: Span::DUMMY,
                },
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        OutputBlock,
        "output_block_anonymous",
        r#"output {
            to: my_party,
            amount: Ada(100),
        }"#,
        OutputBlock {
            name: None,
            fields: vec![
                OutputBlockField::To(Box::new(DataExpr::Identifier(Identifier::new(
                    "my_party".to_string(),
                )))),
                OutputBlockField::Amount(Box::new(DataExpr::StaticAssetConstructor(
                    StaticAssetConstructor {
                        r#type: Identifier::new("Ada"),
                        amount: Box::new(DataExpr::Number(100)),
                        span: Span::DUMMY,
                    },
                ))),
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        ChainSpecificBlock,
        "chain_specific_block_cardano",
        "cardano::vote_delegation_certificate {
            drep: 0x1234567890,
            stake: 0x1234567890,
        }",
        ChainSpecificBlock::Cardano(crate::cardano::CardanoBlock::VoteDelegationCertificate(
            crate::cardano::VoteDelegationCertificate {
                drep: DataExpr::HexString(HexStringLiteral::new("1234567890".to_string())),
                stake: DataExpr::HexString(HexStringLiteral::new("1234567890".to_string())),
                span: Span::DUMMY,
            },
        ))
    );

    input_to_ast_check!(
        EnvDef,
        "basic",
        "env {
            field_a: Int,
            field_b: Bytes,
        }",
        EnvDef {
            fields: vec![
                EnvField {
                    name: "field_a".to_string(),
                    r#type: Type::Int,
                    span: Span::DUMMY,
                },
                EnvField {
                    name: "field_b".to_string(),
                    r#type: Type::Bytes,
                    span: Span::DUMMY,
                },
            ],
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        TxDef,
        "empty",
        "tx my_tx() {}",
        TxDef {
            name: Identifier::new("my_tx"),
            parameters: ParameterList {
                parameters: vec![],
                span: Span::DUMMY,
            },
            locals: None,
            references: vec![],
            inputs: vec![],
            outputs: vec![],
            validity: None,
            burn: None,
            mints: vec![],
            signers: None,
            adhoc: vec![],
            collateral: vec![],
            metadata: None,
            scope: None,
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        TxDef,
        "with_parameters",
        "tx my_tx(a: Int, b: Bytes) {}",
        TxDef {
            name: Identifier::new("my_tx"),
            parameters: ParameterList {
                parameters: vec![
                    ParamDef {
                        name: Identifier::new("a"),
                        r#type: Type::Int,
                    },
                    ParamDef {
                        name: Identifier::new("b"),
                        r#type: Type::Bytes,
                    },
                ],
                span: Span::DUMMY,
            },
            locals: None,
            references: vec![],
            inputs: vec![],
            outputs: vec![],
            validity: None,
            burn: None,
            mints: vec![],
            signers: None,
            adhoc: vec![],
            collateral: vec![],
            metadata: None,
            scope: None,
            span: Span::DUMMY,
        }
    );

    input_to_ast_check!(
        Program,
        "basic",
        "party Abc; tx my_tx() {}",
        Program {
            parties: vec![PartyDef {
                name: Identifier::new("Abc"),
                span: Span::DUMMY,
            }],
            types: vec![],
            txs: vec![TxDef {
                name: Identifier::new("my_tx"),
                parameters: ParameterList {
                    parameters: vec![],
                    span: Span::DUMMY,
                },
                locals: None,
                references: vec![],
                inputs: vec![],
                outputs: vec![],
                validity: None,
                burn: None,
                mints: vec![],
                signers: None,
                adhoc: vec![],
                collateral: vec![],
                metadata: None,
                scope: None,
                span: Span::DUMMY,
            }],
            env: None,
            assets: vec![],
            policies: vec![],
            span: Span::DUMMY,
            scope: None,
        }
    );

    #[test]
    fn test_spans_are_respected() {
        let program = parse_well_known_example("lang_tour");
        assert_eq!(program.span, Span::new(0, 1497));

        assert_eq!(program.parties[0].span, Span::new(47, 61));

        assert_eq!(program.types[0].span, Span::new(63, 158));
    }

    fn make_snapshot_if_missing(example: &str, program: &Program) {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let path = format!("{}/../../examples/{}.ast", manifest_dir, example);

        if !std::fs::exists(&path).unwrap() {
            let ast = serde_json::to_string_pretty(program).unwrap();
            std::fs::write(&path, ast).unwrap();
        }
    }

    fn test_parsing_example(example: &str) {
        let program = parse_well_known_example(example);

        make_snapshot_if_missing(example, &program);

        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let ast_file = format!("{}/../../examples/{}.ast", manifest_dir, example);
        let ast = std::fs::read_to_string(ast_file).unwrap();

        let expected: Program = serde_json::from_str(&ast).unwrap();

        assert_json_eq!(program, expected);
    }

    #[macro_export]
    macro_rules! test_parsing {
        ($name:ident) => {
            paste! {
                #[test]
                fn [<test_example_ $name>]() {
                    test_parsing_example(stringify!($name));
                }
            }
        };
    }

    test_parsing!(lang_tour);

    test_parsing!(transfer);

    test_parsing!(swap);

    test_parsing!(asteria);

    test_parsing!(vesting);

    test_parsing!(faucet);

    test_parsing!(disordered);

    test_parsing!(input_datum);

    test_parsing!(withdrawal);

    test_parsing!(env_vars);

    test_parsing!(local_vars);

    test_parsing!(cardano_witness);
}
