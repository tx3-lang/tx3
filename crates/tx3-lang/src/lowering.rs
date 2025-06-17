//! Lowers the Tx3 language to the intermediate representation.
//!
//! This module takes an AST and performs lowering on it. It converts the AST
//! into the intermediate representation (IR) of the Tx3 language.

use std::collections::HashSet;
use std::ops::Deref;

use crate::ast;
use crate::ir;
use crate::UtxoRef;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("missing analyze phase")]
    MissingAnalyzePhase,

    #[error("symbol '{0}' expected to be '{1}'")]
    InvalidSymbol(String, &'static str),

    #[error("symbol '{0}' expected to be of type '{1}'")]
    InvalidSymbolType(String, &'static str),

    #[error("invalid ast: {0}")]
    InvalidAst(String),

    #[error("failed to decode hex string: {0}")]
    DecodeHexError(#[from] hex::FromHexError),
}

fn expect_type_def(ident: &ast::Identifier) -> Result<&ast::TypeDef, Error> {
    let symbol = ident.symbol.as_ref().ok_or(Error::MissingAnalyzePhase)?;

    symbol
        .as_type_def()
        .ok_or(Error::InvalidSymbol(ident.value.clone(), "TypeDef"))
}

fn expect_case_def(ident: &ast::Identifier) -> Result<&ast::VariantCase, Error> {
    let symbol = ident.symbol.as_ref().ok_or(Error::MissingAnalyzePhase)?;

    symbol
        .as_variant_case()
        .ok_or(Error::InvalidSymbol(ident.value.clone(), "VariantCase"))
}

#[allow(dead_code)]
fn expect_field_def(ident: &ast::Identifier) -> Result<&ast::RecordField, Error> {
    let symbol = ident.symbol.as_ref().ok_or(Error::MissingAnalyzePhase)?;

    symbol
        .as_field_def()
        .ok_or(Error::InvalidSymbol(ident.value.clone(), "FieldDef"))
}

fn coerce_identifier_into_asset_def(identifier: &ast::Identifier) -> Result<ast::AssetDef, Error> {
    match identifier.try_symbol()? {
        ast::Symbol::AssetDef(x) => Ok(x.as_ref().clone()),
        _ => Err(Error::InvalidSymbol(identifier.value.clone(), "AssetDef")),
    }
}

fn coerce_identifier_into_asset_expr(
    identifier: &ast::Identifier,
) -> Result<ir::Expression, Error> {
    match identifier.try_symbol()? {
        ast::Symbol::Input(x, _) => Ok(ir::Expression::EvalInputAssets(x.clone())),
        ast::Symbol::Fees => Ok(ir::Expression::FeeQuery),
        ast::Symbol::ParamVar(name, ty) => match ty.deref() {
            ast::Type::AnyAsset => Ok(ir::Expression::EvalParameter(
                name.to_lowercase().clone(),
                ir::Type::AnyAsset,
            )),
            _ => Err(Error::InvalidSymbolType(
                identifier.value.clone(),
                "AnyAsset",
            )),
        },
        _ => Err(Error::InvalidSymbol(identifier.value.clone(), "AssetExpr")),
    }
}

fn lower_into_address_expr(identifier: &ast::Identifier) -> Result<ir::Expression, Error> {
    match identifier.try_symbol()? {
        ast::Symbol::PolicyDef(x) => Ok(x.into_lower()?.hash),
        ast::Symbol::PartyDef(x) => Ok(ir::Expression::EvalParameter(
            x.name.to_lowercase().clone(),
            ir::Type::Address,
        )),
        _ => Err(Error::InvalidSymbol(
            identifier.value.clone(),
            "AddressExpr",
        )),
    }
}
pub(crate) trait IntoLower {
    type Output;

    fn into_lower(&self) -> Result<Self::Output, Error>;
}

impl<T> IntoLower for Option<&T>
where
    T: IntoLower,
{
    type Output = Option<T::Output>;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        self.map(|x| x.into_lower()).transpose()
    }
}

impl<T> IntoLower for Box<T>
where
    T: IntoLower,
{
    type Output = T::Output;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        self.as_ref().into_lower()
    }
}

impl IntoLower for ast::Identifier {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let symbol = self.symbol.as_ref().expect("analyze phase must be run");

        match symbol {
            ast::Symbol::ParamVar(n, ty) => Ok(ir::Expression::EvalParameter(
                n.to_lowercase().clone(),
                ty.into_lower()?,
            )),
            ast::Symbol::PartyDef(x) => Ok(ir::Expression::EvalParameter(
                x.name.to_lowercase().clone(),
                ir::Type::Address,
            )),
            ast::Symbol::Input(n, _) => Ok(ir::Expression::EvalInputDatum(n.clone())),
            _ => {
                dbg!(&self);
                todo!();
            }
        }
    }
}

impl IntoLower for ast::UtxoRef {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let x = ir::Expression::UtxoRefs(vec![UtxoRef {
            txid: self.txid.clone(),
            index: self.index as u32,
        }]);
        Ok(x)
    }
}

impl IntoLower for ast::StructConstructor {
    type Output = ir::StructExpr;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let type_def = expect_type_def(&self.r#type)?;

        let constructor = type_def
            .find_case_index(&self.case.name.value)
            .ok_or(Error::InvalidAst("case not found".to_string()))?;

        let case_def = expect_case_def(&self.case.name)?;

        let mut fields = vec![];

        for (index, field_def) in case_def.fields.iter().enumerate() {
            let value = self.case.find_field_value(&field_def.name);

            if let Some(value) = value {
                fields.push(value.into_lower()?);
            } else {
                let spread_target = self
                    .case
                    .spread
                    .as_ref()
                    .expect("spread must be set for missing explicit field")
                    .into_lower()?;

                fields.push(ir::Expression::EvalProperty(Box::new(ir::PropertyAccess {
                    object: Box::new(spread_target),
                    field: index as u64,
                })));
            }
        }

        Ok(ir::StructExpr {
            constructor,
            fields,
        })
    }
}

impl IntoLower for ast::PolicyField {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::PolicyField::Hash(x) => x.into_lower(),
            ast::PolicyField::Script(x) => x.into_lower(),
            ast::PolicyField::Ref(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::PolicyDef {
    type Output = ir::PolicyExpr;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match &self.value {
            ast::PolicyValue::Assign(x) => Ok(ir::PolicyExpr {
                name: self.name.clone(),
                hash: ir::Expression::Hash(hex::decode(&x.value)?),
                script: None,
            }),
            ast::PolicyValue::Constructor(x) => {
                let hash = x
                    .find_field("hash")
                    .ok_or(Error::InvalidAst("Missing policy hash".to_string()))?
                    .into_lower()?;

                let ref_field = x.find_field("ref");
                let script_field = x.find_field("script");

                let script = match (ref_field, script_field) {
                    (Some(x), None) => Some(ir::ScriptSource::UtxoRef {
                        r#ref: x.into_lower()?,
                        source: None,
                    }),
                    (None, Some(x)) => Some(ir::ScriptSource::Embedded(x.into_lower()?)),
                    (Some(r#ref), Some(source)) => Some(ir::ScriptSource::UtxoRef {
                        r#ref: r#ref.into_lower()?,
                        source: Some(source.into_lower()?),
                    }),
                    (None, None) => None,
                };

                Ok(ir::PolicyExpr {
                    name: self.name.clone(),
                    hash,
                    script,
                })
            }
        }
    }
}

impl IntoLower for ast::Type {
    type Output = ir::Type;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::Type::Undefined => Ok(ir::Type::Undefined),
            ast::Type::Unit => Ok(ir::Type::Unit),
            ast::Type::Int => Ok(ir::Type::Int),
            ast::Type::Bool => Ok(ir::Type::Bool),
            ast::Type::Bytes => Ok(ir::Type::Bytes),
            ast::Type::Address => Ok(ir::Type::Address),
            ast::Type::UtxoRef => Ok(ir::Type::UtxoRef),
            ast::Type::AnyAsset => Ok(ir::Type::AnyAsset),
            ast::Type::List(_) => Ok(ir::Type::List),
            ast::Type::Custom(x) => Ok(ir::Type::Custom(x.value.clone())),
        }
    }
}

impl IntoLower for ast::DataBinaryOp {
    type Output = ir::BinaryOp;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let left = self.left.into_lower()?;
        let right = self.right.into_lower()?;

        Ok(ir::BinaryOp {
            left,
            right,
            op: match self.operator {
                ast::BinaryOperator::Add => ir::BinaryOpKind::Add,
                ast::BinaryOperator::Subtract => ir::BinaryOpKind::Sub,
            },
        })
    }
}

impl IntoLower for ast::PropertyAccess {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let object = self.object.into_lower()?;
        let ty = self
            .object
            .target_type()
            .ok_or(Error::MissingAnalyzePhase)?;
        let field_name = &self.field.value;
        let field_index = match ty {
            ast::Type::Custom(ref ident) => {
                let type_def = expect_type_def(ident)?;
                let case = &type_def.cases[0];
                case.fields
                    .iter()
                    .position(|f| f.name == *field_name)
                    .ok_or_else(|| Error::InvalidAst(format!("field '{}' not found", field_name)))?
            }
            ast::Type::AnyAsset => {
                // TODO: improve this for each native type
                match field_name.as_str() {
                    "amount" => 0,
                    "policy" => 1,
                    "asset_name" => 2,
                    _ => {
                        return Err(Error::InvalidAst(format!(
                            "field '{}' not found",
                            field_name
                        )))
                    }
                }
            }
            _ => {
                return Err(Error::InvalidAst(format!(
                    "Property access not supported for type: {:?}",
                    ty
                )))
            }
        };

        Ok(ir::Expression::EvalProperty(Box::new(ir::PropertyAccess {
            object: Box::new(object),
            field: field_index as u64,
        })))
    }
}

impl IntoLower for ast::ListConstructor {
    type Output = Vec<ir::Expression>;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let elements = self
            .elements
            .iter()
            .map(|x| x.into_lower())
            .collect::<Result<Vec<_>, _>>()?;

        Ok(elements)
    }
}

impl IntoLower for ast::DataExpr {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let out = match self {
            ast::DataExpr::None => ir::Expression::None,
            ast::DataExpr::Number(x) => Self::Output::Number(*x as i128),
            ast::DataExpr::Bool(x) => ir::Expression::Bool(*x),
            ast::DataExpr::String(x) => ir::Expression::String(x.value.clone()),
            ast::DataExpr::HexString(x) => ir::Expression::Bytes(hex::decode(&x.value)?),
            ast::DataExpr::StructConstructor(x) => ir::Expression::Struct(x.into_lower()?),
            ast::DataExpr::ListConstructor(x) => ir::Expression::List(x.into_lower()?),
            ast::DataExpr::Unit => ir::Expression::Struct(ir::StructExpr::unit()),
            ast::DataExpr::Identifier(x) => x.into_lower()?,
            ast::DataExpr::BinaryOp(x) => ir::Expression::EvalCustom(Box::new(x.into_lower()?)),
            ast::DataExpr::PropertyAccess(x) => x.into_lower()?,
            ast::DataExpr::UtxoRef(x) => x.into_lower()?,
        };

        Ok(out)
    }
}

impl IntoLower for ast::StaticAssetConstructor {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let asset_def = coerce_identifier_into_asset_def(&self.r#type)?;

        let policy = asset_def.policy.into_lower()?;
        let asset_name = asset_def.asset_name.into_lower()?;

        let amount = self.amount.into_lower()?;

        Ok(ir::Expression::Assets(vec![ir::AssetExpr {
            policy,
            asset_name,
            amount,
        }]))
    }
}

impl IntoLower for ast::AnyAssetConstructor {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let policy = self.policy.into_lower()?;
        let asset_name = self.asset_name.into_lower()?;
        let amount = self.amount.into_lower()?;

        Ok(ir::Expression::Assets(vec![ir::AssetExpr {
            policy,
            asset_name,
            amount,
        }]))
    }
}

impl IntoLower for ast::AssetBinaryOp {
    type Output = ir::BinaryOp;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let left = self.left.into_lower()?;
        let right = self.right.into_lower()?;

        Ok(ir::BinaryOp {
            left,
            right,
            op: match self.operator {
                ast::BinaryOperator::Add => ir::BinaryOpKind::Add,
                ast::BinaryOperator::Subtract => ir::BinaryOpKind::Sub,
            },
        })
    }
}

impl IntoLower for ast::AssetExpr {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::AssetExpr::StaticConstructor(x) => x.into_lower(),
            ast::AssetExpr::AnyConstructor(x) => x.into_lower(),
            ast::AssetExpr::BinaryOp(x) => {
                Ok(ir::Expression::EvalCustom(Box::new(x.into_lower()?)))
            }
            ast::AssetExpr::Identifier(x) => coerce_identifier_into_asset_expr(x),
            ast::AssetExpr::PropertyAccess(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::AddressExpr {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::AddressExpr::String(x) => Ok(ir::Expression::String(x.value.clone())),
            ast::AddressExpr::HexString(x) => Ok(ir::Expression::Bytes(hex::decode(&x.value)?)),
            ast::AddressExpr::Identifier(x) => lower_into_address_expr(x),
        }
    }
}

impl IntoLower for ast::InputBlockField {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::InputBlockField::From(x) => x.into_lower(),
            ast::InputBlockField::DatumIs(_) => todo!(),
            ast::InputBlockField::MinAmount(x) => x.into_lower(),
            ast::InputBlockField::Redeemer(x) => x.into_lower(),
            ast::InputBlockField::Ref(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::InputBlock {
    type Output = ir::Input;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let from = self.find("from");
        let min_amount = self.find("min_amount");
        let r#ref = self.find("ref");
        let redeemer = self.find("redeemer");

        let policy = from
            .and_then(ast::InputBlockField::as_address_expr)
            .and_then(ast::AddressExpr::as_identifier)
            .and_then(|x| x.symbol.as_ref())
            .and_then(|x| x.as_policy_def())
            .map(|x| x.into_lower())
            .transpose()?;

        let input = ir::Input {
            name: self.name.to_lowercase().clone(),
            query: ir::InputQuery {
                address: from.into_lower()?,
                min_amount: min_amount.into_lower()?,
                r#ref: r#ref.into_lower()?,
            }
            .into(),
            refs: HashSet::new(),
            redeemer: redeemer.into_lower()?,
            policy,
        };

        Ok(input)
    }
}

impl IntoLower for ast::OutputBlockField {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::OutputBlockField::To(x) => x.into_lower(),
            ast::OutputBlockField::Amount(x) => x.into_lower(),
            ast::OutputBlockField::Datum(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::OutputBlock {
    type Output = ir::Output;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        Ok(ir::Output {
            address: self.find("to").into_lower()?,
            datum: self.find("datum").into_lower()?,
            amount: self.find("amount").into_lower()?,
        })
    }
}

impl IntoLower for ast::ValidityBlockField {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::ValidityBlockField::SinceSlot(x) => x.into_lower(),
            ast::ValidityBlockField::UntilSlot(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::ValidityBlock {
    type Output = ir::Validity;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        Ok(ir::Validity {
            since: self.find("since_slot").into_lower()?,
            until: self.find("until_slot").into_lower()?,
        })
    }
}

impl IntoLower for ast::MintBlockField {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::MintBlockField::Amount(x) => x.into_lower(),
            ast::MintBlockField::Redeemer(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::MintBlock {
    type Output = ir::Mint;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        Ok(ir::Mint {
            amount: self.find("amount").into_lower()?,
            redeemer: self.find("redeemer").into_lower()?,
        })
    }
}
impl IntoLower for ast::MetadataBlockField {
    type Output = ir::Metadata;
    fn into_lower(&self) -> Result<Self::Output, Error> {
        Ok(ir::Metadata {
            key: self.key.into_lower()?,
            value: self.value.into_lower()?,
        })
    }
}

impl IntoLower for ast::MetadataBlock {
    type Output = Vec<ir::Metadata>;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let fields = self
            .fields
            .iter()
            .map(|metadata_field| metadata_field.into_lower())
            .collect::<Result<Vec<_>, _>>()?;

        Ok(fields)
    }
}

impl IntoLower for ast::ChainSpecificBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::ChainSpecificBlock::Cardano(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::ReferenceBlock {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        self.r#ref.into_lower()
    }
}

impl IntoLower for ast::CollateralBlockField {
    type Output = ir::Expression;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        match self {
            ast::CollateralBlockField::From(x) => x.into_lower(),
            ast::CollateralBlockField::MinAmount(x) => x.into_lower(),
            ast::CollateralBlockField::Ref(x) => x.into_lower(),
        }
    }
}

impl IntoLower for ast::CollateralBlock {
    type Output = ir::Collateral;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        let from = self.find("from");
        let min_amount = self.find("min_amount");
        let r#ref = self.find("ref");

        let collateral = ir::Collateral {
            query: ir::InputQuery {
                address: from.into_lower()?,
                min_amount: min_amount.into_lower()?,
                r#ref: r#ref.into_lower()?,
            },
        };

        Ok(collateral)
    }
}

impl IntoLower for ast::SignersBlock {
    type Output = ir::Signers;

    fn into_lower(&self) -> Result<Self::Output, Error> {
        Ok(ir::Signers {
            signers: self
                .signers
                .iter()
                .map(|x| x.into_lower())
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

pub fn lower_tx(ast: &ast::TxDef) -> Result<ir::Tx, Error> {
    let ir = ir::Tx {
        references: ast
            .references
            .iter()
            .map(|x| x.into_lower())
            .collect::<Result<Vec<_>, _>>()?,
        inputs: ast
            .inputs
            .iter()
            .map(|x| x.into_lower())
            .collect::<Result<Vec<_>, _>>()?,
        outputs: ast
            .outputs
            .iter()
            .map(|x| x.into_lower())
            .collect::<Result<Vec<_>, _>>()?,
        validity: ast.validity.as_ref().map(|x| x.into_lower()).transpose()?,
        mints: ast
            .mints
            .iter()
            .map(|x| x.into_lower())
            .collect::<Result<Vec<_>, _>>()?,
        adhoc: ast
            .adhoc
            .iter()
            .map(|x| x.into_lower())
            .collect::<Result<Vec<_>, _>>()?,
        fees: ir::Expression::FeeQuery,
        collateral: ast
            .collateral
            .iter()
            .map(|x| x.into_lower())
            .collect::<Result<Vec<_>, _>>()?,
        signers: ast.signers.as_ref().map(|x| x.into_lower()).transpose()?,
        metadata: ast
            .metadata
            .as_ref()
            .map(|x| x.into_lower())
            .transpose()?
            .unwrap_or(vec![]),
    };

    Ok(ir)
}

/// Lowers the Tx3 language to the intermediate representation.
///
/// This function takes an AST and converts it into the intermediate
/// representation (IR) of the Tx3 language.
///
/// # Arguments
///
/// * `ast` - The AST to lower
///
/// # Returns
///
/// * `Result<ir::Program, Error>` - The lowered intermediate representation
pub fn lower(ast: &ast::Program, template: &str) -> Result<ir::Tx, Error> {
    let tx = ast
        .txs
        .iter()
        .find(|x| x.name == template)
        .ok_or(Error::InvalidAst("tx not found".to_string()))?;

    lower_tx(tx)
}

#[cfg(test)]
mod tests {
    use assert_json_diff::assert_json_eq;
    use paste::paste;

    use super::*;
    use crate::parsing::{self};

    fn make_snapshot_if_missing(example: &str, name: &str, tx: &ir::Tx) {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");

        let path = format!("{}/../../examples/{}.{}.tir", manifest_dir, example, name);

        if !std::fs::exists(&path).unwrap() {
            let ir = serde_json::to_string_pretty(tx).unwrap();
            std::fs::write(&path, ir).unwrap();
        }
    }

    fn test_lowering_example(example: &str) {
        let manifest_dir = env!("CARGO_MANIFEST_DIR");
        let mut program = parsing::parse_well_known_example(example);

        crate::analyzing::analyze(&mut program).ok().unwrap();

        for tx in program.txs.iter() {
            let tir = lower(&program, &tx.name).unwrap();

            make_snapshot_if_missing(example, &tx.name, &tir);

            let tir_file = format!(
                "{}/../../examples/{}.{}.tir",
                manifest_dir, example, tx.name
            );

            let expected = std::fs::read_to_string(tir_file).unwrap();
            let expected: ir::Tx = serde_json::from_str(&expected).unwrap();

            assert_json_eq!(tir, expected);
        }
    }

    #[macro_export]
    macro_rules! test_lowering {
        ($name:ident) => {
            paste! {
                #[test]
                fn [<test_example_ $name>]() {
                    test_lowering_example(stringify!($name));
                }
            }
        };
    }

    test_lowering!(lang_tour);

    test_lowering!(transfer);

    test_lowering!(swap);

    test_lowering!(asteria);

    test_lowering!(vesting);

    test_lowering!(faucet);
}
