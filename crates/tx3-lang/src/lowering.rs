//! Lowers the Tx3 language to the intermediate representation.
//!
//! This module takes an AST and performs lowering on it. It converts the AST
//! into the intermediate representation (IR) of the Tx3 language.

use crate::ast;
use crate::ir;
use crate::UtxoRef;

#[derive(Debug, thiserror::Error)]
pub enum Error {
    #[error("missing analyze phase for {0}")]
    MissingAnalyzePhase(String),

    #[error("symbol '{0}' expected to be '{1}'")]
    InvalidSymbol(String, &'static str),

    #[error("symbol '{0}' expected to be of type '{1}'")]
    InvalidSymbolType(String, &'static str),

    #[error("invalid ast: {0}")]
    InvalidAst(String),

    #[error("invalid property {0} on type {1:?}")]
    InvalidProperty(String, String),

    #[error("missing required field {0} for {1:?}")]
    MissingRequiredField(String, &'static str),

    #[error("failed to decode hex string {0}")]
    DecodeHexError(String),
}

#[inline]
fn hex_decode(s: &str) -> Result<Vec<u8>, Error> {
    hex::decode(s).map_err(|_| Error::DecodeHexError(s.to_string()))
}

fn expect_type_def(ident: &ast::Identifier) -> Result<&ast::TypeDef, Error> {
    let symbol = ident
        .symbol
        .as_ref()
        .ok_or(Error::MissingAnalyzePhase(ident.value.clone()))?;

    symbol
        .as_type_def()
        .ok_or(Error::InvalidSymbol(ident.value.clone(), "TypeDef"))
}

fn expect_alias_def(ident: &ast::Identifier) -> Result<&ast::AliasDef, Error> {
    let symbol = ident
        .symbol
        .as_ref()
        .ok_or(Error::MissingAnalyzePhase(ident.value.clone()))?;

    symbol
        .as_alias_def()
        .ok_or(Error::InvalidSymbol(ident.value.clone(), "AliasDef"))
}

fn expect_case_def(ident: &ast::Identifier) -> Result<&ast::VariantCase, Error> {
    let symbol = ident
        .symbol
        .as_ref()
        .ok_or(Error::MissingAnalyzePhase(ident.value.clone()))?;

    symbol
        .as_variant_case()
        .ok_or(Error::InvalidSymbol(ident.value.clone(), "VariantCase"))
}

#[allow(dead_code)]
fn expect_field_def(ident: &ast::Identifier) -> Result<&ast::RecordField, Error> {
    let symbol = ident
        .symbol
        .as_ref()
        .ok_or(Error::MissingAnalyzePhase(ident.value.clone()))?;

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

#[derive(Debug, Default)]
pub(crate) struct Context {
    is_asset_expr: bool,
    is_datum_expr: bool,
    is_address_expr: bool,
}

impl Context {
    pub fn enter_asset_expr(&self) -> Self {
        Self {
            is_asset_expr: true,
            is_datum_expr: false,
            is_address_expr: false,
        }
    }

    pub fn enter_datum_expr(&self) -> Self {
        Self {
            is_asset_expr: false,
            is_datum_expr: true,
            is_address_expr: false,
        }
    }

    pub fn enter_address_expr(&self) -> Self {
        Self {
            is_asset_expr: false,
            is_datum_expr: false,
            is_address_expr: true,
        }
    }

    pub fn is_address_expr(&self) -> bool {
        self.is_address_expr
    }

    pub fn is_asset_expr(&self) -> bool {
        self.is_asset_expr
    }

    pub fn is_datum_expr(&self) -> bool {
        self.is_datum_expr
    }
}

pub(crate) trait IntoLower {
    type Output;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error>;
}

impl<T> IntoLower for Option<&T>
where
    T: IntoLower,
{
    type Output = Option<T::Output>;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        self.map(|x| x.into_lower(ctx)).transpose()
    }
}

impl<T> IntoLower for Box<T>
where
    T: IntoLower,
{
    type Output = T::Output;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        self.as_ref().into_lower(ctx)
    }
}

impl IntoLower for ast::Identifier {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let symbol = self
            .symbol
            .as_ref()
            .ok_or(Error::MissingAnalyzePhase(self.value.clone()))?;

        match symbol {
            ast::Symbol::ParamVar(n, ty) => {
                Ok(ir::Param::ExpectValue(n.to_lowercase().clone(), ty.into_lower(ctx)?).into())
            }
            ast::Symbol::LocalExpr(expr) => Ok(expr.into_lower(ctx)?),
            ast::Symbol::PartyDef(x) => Ok(ir::Param::ExpectValue(
                x.name.value.to_lowercase().clone(),
                ir::Type::Address,
            )
            .into()),
            ast::Symbol::Input(def) => {
                let inner = def.into_lower(ctx)?.utxos;

                let out = if ctx.is_asset_expr() {
                    ir::Coerce::IntoAssets(inner).into()
                } else if ctx.is_datum_expr() {
                    ir::Coerce::IntoDatum(inner).into()
                } else {
                    inner
                };

                Ok(out)
            }
            ast::Symbol::Fees => Ok(ir::Param::ExpectFees.into()),
            ast::Symbol::EnvVar(n, ty) => {
                Ok(ir::Param::ExpectValue(n.to_lowercase().clone(), ty.into_lower(ctx)?).into())
            }
            ast::Symbol::PolicyDef(x) => {
                let policy = x.into_lower(ctx)?;

                if ctx.is_address_expr() {
                    Ok(ir::CompilerOp::BuildScriptAddress(policy.hash).into())
                } else {
                    Ok(policy.hash)
                }
            }
            ast::Symbol::Output(index) => Ok(ir::Expression::Number(*index as i128)),
            _ => {
                dbg!(&self);
                todo!();
            }
        }
    }
}

impl IntoLower for ast::UtxoRef {
    type Output = ir::Expression;

    fn into_lower(&self, _: &Context) -> Result<Self::Output, Error> {
        let x = ir::Expression::UtxoRefs(vec![UtxoRef {
            txid: self.txid.clone(),
            index: self.index as u32,
        }]);

        Ok(x)
    }
}

impl IntoLower for ast::StructConstructor {
    type Output = ir::StructExpr;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let type_def = expect_type_def(&self.r#type)
            .or_else(|_| {
                expect_alias_def(&self.r#type).and_then(|alias_def| {
                    alias_def.resolve_alias_chain().ok_or_else(|| {
                        Error::InvalidAst("Alias does not resolve to a TypeDef".to_string())
                    })
                })
            })
            .map_err(|_| Error::InvalidSymbol(self.r#type.value.clone(), "TypeDef or AliasDef"))?;

        let constructor = type_def
            .find_case_index(&self.case.name.value)
            .ok_or(Error::InvalidAst("case not found".to_string()))?;

        let case_def = expect_case_def(&self.case.name)?;

        let mut fields = vec![];

        for (index, field_def) in case_def.fields.iter().enumerate() {
            let value = self.case.find_field_value(&field_def.name.value);

            if let Some(value) = value {
                fields.push(value.into_lower(ctx)?);
            } else {
                let spread_target = self
                    .case
                    .spread
                    .as_ref()
                    .expect("spread must be set for missing explicit field")
                    .into_lower(ctx)?;

                fields.push(ir::Expression::EvalBuiltIn(Box::new(
                    ir::BuiltInOp::Property(spread_target, ir::Expression::Number(index as i128)),
                )));
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

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::PolicyField::Hash(x) => x.into_lower(ctx),
            ast::PolicyField::Script(x) => x.into_lower(ctx),
            ast::PolicyField::Ref(x) => x.into_lower(ctx),
        }
    }
}

impl IntoLower for ast::PolicyDef {
    type Output = ir::PolicyExpr;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match &self.value {
            ast::PolicyValue::Assign(x) => {
                let out = ir::PolicyExpr {
                    name: self.name.value.clone(),
                    hash: ir::Expression::Hash(hex_decode(&x.value)?),
                    script: ir::ScriptSource::expect_parameter(self.name.value.clone()),
                };

                Ok(out)
            }
            ast::PolicyValue::Constructor(x) => {
                let hash = x
                    .find_field("hash")
                    .ok_or(Error::InvalidAst("Missing policy hash".to_string()))?
                    .into_lower(ctx)?;

                let rf = x.find_field("ref").map(|x| x.into_lower(ctx)).transpose()?;

                let script = x
                    .find_field("script")
                    .map(|x| x.into_lower(ctx))
                    .transpose()?;

                let script = match (rf, script) {
                    (Some(rf), Some(script)) => ir::ScriptSource::new_ref(rf, script),
                    (Some(rf), None) => {
                        ir::ScriptSource::expect_ref_input(self.name.value.clone(), rf)
                    }
                    (None, Some(script)) => ir::ScriptSource::new_embedded(script),
                    (None, None) => ir::ScriptSource::expect_parameter(self.name.value.clone()),
                };

                Ok(ir::PolicyExpr {
                    name: self.name.value.clone(),
                    hash,
                    script,
                })
            }
        }
    }
}

impl IntoLower for ast::Type {
    type Output = ir::Type;
    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::Type::Undefined => Ok(ir::Type::Undefined),
            ast::Type::Unit => Ok(ir::Type::Unit),
            ast::Type::Int => Ok(ir::Type::Int),
            ast::Type::Bool => Ok(ir::Type::Bool),
            ast::Type::Bytes => Ok(ir::Type::Bytes),
            ast::Type::Address => Ok(ir::Type::Address),
            ast::Type::Utxo => Ok(ir::Type::Utxo),
            ast::Type::UtxoRef => Ok(ir::Type::UtxoRef),
            ast::Type::AnyAsset => Ok(ir::Type::AnyAsset),
            ast::Type::List(_) => Ok(ir::Type::List),
            ast::Type::Map(_, _) => Ok(ir::Type::Map),
            ast::Type::Custom(x) => {
                // Check if this custom type is actually a type alias
                if let Some(symbol) = &x.symbol {
                    if let Some(alias_def) = symbol.as_alias_def() {
                        return alias_def.alias_type.into_lower(ctx);
                    }
                }
                Ok(ir::Type::Custom(x.value.clone()))
            }
        }
    }
}

impl IntoLower for ast::AddOp {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let left = self.lhs.into_lower(ctx)?;
        let right = self.rhs.into_lower(ctx)?;

        Ok(ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Add(
            left, right,
        ))))
    }
}

impl IntoLower for ast::SubOp {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let left = self.lhs.into_lower(ctx)?;
        let right = self.rhs.into_lower(ctx)?;

        Ok(ir::Expression::EvalBuiltIn(Box::new(ir::BuiltInOp::Sub(
            left, right,
        ))))
    }
}

impl IntoLower for ast::ConcatOp {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let left = self.lhs.into_lower(ctx)?;
        let right = self.rhs.into_lower(ctx)?;

        Ok(ir::Expression::EvalBuiltIn(Box::new(
            ir::BuiltInOp::Concat(left, right),
        )))
    }
}

impl IntoLower for ast::NegateOp {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let operand = self.operand.into_lower(ctx)?;

        Ok(ir::Expression::EvalBuiltIn(Box::new(
            ir::BuiltInOp::Negate(operand),
        )))
    }
}

impl IntoLower for ast::PropertyOp {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let object = self.operand.into_lower(ctx)?;

        let ty = self
            .operand
            .target_type()
            .ok_or(Error::MissingAnalyzePhase(format!("{0:?}", self.operand)))?;

        let prop_index =
            ty.property_index(*self.property.clone())
                .ok_or(Error::InvalidProperty(
                    format!("{:?}", self.property),
                    ty.to_string(),
                ))?;

        Ok(ir::Expression::EvalBuiltIn(Box::new(
            ir::BuiltInOp::Property(object, prop_index.into_lower(ctx)?),
        )))
    }
}

impl IntoLower for ast::ListConstructor {
    type Output = Vec<ir::Expression>;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let elements = self
            .elements
            .iter()
            .map(|x| x.into_lower(ctx))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(elements)
    }
}

impl IntoLower for ast::MapConstructor {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let pairs = self
            .fields
            .iter()
            .map(|field| {
                let key = field.key.into_lower(ctx)?;
                let value = field.value.into_lower(ctx)?;
                Ok((key, value))
            })
            .collect::<Result<Vec<_>, _>>()?;

        Ok(ir::Expression::Map(pairs))
    }
}

impl IntoLower for ast::DataExpr {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let out = match self {
            ast::DataExpr::None => ir::Expression::None,
            ast::DataExpr::Number(x) => Self::Output::Number(*x as i128),
            ast::DataExpr::Bool(x) => ir::Expression::Bool(*x),
            ast::DataExpr::String(x) => ir::Expression::String(x.value.clone()),
            ast::DataExpr::HexString(x) => ir::Expression::Bytes(hex_decode(&x.value)?),
            ast::DataExpr::StructConstructor(x) => ir::Expression::Struct(x.into_lower(ctx)?),
            ast::DataExpr::ListConstructor(x) => ir::Expression::List(x.into_lower(ctx)?),
            ast::DataExpr::MapConstructor(x) => x.into_lower(ctx)?,
            ast::DataExpr::StaticAssetConstructor(x) => x.into_lower(ctx)?,
            ast::DataExpr::AnyAssetConstructor(x) => x.into_lower(ctx)?,
            ast::DataExpr::Unit => ir::Expression::Struct(ir::StructExpr::unit()),
            ast::DataExpr::Identifier(x) => x.into_lower(ctx)?,
            ast::DataExpr::AddOp(x) => x.into_lower(ctx)?,
            ast::DataExpr::SubOp(x) => x.into_lower(ctx)?,
            ast::DataExpr::ConcatOp(x) => x.into_lower(ctx)?,
            ast::DataExpr::NegateOp(x) => x.into_lower(ctx)?,
            ast::DataExpr::PropertyOp(x) => x.into_lower(ctx)?,
            ast::DataExpr::UtxoRef(x) => x.into_lower(ctx)?,
            ast::DataExpr::MinUtxo(x) => ir::Expression::EvalCompiler(Box::new(
                ir::CompilerOp::ComputeMinUtxo(x.into_lower(ctx)?),
            )),
            ast::DataExpr::ComputeTipSlot => {
                ir::Expression::EvalCompiler(Box::new(ir::CompilerOp::ComputeTipSlot))
            }
            ast::DataExpr::SlotToTime(x) => ir::Expression::EvalCompiler(Box::new(
                ir::CompilerOp::ComputeSlotToTime(x.into_lower(ctx)?),
            )),
            ast::DataExpr::TimeToSlot(x) => ir::Expression::EvalCompiler(Box::new(
                ir::CompilerOp::ComputeTimeToSlot(x.into_lower(ctx)?),
            )),
        };

        Ok(out)
    }
}

impl IntoLower for ast::StaticAssetConstructor {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let asset_def = coerce_identifier_into_asset_def(&self.r#type)?;

        let policy = asset_def.policy.into_lower(ctx)?;
        let asset_name = asset_def.asset_name.into_lower(ctx)?;

        let amount = self.amount.into_lower(ctx)?;

        Ok(ir::Expression::Assets(vec![ir::AssetExpr {
            policy,
            asset_name,
            amount,
        }]))
    }
}

impl IntoLower for ast::AnyAssetConstructor {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let policy = self.policy.into_lower(ctx)?;
        let asset_name = self.asset_name.into_lower(ctx)?;
        let amount = self.amount.into_lower(ctx)?;

        Ok(ir::Expression::Assets(vec![ir::AssetExpr {
            policy,
            asset_name,
            amount,
        }]))
    }
}

impl IntoLower for ast::InputBlockField {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::InputBlockField::From(x) => {
                let ctx = ctx.enter_address_expr();
                x.into_lower(&ctx)
            }
            ast::InputBlockField::DatumIs(_) => todo!(),
            ast::InputBlockField::MinAmount(x) => {
                let ctx = ctx.enter_asset_expr();
                x.into_lower(&ctx)
            }
            ast::InputBlockField::Redeemer(x) => {
                let ctx = ctx.enter_datum_expr();
                x.into_lower(&ctx)
            }
            ast::InputBlockField::Ref(x) => x.into_lower(ctx),
        }
    }
}

impl IntoLower for ast::InputBlock {
    type Output = ir::Input;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let from_field = self.find("from");

        let address = from_field.map(|x| x.into_lower(ctx)).transpose()?;

        let min_amount = self
            .find("min_amount")
            .map(|x| x.into_lower(ctx))
            .transpose()?;

        let r#ref = self.find("ref").map(|x| x.into_lower(ctx)).transpose()?;

        let redeemer = self
            .find("redeemer")
            .map(|x| x.into_lower(ctx))
            .transpose()?
            .unwrap_or(ir::Expression::None);

        let query = ir::InputQuery {
            address: address.unwrap_or(ir::Expression::None),
            min_amount: min_amount.unwrap_or(ir::Expression::None),
            r#ref: r#ref.unwrap_or(ir::Expression::None),
            many: self.many,
            collateral: false,
        };

        let param = ir::Param::ExpectInput(self.name.to_lowercase().clone(), query);

        let input = ir::Input {
            name: self.name.to_lowercase().clone(),
            utxos: param.into(),
            redeemer,
        };

        Ok(input)
    }
}

impl IntoLower for ast::OutputBlockField {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::OutputBlockField::To(x) => {
                let ctx = ctx.enter_address_expr();
                x.into_lower(&ctx)
            }
            ast::OutputBlockField::Amount(x) => {
                let ctx = ctx.enter_asset_expr();
                x.into_lower(&ctx)
            }
            ast::OutputBlockField::Datum(x) => {
                let ctx = ctx.enter_datum_expr();
                x.into_lower(&ctx)
            }
        }
    }
}

impl IntoLower for ast::OutputBlock {
    type Output = ir::Output;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let address = self.find("to").into_lower(ctx)?.unwrap_or_default();
        let datum = self.find("datum").into_lower(ctx)?.unwrap_or_default();
        let amount = self.find("amount").into_lower(ctx)?.unwrap_or_default();

        Ok(ir::Output {
            address,
            datum,
            amount,
            optional: self.optional,
        })
    }
}

impl IntoLower for ast::ValidityBlockField {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::ValidityBlockField::SinceSlot(x) => x.into_lower(ctx),
            ast::ValidityBlockField::UntilSlot(x) => x.into_lower(ctx),
        }
    }
}

impl IntoLower for ast::ValidityBlock {
    type Output = ir::Validity;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let since = self.find("since_slot").into_lower(ctx)?.unwrap_or_default();
        let until = self.find("until_slot").into_lower(ctx)?.unwrap_or_default();

        Ok(ir::Validity { since, until })
    }
}

impl IntoLower for ast::MintBlockField {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::MintBlockField::Amount(x) => x.into_lower(ctx),
            ast::MintBlockField::Redeemer(x) => x.into_lower(ctx),
        }
    }
}

impl IntoLower for ast::MintBlock {
    type Output = ir::Mint;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let amount = self.find("amount").into_lower(ctx)?.unwrap_or_default();
        let redeemer = self.find("redeemer").into_lower(ctx)?.unwrap_or_default();

        Ok(ir::Mint { amount, redeemer })
    }
}

impl IntoLower for ast::MetadataBlockField {
    type Output = ir::Metadata;
    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        Ok(ir::Metadata {
            key: self.key.into_lower(ctx)?,
            value: self.value.into_lower(ctx)?,
        })
    }
}

impl IntoLower for ast::MetadataBlock {
    type Output = Vec<ir::Metadata>;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let fields = self
            .fields
            .iter()
            .map(|metadata_field| metadata_field.into_lower(ctx))
            .collect::<Result<Vec<_>, _>>()?;

        Ok(fields)
    }
}

impl IntoLower for ast::ChainSpecificBlock {
    type Output = ir::AdHocDirective;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::ChainSpecificBlock::Cardano(x) => x.into_lower(ctx),
        }
    }
}

impl IntoLower for ast::ReferenceBlock {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        self.r#ref.into_lower(ctx)
    }
}

impl IntoLower for ast::CollateralBlockField {
    type Output = ir::Expression;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        match self {
            ast::CollateralBlockField::From(x) => x.into_lower(ctx),
            ast::CollateralBlockField::MinAmount(x) => x.into_lower(ctx),
            ast::CollateralBlockField::Ref(x) => x.into_lower(ctx),
        }
    }
}

impl IntoLower for ast::CollateralBlock {
    type Output = ir::Collateral;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let from = self.find("from").map(|x| x.into_lower(ctx)).transpose()?;

        let min_amount = self
            .find("min_amount")
            .map(|x| x.into_lower(ctx))
            .transpose()?;

        let r#ref = self.find("ref").map(|x| x.into_lower(ctx)).transpose()?;

        let query = ir::InputQuery {
            address: from.unwrap_or(ir::Expression::None),
            min_amount: min_amount.unwrap_or(ir::Expression::None),
            r#ref: r#ref.unwrap_or(ir::Expression::None),
            many: false,
            collateral: true,
        };

        let param = ir::Param::ExpectInput("collateral".to_string(), query);

        let collateral = ir::Collateral {
            utxos: param.into(),
        };

        Ok(collateral)
    }
}

impl IntoLower for ast::SignersBlock {
    type Output = ir::Signers;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        Ok(ir::Signers {
            signers: self
                .signers
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
        })
    }
}

impl IntoLower for ast::TxDef {
    type Output = ir::Tx;

    fn into_lower(&self, ctx: &Context) -> Result<Self::Output, Error> {
        let ir = ir::Tx {
            references: self
                .references
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
            inputs: self
                .inputs
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
            outputs: self
                .outputs
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
            validity: self
                .validity
                .as_ref()
                .map(|x| x.into_lower(ctx))
                .transpose()?,
            mints: self
                .mints
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
            burns: self
                .burns
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
            adhoc: self
                .adhoc
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
            fees: ir::Param::ExpectFees.into(),
            collateral: self
                .collateral
                .iter()
                .map(|x| x.into_lower(ctx))
                .collect::<Result<Vec<_>, _>>()?,
            signers: self
                .signers
                .as_ref()
                .map(|x| x.into_lower(ctx))
                .transpose()?,
            metadata: self
                .metadata
                .as_ref()
                .map(|x| x.into_lower(ctx))
                .transpose()?
                .unwrap_or(vec![]),
        };

        Ok(ir)
    }
}

pub fn lower_tx(ast: &ast::TxDef) -> Result<ir::Tx, Error> {
    let ctx = &Context::default();

    let tx = ast.into_lower(ctx)?;

    Ok(tx)
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
        .find(|x| x.name.value == template)
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
            let tir = lower(&program, &tx.name.value).unwrap();

            make_snapshot_if_missing(example, &tx.name.value, &tir);

            let tir_file = format!(
                "{}/../../examples/{}.{}.tir",
                manifest_dir, example, tx.name.value
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

    test_lowering!(input_datum);

    test_lowering!(env_vars);

    test_lowering!(local_vars);

    test_lowering!(cardano_witness);

    test_lowering!(reference_script);

    test_lowering!(withdrawal);
    test_lowering!(map);
    test_lowering!(burn);

    test_lowering!(min_utxo);

    test_lowering!(donation);

    test_lowering!(list_concat);
}
