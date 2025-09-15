//! The Tx3 language abstract syntax tree (AST).
//!
//! This module defines the abstract syntax tree (AST) for the Tx3 language.
//! It provides the structure for representing Tx3 programs, including
//! transactions, types, assets, and other constructs.
//!
//! This module is not intended to be used directly by end-users. See
//! [`parse_file`](crate::parse_file) and [`parse_string`](crate::parse_string)
//! for parsing Tx3 source code into an AST.

use serde::{Deserialize, Serialize};
use std::{collections::HashMap, rc::Rc};

#[derive(Debug, PartialEq, Eq)]
pub struct Scope {
    pub(crate) symbols: HashMap<String, Symbol>,
    pub(crate) parent: Option<Rc<Scope>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Symbol {
    EnvVar(String, Box<Type>),
    ParamVar(String, Box<Type>),
    LocalExpr(Box<DataExpr>),
    Output(usize),
    Input(Box<InputBlock>),
    PartyDef(Box<PartyDef>),
    PolicyDef(Box<PolicyDef>),
    AssetDef(Box<AssetDef>),
    TypeDef(Box<TypeDef>),
    RecordField(Box<RecordField>),
    VariantCase(Box<VariantCase>),
    Fees,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Span {
    dummy: bool,
    pub start: usize,
    pub end: usize,
}

impl Default for Span {
    fn default() -> Self {
        Self::DUMMY
    }
}

impl Eq for Span {}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        if self.dummy || other.dummy {
            return true;
        }

        self.start == other.start && self.end == other.end
    }
}

impl std::hash::Hash for Span {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.end.hash(state);
    }
}

impl Span {
    pub const DUMMY: Self = Self {
        dummy: true,
        start: 0,
        end: 0,
    };

    pub fn new(start: usize, end: usize) -> Self {
        Self {
            dummy: false,
            start,
            end,
        }
    }
}

impl Symbol {
    pub fn as_type_def(&self) -> Option<&TypeDef> {
        match self {
            Symbol::TypeDef(x) => Some(x.as_ref()),
            _ => None,
        }
    }

    pub fn as_variant_case(&self) -> Option<&VariantCase> {
        match self {
            Symbol::VariantCase(x) => Some(x.as_ref()),
            _ => None,
        }
    }

    pub fn as_field_def(&self) -> Option<&RecordField> {
        match self {
            Symbol::RecordField(x) => Some(x.as_ref()),
            _ => None,
        }
    }

    pub fn as_policy_def(&self) -> Option<&PolicyDef> {
        match self {
            Symbol::PolicyDef(x) => Some(x.as_ref()),
            _ => None,
        }
    }

    pub fn target_type(&self) -> Option<Type> {
        match self {
            Symbol::ParamVar(_, ty) => Some(ty.as_ref().clone()),
            Symbol::RecordField(x) => Some(x.r#type.clone()),
            Symbol::Input(x) => x.datum_is().cloned(),
            x => {
                dbg!(x);
                None
            }
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct Identifier {
    pub value: String,
    pub span: Span,

    // analysis
    #[serde(skip)]
    pub(crate) symbol: Option<Symbol>,
}

impl Identifier {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            symbol: None,
            span: Span::DUMMY,
        }
    }

    pub fn try_symbol(&self) -> Result<&Symbol, crate::lowering::Error> {
        match &self.symbol {
            Some(symbol) => Ok(symbol),
            None => Err(crate::lowering::Error::MissingAnalyzePhase(
                self.value.clone(),
            )),
        }
    }

    pub fn target_type(&self) -> Option<Type> {
        self.symbol.as_ref().and_then(|x| x.target_type())
    }
}

impl AsRef<str> for Identifier {
    fn as_ref(&self) -> &str {
        &self.value
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct Program {
    pub env: Option<EnvDef>,
    pub txs: Vec<TxDef>,
    pub types: Vec<TypeDef>,
    pub assets: Vec<AssetDef>,
    pub parties: Vec<PartyDef>,
    pub policies: Vec<PolicyDef>,
    pub span: Span,

    // analysis
    #[serde(skip)]
    pub(crate) scope: Option<Rc<Scope>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EnvField {
    pub name: String,
    pub r#type: Type,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct EnvDef {
    pub fields: Vec<EnvField>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ParameterList {
    pub parameters: Vec<ParamDef>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TxDef {
    pub name: Identifier,
    pub parameters: ParameterList,
    pub locals: Option<LocalsBlock>,
    pub references: Vec<ReferenceBlock>,
    pub inputs: Vec<InputBlock>,
    pub outputs: Vec<OutputBlock>,
    pub validity: Option<ValidityBlock>,
    pub mints: Vec<MintBlock>,
    pub burns: Vec<MintBlock>,
    pub signers: Option<SignersBlock>,
    pub adhoc: Vec<ChainSpecificBlock>,
    pub span: Span,
    pub collateral: Vec<CollateralBlock>,
    pub metadata: Option<MetadataBlock>,

    // analysis
    #[serde(skip)]
    pub(crate) scope: Option<Rc<Scope>>,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct LocalsAssign {
    pub name: Identifier,
    pub value: DataExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Default)]
pub struct LocalsBlock {
    pub assigns: Vec<LocalsAssign>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct StringLiteral {
    pub value: String,
    pub span: Span,
}

impl StringLiteral {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            span: Span::DUMMY,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, Hash)]
pub struct HexStringLiteral {
    pub value: String,
    pub span: Span,
}

impl HexStringLiteral {
    pub fn new(value: impl Into<String>) -> Self {
        Self {
            value: value.into(),
            span: Span::DUMMY,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum CollateralBlockField {
    From(DataExpr),
    MinAmount(DataExpr),
    Ref(DataExpr),
}

impl CollateralBlockField {
    fn key(&self) -> &str {
        match self {
            CollateralBlockField::From(_) => "from",
            CollateralBlockField::MinAmount(_) => "min_amount",
            CollateralBlockField::Ref(_) => "ref",
        }
    }

    pub fn as_data_expr(&self) -> Option<&DataExpr> {
        match self {
            CollateralBlockField::Ref(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CollateralBlock {
    pub fields: Vec<CollateralBlockField>,
    pub span: Span,
}

impl CollateralBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&CollateralBlockField> {
        self.fields.iter().find(|x| x.key() == key)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum InputBlockField {
    From(DataExpr),
    DatumIs(Type),
    MinAmount(DataExpr),
    Redeemer(DataExpr),
    Ref(DataExpr),
}

impl InputBlockField {
    fn key(&self) -> &str {
        match self {
            InputBlockField::From(_) => "from",
            InputBlockField::DatumIs(_) => "datum_is",
            InputBlockField::MinAmount(_) => "min_amount",
            InputBlockField::Redeemer(_) => "redeemer",
            InputBlockField::Ref(_) => "ref",
        }
    }

    pub fn as_data_expr(&self) -> Option<&DataExpr> {
        match self {
            InputBlockField::Redeemer(x) => Some(x),
            InputBlockField::Ref(x) => Some(x),
            _ => None,
        }
    }

    pub fn as_datum_type(&self) -> Option<&Type> {
        match self {
            InputBlockField::DatumIs(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ReferenceBlock {
    pub name: String,
    pub r#ref: DataExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MetadataBlockField {
    pub key: DataExpr,
    pub value: DataExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MetadataBlock {
    pub fields: Vec<MetadataBlockField>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct InputBlock {
    pub name: String,
    pub many: bool,
    pub fields: Vec<InputBlockField>,
    pub span: Span,
}

impl InputBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&InputBlockField> {
        self.fields.iter().find(|x| x.key() == key)
    }

    pub(crate) fn datum_is(&self) -> Option<&Type> {
        self.find("datum_is").and_then(|x| x.as_datum_type())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum OutputBlockField {
    To(Box<DataExpr>),
    Amount(Box<DataExpr>),
    Datum(Box<DataExpr>),
}

impl OutputBlockField {
    fn key(&self) -> &str {
        match self {
            OutputBlockField::To(_) => "to",
            OutputBlockField::Amount(_) => "amount",
            OutputBlockField::Datum(_) => "datum",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct OutputBlock {
    pub name: Option<Identifier>,
    pub fields: Vec<OutputBlockField>,
    pub span: Span,
}

impl OutputBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&OutputBlockField> {
        self.fields.iter().find(|x| x.key() == key)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ValidityBlockField {
    UntilSlot(Box<DataExpr>),
    SinceSlot(Box<DataExpr>),
}

impl ValidityBlockField {
    fn key(&self) -> &str {
        match self {
            ValidityBlockField::UntilSlot(_) => "until_slot",
            ValidityBlockField::SinceSlot(_) => "since_slot",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ValidityBlock {
    pub fields: Vec<ValidityBlockField>,
    pub span: Span,
}

impl ValidityBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&ValidityBlockField> {
        self.fields.iter().find(|x| x.key() == key)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum MintBlockField {
    Amount(Box<DataExpr>),
    Redeemer(Box<DataExpr>),
}

impl MintBlockField {
    fn key(&self) -> &str {
        match self {
            MintBlockField::Amount(_) => "amount",
            MintBlockField::Redeemer(_) => "redeemer",
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct MintBlock {
    pub fields: Vec<MintBlockField>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SignersBlock {
    pub signers: Vec<DataExpr>,
    pub span: Span,
}

impl MintBlock {
    pub(crate) fn find(&self, key: &str) -> Option<&MintBlockField> {
        self.fields.iter().find(|x| x.key() == key)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RecordField {
    pub name: Identifier,
    pub r#type: Type,
    pub span: Span,
}

impl RecordField {
    pub fn new(name: &str, r#type: Type) -> Self {
        Self {
            name: Identifier::new(name),
            r#type,
            span: Span::DUMMY,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PartyDef {
    pub name: Identifier,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PartyField {
    pub name: String,
    pub party_type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PolicyDef {
    pub name: Identifier,
    pub value: PolicyValue,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PolicyField {
    Hash(DataExpr),
    Script(DataExpr),
    Ref(DataExpr),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PolicyConstructor {
    pub fields: Vec<PolicyField>,
    pub span: Span,
}

impl PolicyConstructor {
    pub(crate) fn find_field(&self, field: &str) -> Option<&PolicyField> {
        self.fields.iter().find(|x| match x {
            PolicyField::Hash(_) => field == "hash",
            PolicyField::Script(_) => field == "script",
            PolicyField::Ref(_) => field == "ref",
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum PolicyValue {
    Constructor(PolicyConstructor),
    Assign(HexStringLiteral),
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StaticAssetConstructor {
    pub r#type: Identifier,
    pub amount: Box<DataExpr>,
    pub span: Span,
}

impl StaticAssetConstructor {
    pub fn target_type(&self) -> Option<Type> {
        Some(Type::AnyAsset)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AnyAssetConstructor {
    pub policy: Box<DataExpr>,
    pub asset_name: Box<DataExpr>,
    pub amount: Box<DataExpr>,
    pub span: Span,
}

impl AnyAssetConstructor {
    pub fn target_type(&self) -> Option<Type> {
        Some(Type::AnyAsset)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct RecordConstructorField {
    pub name: Identifier,
    pub value: Box<DataExpr>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct StructConstructor {
    pub r#type: Identifier,
    pub case: VariantCaseConstructor,
    pub span: Span,

    // analysis
    #[serde(skip)]
    pub scope: Option<Rc<Scope>>,
}

impl StructConstructor {
    pub fn target_type(&self) -> Option<Type> {
        self.r#type.symbol.as_ref().and_then(|x| x.target_type())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VariantCaseConstructor {
    pub name: Identifier,
    pub fields: Vec<RecordConstructorField>,
    pub spread: Option<Box<DataExpr>>,
    pub span: Span,

    // analysis
    #[serde(skip)]
    pub scope: Option<Rc<Scope>>,
}

impl VariantCaseConstructor {
    pub fn find_field_value(&self, field: &str) -> Option<&DataExpr> {
        self.fields
            .iter()
            .find(|x| x.name.value == field)
            .map(|x| x.value.as_ref())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ListConstructor {
    pub elements: Vec<DataExpr>,
    pub span: Span,
}

impl ListConstructor {
    pub fn target_type(&self) -> Option<Type> {
        self.elements.first().and_then(|x| x.target_type())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct UtxoRef {
    pub txid: Vec<u8>,
    pub index: u64,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct NegateOp {
    pub operand: Box<DataExpr>,
    pub span: Span,
}

impl NegateOp {
    pub fn target_type(&self) -> Option<Type> {
        self.operand.target_type()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct PropertyOp {
    pub operand: Box<DataExpr>,
    pub property: Box<Identifier>,
    pub span: Span,

    // analysis
    #[serde(skip)]
    pub(crate) scope: Option<Rc<Scope>>,
}

impl PropertyOp {
    pub fn target_type(&self) -> Option<Type> {
        self.property.target_type()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AddOp {
    pub lhs: Box<DataExpr>,
    pub rhs: Box<DataExpr>,
    pub span: Span,
}

impl AddOp {
    pub fn target_type(&self) -> Option<Type> {
        self.lhs.target_type()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct SubOp {
    pub lhs: Box<DataExpr>,
    pub rhs: Box<DataExpr>,
    pub span: Span,
}

impl SubOp {
    pub fn target_type(&self) -> Option<Type> {
        self.lhs.target_type()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ConcatOp {
    pub lhs: Box<DataExpr>,
    pub rhs: Box<DataExpr>,
    pub span: Span,
}

impl ConcatOp {
    pub fn target_type(&self) -> Option<Type> {
        self.lhs.target_type()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum DataExpr {
    None,
    Unit,
    Number(i64),
    Bool(bool),
    String(StringLiteral),
    HexString(HexStringLiteral),
    StructConstructor(StructConstructor),
    ListConstructor(ListConstructor),
    StaticAssetConstructor(StaticAssetConstructor),
    AnyAssetConstructor(AnyAssetConstructor),
    Identifier(Identifier),
    MinUtxo(Identifier),
    AddOp(AddOp),
    SubOp(SubOp),
    ConcatOp(ConcatOp),
    NegateOp(NegateOp),
    PropertyOp(PropertyOp),
    UtxoRef(UtxoRef),
}

impl DataExpr {
    pub fn as_identifier(&self) -> Option<&Identifier> {
        match self {
            DataExpr::Identifier(x) => Some(x),
            _ => None,
        }
    }

    pub fn target_type(&self) -> Option<Type> {
        match self {
            DataExpr::Identifier(x) => x.target_type(),
            DataExpr::None => Some(Type::Undefined),
            DataExpr::Unit => Some(Type::Unit),
            DataExpr::Number(_) => Some(Type::Int),
            DataExpr::Bool(_) => Some(Type::Bool),
            DataExpr::String(_) => Some(Type::Bytes),
            DataExpr::HexString(_) => Some(Type::Bytes),
            DataExpr::StructConstructor(x) => x.target_type(),
            DataExpr::ListConstructor(x) => x.target_type(),
            DataExpr::AddOp(x) => x.target_type(),
            DataExpr::SubOp(x) => x.target_type(),
            DataExpr::ConcatOp(x) => x.target_type(),
            DataExpr::NegateOp(x) => x.target_type(),
            DataExpr::PropertyOp(x) => x.target_type(),
            DataExpr::StaticAssetConstructor(x) => x.target_type(),
            DataExpr::AnyAssetConstructor(x) => x.target_type(),
            DataExpr::UtxoRef(_) => Some(Type::UtxoRef),
            DataExpr::MinUtxo(_) => Some(Type::AnyAsset),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum AddressExpr {
    String(StringLiteral),
    HexString(HexStringLiteral),
    Identifier(Identifier),
}

impl AddressExpr {
    pub fn as_identifier(&self) -> Option<&Identifier> {
        match self {
            AddressExpr::Identifier(x) => Some(x),
            _ => None,
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Type {
    Undefined,
    Unit,
    Int,
    Bool,
    Bytes,
    Address,
    Utxo,
    UtxoRef,
    AnyAsset,
    List(Box<Type>),
    Custom(Identifier),
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Undefined => write!(f, "Undefined"),
            Type::Unit => write!(f, "Unit"),
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Bytes => write!(f, "Bytes"),
            Type::Address => write!(f, "Address"),
            Type::UtxoRef => write!(f, "UtxoRef"),
            Type::AnyAsset => write!(f, "AnyAsset"),
            Type::Utxo => write!(f, "Utxo"),
            Type::List(inner) => write!(f, "List<{inner}>"),
            Type::Custom(id) => write!(f, "{}", id.value),
        }
    }
}

impl Type {
    pub fn properties(&self) -> Vec<(String, Type)> {
        match self {
            Type::AnyAsset => {
                vec![
                    ("amount".to_string(), Type::Int),
                    ("policy".to_string(), Type::Bytes),
                    ("asset_name".to_string(), Type::Bytes),
                ]
            }
            Type::UtxoRef => {
                vec![
                    ("tx_hash".to_string(), Type::Bytes),
                    ("output_index".to_string(), Type::Int),
                ]
            }
            Type::Custom(identifier) => {
                let def = identifier.symbol.as_ref().and_then(|s| s.as_type_def());

                match def {
                    Some(ty) if ty.cases.len() == 1 => ty.cases[0]
                        .fields
                        .iter()
                        .map(|f| (f.name.value.clone(), f.r#type.clone()))
                        .collect(),
                    _ => vec![],
                }
            }
            _ => vec![],
        }
    }

    pub fn property_index(&self, property: &str) -> Option<usize> {
        let properties = Self::properties(self);
        properties.iter().position(|(name, _)| name == property)
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ParamDef {
    pub name: Identifier,
    pub r#type: Type,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TypeDef {
    pub name: Identifier,
    pub cases: Vec<VariantCase>,
    pub span: Span,
}

impl TypeDef {
    pub(crate) fn find_case_index(&self, case: &str) -> Option<usize> {
        self.cases.iter().position(|x| x.name.value == case)
    }

    #[allow(dead_code)]
    pub(crate) fn find_case(&self, case: &str) -> Option<&VariantCase> {
        self.cases.iter().find(|x| x.name.value == case)
    }

    pub fn json_schema(&self) -> serde_json::Value {
        use serde_json::{json, Map, Value};

        if self.cases.is_empty() {
            return json!({ "type": "null" });
        }

        // If there's only one case, treat it as a simple object
        if self.cases.len() == 1 {
            let case = &self.cases[0];
            let mut properties = Map::new();
            let mut required = Vec::new();

            // Add constructor field (always 0 for single case)
            properties.insert(
                "constructor".to_string(),
                json!({
                    "type": "integer",
                    "const": 0,
                    "description": format!("Constructor index for variant '{}'", case.name.value)
                })
            );
            required.push("constructor".to_string());

            for field in &case.fields {
                properties.insert(field.name.value.clone(), type_to_json_schema(&field.r#type));
                required.push(field.name.value.clone());
            }

            return json!({
                "type": "object",
                "properties": properties,
                "required": required,
                "additionalProperties": false,
                "description": format!("Type {} with single constructor '{}' (index 0)", self.name.value, case.name.value)
            });
        }

        // Multiple cases - use oneOf with discriminator
        let mut one_of = Vec::new();

        for (index, case) in self.cases.iter().enumerate() {
            let mut properties = Map::new();
            let mut required = vec!["constructor".to_string()];

            // Add discriminator field with constructor index
            properties.insert(
                "constructor".to_string(),
                json!({
                    "type": "integer",
                    "const": index,
                    "description": format!("Constructor {} for variant '{}'", index, case.name.value)
                }),
            );

            // Add case fields
            for field in &case.fields {
                properties.insert(field.name.value.clone(), type_to_json_schema(&field.r#type));
                required.push(field.name.value.clone());
            }

            one_of.push(json!({
                "type": "object",
                "properties": properties,
                "required": required,
                "additionalProperties": false,
                "title": format!("Constructor {} ({})", index, case.name.value),
                "description": format!("Variant '{}' with constructor index {}", case.name.value, index)
            }));
        }

        json!({
            "oneOf": one_of,
            "discriminator": {
                "propertyName": "constructor",
                "mapping": self.cases.iter().enumerate().map(|(i, case)| {
                    (i.to_string(), serde_json::Value::String(format!("#/oneOf/{}", i)))
                }).collect::<Map<String, Value>>()
            },
            "title": self.name.value.clone(),
            "description": format!("Type {} with {} constructor variants", self.name.value, self.cases.len())
        })
    }

    /// Get the constructor index for a given case name
    pub fn constructor_index(&self, case_name: &str) -> Option<usize> {
        self.cases.iter().position(|case| case.name.value == case_name)
    }

    /// Get the case name for a given constructor index
    pub fn case_name(&self, constructor_index: usize) -> Option<&str> {
        self.cases.get(constructor_index).map(|case| case.name.value.as_str())
    }

    /// Get all constructor indices with their corresponding case names
    pub fn constructors(&self) -> Vec<(usize, &str)> {
        self.cases.iter().enumerate().map(|(i, case)| (i, case.name.value.as_str())).collect()
    }
}

fn type_to_json_schema(ty: &Type) -> serde_json::Value {
    use serde_json::json;

    match ty {
        Type::Undefined => json!({ "type": "null" }),
        Type::Unit => json!({ "type": "null" }),
        Type::Int => json!({ "type": "integer" }),
        Type::Bool => json!({ "type": "boolean" }),
        Type::Bytes => json!({ "type": "string", "format": "byte" }),
        Type::Address => json!({ "type": "string", "format": "address" }),
        Type::Utxo => json!({ "type": "object" }), // Could be more specific
        Type::UtxoRef => json!({
            "type": "object",
            "properties": {
                "tx_hash": { "type": "string", "format": "byte" },
                "output_index": { "type": "integer", "minimum": 0 }
            },
            "required": ["tx_hash", "output_index"],
            "additionalProperties": false
        }),
        Type::AnyAsset => json!({
            "type": "object",
            "properties": {
                "amount": { "type": "integer", "minimum": 0 },
                "policy": { "type": "string", "format": "byte" },
                "asset_name": { "type": "string", "format": "byte" }
            },
            "required": ["amount", "policy", "asset_name"],
            "additionalProperties": false
        }),
        Type::List(inner) => json!({
            "type": "array",
            "items": type_to_json_schema(inner)
        }),
        Type::Custom(identifier) => {
            // For custom types, we reference them by name
            // In a full implementation, you'd want to resolve these references
            json!({
                "$ref": format!("#/definitions/{}", identifier.value)
            })
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct VariantCase {
    pub name: Identifier,
    pub fields: Vec<RecordField>,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct AssetDef {
    pub name: Identifier,
    pub policy: DataExpr,
    pub asset_name: DataExpr,
    pub span: Span,
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum ChainSpecificBlock {
    Cardano(crate::cardano::CardanoBlock),
}
