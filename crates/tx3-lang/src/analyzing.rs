//! Semantic analysis of the Tx3 language.
//!
//! This module takes an AST and performs semantic analysis on it. It checks for
//! duplicate definitions, unknown symbols, and other semantic errors.

use std::{collections::HashMap, rc::Rc};

use crate::ast::*;

#[derive(Debug, thiserror::Error, miette::Diagnostic, PartialEq, Eq)]
#[error("not in scope: {name}")]
#[diagnostic(code(tx3::not_in_scope))]
pub struct NotInScopeError {
    pub name: String,

    #[source_code]
    src: Option<String>,

    #[label]
    span: Span,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, PartialEq, Eq)]
#[error("invalid symbol, expected {expected}, got {got}")]
#[diagnostic(code(tx3::invalid_symbol))]
pub struct InvalidSymbolError {
    pub expected: &'static str,
    pub got: String,

    #[source_code]
    src: Option<String>,

    #[label]
    span: Span,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, PartialEq, Eq)]
#[error("invalid type ({got}), expected: {expected}")]
#[diagnostic(code(tx3::invalid_type))]
pub struct InvalidTargetTypeError {
    pub expected: &'static str,
    pub got: String,

    #[source_code]
    src: Option<String>,

    #[label]
    span: Span,
}

#[derive(thiserror::Error, Debug, miette::Diagnostic, PartialEq, Eq)]
pub enum Error {
    #[error("duplicate definition: {0}")]
    #[diagnostic(code(tx3::duplicate_definition))]
    DuplicateDefinition(String),

    #[error(transparent)]
    #[diagnostic(transparent)]
    NotInScope(#[from] NotInScopeError),

    #[error("needs parent scope")]
    #[diagnostic(code(tx3::needs_parent_scope))]
    NeedsParentScope,

    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidSymbol(#[from] InvalidSymbolError),

    // Invalid type for extension
    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidTargetType(#[from] InvalidTargetTypeError),
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::NotInScope(x) => &x.span,
            Self::InvalidSymbol(x) => &x.span,
            Self::InvalidTargetType(x) => &x.span,
            _ => &Span::DUMMY,
        }
    }

    pub fn src(&self) -> Option<&str> {
        match self {
            Self::NotInScope(x) => x.src.as_deref(),
            _ => None,
        }
    }

    pub fn not_in_scope(name: String, ast: &impl crate::parsing::AstNode) -> Self {
        Self::NotInScope(NotInScopeError {
            name,
            src: None,
            span: ast.span().clone(),
        })
    }

    pub fn invalid_symbol(
        expected: &'static str,
        got: &Symbol,
        ast: &impl crate::parsing::AstNode,
    ) -> Self {
        Self::InvalidSymbol(InvalidSymbolError {
            expected,
            got: format!("{:?}", got),
            src: None,
            span: ast.span().clone(),
        })
    }

    pub fn invalid_target_type(
        expected: &'static str,
        got: &Type,
        ast: &impl crate::parsing::AstNode,
    ) -> Self {
        Self::InvalidTargetType(InvalidTargetTypeError {
            expected,
            got: format!("{:?}", got),
            src: None,
            span: ast.span().clone(),
        })
    }
}

#[derive(Debug, Default)]
pub struct AnalyzeReport {
    pub errors: Vec<Error>,
}

impl AnalyzeReport {
    pub fn is_empty(&self) -> bool {
        self.errors.is_empty()
    }

    pub fn ok(self) -> Result<(), Self> {
        if self.is_empty() {
            Ok(())
        } else {
            Err(self)
        }
    }
}

impl std::error::Error for AnalyzeReport {}

impl std::fmt::Display for AnalyzeReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "AnalyzeReport {{ errors: {:?} }}", self.errors)
    }
}

impl std::ops::Add for Error {
    type Output = AnalyzeReport;

    fn add(self, other: Self) -> Self::Output {
        Self::Output {
            errors: vec![self, other],
        }
    }
}

impl From<Error> for AnalyzeReport {
    fn from(error: Error) -> Self {
        Self {
            errors: vec![error],
        }
    }
}

impl From<Vec<Error>> for AnalyzeReport {
    fn from(errors: Vec<Error>) -> Self {
        Self { errors }
    }
}

impl std::ops::Add for AnalyzeReport {
    type Output = AnalyzeReport;

    fn add(self, other: Self) -> Self::Output {
        [self, other].into_iter().collect()
    }
}

impl FromIterator<Error> for AnalyzeReport {
    fn from_iter<T: IntoIterator<Item = Error>>(iter: T) -> Self {
        Self {
            errors: iter.into_iter().collect(),
        }
    }
}

impl FromIterator<AnalyzeReport> for AnalyzeReport {
    fn from_iter<T: IntoIterator<Item = AnalyzeReport>>(iter: T) -> Self {
        Self {
            errors: iter.into_iter().flat_map(|r| r.errors).collect(),
        }
    }
}

macro_rules! bail_report {
    ($($args:expr),*) => {
        { return AnalyzeReport::from(vec![$($args),*]); }
    };
}

impl Scope {
    pub fn new(parent: Option<Rc<Scope>>) -> Self {
        Self {
            symbols: HashMap::new(),
            parent,
        }
    }

    pub fn track_type_def(&mut self, type_: &TypeDef) {
        self.symbols
            .insert(type_.name.clone(), Symbol::TypeDef(Box::new(type_.clone())));
    }

    pub fn track_variant_case(&mut self, case: &VariantCase) {
        self.symbols.insert(
            case.name.clone(),
            Symbol::VariantCase(Box::new(case.clone())),
        );
    }

    pub fn track_record_field(&mut self, field: &RecordField) {
        self.symbols.insert(
            field.name.clone(),
            Symbol::RecordField(Box::new(field.clone())),
        );
    }

    pub fn track_party_def(&mut self, party: &PartyDef) {
        self.symbols.insert(
            party.name.clone(),
            Symbol::PartyDef(Box::new(party.clone())),
        );
    }

    pub fn track_policy_def(&mut self, policy: &PolicyDef) {
        self.symbols.insert(
            policy.name.clone(),
            Symbol::PolicyDef(Box::new(policy.clone())),
        );
    }

    pub fn track_asset_def(&mut self, asset: &AssetDef) {
        self.symbols.insert(
            asset.name.clone(),
            Symbol::AssetDef(Box::new(asset.clone())),
        );
    }

    pub fn track_param_var(&mut self, param: &str, ty: Type) {
        self.symbols.insert(
            param.to_string(),
            Symbol::ParamVar(param.to_string(), Box::new(ty)),
        );
    }

    pub fn track_input(&mut self, name: &str, ty: Type) {
        self.symbols.insert(
            name.to_string(),
            Symbol::Input(name.to_string(), Box::new(ty)),
        );
    }

    pub fn track_record_fields_for_type(&mut self, r#type: &Type) {
        let schema = resolve_type_schema(r#type);

        for (name, r#type) in schema {
            self.track_record_field(&RecordField {
                name,
                r#type,
                span: Span::DUMMY,
            });
        }
    }

    pub fn resolve(&self, name: &str) -> Option<Symbol> {
        if let Some(symbol) = self.symbols.get(name) {
            Some(symbol.clone())
        } else if let Some(parent) = &self.parent {
            parent.resolve(name)
        } else {
            None
        }
    }
}

fn resolve_type_schema(ty: &Type) -> Vec<(String, Type)> {
    match ty {
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
                    .map(|f| (f.name.clone(), f.r#type.clone()))
                    .collect(),
                _ => vec![],
            }
        }
        _ => vec![],
    }
}

/// A trait for types that can be semantically analyzed.
///
/// Types implementing this trait can validate their semantic correctness and
/// resolve symbol references within a given scope.
pub trait Analyzable {
    /// Performs semantic analysis on the type.
    ///
    /// # Arguments
    /// * `parent` - Optional parent scope containing symbol definitions
    ///
    /// # Returns
    /// * `AnalyzeReport` of the analysis. Empty if no errors are found.
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport;

    /// Returns true if all of the symbols have been resolved .
    fn is_resolved(&self) -> bool;
}

impl<T: Analyzable> Analyzable for Option<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        if let Some(item) = self {
            item.analyze(parent)
        } else {
            AnalyzeReport::default()
        }
    }

    fn is_resolved(&self) -> bool {
        self.as_ref().is_none_or(|x| x.is_resolved())
    }
}

impl<T: Analyzable> Analyzable for Box<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.as_mut().analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.as_ref().is_resolved()
    }
}

impl<T: Analyzable> Analyzable for Vec<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.iter_mut()
            .map(|item| item.analyze(parent.clone()))
            .collect()
    }

    fn is_resolved(&self) -> bool {
        self.iter().all(|x| x.is_resolved())
    }
}

impl Analyzable for PolicyField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            PolicyField::Hash(x) => x.analyze(parent),
            PolicyField::Script(x) => x.analyze(parent),
            PolicyField::Ref(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            PolicyField::Hash(x) => x.is_resolved(),
            PolicyField::Script(x) => x.is_resolved(),
            PolicyField::Ref(x) => x.is_resolved(),
        }
    }
}
impl Analyzable for PolicyConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for PolicyDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match &mut self.value {
            PolicyValue::Constructor(x) => x.analyze(parent),
            PolicyValue::Assign(_) => AnalyzeReport::default(),
        }
    }

    fn is_resolved(&self) -> bool {
        match &self.value {
            PolicyValue::Constructor(x) => x.is_resolved(),
            PolicyValue::Assign(_) => true,
        }
    }
}

impl Analyzable for DataBinaryOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let left = self.left.analyze(parent.clone());
        let right = self.right.analyze(parent.clone());

        left + right
    }

    fn is_resolved(&self) -> bool {
        self.left.is_resolved() && self.right.is_resolved()
    }
}

impl Analyzable for RecordConstructorField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let name = self.name.analyze(parent.clone());
        let value = self.value.analyze(parent.clone());

        name + value
    }

    fn is_resolved(&self) -> bool {
        self.name.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for VariantCaseConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let name = self.name.analyze(parent.clone());

        let mut scope = Scope::new(parent);

        let case = match &self.name.symbol {
            Some(Symbol::VariantCase(x)) => x,
            Some(x) => bail_report!(Error::invalid_symbol("VariantCase", x, &self.name)),
            None => bail_report!(Error::not_in_scope(self.name.value.clone(), &self.name)),
        };

        for field in case.fields.iter() {
            scope.track_record_field(field);
        }

        self.scope = Some(Rc::new(scope));

        let fields = self.fields.analyze(self.scope.clone());

        let spread = self.spread.analyze(self.scope.clone());

        name + fields + spread
    }

    fn is_resolved(&self) -> bool {
        self.name.is_resolved() && self.fields.is_resolved() && self.spread.is_resolved()
    }
}

impl Analyzable for StructConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let r#type = self.r#type.analyze(parent.clone());

        let mut scope = Scope::new(parent);

        let type_def = match &self.r#type.symbol {
            Some(Symbol::TypeDef(x)) => x,
            Some(x) => bail_report!(Error::invalid_symbol("TypeDef", x, &self.r#type)),
            _ => unreachable!(),
        };

        for case in type_def.cases.iter() {
            scope.track_variant_case(case);
        }

        self.scope = Some(Rc::new(scope));

        let case = self.case.analyze(self.scope.clone());

        r#type + case
    }

    fn is_resolved(&self) -> bool {
        self.r#type.is_resolved() && self.case.is_resolved()
    }
}

impl Analyzable for ListConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.elements.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.elements.is_resolved()
    }
}

impl Analyzable for DataExpr {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            DataExpr::StructConstructor(x) => x.analyze(parent),
            DataExpr::ListConstructor(x) => x.analyze(parent),
            DataExpr::Identifier(x) => x.analyze(parent),
            DataExpr::PropertyAccess(x) => x.analyze(parent),
            DataExpr::BinaryOp(x) => x.analyze(parent),
            _ => AnalyzeReport::default(),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            DataExpr::StructConstructor(x) => x.is_resolved(),
            DataExpr::ListConstructor(x) => x.is_resolved(),
            DataExpr::Identifier(x) => x.is_resolved(),
            DataExpr::PropertyAccess(x) => x.is_resolved(),
            DataExpr::BinaryOp(x) => x.is_resolved(),
            _ => true,
        }
    }
}

impl Analyzable for AssetBinaryOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let left = self.left.analyze(parent.clone());
        let right = self.right.analyze(parent.clone());

        left + right
    }

    fn is_resolved(&self) -> bool {
        self.left.is_resolved() && self.right.is_resolved()
    }
}

impl Analyzable for StaticAssetConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let amount = self.amount.analyze(parent.clone());
        let r#type = self.r#type.analyze(parent.clone());

        amount + r#type
    }

    fn is_resolved(&self) -> bool {
        self.amount.is_resolved() && self.r#type.is_resolved()
    }
}

impl Analyzable for AnyAssetConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let policy = self.policy.analyze(parent.clone());
        let asset_name = self.asset_name.analyze(parent.clone());
        let amount = self.amount.analyze(parent.clone());

        policy + asset_name + amount
    }

    fn is_resolved(&self) -> bool {
        self.policy.is_resolved() && self.asset_name.is_resolved() && self.amount.is_resolved()
    }
}

impl Analyzable for PropertyAccess {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let object = self.object.analyze(parent.clone());

        let mut scope = Scope::new(parent);

        if let Some(ty) = self.object.symbol.as_ref().and_then(|s| s.target_type()) {
            scope.track_record_fields_for_type(&ty);
        }

        self.scope = Some(Rc::new(scope));

        let path = self.path.analyze(self.scope.clone());

        object + path
    }

    fn is_resolved(&self) -> bool {
        self.object.is_resolved() && self.path.is_resolved()
    }
}

impl Analyzable for AssetExpr {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            AssetExpr::Identifier(x) => x.analyze(parent),
            AssetExpr::StaticConstructor(x) => x.analyze(parent),
            AssetExpr::AnyConstructor(x) => x.analyze(parent),
            AssetExpr::BinaryOp(x) => x.analyze(parent),
            AssetExpr::PropertyAccess(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            AssetExpr::Identifier(x) => x.is_resolved(),
            AssetExpr::StaticConstructor(x) => x.is_resolved(),
            AssetExpr::AnyConstructor(x) => x.is_resolved(),
            AssetExpr::BinaryOp(x) => x.is_resolved(),
            AssetExpr::PropertyAccess(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for AddressExpr {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            AddressExpr::Identifier(x) => x.analyze(parent),
            _ => AnalyzeReport::default(),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            AddressExpr::Identifier(x) => x.is_resolved(),
            _ => true,
        }
    }
}

impl Analyzable for AssetDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let policy = self.policy.analyze(parent.clone());
        let asset_name = self.asset_name.analyze(parent.clone());

        let policy_type = self.policy.target_type();
        let asset_name_type = self.asset_name.target_type();

        let policy_type = if policy_type != Some(Type::Bytes) {
            AnalyzeReport::from(Error::invalid_target_type(
                "Bytes",
                policy_type.as_ref().unwrap_or(&Type::Undefined),
                &self.policy,
            ))
        } else {
            AnalyzeReport::default()
        };

        let asset_name_type = if asset_name_type != Some(Type::Bytes) {
            AnalyzeReport::from(Error::invalid_target_type(
                "Bytes",
                asset_name_type.as_ref().unwrap_or(&Type::Undefined),
                &self.asset_name,
            ))
        } else {
            AnalyzeReport::default()
        };

        policy + asset_name + policy_type + asset_name_type
    }

    fn is_resolved(&self) -> bool {
        self.policy.is_resolved() && self.asset_name.is_resolved()
    }
}

impl Analyzable for Identifier {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let symbol = parent.and_then(|p| p.resolve(&self.value));

        if symbol.is_none() {
            bail_report!(Error::not_in_scope(self.value.clone(), self));
        }

        self.symbol = symbol;

        AnalyzeReport::default()
    }

    fn is_resolved(&self) -> bool {
        self.symbol.is_some()
    }
}

impl Analyzable for Type {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            Type::Custom(x) => x.analyze(parent),
            Type::List(x) => x.analyze(parent),
            _ => AnalyzeReport::default(),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            Type::Custom(x) => x.is_resolved(),
            Type::List(x) => x.is_resolved(),
            _ => true,
        }
    }
}

impl Analyzable for InputBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            InputBlockField::From(x) => x.analyze(parent),
            InputBlockField::DatumIs(x) => x.analyze(parent),
            InputBlockField::MinAmount(x) => x.analyze(parent),
            InputBlockField::Redeemer(x) => x.analyze(parent),
            InputBlockField::Ref(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            InputBlockField::From(x) => x.is_resolved(),
            InputBlockField::DatumIs(x) => x.is_resolved(),
            InputBlockField::MinAmount(x) => x.is_resolved(),
            InputBlockField::Redeemer(x) => x.is_resolved(),
            InputBlockField::Ref(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for InputBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for MetadataBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        // TODO: check keys are actually numbers
        self.key.analyze(parent.clone()) + self.value.analyze(parent.clone())
    }
    fn is_resolved(&self) -> bool {
        self.key.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for MetadataBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }
    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for WithdrawBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let key = self.key.analyze(parent.clone());
        let value = self.value.analyze(parent.clone());

        let value_validation = match &self.value {
            DataExpr::Number(_) => AnalyzeReport::default(),
            DataExpr::Identifier(id) => match id.symbol.as_ref().and_then(|s| s.target_type()) {
                Some(Type::Int) => AnalyzeReport::default(),
                Some(other_type) => AnalyzeReport::from(Error::invalid_target_type(
                    "Int (number)",
                    &other_type,
                    &self.value,
                )),
                None => AnalyzeReport::default(),
            },
            _ => AnalyzeReport::from(Error::invalid_target_type(
                "Int (number)",
                &Type::Undefined,
                &self.value,
            )),
        };

        key + value + value_validation
    }

    fn is_resolved(&self) -> bool {
        self.key.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for WithdrawBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }
    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for ValidityBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            ValidityBlockField::SinceSlot(x) => x.analyze(parent),
            ValidityBlockField::UntilSlot(x) => x.analyze(parent),
        }
    }
    fn is_resolved(&self) -> bool {
        match self {
            ValidityBlockField::SinceSlot(x) => x.is_resolved(),
            ValidityBlockField::UntilSlot(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for ValidityBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }
    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for OutputBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            OutputBlockField::To(x) => x.analyze(parent),
            OutputBlockField::Amount(x) => x.analyze(parent),
            OutputBlockField::Datum(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            OutputBlockField::To(x) => x.is_resolved(),
            OutputBlockField::Amount(x) => x.is_resolved(),
            OutputBlockField::Datum(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for OutputBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for RecordField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.r#type.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.r#type.is_resolved()
    }
}

impl Analyzable for VariantCase {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for TypeDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.cases.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.cases.is_resolved()
    }
}

impl Analyzable for MintBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            MintBlockField::Amount(x) => x.analyze(parent),
            MintBlockField::Redeemer(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            MintBlockField::Amount(x) => x.is_resolved(),
            MintBlockField::Redeemer(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for MintBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for SignersBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.signers.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.signers.is_resolved()
    }
}

impl Analyzable for ChainSpecificBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            ChainSpecificBlock::Cardano(x) => x.analyze(parent),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            ChainSpecificBlock::Cardano(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for TxDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        // analyze static types before anything else

        let params = self
            .parameters
            .parameters
            .iter_mut()
            .map(|param| param.r#type.analyze(parent.clone()))
            .collect::<AnalyzeReport>();

        let input_types = self
            .inputs
            .iter_mut()
            .flat_map(|input| input.fields.iter_mut())
            .map(|field| match field {
                InputBlockField::DatumIs(x) => x.analyze(parent.clone()),
                _ => AnalyzeReport::default(),
            })
            .collect::<AnalyzeReport>();

        // create the new scope and populate its symbols

        let mut scope = Scope::new(parent.clone());

        scope.symbols.insert("fees".to_string(), Symbol::Fees);

        for param in self.parameters.parameters.iter() {
            scope.track_param_var(&param.name, param.r#type.clone());
        }

        for input in self.inputs.iter() {
            scope.track_input(
                &input.name,
                input.datum_is().cloned().unwrap_or(Type::Undefined),
            );
        }

        // enter the new scope and analyze the rest of the program

        self.scope = Some(Rc::new(scope));

        let inputs = self.inputs.analyze(self.scope.clone());

        let outputs = self.outputs.analyze(self.scope.clone());

        let mints = self.mints.analyze(self.scope.clone());

        let adhoc = self.adhoc.analyze(self.scope.clone());

        let validity = self.validity.analyze(self.scope.clone());

        let metadata = self.metadata.analyze(self.scope.clone());

        let signers = self.signers.analyze(self.scope.clone());

        params + input_types + inputs + outputs + mints + adhoc + validity + metadata + signers
    }

    fn is_resolved(&self) -> bool {
        self.inputs.is_resolved()
            && self.outputs.is_resolved()
            && self.mints.is_resolved()
            && self.adhoc.is_resolved()
    }
}

fn ada_asset_def() -> AssetDef {
    AssetDef {
        name: "Ada".to_string(),
        policy: DataExpr::None,
        asset_name: DataExpr::None,
        span: Span::DUMMY,
    }
}

impl Analyzable for Program {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        let mut scope = Scope::new(parent);

        for party in self.parties.iter() {
            scope.track_party_def(party);
        }

        for policy in self.policies.iter() {
            scope.track_policy_def(policy);
        }

        scope.track_asset_def(&ada_asset_def());

        for asset in self.assets.iter() {
            scope.track_asset_def(asset);
        }

        for type_def in self.types.iter() {
            scope.track_type_def(type_def);
        }

        self.scope = Some(Rc::new(scope));

        // TODO: Add parties
        // let parties = self.parties.analyze(self.scope.clone());

        let policies = self.policies.analyze(self.scope.clone());

        let assets = self.assets.analyze(self.scope.clone());

        let types = self.types.analyze(self.scope.clone());

        let txs = self.txs.analyze(self.scope.clone());

        policies + types + txs + assets
    }

    fn is_resolved(&self) -> bool {
        self.policies.is_resolved()
            && self.types.is_resolved()
            && self.txs.is_resolved()
            && self.assets.is_resolved()
    }
}

/// Performs semantic analysis on a Tx3 program AST.
///
/// This function validates the entire program structure, checking for:
/// - Duplicate definitions
/// - Unknown symbol references
/// - Type correctness
/// - Other semantic constraints
///
/// # Arguments
/// * `ast` - Mutable reference to the program AST to analyze
///
/// # Returns
/// * `AnalyzeReport` of the analysis. Empty if no errors are found.
pub fn analyze(ast: &mut Program) -> AnalyzeReport {
    ast.analyze(None)
}

#[cfg(test)]
mod tests {
    use crate::parsing::parse_well_known_example;

    use super::*;

    #[test]
    fn test_program_with_semantic_errors() {
        let mut ast = parse_well_known_example("semantic_errors");

        let report = analyze(&mut ast);

        assert_eq!(report.errors.len(), 3);

        assert_eq!(
            report.errors[0],
            Error::NotInScope(NotInScopeError {
                name: "missing_symbol".to_string(),
                src: None,
                span: Span::DUMMY,
            })
        );

        assert_eq!(
            report.errors[1],
            Error::InvalidTargetType(InvalidTargetTypeError {
                expected: "Bytes",
                got: "Int".to_string(),
                src: None,
                span: Span::DUMMY,
            })
        );

        assert_eq!(
            report.errors[2],
            Error::InvalidTargetType(InvalidTargetTypeError {
                expected: "Bytes",
                got: "Int".to_string(),
                src: None,
                span: Span::DUMMY,
            })
        );
    }
}
