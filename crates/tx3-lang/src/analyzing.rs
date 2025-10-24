//! Semantic analysis of the Tx3 language.
//!
//! This module takes an AST and performs semantic analysis on it. It checks for
//! duplicate definitions, unknown symbols, and other semantic errors.

use std::{collections::HashMap, rc::Rc};

use miette::Diagnostic;

use crate::ast::*;
use crate::rules::validate_optional_output;

#[derive(Debug, Clone)]
pub struct Context {
    pub target_type: Type,
}

impl Default for Context {
    fn default() -> Self {
        Self {
            target_type: Type::Undefined,
        }
    }
}
use crate::parsing::AstNode;

const METADATA_MAX_SIZE_BYTES: usize = 64;

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
    pub expected: String,
    pub got: String,

    #[source_code]
    src: Option<String>,

    #[label]
    span: Span,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, PartialEq, Eq)]
#[error("optional output ({name}) cannot have a datum")]
#[diagnostic(code(tx3::optional_output_datum))]
pub struct OptionalOutputError {
    pub name: String,

    #[source_code]
    pub src: Option<String>,

    #[label]
    pub span: Span,
}
#[derive(Debug, thiserror::Error, miette::Diagnostic, PartialEq, Eq)]
#[error("metadata value exceeds 64 bytes: {size} bytes found")]
#[diagnostic(code(tx3::metadata_size_limit_exceeded))]
pub struct MetadataSizeLimitError {
    pub size: usize,

    #[source_code]
    src: Option<String>,

    #[label("value too large")]
    span: Span,
}

#[derive(Debug, thiserror::Error, miette::Diagnostic, PartialEq, Eq)]
#[error("metadata key must be an integer, got: {key_type}")]
#[diagnostic(code(tx3::metadata_invalid_key_type))]
pub struct MetadataInvalidKeyTypeError {
    pub key_type: String,

    #[source_code]
    src: Option<String>,

    #[label("expected integer key")]
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

    #[error(transparent)]
    #[diagnostic(transparent)]
    InvalidOptionalOutput(#[from] OptionalOutputError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    MetadataSizeLimitExceeded(#[from] MetadataSizeLimitError),

    #[error(transparent)]
    #[diagnostic(transparent)]
    MetadataInvalidKeyType(#[from] MetadataInvalidKeyTypeError),
}

impl Error {
    pub fn span(&self) -> &Span {
        match self {
            Self::NotInScope(x) => &x.span,
            Self::InvalidSymbol(x) => &x.span,
            Self::InvalidTargetType(x) => &x.span,
            Self::InvalidOptionalOutput(x) => &x.span,
            Self::MetadataSizeLimitExceeded(x) => &x.span,
            Self::MetadataInvalidKeyType(x) => &x.span,
            _ => &Span::DUMMY,
        }
    }

    pub fn src(&self) -> Option<&str> {
        match self {
            Self::NotInScope(x) => x.src.as_deref(),
            Self::MetadataSizeLimitExceeded(x) => x.src.as_deref(),
            Self::MetadataInvalidKeyType(x) => x.src.as_deref(),
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

    fn symbol_type_name(symbol: &Symbol) -> String {
        match symbol {
            Symbol::TypeDef(type_def) => format!("TypeDef({})", type_def.name.value),
            Symbol::AliasDef(alias_def) => format!("AliasDef({})", alias_def.name.value),
            Symbol::VariantCase(case) => format!("VariantCase({})", case.name.value),
            Symbol::RecordField(field) => format!("RecordField({})", field.name.value),
            Symbol::PartyDef(party) => format!("PartyDef({})", party.name.value),
            Symbol::PolicyDef(policy) => format!("PolicyDef({})", policy.name.value),
            Symbol::AssetDef(asset) => format!("AssetDef({})", asset.name.value),
            Symbol::EnvVar(name, _) => format!("EnvVar({})", name),
            Symbol::ParamVar(name, _) => format!("ParamVar({})", name),
            Symbol::LocalExpr(_) => "LocalExpr".to_string(),
            Symbol::Input(_) => "Input".to_string(),
            Symbol::Output(_) => "Output".to_string(),
            Symbol::Fees => "Fees".to_string(),
        }
    }

    pub fn invalid_symbol(
        expected: &'static str,
        got: &Symbol,
        ast: &impl crate::parsing::AstNode,
    ) -> Self {
        Self::InvalidSymbol(InvalidSymbolError {
            expected,
            got: Self::symbol_type_name(got),
            src: None,
            span: ast.span().clone(),
        })
    }

    pub fn invalid_target_type(
        expected: &Type,
        got: &Type,
        ast: &impl crate::parsing::AstNode,
    ) -> Self {
        Self::InvalidTargetType(InvalidTargetTypeError {
            expected: expected.to_string(),
            got: got.to_string(),
            src: None,
            span: ast.span().clone(),
        })
    }
}

#[derive(Debug, Default, thiserror::Error, Diagnostic)]
pub struct AnalyzeReport {
    #[related]
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

    pub fn expect_data_expr_type(expr: &DataExpr, expected: &Type, ctx: &mut Context) -> Self {
        if expr.target_type(Some(ctx)).as_ref() != Some(expected) {
            Self::from(Error::invalid_target_type(
                expected,
                expr.target_type(Some(ctx))
                    .as_ref()
                    .unwrap_or(&Type::Undefined),
                expr,
            ))
        } else {
            Self::default()
        }
    }
}

impl std::fmt::Display for AnalyzeReport {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        if self.errors.is_empty() {
            write!(f, "")
        } else {
            write!(f, "Failed with {} errors:", self.errors.len())?;
            for error in &self.errors {
                write!(f, "\n{} ({:?})", error, error)?;
            }
            Ok(())
        }
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

    pub fn track_env_var(&mut self, name: &str, ty: Type) {
        self.symbols.insert(
            name.to_string(),
            Symbol::EnvVar(name.to_string(), Box::new(ty)),
        );
    }

    pub fn track_type_def(&mut self, type_: &TypeDef) {
        self.symbols.insert(
            type_.name.value.clone(),
            Symbol::TypeDef(Box::new(type_.clone())),
        );
    }

    pub fn track_alias_def(&mut self, alias: &AliasDef) {
        self.symbols.insert(
            alias.name.value.clone(),
            Symbol::AliasDef(Box::new(alias.clone())),
        );
    }

    pub fn track_variant_case(&mut self, case: &VariantCase) {
        self.symbols.insert(
            case.name.value.clone(),
            Symbol::VariantCase(Box::new(case.clone())),
        );
    }

    pub fn track_record_field(&mut self, field: &RecordField) {
        self.symbols.insert(
            field.name.value.clone(),
            Symbol::RecordField(Box::new(field.clone())),
        );
    }

    pub fn track_party_def(&mut self, party: &PartyDef) {
        self.symbols.insert(
            party.name.value.clone(),
            Symbol::PartyDef(Box::new(party.clone())),
        );
    }

    pub fn track_policy_def(&mut self, policy: &PolicyDef) {
        self.symbols.insert(
            policy.name.value.clone(),
            Symbol::PolicyDef(Box::new(policy.clone())),
        );
    }

    pub fn track_asset_def(&mut self, asset: &AssetDef) {
        self.symbols.insert(
            asset.name.value.clone(),
            Symbol::AssetDef(Box::new(asset.clone())),
        );
    }

    pub fn track_param_var(&mut self, param: &str, ty: Type) {
        self.symbols.insert(
            param.to_string(),
            Symbol::ParamVar(param.to_string(), Box::new(ty)),
        );
    }

    pub fn track_local_expr(&mut self, name: &str, expr: DataExpr) {
        self.symbols
            .insert(name.to_string(), Symbol::LocalExpr(Box::new(expr)));
    }

    pub fn track_input(&mut self, name: &str, input: InputBlock) {
        self.symbols
            .insert(name.to_string(), Symbol::Input(Box::new(input)));
    }

    pub fn track_output(&mut self, index: usize, output: OutputBlock) {
        if let Some(n) = output.name {
            self.symbols.insert(n.value, Symbol::Output(index));
        }
    }

    pub fn track_record_fields_for_type(&mut self, ty: &Type) {
        let schema = ty.properties();

        for (name, subty) in schema {
            self.track_record_field(&RecordField {
                name: Identifier::new(name),
                r#type: subty,
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport;

    /// Returns true if all of the symbols have been resolved .
    fn is_resolved(&self) -> bool;
}

impl<T: Analyzable> Analyzable for Option<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        if let Some(item) = self {
            item.analyze(parent, ctx)
        } else {
            AnalyzeReport::default()
        }
    }

    fn is_resolved(&self) -> bool {
        self.as_ref().is_none_or(|x| x.is_resolved())
    }
}

impl<T: Analyzable> Analyzable for Box<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.as_mut().analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.as_ref().is_resolved()
    }
}

impl<T: Analyzable> Analyzable for Vec<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.iter_mut()
            .map(|item| item.analyze(parent.clone(), ctx))
            .collect()
    }

    fn is_resolved(&self) -> bool {
        self.iter().all(|x| x.is_resolved())
    }
}

impl Analyzable for PartyDef {
    fn analyze(&mut self, _parent: Option<Rc<Scope>>, _ctx: &mut Context) -> AnalyzeReport {
        AnalyzeReport::default()
    }

    fn is_resolved(&self) -> bool {
        true
    }
}

impl Analyzable for PolicyField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            PolicyField::Hash(x) => x.analyze(parent, ctx),
            PolicyField::Script(x) => x.analyze(parent, ctx),
            PolicyField::Ref(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for PolicyDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match &mut self.value {
            PolicyValue::Constructor(x) => x.analyze(parent, ctx),
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

impl Analyzable for AddOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let left = self.lhs.analyze(parent.clone(), ctx);
        let right = self.rhs.analyze(parent.clone(), ctx);

        let left_type = self.lhs.target_type(Some(ctx));
        let right_type = self.rhs.target_type(Some(ctx));

        let type_check = match (left_type.as_ref(), right_type.as_ref()) {
            (Some(l), Some(r)) if l != r => {
                AnalyzeReport::from(Error::invalid_target_type(l, r, self.rhs.as_ref()))
            }
            _ => AnalyzeReport::default(),
        };

        left + right + type_check
    }

    fn is_resolved(&self) -> bool {
        self.lhs.is_resolved() && self.rhs.is_resolved()
    }
}

impl Analyzable for ConcatOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let left = self.lhs.analyze(parent.clone(), ctx);
        let right = self.rhs.analyze(parent.clone(), ctx);

        left + right
    }

    fn is_resolved(&self) -> bool {
        self.lhs.is_resolved() && self.rhs.is_resolved()
    }
}

impl Analyzable for SubOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let left = self.lhs.analyze(parent.clone(), ctx);
        let right = self.rhs.analyze(parent.clone(), ctx);

        let left_type = self.lhs.target_type(Some(ctx));
        let right_type = self.rhs.target_type(Some(ctx));

        let type_check = match (left_type.as_ref(), right_type.as_ref()) {
            (Some(l), Some(r)) if l != r => {
                AnalyzeReport::from(Error::invalid_target_type(l, r, self.rhs.as_ref()))
            }
            _ => AnalyzeReport::default(),
        };

        left + right + type_check
    }

    fn is_resolved(&self) -> bool {
        self.lhs.is_resolved() && self.rhs.is_resolved()
    }
}

impl Analyzable for NegateOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.operand.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.operand.is_resolved()
    }
}

impl Analyzable for RecordConstructorField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let name = self.name.analyze(parent.clone(), ctx);
        let value = self.value.analyze(parent.clone(), ctx);

        name + value
    }

    fn is_resolved(&self) -> bool {
        self.name.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for VariantCaseConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let name = if self.name.symbol.is_some() {
            AnalyzeReport::default()
        } else {
            self.name.analyze(parent.clone(), ctx)
        };

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

        let fields = self.fields.analyze(self.scope.clone(), ctx);

        let spread = self.spread.analyze(self.scope.clone(), ctx);

        name + fields + spread
    }

    fn is_resolved(&self) -> bool {
        self.name.is_resolved() && self.fields.is_resolved() && self.spread.is_resolved()
    }
}

impl Analyzable for StructConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let r#type = self.r#type.analyze(parent.clone(), ctx);

        let mut scope = Scope::new(parent);

        let type_def = match &self.r#type.symbol {
            Some(Symbol::TypeDef(type_def)) => type_def.as_ref(),
            Some(Symbol::AliasDef(alias_def)) => match alias_def.resolve_alias_chain() {
                Some(resolved_type_def) => resolved_type_def,
                None => {
                    bail_report!(Error::invalid_symbol(
                        "struct type",
                        &Symbol::AliasDef(alias_def.clone()),
                        &self.r#type
                    ));
                }
            },
            Some(symbol) => {
                bail_report!(Error::invalid_symbol("struct type", symbol, &self.r#type));
            }
            _ => unreachable!(),
        };

        for case in type_def.cases.iter() {
            scope.track_variant_case(case);
        }

        self.scope = Some(Rc::new(scope));

        let case = self.case.analyze(self.scope.clone(), ctx);

        r#type + case
    }

    fn is_resolved(&self) -> bool {
        self.r#type.is_resolved() && self.case.is_resolved()
    }
}

impl Analyzable for ListConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.elements.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.elements.is_resolved()
    }
}

impl Analyzable for MapField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.key.analyze(parent.clone(), ctx) + self.value.analyze(parent.clone(), ctx)
    }

    fn is_resolved(&self) -> bool {
        self.key.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for MapConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for DataExpr {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            DataExpr::StructConstructor(x) => x.analyze(parent, ctx),
            DataExpr::ListConstructor(x) => x.analyze(parent, ctx),
            DataExpr::MapConstructor(x) => x.analyze(parent, ctx),
            DataExpr::Identifier(x) => x.analyze(parent, ctx),
            DataExpr::AddOp(x) => x.analyze(parent, ctx),
            DataExpr::SubOp(x) => x.analyze(parent, ctx),
            DataExpr::NegateOp(x) => x.analyze(parent, ctx),
            DataExpr::PropertyOp(x) => x.analyze(parent, ctx),
            DataExpr::StaticAssetConstructor(x) => x.analyze(parent, ctx),
            DataExpr::AnyAssetConstructor(x) => x.analyze(parent, ctx),
            DataExpr::MinUtxo(x) => x.analyze(parent, ctx),
            DataExpr::ConcatOp(x) => x.analyze(parent, ctx),
            _ => AnalyzeReport::default(),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            DataExpr::StructConstructor(x) => x.is_resolved(),
            DataExpr::ListConstructor(x) => x.is_resolved(),
            DataExpr::MapConstructor(x) => x.is_resolved(),
            DataExpr::Identifier(x) => x.is_resolved(),
            DataExpr::AddOp(x) => x.is_resolved(),
            DataExpr::SubOp(x) => x.is_resolved(),
            DataExpr::NegateOp(x) => x.is_resolved(),
            DataExpr::PropertyOp(x) => x.is_resolved(),
            DataExpr::StaticAssetConstructor(x) => x.is_resolved(),
            DataExpr::AnyAssetConstructor(x) => x.is_resolved(),
            DataExpr::MinUtxo(x) => x.is_resolved(),
            DataExpr::SlotToTime(x) => x.is_resolved(),
            DataExpr::TimeToSlot(x) => x.is_resolved(),
            DataExpr::ConcatOp(x) => x.is_resolved(),
            _ => true,
        }
    }
}

impl Analyzable for StaticAssetConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let amount = self.amount.analyze(parent.clone(), ctx);
        let r#type = self.r#type.analyze(parent.clone(), ctx);

        amount + r#type
    }

    fn is_resolved(&self) -> bool {
        self.amount.is_resolved() && self.r#type.is_resolved()
    }
}

impl Analyzable for AnyAssetConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let policy = self.policy.analyze(parent.clone(), ctx);
        let asset_name = self.asset_name.analyze(parent.clone(), ctx);
        let amount = self.amount.analyze(parent.clone(), ctx);

        policy + asset_name + amount
    }

    fn is_resolved(&self) -> bool {
        self.policy.is_resolved() && self.asset_name.is_resolved() && self.amount.is_resolved()
    }
}

impl Analyzable for PropertyOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let object = self.operand.analyze(parent.clone(), ctx);

        let mut scope = Scope::new(parent);

        if let Some(ty) = self.operand.target_type(None) {
            scope.track_record_fields_for_type(&ty);
        }

        self.scope = Some(Rc::new(scope));

        let path = self.property.analyze(self.scope.clone(), ctx);

        object + path
    }

    fn is_resolved(&self) -> bool {
        self.operand.is_resolved() && self.property.is_resolved()
    }
}

impl Analyzable for AddressExpr {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            AddressExpr::Identifier(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let policy = self.policy.analyze(parent.clone(), ctx);
        let asset_name = self.asset_name.analyze(parent.clone(), ctx);

        let policy_type = AnalyzeReport::expect_data_expr_type(&self.policy, &Type::Bytes, ctx);
        let asset_name_type =
            AnalyzeReport::expect_data_expr_type(&self.asset_name, &Type::Bytes, ctx);

        policy + asset_name + policy_type + asset_name_type
    }

    fn is_resolved(&self) -> bool {
        self.policy.is_resolved() && self.asset_name.is_resolved()
    }
}

impl Analyzable for Identifier {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, _ctx: &mut Context) -> AnalyzeReport {
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            Type::Custom(x) => x.analyze(parent, ctx),
            Type::List(x) => x.analyze(parent, ctx),
            Type::Map(key_type, value_type) => {
                key_type.analyze(parent.clone(), ctx) + value_type.analyze(parent, ctx)
            }
            _ => AnalyzeReport::default(),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            Type::Custom(x) => x.is_resolved(),
            Type::List(x) => x.is_resolved(),
            Type::Map(key_type, value_type) => key_type.is_resolved() && value_type.is_resolved(),
            _ => true,
        }
    }
}

impl Analyzable for InputBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            InputBlockField::From(x) => x.analyze(parent, ctx),
            InputBlockField::DatumIs(x) => x.analyze(parent, ctx),
            InputBlockField::MinAmount(x) => x.analyze(parent, ctx),
            InputBlockField::Redeemer(x) => x.analyze(parent, ctx),
            InputBlockField::Ref(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

fn validate_metadata_value_size(expr: &DataExpr) -> Result<(), MetadataSizeLimitError> {
    match expr {
        DataExpr::String(string_literal) => {
            let utf8_bytes = string_literal.value.as_bytes();
            if utf8_bytes.len() > METADATA_MAX_SIZE_BYTES {
                return Err(MetadataSizeLimitError {
                    size: utf8_bytes.len(),
                    src: None,
                    span: string_literal.span.clone(),
                });
            }
        }
        DataExpr::HexString(hex_literal) => {
            let hex_str = &hex_literal.value;
            let hex_str = hex_str.strip_prefix("0x").unwrap_or(hex_str);
            let byte_length = hex_str.len() / 2;

            if byte_length > METADATA_MAX_SIZE_BYTES {
                return Err(MetadataSizeLimitError {
                    size: byte_length,
                    src: None,
                    span: hex_literal.span.clone(),
                });
            }
        }
        _ => {}
    }
    Ok(())
}

fn validate_metadata_key_type(
    expr: &DataExpr,
    ctx: &mut Context,
) -> Result<(), MetadataInvalidKeyTypeError> {
    match expr {
        DataExpr::Number(_) => Ok(()),
        DataExpr::Identifier(id) => match id.target_type(Some(ctx)) {
            Some(Type::Int) => Ok(()),
            Some(other_type) => Err(MetadataInvalidKeyTypeError {
                key_type: format!("identifier of type {}", other_type),
                src: None,
                span: id.span().clone(),
            }),
            None => Err(MetadataInvalidKeyTypeError {
                key_type: "unresolved identifier".to_string(),
                src: None,
                span: id.span().clone(),
            }),
        },
        _ => {
            let key_type = match expr {
                DataExpr::String(_) => "string",
                DataExpr::HexString(_) => "hex string",
                DataExpr::ListConstructor(_) => "list",
                DataExpr::MapConstructor(_) => "map",
                DataExpr::StructConstructor(_) => "struct",
                _ => "unknown",
            };

            Err(MetadataInvalidKeyTypeError {
                key_type: key_type.to_string(),
                src: None,
                span: expr.span().clone(),
            })
        }
    }
}

impl Analyzable for MetadataBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let mut report =
            self.key.analyze(parent.clone(), ctx) + self.value.analyze(parent.clone(), ctx);

        validate_metadata_key_type(&self.key, ctx)
            .map_err(Error::MetadataInvalidKeyType)
            .err()
            .map(|e| report.errors.push(e));

        validate_metadata_value_size(&self.value)
            .map_err(Error::MetadataSizeLimitExceeded)
            .err()
            .map(|e| report.errors.push(e));

        report
    }

    fn is_resolved(&self) -> bool {
        self.key.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for MetadataBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for ValidityBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            ValidityBlockField::SinceSlot(x) => x.analyze(parent, ctx),
            ValidityBlockField::UntilSlot(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for OutputBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            OutputBlockField::To(x) => x.analyze(parent, ctx),
            OutputBlockField::Amount(x) => {
                ctx.target_type = Type::AnyAsset;

                let expr_report = x.analyze(parent, ctx);
                let ty = x.target_type(Some(ctx));

                let type_report = if !matches!(ty.as_ref(), Some(&Type::AnyAsset)) {
                    AnalyzeReport::from(Error::invalid_target_type(
                        &Type::AnyAsset,
                        ty.as_ref().unwrap_or(&Type::Undefined),
                        x.as_ref(),
                    ))
                } else {
                    AnalyzeReport::default()
                };

                expr_report + type_report
            }
            OutputBlockField::Datum(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        validate_optional_output(self)
            .map(AnalyzeReport::from)
            .unwrap_or_else(|| AnalyzeReport::default())
            + self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for RecordField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.r#type.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.r#type.is_resolved()
    }
}

impl Analyzable for VariantCase {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for AliasDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.alias_type.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.alias_type.is_resolved() && self.is_alias_chain_resolved()
    }
}

impl Analyzable for TypeDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.cases.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.cases.is_resolved()
    }
}

impl Analyzable for MintBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            MintBlockField::Amount(x) => x.analyze(parent, ctx),
            MintBlockField::Redeemer(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for SignersBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.signers.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.signers.is_resolved()
    }
}

impl Analyzable for ReferenceBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.r#ref.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.r#ref.is_resolved()
    }
}

impl Analyzable for CollateralBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            CollateralBlockField::From(x) => x.analyze(parent, ctx),
            CollateralBlockField::MinAmount(x) => x.analyze(parent, ctx),
            CollateralBlockField::Ref(x) => x.analyze(parent, ctx),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            CollateralBlockField::From(x) => x.is_resolved(),
            CollateralBlockField::MinAmount(x) => x.is_resolved(),
            CollateralBlockField::Ref(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for CollateralBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for ChainSpecificBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        match self {
            ChainSpecificBlock::Cardano(x) => x.analyze(parent, ctx),
        }
    }

    fn is_resolved(&self) -> bool {
        match self {
            ChainSpecificBlock::Cardano(x) => x.is_resolved(),
        }
    }
}

impl Analyzable for LocalsAssign {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.value.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.value.is_resolved()
    }
}

impl Analyzable for LocalsBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.assigns.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.assigns.is_resolved()
    }
}

impl Analyzable for ParamDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.r#type.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.r#type.is_resolved()
    }
}

impl Analyzable for ParameterList {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        self.parameters.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.parameters.is_resolved()
    }
}

impl Analyzable for TxDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        // analyze static types before anything else

        let params = self.parameters.analyze(parent.clone(), ctx);

        // create the new scope and populate its symbols

        let parent = {
            let mut current = Scope::new(parent.clone());

            current.symbols.insert("fees".to_string(), Symbol::Fees);

            for param in self.parameters.parameters.iter() {
                current.track_param_var(&param.name.value, param.r#type.clone());
            }
            Rc::new(current)
        };

        let mut locals = self.locals.take().unwrap_or_default();

        let locals_report = locals.analyze(Some(parent.clone()), ctx);

        let parent = {
            let mut current = Scope::new(Some(parent.clone()));

            for assign in locals.assigns.iter() {
                current.track_local_expr(&assign.name.value, assign.value.clone());
            }

            for (index, output) in self.outputs.iter().enumerate() {
                current.track_output(index, output.clone())
            }

            Rc::new(current)
        };

        let inputs = self.inputs.analyze(Some(parent.clone()), ctx);

        let parent = {
            let mut current = Scope::new(Some(parent.clone()));

            for input in self.inputs.iter() {
                current.track_input(&input.name, input.clone());
            }

            for (index, output) in self.outputs.iter().enumerate() {
                current.track_output(index, output.clone())
            }

            Rc::new(current)
        };

        let outputs = self.outputs.analyze(Some(parent.clone()), ctx);

        let mints = self.mints.analyze(Some(parent.clone()), ctx);

        let burns = self.burns.analyze(Some(parent.clone()), ctx);

        let adhoc = self.adhoc.analyze(Some(parent.clone()), ctx);

        let validity = self.validity.analyze(Some(parent.clone()), ctx);

        let metadata = self.metadata.analyze(Some(parent.clone()), ctx);

        let signers = self.signers.analyze(Some(parent.clone()), ctx);

        let references = self.references.analyze(Some(parent.clone()), ctx);

        let collateral = self.collateral.analyze(Some(parent.clone()), ctx);

        self.scope = Some(parent);

        params
            + locals_report
            + inputs
            + outputs
            + mints
            + burns
            + adhoc
            + validity
            + metadata
            + signers
            + references
            + collateral
    }

    fn is_resolved(&self) -> bool {
        self.inputs.is_resolved()
            && self.outputs.is_resolved()
            && self.mints.is_resolved()
            && self.locals.is_resolved()
            && self.adhoc.is_resolved()
            && self.validity.is_resolved()
            && self.metadata.is_resolved()
            && self.signers.is_resolved()
            && self.references.is_resolved()
            && self.collateral.is_resolved()
    }
}

fn ada_asset_def() -> AssetDef {
    AssetDef {
        name: Identifier {
            value: "Ada".to_string(),
            symbol: None,
            span: Span::DUMMY,
        },
        policy: DataExpr::None,
        asset_name: DataExpr::None,
        span: Span::DUMMY,
    }
}

fn resolve_types_and_aliases(
    scope_rc: &mut Rc<Scope>,
    types: &mut Vec<TypeDef>,
    aliases: &mut Vec<AliasDef>,
    ctx: &mut Context,
) -> (AnalyzeReport, AnalyzeReport) {
    let mut types_report = AnalyzeReport::default();
    let mut aliases_report = AnalyzeReport::default();

    let mut pass_count = 0usize;
    let max_passes = 100usize; // prevent infinite loops

    while pass_count < max_passes && !(types.is_resolved() && aliases.is_resolved()) {
        pass_count += 1;

        let scope = Rc::get_mut(scope_rc).expect("scope should be unique during resolution");

        for type_def in types.iter() {
            scope.track_type_def(type_def);
        }
        for alias_def in aliases.iter() {
            scope.track_alias_def(alias_def);
        }

        types_report = types.analyze(Some(scope_rc.clone()), ctx);
        aliases_report = aliases.analyze(Some(scope_rc.clone()), ctx);
    }

    (types_report, aliases_report)
}

impl Analyzable for Program {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &mut Context) -> AnalyzeReport {
        let mut scope = Scope::new(parent);

        if let Some(env) = self.env.take() {
            for field in env.fields.iter() {
                scope.track_env_var(&field.name, field.r#type.clone());
            }
        }

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

        for alias_def in self.aliases.iter() {
            scope.track_alias_def(alias_def);
        }

        self.scope = Some(Rc::new(scope));

        let parties = self.parties.analyze(self.scope.clone(), ctx);

        let policies = self.policies.analyze(self.scope.clone(), ctx);

        let assets = self.assets.analyze(self.scope.clone(), ctx);

        let mut types = self.types.clone();
        let mut aliases = self.aliases.clone();

        let scope_rc = self.scope.as_mut().unwrap();

        let (types, aliases) = resolve_types_and_aliases(scope_rc, &mut types, &mut aliases, ctx);

        let txs = self.txs.analyze(self.scope.clone(), ctx);

        parties + policies + types + aliases + txs + assets
    }

    fn is_resolved(&self) -> bool {
        self.policies.is_resolved()
            && self.types.is_resolved()
            && self.aliases.is_resolved()
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
    let mut ctx = Context::default();
    ast.analyze(None, &mut ctx)
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
                expected: "Bytes".to_string(),
                got: "Int".to_string(),
                src: None,
                span: Span::DUMMY,
            })
        );

        assert_eq!(
            report.errors[2],
            Error::InvalidTargetType(InvalidTargetTypeError {
                expected: "Bytes".to_string(),
                got: "Int".to_string(),
                src: None,
                span: Span::DUMMY,
            })
        );
    }

    #[test]
    fn test_min_utxo_analysis() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        tx test() {
            output my_output {
                to: Alice,
                amount: min_utxo(my_output),
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_alias_undefined_type_error() {
        let mut ast = crate::parsing::parse_string(
            r#"
        type MyAlias = UndefinedType;
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);

        assert!(!result.errors.is_empty());
        assert!(result
            .errors
            .iter()
            .any(|e| matches!(e, Error::NotInScope(_))));
    }

    #[test]
    fn test_alias_valid_type_success() {
        let mut ast = crate::parsing::parse_string(
            r#"
        type Address = Bytes;
        type Amount = Int;
        type ValidAlias = Address;
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);

        assert!(result.errors.is_empty());
    }

    #[test]
    fn test_min_utxo_undefined_output_error() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        tx test() {
            output {
                to: Alice,
                amount: min_utxo(nonexistent_output),
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_optional_output_with_datum_error() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        type MyDatum {
            field1: Int,
        }
        tx test() {
            output ? my_output {
                to: Alice,
                amount: Ada(1),
                datum: MyDatum { field1: 1, },
            }
        }
    "#,
        )
        .unwrap();
        let report = analyze(&mut ast);

        assert!(!report.errors.is_empty());
        assert!(report
            .errors
            .iter()
            .any(|e| matches!(e, Error::InvalidOptionalOutput(_))));
    }

    #[test]
    fn test_optional_output_ok() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        tx test() {
            output ? my_output {
                to: Alice,
                amount: Ada(0),
            }
        }
    "#,
        )
        .unwrap();

        let report = analyze(&mut ast);
        assert!(report.errors.is_empty());
    }

    #[test]
    fn test_output_amount_must_be_any_asset_type() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        tx test() {
            output my_output {
                to: Alice,
                amount: 123,
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(!result.errors.is_empty());

        assert_eq!(
            result.errors[0],
            Error::InvalidTargetType(InvalidTargetTypeError {
                expected: "AnyAsset".to_string(),
                got: "Int".to_string(),
                src: None,
                span: Span::DUMMY,
            })
        );
    }

    #[test]
    fn test_output_amount_accepts_any_asset_expressions() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        tx test(quantity: Int) {
            output {
                to: Alice,
                amount: AnyAsset(0x123, 0x456, 100),
            }
            output {
                to: Alice,
                amount: Ada(quantity),
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(result.errors.is_empty());
    }
    #[test]
    fn test_time_and_slot_conversion() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Sender;

        type TimestampDatum {
            slot_time: Int,
            time: Int,
        }

        tx create_timestamp_tx() {
            input source {
                from: Sender,
                min_amount: Ada(2000000),
            }

            output timestamp_output {
                to: Sender,
                amount: source - fees,
                datum: TimestampDatum {
                    slot_time: time_to_slot(1666716638000),
                    time: slot_to_time(60638),
                },
            }
        }
        "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(result.errors.is_empty());
    }
    #[test]
    fn test_output_mixed_amount_expressions() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        tx test() {
            input source {
                from: Alice,
                min_amount: Ada(100),
            }
            output {
                to: Alice,
                amount: source - 50,
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(!result.errors.is_empty());

        assert_eq!(
            result.errors[0],
            Error::InvalidTargetType(InvalidTargetTypeError {
                expected: "AnyAsset".to_string(),
                got: "Int".to_string(),
                src: None,
                span: Span::DUMMY,
            })
        );
    }

    #[test]
    fn test_metadata_value_size_validation_string_within_limit() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test() {
            metadata {
                123: "This is a short string that is within the 64-byte limit",
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);

        assert!(
            result.errors.is_empty(),
            "Expected no errors for string within limit, but got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_metadata_value_size_validation_string_exceeds_limit() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test() {
            metadata {
                123: "This is a very long string that definitely exceeds the 64-byte limit here",
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert_eq!(result.errors.len(), 1);

        match &result.errors[0] {
            Error::MetadataSizeLimitExceeded(error) => {
                assert_eq!(error.size, 73);
            }
            _ => panic!(
                "Expected MetadataSizeLimitExceeded error, got: {:?}",
                result.errors[0]
            ),
        }
    }

    #[test]
    fn test_metadata_value_size_validation_hex_string_within_limit() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test() {
            metadata {
                123: 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef,
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert!(
            result.errors.is_empty(),
            "Expected no errors for hex string within limit, but got: {:?}",
            result.errors
        );
    }

    #[test]
    fn test_metadata_value_size_validation_hex_string_exceeds_limit() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test() {
            metadata {
                123: 0x1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef1234567890abcdef12,
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert_eq!(result.errors.len(), 1);

        match &result.errors[0] {
            Error::MetadataSizeLimitExceeded(error) => {
                assert_eq!(error.size, 65);
            }
            _ => panic!(
                "Expected MetadataSizeLimitExceeded error, got: {:?}",
                result.errors[0]
            ),
        }
    }

    #[test]
    fn test_metadata_value_size_validation_multiple_fields() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test() {
            metadata {
                123: "Short string",
                456: "This is a very long string that definitely exceeds the 64-byte limit here",
                789: "Another short one",
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert_eq!(result.errors.len(), 1);

        match &result.errors[0] {
            Error::MetadataSizeLimitExceeded(error) => {
                assert_eq!(error.size, 73);
            }
            _ => panic!(
                "Expected MetadataSizeLimitExceeded error, got: {:?}",
                result.errors[0]
            ),
        }
    }

    #[test]
    fn test_metadata_value_size_validation_non_literal_expression() {
        let mut ast = crate::parsing::parse_string(
            r#"
        party Alice;
        tx test(my_param: Bytes) {
            metadata {
                123: my_param,
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        let metadata_errors: Vec<_> = result
            .errors
            .iter()
            .filter(|e| matches!(e, Error::MetadataSizeLimitExceeded(_)))
            .collect();
        assert!(
            metadata_errors.is_empty(),
            "Expected no metadata size errors for non-literal expressions"
        );
    }

    #[test]
    fn test_metadata_key_type_validation_string_key() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test() {
            metadata {
                "invalid_key": "some value",
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert_eq!(result.errors.len(), 1);

        match &result.errors[0] {
            Error::MetadataInvalidKeyType(error) => {
                assert_eq!(error.key_type, "string");
            }
            _ => panic!(
                "Expected MetadataInvalidKeyType error, got: {:?}",
                result.errors[0]
            ),
        }
    }

    #[test]
    fn test_metadata_key_type_validation_identifier_with_int_type() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test(my_key: Int) {
            metadata {
                my_key: "valid value",
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        let key_type_errors: Vec<_> = result
            .errors
            .iter()
            .filter(|e| matches!(e, Error::MetadataInvalidKeyType(_)))
            .collect();
        assert!(
            key_type_errors.is_empty(),
            "Expected no key type errors for Int parameter used as key"
        );
    }

    #[test]
    fn test_metadata_key_type_validation_identifier_with_wrong_type() {
        let mut ast = crate::parsing::parse_string(
            r#"
        tx test(my_key: Bytes) {
            metadata {
                my_key: "some value",
            }
        }
    "#,
        )
        .unwrap();

        let result = analyze(&mut ast);
        assert_eq!(result.errors.len(), 1);

        match &result.errors[0] {
            Error::MetadataInvalidKeyType(error) => {
                assert!(error.key_type.contains("identifier of type Bytes"));
            }
            _ => panic!(
                "Expected MetadataInvalidKeyType error, got: {:?}",
                result.errors[0]
            ),
        }
    }
}
