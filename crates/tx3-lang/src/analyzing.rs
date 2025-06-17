//! Semantic analysis of the Tx3 language.
//!
//! This module takes an AST and performs semantic analysis on it. It checks for
//! duplicate definitions, unknown symbols, and other semantic errors.

use std::{collections::HashMap, rc::Rc};

use crate::ast::*;

#[derive(Clone, Debug)]
pub struct AnalyzeContext {
    pub source: Option<ropey::Rope>,
}

impl AnalyzeContext {
    pub fn new(source: Option<ropey::Rope>) -> Self {
        Self {
            source,
        }
    }

    pub fn get_source_slice_context(&self, span: &Span) -> Option<(String, Span)> {
        if self.source.is_none() || span.is_dummy() {
            return None;
        }

        let source = self.source.as_ref().unwrap();

        if span.start >= source.len_chars() {
            return None;
        }

        // Search line for error
        let error_line = source.char_to_line(span.start);
        // Get the previous line if it exists
        let start_line = error_line.saturating_sub(1); 
        // Get the next line if it exists
        let end_line = std::cmp::min(error_line + 1, source.len_lines().saturating_sub(1));

        // Original start/end positions on the source code using prev and next lines
        let context_start = source.line_to_char(start_line);
        let context_end = if end_line < source.len_lines() - 1 {
            source.line_to_char(end_line + 1)
        } else {
            source.len_chars()
        };

        // Extract the context text
        let context_text = source.slice(context_start..context_end).to_string();

        // To the original span, we need to adjust the start position to the new context_text
        let relative_start = span.start.saturating_sub(context_start);
        let relative_end = std::cmp::min(
            span.end.saturating_sub(context_start),
            context_text.len()
        );

        // We create a new span relative to the context text
        let context_span = Span::new(relative_start, relative_end);

        Some((context_text, context_span))

    }
}

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

    pub fn not_in_scope(name: String, ast: &impl crate::parsing::AstNode, ctx: &AnalyzeContext) -> Self {
        let (src, span) = ctx.get_source_slice_context(&ast.span())
            .map(|(src, span)| (Some(src), span))
            .unwrap_or_else(|| (None, ast.span().clone()));

        Self::NotInScope(NotInScopeError {
            name,
            src,
            span,
        })
    }

    pub fn invalid_symbol(
        expected: &'static str,
        got: &Symbol,
        ast: &impl crate::parsing::AstNode,
        ctx: &AnalyzeContext,
    ) -> Self {
        let (src, span) = ctx.get_source_slice_context(&ast.span())
            .map(|(src, span)| (Some(src), span))
            .unwrap_or_else(|| (None, ast.span().clone()));

        Self::InvalidSymbol(InvalidSymbolError {
            expected,
            got: format!("{:?}", got),
            src,
            span,
        })
    }

    pub fn invalid_target_type(
        expected: &'static str,
        got: &Type,
        ast: &impl crate::parsing::AstNode,
        ctx: &AnalyzeContext,
    ) -> Self {
        let (src, span) = ctx.get_source_slice_context(&ast.span())
            .map(|(src, span)| (Some(src), span))
            .unwrap_or_else(|| (None, ast.span().clone()));

        Self::InvalidTargetType(InvalidTargetTypeError {
            expected,
            got: format!("{:?}", got),
            src,
            span,
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport;

    /// Returns true if all of the symbols have been resolved .
    fn is_resolved(&self) -> bool;
}

impl<T: Analyzable> Analyzable for Option<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.as_mut().analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.as_ref().is_resolved()
    }
}

impl<T: Analyzable> Analyzable for Vec<T> {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.iter_mut()
            .map(|item| item.analyze(parent.clone(), ctx))
            .collect()
    }

    fn is_resolved(&self) -> bool {
        self.iter().all(|x| x.is_resolved())
    }
}

impl Analyzable for PolicyField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for PolicyDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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

impl Analyzable for DataBinaryOp {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let left = self.left.analyze(parent.clone(), ctx);
        let right = self.right.analyze(parent.clone(), ctx);

        left + right
    }

    fn is_resolved(&self) -> bool {
        self.left.is_resolved() && self.right.is_resolved()
    }
}

impl Analyzable for RecordConstructorField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let name = self.name.analyze(parent.clone(), ctx);
        let value = self.value.analyze(parent.clone(), ctx);

        name + value
    }

    fn is_resolved(&self) -> bool {
        self.name.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for VariantCaseConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let name = self.name.analyze(parent.clone(), ctx);

        let mut scope = Scope::new(parent);

        let case = match &self.name.symbol {
            Some(Symbol::VariantCase(x)) => x,
            Some(x) => bail_report!(Error::invalid_symbol("VariantCase", x, &self.name, ctx)),
            None => bail_report!(Error::not_in_scope(self.name.value.clone(), &self.name, ctx)),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let r#type = self.r#type.analyze(parent.clone(), ctx);

        let mut scope = Scope::new(parent);

        let type_def = match &self.r#type.symbol {
            Some(Symbol::TypeDef(x)) => x,
            Some(x) => bail_report!(Error::invalid_symbol("TypeDef", x, &self.r#type, ctx)),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.elements.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.elements.is_resolved()
    }
}

impl Analyzable for DataExpr {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        match self {
            DataExpr::StructConstructor(x) => x.analyze(parent, ctx),
            DataExpr::ListConstructor(x) => x.analyze(parent, ctx),
            DataExpr::Identifier(x) => x.analyze(parent, ctx),
            DataExpr::PropertyAccess(x) => x.analyze(parent, ctx),
            DataExpr::BinaryOp(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let left = self.left.analyze(parent.clone(), ctx);
        let right = self.right.analyze(parent.clone(), ctx);

        left + right
    }

    fn is_resolved(&self) -> bool {
        self.left.is_resolved() && self.right.is_resolved()
    }
}

impl Analyzable for StaticAssetConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let amount = self.amount.analyze(parent.clone(), ctx);
        let r#type = self.r#type.analyze(parent.clone(), ctx);

        amount + r#type
    }

    fn is_resolved(&self) -> bool {
        self.amount.is_resolved() && self.r#type.is_resolved()
    }
}

impl Analyzable for AnyAssetConstructor {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let policy = self.policy.analyze(parent.clone(), ctx);
        let asset_name = self.asset_name.analyze(parent.clone(), ctx);
        let amount = self.amount.analyze(parent.clone(), ctx);

        policy + asset_name + amount
    }

    fn is_resolved(&self) -> bool {
        self.policy.is_resolved() && self.asset_name.is_resolved() && self.amount.is_resolved()
    }
}

impl Analyzable for PropertyAccess {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let object = self.object.analyze(parent.clone(), ctx);

        let mut scope = Scope::new(parent);

        if let Some(ty) = self.object.symbol.as_ref().and_then(|s| s.target_type()) {
            scope.track_record_fields_for_type(&ty);
        }

        self.scope = Some(Rc::new(scope));

        let path = self.path.analyze(self.scope.clone(), ctx);

        object + path
    }

    fn is_resolved(&self) -> bool {
        self.object.is_resolved() && self.path.is_resolved()
    }
}

impl Analyzable for AssetExpr {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        match self {
            AssetExpr::Identifier(x) => x.analyze(parent, ctx),
            AssetExpr::StaticConstructor(x) => x.analyze(parent, ctx),
            AssetExpr::AnyConstructor(x) => x.analyze(parent, ctx),
            AssetExpr::BinaryOp(x) => x.analyze(parent, ctx),
            AssetExpr::PropertyAccess(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let policy = self.policy.analyze(parent.clone(), ctx);
        let asset_name = self.asset_name.analyze(parent.clone(), ctx);

        let policy_type = self.policy.target_type();
        let asset_name_type = self.asset_name.target_type();

        let policy_type = if policy_type != Some(Type::Bytes) {
            AnalyzeReport::from(Error::invalid_target_type(
                "Bytes",
                policy_type.as_ref().unwrap_or(&Type::Undefined),
                &self.policy,
                ctx,
            ))
        } else {
            AnalyzeReport::default()
        };

        let asset_name_type = if asset_name_type != Some(Type::Bytes) {
            AnalyzeReport::from(Error::invalid_target_type(
                "Bytes",
                asset_name_type.as_ref().unwrap_or(&Type::Undefined),
                &self.asset_name,
                ctx,
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        let symbol = parent.and_then(|p| p.resolve(&self.value));

        if symbol.is_none() {
            bail_report!(Error::not_in_scope(self.value.clone(), self, ctx));
        }

        self.symbol = symbol;

        AnalyzeReport::default()
    }

    fn is_resolved(&self) -> bool {
        self.symbol.is_some()
    }
}

impl Analyzable for Type {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        match self {
            Type::Custom(x) => x.analyze(parent, ctx),
            Type::List(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for MetadataBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        // TODO: check keys are actually numbers
        self.key.analyze(parent.clone(), ctx) + self.value.analyze(parent.clone(), ctx)
    }
    fn is_resolved(&self) -> bool {
        self.key.is_resolved() && self.value.is_resolved()
    }
}

impl Analyzable for MetadataBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }
    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for ValidityBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }
    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for OutputBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        match self {
            OutputBlockField::To(x) => x.analyze(parent, ctx),
            OutputBlockField::Amount(x) => x.analyze(parent, ctx),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for RecordField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.r#type.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.r#type.is_resolved()
    }
}

impl Analyzable for VariantCase {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for TypeDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.cases.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.cases.is_resolved()
    }
}

impl Analyzable for MintBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.fields.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for SignersBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        self.signers.analyze(parent, ctx)
    }

    fn is_resolved(&self) -> bool {
        self.signers.is_resolved()
    }
}

impl Analyzable for ReferenceBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.r#ref.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.r#ref.is_resolved()
    }
}

impl Analyzable for CollateralBlockField {
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        match self {
            CollateralBlockField::From(x) => x.analyze(parent),
            CollateralBlockField::MinAmount(x) => x.analyze(parent),
            CollateralBlockField::Ref(x) => x.analyze(parent),
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>) -> AnalyzeReport {
        self.fields.analyze(parent)
    }

    fn is_resolved(&self) -> bool {
        self.fields.is_resolved()
    }
}

impl Analyzable for ChainSpecificBlock {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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

impl Analyzable for TxDef {
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
        // analyze static types before anything else

        let params = self
            .parameters
            .parameters
            .iter_mut()
            .map(|param| param.r#type.analyze(parent.clone(), ctx))
            .collect::<AnalyzeReport>();

        let input_types = self
            .inputs
            .iter_mut()
            .flat_map(|input| input.fields.iter_mut())
            .map(|field| match field {
                InputBlockField::DatumIs(x) => x.analyze(parent.clone(), ctx),
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

        let inputs = self.inputs.analyze(self.scope.clone(), ctx);

        let outputs = self.outputs.analyze(self.scope.clone(), ctx);

        let mints = self.mints.analyze(self.scope.clone(), ctx);

        let adhoc = self.adhoc.analyze(self.scope.clone(), ctx);

        let validity = self.validity.analyze(self.scope.clone(), ctx);

        let metadata = self.metadata.analyze(self.scope.clone(), ctx);

        let signers = self.signers.analyze(self.scope.clone(), ctx);

        let references = self.references.analyze(self.scope.clone());

        let collateral = self.collateral.analyze(self.scope.clone());

        params
            + input_types
            + inputs
            + outputs
            + mints
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
    fn analyze(&mut self, parent: Option<Rc<Scope>>, ctx: &AnalyzeContext) -> AnalyzeReport {
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

        let policies = self.policies.analyze(self.scope.clone(), ctx);

        let assets = self.assets.analyze(self.scope.clone(), ctx);

        let types = self.types.analyze(self.scope.clone(), ctx);

        let txs = self.txs.analyze(self.scope.clone(), ctx);

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
    // Create a new analysis context with the source code
    // This context can be used to retrieve source code slices for error reporting
    let ctx = AnalyzeContext::new(ast.source_code.clone());

    ast.analyze(None, &ctx)
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
                src: Some("    output {\n        to: missing_symbol,\n    }\n".to_string()),
                span: Span::new(25, 39),
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
