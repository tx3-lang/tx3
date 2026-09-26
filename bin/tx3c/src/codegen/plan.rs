//! Language-neutral declaration planning.
//!
//! The planner walks parsed [`Shape`]s, asks the backend how to spell each
//! type, allocates names for anonymous nested shapes, and rejects identifier
//! collisions. Its output is a tree of [`Declaration`]s that the backend only
//! has to print.

use std::collections::BTreeSet;

use anyhow::{Context, Result};
use convert_case::Case;
use serde_json::Value;

use super::{
    backend::{Backend, FieldOrder, Placement},
    names::{identifier, identifier_in, Role, Scope},
    schema::{Builtin, Field, Scalar, Shape},
};

#[derive(Debug, Clone, PartialEq)]
pub struct Declaration {
    pub name: String,
    /// Source transaction name when this declares a transaction's params.
    pub params_of: Option<String>,
    pub kind: DeclKind,
    /// Declarations for anonymous shapes used by this one. Only populated for
    /// [`Placement::Nested`]; hoisting backends receive them at top level.
    pub nested: Vec<Declaration>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum DeclKind {
    Record(Vec<Member>),
    /// Positional members named `item0`, `item1`, and so on.
    Tuple(Vec<Member>),
    Variant(Vec<VariantCase>),
    /// A named declaration for a shape that is not a record, tuple, or variant.
    Alias(String),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    /// Identifier as written in the schema, used for wire names.
    pub source: String,
    /// Normalized, escaped identifier.
    pub name: String,
    /// Rendered type expression.
    pub ty: String,
}

#[derive(Debug, Clone, PartialEq)]
pub struct VariantCase {
    pub source: String,
    pub name: String,
    pub fields: Vec<Member>,
    /// Declarations nested inside this case. Only populated for
    /// [`Placement::Nested`].
    pub nested: Vec<Declaration>,
}

/// Which native and SDK types the planned output refers to.
#[derive(Debug, Default)]
pub struct Usage {
    pub scalars: BTreeSet<Scalar>,
    pub builtins: BTreeSet<Builtin>,
    pub fallback: bool,
}

/// Declarations collected while planning one parent declaration.
struct Children {
    declarations: Vec<Declaration>,
    scope: Scope,
}

impl Children {
    fn new(parent: &str) -> Self {
        Self {
            declarations: Vec::new(),
            scope: Scope::new(format!("declaration {parent}")),
        }
    }
}

pub struct Planner {
    backend: &'static dyn Backend,
    usage: Usage,
    top: Scope,
}

impl Planner {
    pub fn new(backend: &'static dyn Backend) -> Self {
        Self {
            backend,
            usage: Usage::default(),
            top: Scope::new("top-level declarations"),
        }
    }

    pub fn usage(&self) -> &Usage {
        &self.usage
    }

    /// Plans every entry of `components.schemas`, sorted by name.
    pub fn components(&mut self, schemas: &Value) -> Result<Vec<Declaration>> {
        let Some(schemas) = schemas.as_object() else {
            return Ok(Vec::new());
        };

        let mut names: Vec<&String> = schemas.keys().collect();
        names.sort();

        let mut declarations = Vec::with_capacity(names.len());
        for source in names {
            let mut plan = || -> Result<Declaration> {
                let shape = Shape::parse(&schemas[source])?;
                let name = self.claim_top(source, identifier(self.backend, source, Role::Type)?)?;
                self.declaration(name, &shape, None)
            };
            declarations.push(plan().with_context(|| format!("component `{source}`"))?);
        }
        Ok(declarations)
    }

    /// Plans one `<Transaction>Params` declaration per transaction, sorted by
    /// transaction name.
    pub fn params(&mut self, tii: &Value) -> Result<Vec<Declaration>> {
        let Some(transactions) = tii.get("transactions").and_then(Value::as_object) else {
            return Ok(Vec::new());
        };

        let mut names: Vec<&String> = transactions.keys().collect();
        names.sort();

        let mut declarations = Vec::with_capacity(names.len());
        for source in names {
            let Some(params) = transactions[source].get("params") else {
                continue;
            };
            let mut plan = || -> Result<Declaration> {
                let shape = Shape::parse(params)?;
                let base = identifier(self.backend, source, Role::Type)?;
                let name = identifier(self.backend, &format!("{base}Params"), Role::Type)?;
                let name = self.claim_top(source, name)?;
                self.declaration(name, &shape, Some(source))
            };
            declarations.push(plan().with_context(|| format!("transaction `{source}` params"))?);
        }
        Ok(declarations)
    }

    /// Plans components followed by transaction params.
    pub fn document(&mut self, tii: &Value) -> Result<Vec<Declaration>> {
        let mut declarations =
            self.components(tii.pointer("/components/schemas").unwrap_or(&Value::Null))?;
        declarations.extend(self.params(tii)?);
        Ok(declarations)
    }

    /// Spells the type of `shape`. A compound shape is referenced by the name
    /// derived from `hint` when the backend declares nested shapes; without a
    /// hint it uses the backend's undeclared spelling.
    pub fn type_of(&mut self, shape: &Shape, hint: Option<&str>) -> Result<String> {
        self.type_expr(shape, hint, None)
    }

    /// Prints planned declarations with the backend.
    pub fn render(&self, declarations: Vec<Declaration>) -> String {
        let declarations = match self.backend.placement() {
            Placement::Hoisted => {
                let mut flat = Vec::new();
                hoist(declarations, &mut flat);
                flat
            }
            Placement::None | Placement::Nested => declarations,
        };
        self.backend.join(
            declarations
                .iter()
                .map(|declaration| self.backend.declaration(declaration))
                .collect(),
        )
    }

    fn claim_top(&mut self, source: &str, name: String) -> Result<String> {
        self.top.claim(self.backend, source, &name)?;
        Ok(name)
    }

    fn declaration(
        &mut self,
        name: String,
        shape: &Shape,
        params_of: Option<&str>,
    ) -> Result<Declaration> {
        let mut children = Children::new(&name);
        let kind = match shape {
            Shape::Record(fields) => {
                let role = if params_of.is_some() {
                    Role::Param
                } else {
                    Role::Field
                };
                let label = format!("record {name}");
                DeclKind::Record(self.members(&name, fields, role, &label, &mut children)?)
            }
            Shape::Tuple(items) => {
                let mut members = Vec::with_capacity(items.len());
                for (index, item) in items.iter().enumerate() {
                    let hint = format!("{name}Item{index}");
                    members.push(Member {
                        source: format!("item{index}"),
                        name: format!("item{index}"),
                        ty: self.type_expr(item, Some(&hint), Some(&mut children))?,
                    });
                }
                DeclKind::Tuple(members)
            }
            Shape::Variant(cases) => {
                let mut scope = Scope::new(format!("variant {name}"));
                let mut planned = Vec::with_capacity(cases.len());
                for case in cases {
                    let case_name = identifier(self.backend, case.tag, Role::Case)?;
                    scope.claim(self.backend, case.tag, &case_name)?;
                    let tag = identifier_in(self.backend, case.tag, Case::Pascal)?;

                    // Nested backends declare a case's anonymous shapes inside
                    // the case, prefixed by its own name; hoisting backends
                    // prefix them with the variant and case names.
                    let (prefix, mut own_children) = match self.backend.placement() {
                        Placement::Nested => (tag.clone(), Some(Children::new(&tag))),
                        Placement::None | Placement::Hoisted => (format!("{name}{tag}"), None),
                    };
                    let label = format!("variant {name} case {case_name}");
                    let fields = self.members(
                        &prefix,
                        &case.fields,
                        Role::Field,
                        &label,
                        own_children.as_mut().unwrap_or(&mut children),
                    )?;
                    planned.push(VariantCase {
                        source: case.tag.to_string(),
                        name: case_name,
                        fields,
                        nested: own_children
                            .map(|children| children.declarations)
                            .unwrap_or_default(),
                    });
                }
                DeclKind::Variant(planned)
            }
            other => DeclKind::Alias(self.type_expr(other, Some(&name), Some(&mut children))?),
        };

        Ok(Declaration {
            name,
            params_of: params_of.map(str::to_string),
            kind,
            nested: children.declarations,
        })
    }

    fn members(
        &mut self,
        prefix: &str,
        fields: &[Field],
        role: Role,
        label: &str,
        children: &mut Children,
    ) -> Result<Vec<Member>> {
        let mut ordered: Vec<&Field> = fields.iter().collect();
        if self.backend.field_order() == FieldOrder::Alphabetical {
            ordered.sort_by_key(|field| field.name);
        }

        let mut scope = Scope::new(label);
        let mut members = Vec::with_capacity(ordered.len());
        for field in ordered {
            let name = identifier(self.backend, field.name, role)?;
            scope.claim(self.backend, field.name, &name)?;
            let hint = format!(
                "{prefix}{}",
                identifier_in(self.backend, field.name, Case::Pascal)?
            );
            members.push(Member {
                source: field.name.to_string(),
                name,
                ty: self.type_expr(&field.shape, Some(&hint), Some(children))?,
            });
        }
        Ok(members)
    }

    fn type_expr(
        &mut self,
        shape: &Shape,
        hint: Option<&str>,
        mut children: Option<&mut Children>,
    ) -> Result<String> {
        let backend = self.backend;
        let ty = match shape {
            Shape::Scalar(scalar) => {
                self.usage.scalars.insert(*scalar);
                backend.scalar(*scalar).to_string()
            }
            Shape::Builtin(builtin) => {
                self.usage.builtins.insert(*builtin);
                backend.builtin(*builtin).to_string()
            }
            Shape::Component(source) => identifier(backend, source, Role::Type)?,
            Shape::Unknown => {
                self.usage.fallback = true;
                backend.fallback().to_string()
            }
            Shape::List(item) => {
                let hint = hint.map(|hint| format!("{hint}Element"));
                let item = self.type_expr(item, hint.as_deref(), children.as_deref_mut())?;
                backend.list(&item)
            }
            Shape::Map(value) => {
                let hint = hint.map(|hint| format!("{hint}Value"));
                let value = self.type_expr(value, hint.as_deref(), children.as_deref_mut())?;
                backend.map(&value)
            }
            Shape::Tuple(_) | Shape::Record(_) | Shape::Variant(_) => {
                match (backend.placement(), hint) {
                    (Placement::Nested | Placement::Hoisted, Some(hint)) => {
                        let name = identifier(backend, hint, Role::Type)?;
                        if let Some(children) = children {
                            match backend.placement() {
                                Placement::Hoisted => self.top.claim(backend, hint, &name)?,
                                _ => children.scope.claim(backend, hint, &name)?,
                            }
                            let declaration = self.declaration(name.clone(), shape, None)?;
                            children.declarations.push(declaration);
                        }
                        name
                    }
                    _ => {
                        self.usage.fallback = true;
                        backend.undeclared(shape)
                    }
                }
            }
        };
        Ok(ty)
    }
}

/// Moves nested declarations to top level, each before its parent.
fn hoist(declarations: Vec<Declaration>, out: &mut Vec<Declaration>) {
    for mut declaration in declarations {
        hoist(std::mem::take(&mut declaration.nested), out);
        if let DeclKind::Variant(cases) = &mut declaration.kind {
            for case in cases {
                hoist(std::mem::take(&mut case.nested), out);
            }
        }
        out.push(declaration);
    }
}
