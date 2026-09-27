//! Language-neutral declaration planning.
//!
//! The planner walks parsed [`Shape`]s, asks the backend how to spell each
//! type, allocates names for anonymous nested shapes, and rejects identifier
//! collisions. Its output is a tree of [`Declaration`]s that the backend only
//! has to print. Alongside each member's type it records the member's
//! [`Encoding`], so a backend whose generated clients construct the SDK's
//! canonical argument values statically can spell that conversion without
//! looking at the schema again.

use std::collections::{BTreeMap, BTreeSet};

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
    Alias {
        /// Rendered type expression of the aliased shape.
        target: String,
        /// How a value of the aliased shape is converted.
        encoding: Encoding,
    },
}

#[derive(Debug, Clone, PartialEq)]
pub struct Member {
    /// Identifier as written in the schema, used for wire names.
    pub source: String,
    /// Normalized, escaped identifier.
    pub name: String,
    /// Rendered type expression.
    pub ty: String,
    /// How a value of this member becomes the SDK's canonical argument.
    pub encoding: Encoding,
}

/// How a value of a shape is converted to the SDK's canonical argument value,
/// mirroring the SDK's own type-directed encoder: scalars and builtins by
/// their tag, declared records, tuples and variants by the declaration's own
/// conversion, containers element by element.
#[derive(Debug, Clone, PartialEq)]
pub enum Encoding {
    Scalar(Scalar),
    Builtin(Builtin),
    /// A declared record, tuple or variant, named by its declaration.
    Declared(String),
    /// A reference to a top-level component by its declared name. Resolved to
    /// [`Encoding::Declared`], or to the target encoding of an alias, once
    /// every component is planned; only a params-only plan leaves it as is.
    Component(String),
    List(Box<Encoding>),
    Map(Box<Encoding>),
    /// The value already has the SDK's fallback type; it is passed through.
    Fallback,
}

/// A rendered type expression together with its conversion.
struct Typed {
    ty: String,
    encoding: Encoding,
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
        resolve_components(&mut declarations, self.backend.declares_aliases());
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
                let name = params_type_name(self.backend, source)?;
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
        resolve_components(&mut declarations, self.backend.declares_aliases());
        Ok(declarations)
    }

    /// Spells the type of `shape`. A compound shape is referenced by the name
    /// derived from `hint` when the backend declares nested shapes; without a
    /// hint it uses the backend's undeclared spelling.
    pub fn type_of(&mut self, shape: &Shape, hint: Option<&str>) -> Result<String> {
        Ok(self.type_expr(shape, hint, None)?.ty)
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
                    let typed = self.type_expr(item, Some(&hint), Some(&mut children))?;
                    members.push(Member {
                        source: format!("item{index}"),
                        name: format!("item{index}"),
                        ty: typed.ty,
                        encoding: typed.encoding,
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
            other => {
                let typed = self.type_expr(other, Some(&name), Some(&mut children))?;
                DeclKind::Alias {
                    target: typed.ty,
                    encoding: typed.encoding,
                }
            }
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
            let typed = self.type_expr(&field.shape, Some(&hint), Some(children))?;
            members.push(Member {
                source: field.name.to_string(),
                name,
                ty: typed.ty,
                encoding: typed.encoding,
            });
        }
        Ok(members)
    }

    fn type_expr(
        &mut self,
        shape: &Shape,
        hint: Option<&str>,
        mut children: Option<&mut Children>,
    ) -> Result<Typed> {
        let backend = self.backend;
        let typed = match shape {
            Shape::Scalar(scalar) => {
                self.usage.scalars.insert(*scalar);
                Typed {
                    ty: backend.scalar(*scalar).to_string(),
                    encoding: Encoding::Scalar(*scalar),
                }
            }
            Shape::Builtin(builtin) => {
                self.usage.builtins.insert(*builtin);
                Typed {
                    ty: backend.builtin(*builtin).to_string(),
                    encoding: Encoding::Builtin(*builtin),
                }
            }
            Shape::Component(source) => {
                let name = identifier(backend, source, Role::Type)?;
                Typed {
                    encoding: Encoding::Component(name.clone()),
                    ty: name,
                }
            }
            Shape::Unknown => {
                self.usage.fallback = true;
                Typed {
                    ty: backend.fallback().to_string(),
                    encoding: Encoding::Fallback,
                }
            }
            Shape::List(item) => {
                let hint = hint.map(|hint| format!("{hint}Element"));
                let item = self.type_expr(item, hint.as_deref(), children.as_deref_mut())?;
                Typed {
                    ty: backend.list(&item.ty),
                    encoding: Encoding::List(Box::new(item.encoding)),
                }
            }
            Shape::Map(value) => {
                let hint = hint.map(|hint| format!("{hint}Value"));
                let value = self.type_expr(value, hint.as_deref(), children.as_deref_mut())?;
                Typed {
                    ty: backend.map(&value.ty),
                    encoding: Encoding::Map(Box::new(value.encoding)),
                }
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
                        Typed {
                            encoding: Encoding::Declared(name.clone()),
                            ty: name,
                        }
                    }
                    _ => {
                        self.usage.fallback = true;
                        Typed {
                            ty: backend.undeclared(shape),
                            encoding: Encoding::Fallback,
                        }
                    }
                }
            }
        };
        Ok(typed)
    }
}

/// Resolves every [`Encoding::Component`] in `declarations` against the
/// top-level declarations of the same plan: a component declared as a record,
/// tuple or variant converts itself, while an alias converts as its target.
/// A backend that declares aliases as types of their own
/// ([`Backend::declares_aliases`]) has them convert themselves too.
fn resolve_components(declarations: &mut [Declaration], declared_aliases: bool) {
    let aliases: BTreeMap<String, Encoding> = declarations
        .iter()
        .filter(|_| !declared_aliases)
        .filter_map(|declaration| match &declaration.kind {
            DeclKind::Alias { encoding, .. } => Some((declaration.name.clone(), encoding.clone())),
            _ => None,
        })
        .collect();

    fn resolve(encoding: &mut Encoding, aliases: &BTreeMap<String, Encoding>) {
        resolve_through(encoding, aliases, &mut BTreeSet::new());
    }

    /// `visited` follows the one path from a member down through containers,
    /// so an alias met twice on it, directly or inside a list or map, is a
    /// cycle.
    fn resolve_through(
        encoding: &mut Encoding,
        aliases: &BTreeMap<String, Encoding>,
        visited: &mut BTreeSet<String>,
    ) {
        while let Encoding::Component(name) = encoding {
            // An alias cycle cannot be declared in any language; the value is
            // passed through rather than looping.
            if !visited.insert(name.clone()) {
                *encoding = Encoding::Fallback;
                return;
            }
            *encoding = match aliases.get(name) {
                Some(target) => target.clone(),
                None => Encoding::Declared(name.clone()),
            };
        }
        match encoding {
            Encoding::List(item) | Encoding::Map(item) => resolve_through(item, aliases, visited),
            _ => {}
        }
    }

    fn resolve_declaration(declaration: &mut Declaration, aliases: &BTreeMap<String, Encoding>) {
        match &mut declaration.kind {
            DeclKind::Record(members) | DeclKind::Tuple(members) => {
                for member in members {
                    resolve(&mut member.encoding, aliases);
                }
            }
            DeclKind::Variant(cases) => {
                for case in cases {
                    for member in &mut case.fields {
                        resolve(&mut member.encoding, aliases);
                    }
                    for nested in &mut case.nested {
                        resolve_declaration(nested, aliases);
                    }
                }
            }
            DeclKind::Alias { encoding, .. } => {
                // The alias's own name is already on the path.
                let mut visited = BTreeSet::from([declaration.name.clone()]);
                resolve_through(encoding, aliases, &mut visited)
            }
        }
        for nested in &mut declaration.nested {
            resolve_declaration(nested, aliases);
        }
    }

    for declaration in declarations {
        resolve_declaration(declaration, &aliases);
    }
}

/// Name of the params declaration for transaction `source`. Templates reach
/// it through the `paramsTypeName` helper so references always match.
pub fn params_type_name(backend: &dyn Backend, source: &str) -> Result<String> {
    let base = identifier(backend, source, Role::Type)?;
    identifier(backend, &format!("{base}Params"), Role::Type)
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
