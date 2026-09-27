//! Handlebars helpers exposed to codegen templates.

use anyhow::{anyhow, Result};
use convert_case::{Case, Casing};
use handlebars::{
    Context as HbContext, Handlebars, Helper, HelperDef, Output, RenderContext, RenderErrorReason,
};
use serde_json::Value;

use super::{
    backend,
    names::{identifier, Role},
    plan::{params_type_name, DeclKind, Planner},
    schema::Shape,
};

/// Wraps a fallible function of the helper's positional arguments.
fn helper<F>(name: &'static str, f: F) -> impl HelperDef + Send + Sync + 'static
where
    F: Fn(&[&Value]) -> Result<String> + Send + Sync + 'static,
{
    move |h: &Helper, _: &Handlebars, _: &HbContext, _: &mut RenderContext, out: &mut dyn Output| {
        let args: Vec<&Value> = h.params().iter().map(|param| param.value()).collect();
        let rendered =
            f(&args).map_err(|error| RenderErrorReason::Other(format!("{name}: {error:#}")))?;
        out.write(&rendered)?;
        Ok(())
    }
}

fn arg<'a>(args: &[&'a Value], index: usize) -> Result<&'a Value> {
    args.get(index)
        .copied()
        .ok_or_else(|| anyhow!("missing argument {index}"))
}

fn str_arg<'a>(args: &[&'a Value], index: usize) -> Result<&'a str> {
    arg(args, index)?
        .as_str()
        .ok_or_else(|| anyhow!("argument {index} must be a string"))
}

/// `schemaTypeFor <schema> <language> [<name hint>]`
fn schema_type_for(args: &[&Value]) -> Result<String> {
    let backend = backend::for_language(str_arg(args, 1)?)?;
    let shape = Shape::parse(arg(args, 0)?)?;
    let hint = args.get(2).and_then(|hint| hint.as_str());
    Planner::new(backend).type_of(&shape, hint)
}

/// `declarations <tii> <language>`: every component and transaction params
/// declaration, including nested declarations for anonymous shapes.
fn declarations(tii: &Value, language: &str) -> Result<String> {
    let mut planner = Planner::new(backend::for_language(language)?);
    let declarations = planner.document(tii)?;
    Ok(planner.render(declarations))
}

/// `imports <tii> <language>`: import lines needed by `declarations`.
fn imports(tii: &Value, language: &str) -> Result<String> {
    let backend = backend::for_language(language)?;
    let mut planner = Planner::new(backend);
    planner.document(tii)?;
    Ok(backend.imports(planner.usage()))
}

/// `usesModule <tii> <language> <module>`: whether the import lines of
/// `declarations` name `module`. Renders `true` or nothing, for `#if`.
fn uses_module(tii: &Value, language: &str, module: &str) -> Result<String> {
    let backend = backend::for_language(language)?;
    let mut planner = Planner::new(backend);
    planner.document(tii)?;
    Ok(if backend.modules(planner.usage()).contains(&module) {
        "true".to_string()
    } else {
        String::new()
    })
}

/// `argValue <tii> <transaction> <param> <language> <params-expr>`: the SDK's
/// canonical argument value for one transaction parameter, read from the
/// params value `params-expr`. The expression is derived from the same plan
/// as the params declaration, so it always matches the declared field types.
fn arg_value(
    tii: &Value,
    transaction: &str,
    param: &str,
    language: &str,
    params: &str,
) -> Result<String> {
    let backend = backend::for_language(language)?;
    let mut planner = Planner::new(backend);
    let declarations = planner.document(tii)?;
    let declaration = declarations
        .iter()
        .find(|declaration| declaration.params_of.as_deref() == Some(transaction))
        .ok_or_else(|| anyhow!("transaction `{transaction}` declares no params"))?;
    let member = match &declaration.kind {
        DeclKind::Record(members) => members.iter().find(|member| member.source == param),
        _ => None,
    }
    .ok_or_else(|| anyhow!("transaction `{transaction}` has no `{param}` parameter"))?;
    backend.argument(&member.encoding, &backend.member(params, &member.name))
}

/// `profile <profile> <language>`: the SDK's `Profile` value for one
/// `tii.profiles` entry.
fn profile(profile: &Value, language: &str) -> Result<String> {
    backend::for_language(language)?.profile(profile)
}

/// `identifier <name> <language> <role>`: the escaped identifier the
/// renderer uses for `name` in that role.
fn identifier_for(name: &str, language: &str, role: &str) -> Result<String> {
    let role = Role::parse(role).ok_or_else(|| {
        anyhow!(
            "unknown identifier role `{role}`; expected one of: {}",
            Role::NAMES.join(", ")
        )
    })?;
    identifier(backend::for_language(language)?, name, role)
}

/// `componentTypes <components.schemas> <language>`: component declarations
/// only. Superseded by `declarations`; kept for existing templates.
fn component_types(args: &[&Value]) -> Result<String> {
    let backend = backend::for_language(str_arg(args, 1)?)?;
    // The schemas table is absent when a protocol declares no custom types.
    let schemas = args.first().copied().unwrap_or(&Value::Null);
    let mut planner = Planner::new(backend);
    let declarations = planner.components(schemas)?;
    Ok(planner.render(declarations))
}

pub fn register(handlebars: &mut Handlebars<'_>) {
    #[allow(clippy::type_complexity)]
    let cases: &[(&'static str, fn(&str) -> String)] = &[
        ("pascalCase", |s| s.to_case(Case::Pascal)),
        ("camelCase", |s| s.to_case(Case::Camel)),
        ("constantCase", |s| s.to_case(Case::UpperSnake)),
        ("snakeCase", |s| s.to_case(Case::Snake)),
        ("lowerCase", |s| s.to_case(Case::Lower)),
    ];
    for (name, convert) in cases {
        let convert = *convert;
        handlebars.register_helper(
            name,
            Box::new(helper(name, move |args| Ok(convert(str_arg(args, 0)?)))),
        );
    }

    handlebars.register_helper(
        "schemaTypeFor",
        Box::new(helper("schemaTypeFor", schema_type_for)),
    );
    handlebars.register_helper(
        "declarations",
        Box::new(helper("declarations", |args| {
            declarations(arg(args, 0)?, str_arg(args, 1)?)
        })),
    );
    handlebars.register_helper(
        "imports",
        Box::new(helper("imports", |args| {
            imports(arg(args, 0)?, str_arg(args, 1)?)
        })),
    );
    handlebars.register_helper(
        "identifier",
        Box::new(helper("identifier", |args| {
            identifier_for(str_arg(args, 0)?, str_arg(args, 1)?, str_arg(args, 2)?)
        })),
    );
    handlebars.register_helper(
        "paramsTypeName",
        Box::new(helper("paramsTypeName", |args| {
            params_type_name(backend::for_language(str_arg(args, 1)?)?, str_arg(args, 0)?)
        })),
    );
    handlebars.register_helper(
        "json",
        Box::new(helper("json", |args| {
            Ok(serde_json::to_string(arg(args, 0)?)?)
        })),
    );
    handlebars.register_helper(
        "usesModule",
        Box::new(helper("usesModule", |args| {
            uses_module(arg(args, 0)?, str_arg(args, 1)?, str_arg(args, 2)?)
        })),
    );
    handlebars.register_helper(
        "argValue",
        Box::new(helper("argValue", |args| {
            arg_value(
                arg(args, 0)?,
                str_arg(args, 1)?,
                str_arg(args, 2)?,
                str_arg(args, 3)?,
                str_arg(args, 4)?,
            )
        })),
    );
    handlebars.register_helper(
        "profile",
        Box::new(helper("profile", |args| {
            profile(arg(args, 0)?, str_arg(args, 1)?)
        })),
    );

    // Superseded helpers, kept so existing templates keep rendering.
    handlebars.register_helper(
        "componentTypes",
        Box::new(helper("componentTypes", component_types)),
    );
    handlebars.register_helper(
        "swiftDeclarations",
        Box::new(helper("swiftDeclarations", |args| {
            declarations(arg(args, 0)?, "swift")
        })),
    );
    handlebars.register_helper(
        "swiftImports",
        Box::new(helper("swiftImports", |args| {
            imports(arg(args, 0)?, "swift")
        })),
    );
    let java_aliases: &[(&'static str, &'static str)] = &[
        ("javaPascalCase", "type"),
        ("javaCamelCase", "method"),
        ("javaConstantCase", "constant"),
    ];
    for (name, role) in java_aliases {
        handlebars.register_helper(
            name,
            Box::new(helper(name, move |args| {
                identifier_for(str_arg(args, 0)?, "java", role)
            })),
        );
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    fn render(template: &str, data: Value) -> std::result::Result<String, String> {
        let mut handlebars = Handlebars::new();
        register(&mut handlebars);
        handlebars
            .render_template(template, &data)
            .map_err(|error| error.to_string())
    }

    fn type_for(schema: Value, language: &str) -> String {
        render(
            &format!("{{{{schemaTypeFor schema \"{language}\"}}}}"),
            json!({ "schema": schema }),
        )
        .unwrap()
    }

    fn declarations(language: &str, schemas: Value) -> std::result::Result<String, String> {
        render(
            &format!("{{{{{{componentTypes schemas \"{language}\"}}}}}}"),
            json!({ "schemas": schemas }),
        )
    }

    fn builtin(name: &str) -> Value {
        json!({ "$ref": format!("https://tx3.land/specs/v1beta0/tii#/$defs/{name}") })
    }

    #[test]
    fn java_schema_types_cover_approved_native_mappings() {
        let cases = [
            (json!({ "type": "boolean" }), "Boolean"),
            (json!({ "type": "integer" }), "java.math.BigInteger"),
            (json!({ "type": "string" }), "String"),
            (builtin("Bytes"), "byte[]"),
            (builtin("Address"), "land.tx3.sdk.Address"),
            (builtin("UtxoRef"), "land.tx3.sdk.UtxoRef"),
            (builtin("Utxo"), "land.tx3.sdk.ArgValue"),
            (builtin("AnyAsset"), "land.tx3.sdk.ArgValue"),
            (
                json!({ "type": "array", "items": { "type": "integer" } }),
                "java.util.List<java.math.BigInteger>",
            ),
            (
                json!({ "type": "object", "additionalProperties": { "type": "boolean" } }),
                "java.util.Map<String, Boolean>",
            ),
            (
                json!({ "$ref": "#/components/schemas/payment datum" }),
                "PaymentDatum",
            ),
            (json!({ "future": true }), "land.tx3.sdk.ArgValue"),
        ];
        for (schema, expected) in cases {
            assert_eq!(type_for(schema, "java"), expected);
        }
    }

    #[test]
    fn swift_schema_types_cover_the_approved_table() {
        let cases = [
            (json!({ "type": "null" }), "Void"),
            (json!({ "type": "boolean" }), "Bool"),
            (json!({ "type": "integer" }), "BigInt"),
            (json!({ "type": "string" }), "ArgValue"),
            (builtin("Bytes"), "Data"),
            (builtin("Address"), "Address"),
            (builtin("UtxoRef"), "UtxoRef"),
            (builtin("Utxo"), "ArgValue"),
            (builtin("AnyAsset"), "ArgValue"),
            (
                json!({ "$ref": "#/components/schemas/order-item" }),
                "OrderItem",
            ),
            (json!({ "$ref": "#/components/schemas/Bytes" }), "Bytes"),
            (
                json!({ "type": "array", "items": { "type": "integer" } }),
                "[BigInt]",
            ),
            (
                json!({ "type": "object", "additionalProperties": { "type": "boolean" } }),
                "[String: Bool]",
            ),
            (json!({ "type": "object" }), "ArgValue"),
            (
                json!({ "type": "object", "additionalProperties": false }),
                "ArgValue",
            ),
        ];
        for (schema, expected) in cases {
            assert_eq!(type_for(schema, "swift"), expected);
        }
    }

    #[test]
    fn every_language_classifies_refs_by_origin() {
        let local = json!({ "$ref": "#/components/schemas/Address" });
        let external = json!({ "$ref": "https://example.com/schema#/$defs/Address" });
        let legacy = json!({ "$ref": "https://tx3.land/specs/v1beta0/core#Address" });
        let expected = [
            ("rust", "Address", "serde_json::Value", "Address"),
            ("typescript", "Address", "any", "string"),
            ("python", "Address", "Any", "str"),
            ("go", "Address", "interface{}", "string"),
            (
                "java",
                "Address",
                "land.tx3.sdk.ArgValue",
                "land.tx3.sdk.Address",
            ),
            ("swift", "Address", "ArgValue", "Address"),
        ];
        for (language, local_type, external_type, legacy_type) in expected {
            assert_eq!(type_for(local.clone(), language), local_type, "{language}");
            assert_eq!(
                type_for(external.clone(), language),
                external_type,
                "{language}"
            );
            assert_eq!(
                type_for(legacy.clone(), language),
                legacy_type,
                "{language}"
            );
        }
    }

    #[test]
    fn compound_types_use_the_hint_or_the_fallback() {
        let tuple = json!({ "type": "array", "prefixItems": [], "items": false });
        let data = json!({ "schema": tuple });
        assert_eq!(
            render("{{schemaTypeFor schema \"swift\" \"pair\"}}", data.clone()).unwrap(),
            "Pair"
        );
        assert_eq!(
            render("{{schemaTypeFor schema \"swift\"}}", data.clone()).unwrap(),
            "ArgValue"
        );
        assert_eq!(
            render("{{schemaTypeFor schema \"rust\" \"pair\"}}", data).unwrap(),
            "Vec<serde_json::Value>"
        );
    }

    #[test]
    fn unknown_languages_are_errors() {
        let error = render(
            "{{schemaTypeFor schema \"cobol\"}}",
            json!({ "schema": { "type": "integer" } }),
        )
        .unwrap_err();
        assert!(
            error.contains("unknown codegen language `cobol`; expected one of: rust, typescript, python, go, java, swift"),
            "{error}"
        );
    }

    #[test]
    fn java_declarations_cover_tuples_reserved_names_and_nesting() {
        let schemas = json!({
            "Envelope": {
                "type": "object",
                "properties": {
                    "class": { "type": "boolean" },
                    "pair": {
                        "type": "array",
                        "prefixItems": [
                            { "type": "integer" },
                            {
                                "type": "object",
                                "properties": { "record": { "type": "string" } },
                                "required": ["record"]
                            }
                        ],
                        "items": false
                    }
                },
                "required": ["class", "pair"]
            }
        });

        assert_eq!(
            declarations("java", schemas).unwrap(),
            concat!(
                "record Envelope(Boolean class_, EnvelopePair pair) {\n",
                "    record EnvelopePair(java.math.BigInteger item0, EnvelopePairItem1 item1) {\n",
                "        record EnvelopePairItem1(String record_) {}\n",
                "    }\n",
                "}\n",
                "\n",
            )
        );
    }

    #[test]
    fn swift_declarations_escape_keywords() {
        let schemas = json!({
            "protocol": {
                "type": "object",
                "properties": { "class": { "type": "boolean" } },
                "required": ["class"]
            }
        });
        let output = declarations("swift", schemas).unwrap();
        assert!(output.contains("struct Protocol_: Sendable"), "{output}");
        assert!(output.contains("public let class_: Bool"), "{output}");
    }

    #[test]
    fn identifier_collisions_name_both_sources_in_every_language() {
        let types = json!({
            "payment-value": { "type": "object", "properties": {} },
            "payment_value": { "type": "object", "properties": {} }
        });
        let fields = json!({
            "Collision": {
                "type": "object",
                "properties": {
                    "some-value": { "type": "boolean" },
                    "some_value": { "type": "boolean" }
                },
                "required": ["some-value", "some_value"]
            }
        });

        let error = declarations("java", types.clone()).unwrap_err();
        assert!(
            error.contains("Java identifier collision in top-level declarations: source identifiers `payment-value` and `payment_value` both normalize to `PaymentValue`"),
            "{error}"
        );
        for language in ["rust", "typescript", "python", "go", "java", "swift"] {
            assert!(declarations(language, types.clone()).is_err(), "{language}");
        }
        let error = declarations("swift", fields).unwrap_err();
        assert!(
            error.contains("in record Collision: source identifiers `some-value` and `some_value` both normalize to `someValue`"),
            "{error}"
        );
    }

    #[test]
    fn empty_identifiers_are_errors() {
        let schemas = json!({ "_": { "type": "object", "properties": {} } });
        let error = declarations("java", schemas).unwrap_err();
        assert!(
            error.contains("Java identifier `_` is empty after normalization"),
            "{error}"
        );
    }

    #[test]
    fn malformed_variants_are_errors_with_context() {
        let schemas = json!({
            "Side": { "oneOf": [{ "type": "object", "required": [], "properties": {} }] }
        });
        for language in ["rust", "java", "swift"] {
            let error = declarations(language, schemas.clone()).unwrap_err();
            assert!(
                error.contains(
                    "component `Side`: variant case 0 must name exactly one required tag"
                ),
                "{language}: {error}"
            );
        }
    }

    #[test]
    fn java_naming_helpers_apply_case_and_keyword_escaping() {
        assert_eq!(
            render(
                "{{javaPascalCase type}} {{javaCamelCase method}} {{javaConstantCase constant}}",
                json!({ "type": "payment datum", "method": "class", "constant": "api url" }),
            )
            .unwrap(),
            "PaymentDatum class_ API_URL"
        );
    }

    #[test]
    fn swift_imports_follow_the_types_used() {
        let opaque = json!({ "components": { "schemas": { "Opaque": { "type": "object" } } } });
        assert_eq!(
            render("{{{swiftImports tii}}}", json!({ "tii": opaque })).unwrap(),
            "import Tx3SDK\n"
        );
        assert_eq!(
            render("{{{swiftDeclarations tii}}}", json!({ "tii": opaque })).unwrap(),
            "public typealias Opaque = ArgValue"
        );
    }

    #[test]
    fn swift_arg_values_follow_the_params_declaration() {
        let tii = json!({
            "components": { "schemas": {
                "Amounts": { "type": "array", "items": { "type": "integer" } },
                "Asset": {
                    "type": "object",
                    "properties": { "policy": builtin("Bytes") },
                    "required": ["policy"]
                },
                "Opaque": { "type": "object" }
            } },
            "transactions": { "place-order": { "params": {
                "type": "object",
                "properties": {
                    "quantity": { "type": "integer" },
                    "flag": { "type": "boolean" },
                    "nothing": { "type": "null" },
                    "memo": { "type": "string" },
                    "ship-to": builtin("Address"),
                    "source": builtin("UtxoRef"),
                    "datum": builtin("Bytes"),
                    "bag": builtin("AnyAsset"),
                    "amounts": { "$ref": "#/components/schemas/Amounts" },
                    "asset": { "$ref": "#/components/schemas/Asset" },
                    "opaque": { "$ref": "#/components/schemas/Opaque" },
                    "pair": {
                        "type": "array",
                        "prefixItems": [{ "type": "integer" }, { "type": "boolean" }],
                        "items": false
                    },
                    "labels": { "type": "object", "additionalProperties": { "type": "integer" } },
                    "matrix": {
                        "type": "array",
                        "items": { "type": "array", "items": { "type": "integer" } }
                    }
                },
                "required": [
                    "quantity", "flag", "nothing", "memo", "ship-to", "source", "datum", "bag",
                    "amounts", "asset", "opaque", "pair", "labels", "matrix"
                ]
            } } }
        });
        let data = json!({ "tii": tii });
        let cases = [
            ("quantity", "ArgValue.integer(params.quantity)"),
            ("flag", "ArgValue.boolean(params.flag)"),
            ("nothing", "ArgValue.structure(constructor: 0, fields: [])"),
            ("memo", "params.memo"),
            ("ship-to", "ArgValue.address(params.shipTo)"),
            ("source", "ArgValue.utxoRef(params.source)"),
            ("datum", "ArgValue.bytes(params.datum)"),
            ("bag", "params.bag"),
            // A component alias converts as its target; a component record
            // converts itself; an opaque component is already an `ArgValue`.
            ("amounts", "ArgValue.list(params.amounts.map { ArgValue.integer($0) })"),
            ("asset", "params.asset.argValue"),
            ("opaque", "params.opaque"),
            ("pair", "params.pair.argValue"),
            (
                "labels",
                "ArgValue.mapPairs(params.labels.sorted { $0.key < $1.key }.map { \
                 ArgMapEntry(key: ArgValue.string($0.key), value: ArgValue.integer($0.value)) })",
            ),
            (
                "matrix",
                "ArgValue.list(params.matrix.map { ArgValue.list($0.map { ArgValue.integer($0) }) })",
            ),
        ];
        for (param, expected) in cases {
            let template =
                format!("{{{{argValue tii \"place-order\" \"{param}\" \"swift\" \"params\"}}}}");
            assert_eq!(
                render(&template, data.clone()).unwrap(),
                expected,
                "{param}"
            );
        }

        let error = render(
            "{{argValue tii \"place-order\" \"missing\" \"swift\" \"params\"}}",
            data.clone(),
        )
        .unwrap_err();
        assert!(
            error.contains("transaction `place-order` has no `missing` parameter"),
            "{error}"
        );
        let error = render(
            "{{argValue tii \"transfer\" \"quantity\" \"swift\" \"params\"}}",
            data.clone(),
        )
        .unwrap_err();
        assert!(
            error.contains("transaction `transfer` declares no params"),
            "{error}"
        );
        let error = render(
            "{{argValue tii \"place-order\" \"quantity\" \"go\" \"params\"}}",
            data,
        )
        .unwrap_err();
        assert!(
            error.contains("Go clients do not construct argument values statically"),
            "{error}"
        );
    }

    #[test]
    fn swift_declarations_convert_themselves() {
        let schemas = json!({
            "Side": { "oneOf": [
                {
                    "type": "object",
                    "required": ["Buy"],
                    "properties": { "Buy": { "type": "object", "properties": {} } }
                },
                {
                    "type": "object",
                    "required": ["Sell"],
                    "properties": { "Sell": {
                        "type": "object",
                        "properties": { "price": { "type": "integer" } },
                        "required": ["price"]
                    } }
                }
            ] }
        });
        assert_eq!(
            declarations("swift", schemas).unwrap(),
            concat!(
                "public enum Side: Sendable {\n",
                "    case buy\n",
                "    case sell(price: BigInt)\n",
                "\n",
                "    /// The canonical argument value of this variant.\n",
                "    public var argValue: ArgValue {\n",
                "        switch self {\n",
                "        case .buy:\n",
                "            return ArgValue.structure(constructor: 0, fields: [])\n",
                "        case .sell(let price):\n",
                "            return ArgValue.structure(\n",
                "                constructor: 1,\n",
                "                fields: [\n",
                "                    ArgValue.integer(price),\n",
                "                ]\n",
                "            )\n",
                "        }\n",
                "    }\n",
                "}",
            )
        );
    }

    #[test]
    fn swift_profiles_are_sdk_values() {
        let profile = json!({
            "environment": {
                "flags": [true, null],
                "name": "pre\"prod",
                "tax": 5000000
            },
            "parties": { "sender": "addr1" }
        });
        assert_eq!(
            render(
                "{{{profile profile \"swift\"}}}",
                json!({ "profile": profile })
            )
            .unwrap(),
            concat!(
                "Tx3SDK.Profile(\n",
                "    environment: [\n",
                "        \"flags\": JSONValue.array([\n",
                "            JSONValue.boolean(true),\n",
                "            JSONValue.null,\n",
                "        ]),\n",
                "        \"name\": JSONValue.string(\"pre\\\"prod\"),\n",
                "        \"tax\": JSONValue.number(5000000.0),\n",
                "    ],\n",
                "    parties: [\n",
                "        \"sender\": \"addr1\",\n",
                "    ]\n",
                ")",
            )
        );
        assert_eq!(
            render(
                "{{{profile profile \"swift\"}}}",
                json!({ "profile": { "environment": {}, "parties": {} } })
            )
            .unwrap(),
            "Tx3SDK.Profile(\n    environment: [:],\n    parties: [:]\n)"
        );

        let error = render(
            "{{{profile profile \"swift\"}}}",
            json!({ "profile": { "parties": { "sender": 1 } } }),
        )
        .unwrap_err();
        assert!(
            error.contains("party `sender` must map to an address string"),
            "{error}"
        );
        let error = render("{{{profile profile \"rust\"}}}", json!({ "profile": {} })).unwrap_err();
        assert!(
            error.contains("Rust clients do not embed profiles as SDK values"),
            "{error}"
        );
    }

    #[test]
    fn uses_module_reports_the_planned_imports() {
        let integers = json!({ "transactions": { "transfer": { "params": {
            "type": "object",
            "properties": { "quantity": { "type": "integer" } },
            "required": ["quantity"]
        } } } });
        let template = "{{#if (usesModule tii \"swift\" \"BigInt\")}}big{{else}}small{{/if}}";
        assert_eq!(render(template, json!({ "tii": integers })).unwrap(), "big");
        assert_eq!(
            render(template, json!({ "tii": { "transactions": {} } })).unwrap(),
            "small"
        );
        assert_eq!(
            render(
                "{{usesModule tii \"swift\" \"Tx3SDK\"}}",
                json!({ "tii": { "transactions": {} } })
            )
            .unwrap(),
            "true"
        );
    }

    #[test]
    fn identifier_applies_each_role() {
        let template = concat!(
            "{{identifier name \"rust\" \"type\"}} ",
            "{{identifier name \"rust\" \"param\"}} ",
            "{{identifier name \"rust\" \"constant\"}} ",
            "{{identifier name \"typescript\" \"field\"}} ",
            "{{identifier name \"typescript\" \"param\"}} ",
            "{{identifier name \"java\" \"method\"}} ",
            "{{identifier name \"swift\" \"case\"}}",
        );
        assert_eq!(
            render(template, json!({ "name": "place_order" })).unwrap(),
            "PlaceOrder place_order PLACE_ORDER place_order placeOrder placeOrder placeOrder"
        );
        assert_eq!(
            render(
                "{{identifier name \"java\" \"field\"}}",
                json!({ "name": "class" })
            )
            .unwrap(),
            "class_"
        );

        let error = render(
            "{{identifier name \"java\" \"module\"}}",
            json!({ "name": "x" }),
        )
        .unwrap_err();
        assert!(
            error.contains("unknown identifier role `module`; expected one of: type, field, param, case, method, constant"),
            "{error}"
        );
    }

    #[test]
    fn params_type_names_match_declarations() {
        let tii = json!({
            "transactions": {
                "place-order": {
                    "params": {
                        "type": "object",
                        "properties": { "quantity": { "type": "integer" } },
                        "required": ["quantity"]
                    }
                }
            }
        });
        let data = json!({ "tii": tii });
        assert_eq!(
            render("{{paramsTypeName \"place-order\" \"go\"}}", data.clone()).unwrap(),
            "PlaceOrderParams"
        );
        assert_eq!(
            render("{{{declarations tii \"go\"}}}", data.clone()).unwrap(),
            concat!(
                "// PlaceOrderParams holds the arguments for the place-order transaction.\n",
                "type PlaceOrderParams struct {\n",
                "\tQuantity int64 `json:\"quantity\"`\n",
                "}\n",
                "\n",
            )
        );
        assert_eq!(render("{{{imports tii \"go\"}}}", data).unwrap(), "");
    }
}
