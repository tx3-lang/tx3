//! Handlebars helpers exposed to codegen templates.

use anyhow::{anyhow, Result};
use convert_case::{Case, Casing};
use handlebars::{
    Context as HbContext, Handlebars, Helper, HelperDef, Output, RenderContext, RenderErrorReason,
};
use serde_json::Value;

use super::{backend, names::identifier_in, plan::Planner, schema::Shape};

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

/// `componentTypes <components.schemas> <language>`
fn component_types(args: &[&Value]) -> Result<String> {
    let backend = backend::for_language(str_arg(args, 1)?)?;
    // The schemas table is absent when a protocol declares no custom types.
    let schemas = args.first().copied().unwrap_or(&Value::Null);
    let mut planner = Planner::new(backend);
    let declarations = planner.components(schemas)?;
    Ok(planner.render(declarations))
}

/// `swiftDeclarations <tii>`: components and transaction params.
fn swift_declarations(args: &[&Value]) -> Result<String> {
    let mut planner = Planner::new(backend::for_language("swift")?);
    let declarations = planner.document(arg(args, 0)?)?;
    Ok(planner.render(declarations))
}

/// `swiftImports <tii>`: modules used by `swiftDeclarations`.
fn swift_imports(args: &[&Value]) -> Result<String> {
    let backend = backend::for_language("swift")?;
    let mut planner = Planner::new(backend);
    planner.document(arg(args, 0)?)?;
    Ok(backend.imports(planner.usage()))
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

    let java_cases: &[(&'static str, Case)] = &[
        ("javaPascalCase", Case::Pascal),
        ("javaCamelCase", Case::Camel),
        ("javaConstantCase", Case::UpperSnake),
    ];
    for (name, case) in java_cases {
        let case = *case;
        handlebars.register_helper(
            name,
            Box::new(helper(name, move |args| {
                identifier_in(backend::for_language("java")?, str_arg(args, 0)?, case)
            })),
        );
    }

    handlebars.register_helper(
        "schemaTypeFor",
        Box::new(helper("schemaTypeFor", schema_type_for)),
    );
    handlebars.register_helper(
        "componentTypes",
        Box::new(helper("componentTypes", component_types)),
    );
    handlebars.register_helper(
        "swiftDeclarations",
        Box::new(helper("swiftDeclarations", swift_declarations)),
    );
    handlebars.register_helper(
        "swiftImports",
        Box::new(helper("swiftImports", swift_imports)),
    );
    handlebars.register_helper(
        "json",
        Box::new(helper("json", |args| {
            Ok(serde_json::to_string(arg(args, 0)?)?)
        })),
    );
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
}
