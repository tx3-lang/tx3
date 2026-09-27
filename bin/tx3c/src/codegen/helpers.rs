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
    plan::{params_type_name, Planner},
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

/// `argValue <schema> <language> <expr> [<member>]`: the SDK's canonical
/// tagged argument built from `expr`, a value of the type `declarations`
/// gives `schema`; with `member`, from that member of `expr` instead, read
/// through the backend's accessor syntax. Compound shapes convert through
/// the `toArgValue()` their declaration carries, so the value must have been
/// typed by `declarations` or `paramsTypeName`.
fn arg_value(args: &[&Value]) -> Result<String> {
    let backend = backend::for_language(str_arg(args, 1)?)?;
    let shape = Shape::parse(arg(args, 0)?)?;
    let mut expr = str_arg(args, 2)?.to_string();
    if let Some(member) = args.get(3) {
        let member = member
            .as_str()
            .ok_or_else(|| anyhow!("argument 3 must be a string"))?;
        expr = backend.accessor(&expr, &identifier(backend, member, Role::Param)?);
    }
    backend.argument(&shape, &expr).ok_or_else(|| {
        anyhow!(
            "{} has no static argument construction; use the SDK's dynamic encoding",
            backend.display_name()
        )
    })
}

/// `stringLiteral <text> <language>`: `text` as a string literal of that
/// language, quoted and escaped.
fn string_literal(args: &[&Value]) -> Result<String> {
    let backend = backend::for_language(str_arg(args, 1)?)?;
    Ok(backend.string_literal(str_arg(args, 0)?))
}

/// `indent <text> <columns>`: `text` with every non-empty line indented by
/// `columns` spaces, so a rendered block can nest inside a declaration.
/// Trailing newlines are dropped; the template controls the spacing after
/// the block.
fn indent(args: &[&Value]) -> Result<String> {
    let text = str_arg(args, 0)?.trim_end_matches('\n');
    let columns = arg(args, 1)?
        .as_u64()
        .ok_or_else(|| anyhow!("argument 1 must be a non-negative integer"))?;
    let pad = " ".repeat(columns as usize);
    let mut out = String::with_capacity(text.len());
    for (index, line) in text.split('\n').enumerate() {
        if index > 0 {
            out.push('\n');
        }
        if !line.is_empty() {
            out.push_str(&pad);
            out.push_str(line);
        }
    }
    Ok(out)
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
        ("kebabCase", |s| s.to_case(Case::Kebab)),
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
    handlebars.register_helper("argValue", Box::new(helper("argValue", arg_value)));
    handlebars.register_helper(
        "stringLiteral",
        Box::new(helper("stringLiteral", string_literal)),
    );
    handlebars.register_helper("indent", Box::new(helper("indent", indent)));
    handlebars.register_helper(
        "json",
        Box::new(helper("json", |args| {
            Ok(serde_json::to_string(arg(args, 0)?)?)
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
                "public record Envelope(Boolean class_, EnvelopePair pair) {\n",
                "    /** Converts this value to the SDK's canonical tagged argument. */\n",
                "    public land.tx3.sdk.ArgValue toArgValue() {\n",
                "        return land.tx3.sdk.ArgValue.struct(0, java.util.List.of(\n",
                "            land.tx3.sdk.ArgValue.bool(class_),\n",
                "            pair.toArgValue()));\n",
                "    }\n",
                "\n",
                "    public record EnvelopePair(java.math.BigInteger item0, EnvelopePairItem1 item1) {\n",
                "        /** Converts this value to the SDK's canonical tagged argument. */\n",
                "        public land.tx3.sdk.ArgValue toArgValue() {\n",
                "            return land.tx3.sdk.ArgValue.tuple(java.util.List.of(\n",
                "                land.tx3.sdk.ArgValue.integer(item0),\n",
                "                item1.toArgValue()));\n",
                "        }\n",
                "\n",
                "        public record EnvelopePairItem1(String record_) {\n",
                "            /** Converts this value to the SDK's canonical tagged argument. */\n",
                "            public land.tx3.sdk.ArgValue toArgValue() {\n",
                "                return land.tx3.sdk.ArgValue.struct(0, java.util.List.of(\n",
                "                    land.tx3.sdk.ArgValue.string(record_)));\n",
                "            }\n",
                "        }\n",
                "    }\n",
                "}\n",
                "\n",
            )
        );
    }

    #[test]
    fn java_variants_number_their_cases_and_aliases_wrap_their_value() {
        let schemas = json!({
            "Side": {
                "oneOf": [
                    { "type": "object", "required": ["Buy"], "properties": { "Buy": { "type": "object", "properties": {} } } },
                    { "type": "object", "required": ["Sell"], "properties": { "Sell": {
                        "type": "object",
                        "properties": { "price": { "type": "integer" } },
                        "required": ["price"]
                    } } }
                ]
            },
            "Amount": { "type": "integer" },
            "Opaque": { "type": "object" }
        });

        assert_eq!(
            declarations("java", schemas).unwrap(),
            concat!(
                "public record Amount(java.math.BigInteger value) {\n",
                "    /** Converts this value to the SDK's canonical tagged argument. */\n",
                "    public land.tx3.sdk.ArgValue toArgValue() {\n",
                "        return land.tx3.sdk.ArgValue.integer(value);\n",
                "    }\n",
                "}\n",
                "\n",
                "public record Opaque(land.tx3.sdk.ArgValue value) {\n",
                "    /** Converts this value to the SDK's canonical tagged argument. */\n",
                "    public land.tx3.sdk.ArgValue toArgValue() {\n",
                "        return value;\n",
                "    }\n",
                "}\n",
                "\n",
                "public sealed interface Side permits Side.Buy, Side.Sell {\n",
                "    /** Converts this value to the SDK's canonical tagged argument. */\n",
                "    land.tx3.sdk.ArgValue toArgValue();\n",
                "\n",
                "    record Buy() implements Side {\n",
                "        @Override\n",
                "        public land.tx3.sdk.ArgValue toArgValue() {\n",
                "            return land.tx3.sdk.ArgValue.struct(0, java.util.List.of());\n",
                "        }\n",
                "    }\n",
                "\n",
                "    record Sell(java.math.BigInteger price) implements Side {\n",
                "        @Override\n",
                "        public land.tx3.sdk.ArgValue toArgValue() {\n",
                "            return land.tx3.sdk.ArgValue.struct(1, java.util.List.of(\n",
                "                land.tx3.sdk.ArgValue.integer(price)));\n",
                "        }\n",
                "    }\n",
                "}\n",
                "\n",
            )
        );
    }

    #[test]
    fn java_params_records_carry_no_conversion() {
        let tii = json!({
            "transactions": {
                "transfer": {
                    "params": {
                        "type": "object",
                        "properties": { "quantity": { "type": "integer" } },
                        "required": ["quantity"]
                    }
                }
            }
        });
        assert_eq!(
            render("{{{declarations tii \"java\"}}}", json!({ "tii": tii })).unwrap(),
            "public record TransferParams(java.math.BigInteger quantity) {}\n\n"
        );
    }

    fn arg_value(schema: Value, expr: &str) -> std::result::Result<String, String> {
        render(
            &format!("{{{{{{argValue schema \"java\" \"{expr}\"}}}}}}"),
            json!({ "schema": schema }),
        )
    }

    #[test]
    fn java_arg_values_cover_every_shape() {
        let cases = [
            (json!({ "type": "boolean" }), "land.tx3.sdk.ArgValue.bool(x)"),
            (json!({ "type": "integer" }), "land.tx3.sdk.ArgValue.integer(x)"),
            (json!({ "type": "string" }), "land.tx3.sdk.ArgValue.string(x)"),
            (json!({ "type": "null" }), "x"),
            (builtin("Bytes"), "land.tx3.sdk.ArgValue.bytes(x)"),
            (builtin("Address"), "land.tx3.sdk.ArgValue.address(x)"),
            (builtin("UtxoRef"), "land.tx3.sdk.ArgValue.utxoRef(x)"),
            (builtin("Utxo"), "x"),
            (builtin("AnyAsset"), "x"),
            (json!({ "future": true }), "x"),
            (json!({ "$ref": "#/components/schemas/asset-class" }), "x.toArgValue()"),
            (
                json!({ "type": "array", "prefixItems": [{ "type": "integer" }], "items": false }),
                "x.toArgValue()",
            ),
            (
                json!({ "type": "array", "items": { "type": "integer" } }),
                "land.tx3.sdk.ArgValue.list(x.stream().map(v0 -> land.tx3.sdk.ArgValue.integer(v0)).toList())",
            ),
            (
                json!({ "type": "array", "items": builtin("Utxo") }),
                "land.tx3.sdk.ArgValue.list(x)",
            ),
            (
                json!({ "type": "array", "items": { "type": "array", "items": { "type": "boolean" } } }),
                "land.tx3.sdk.ArgValue.list(x.stream().map(v0 -> land.tx3.sdk.ArgValue.list(v0.stream().map(v1 -> land.tx3.sdk.ArgValue.bool(v1)).toList())).toList())",
            ),
            (
                json!({ "type": "object", "additionalProperties": { "$ref": "#/components/schemas/Side" } }),
                "land.tx3.sdk.ArgValue.map(x.entrySet().stream().sorted(java.util.Map.Entry.comparingByKey()).map(v0 -> new land.tx3.sdk.ArgValue.MapEntry(land.tx3.sdk.ArgValue.string(v0.getKey()), v0.getValue().toArgValue())).toList())",
            ),
        ];
        for (schema, expected) in cases {
            assert_eq!(
                arg_value(schema.clone(), "x").unwrap(),
                expected,
                "{schema}"
            );
        }
    }

    #[test]
    fn arg_value_reads_members_through_the_backend_accessor() {
        assert_eq!(
            render(
                "{{{argValue schema \"java\" \"args\" \"ship-to\"}}}",
                json!({ "schema": { "$ref": "#/components/schemas/Address" } }),
            )
            .unwrap(),
            "args.shipTo().toArgValue()"
        );
        assert_eq!(
            render(
                "{{{argValue schema \"java\" \"args\" \"class\"}}}",
                json!({ "schema": { "type": "boolean" } }),
            )
            .unwrap(),
            "land.tx3.sdk.ArgValue.bool(args.class_())"
        );
    }

    #[test]
    fn arg_value_needs_a_backend_with_static_construction() {
        let error = render(
            "{{{argValue schema \"rust\" \"x\"}}}",
            json!({ "schema": { "type": "integer" } }),
        )
        .unwrap_err();
        assert!(
            error.contains("Rust has no static argument construction"),
            "{error}"
        );
    }

    #[test]
    fn java_identifiers_collapse_illegal_characters() {
        assert_eq!(
            render(
                "{{identifier a \"java\" \"method\"}} {{identifier b \"java\" \"type\"}} {{identifier c \"java\" \"method\"}}",
                json!({ "a": "my.protocol", "b": "acme@v2!", "c": "9.lives" }),
            )
            .unwrap(),
            "my_protocol Acme_v2_ _9_lives"
        );
    }

    #[test]
    fn string_literals_escape_for_the_language() {
        assert_eq!(
            render(
                "{{{stringLiteral text \"java\"}}}",
                json!({ "text": "say \"hi\"\n\\ \u{1}" }),
            )
            .unwrap(),
            "\"say \\\"hi\\\"\\n\\\\ \\u0001\""
        );
    }

    #[test]
    fn long_java_string_literals_are_joined_at_runtime() {
        let text = "a".repeat(65535 * 2 + 1);
        let literal = render("{{{stringLiteral text \"java\"}}}", json!({ "text": text })).unwrap();
        // The separator after the empty joiner, then one between each chunk.
        let pieces: Vec<&str> = literal.split("\", \"").collect();
        assert_eq!(pieces.len(), 4, "{}", &literal[..40]);
        assert_eq!(pieces[0], "String.join(\"");
        assert_eq!(pieces[1].len(), 65535);
        assert_eq!(pieces[2].len(), 65535);
        assert_eq!(pieces[3], "a\")");
    }

    #[test]
    fn indent_pads_non_empty_lines_and_drops_trailing_newlines() {
        assert_eq!(
            render(
                "{{{indent text 4}}}|",
                json!({ "text": "a {\n\n    b\n}\n\n" }),
            )
            .unwrap(),
            "    a {\n\n        b\n    }|"
        );
    }

    #[test]
    fn kebab_case_names_maven_artifacts() {
        assert_eq!(
            render("{{kebabCase name}}", json!({ "name": "My Protocol_v2" })).unwrap(),
            "my-protocol-v-2"
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
