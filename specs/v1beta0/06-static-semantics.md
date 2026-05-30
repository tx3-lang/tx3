# 6. Static semantics

This section defines what a conforming compiler MUST check after parsing
and before lowering. Violations of any rule in this section are *semantic
errors* and MUST be reported with at least one diagnostic.

The rules are organised by scope (§6.1–§6.2), then by expression kind
(§6.3–§6.7), then by block-level validation (§6.8).

## 6.1 Scopes

A *scope* is a partial map from identifiers to *symbols*. Scopes are
arranged in a tree by lexical containment. Name resolution walks from the
innermost scope outward through parent scopes (§6.2).

The scope tree for a program has the following shape:

```
program scope
├── (per tx_def) tx scope
│   ├── (per struct/property/variant access) inner scope (§6.3)
```

The *program scope* contains:

- one symbol per `env` field, of kind *env-var*;
- one symbol per `party_def`, of kind *party*;
- one symbol per `policy_def`, of kind *policy*;
- one symbol per `asset_def` (including the implicit `Ada` asset), of
  kind *asset*;
- one symbol per `record_def` or `variant_def`, of kind *type*;
- one symbol per `alias_def`, of kind *alias*;
- the built-in functions `min_utxo`, `tip_slot`, `slot_to_time`,
  `time_to_slot` (§5.6).

A *tx scope* (one per `tx_def`) extends the program scope with:

- one symbol per parameter, of kind *param-var*, in the order declared;
- one symbol per `locals_assign` (across all `locals_block`s of the tx),
  of kind *local-expr*;
- one symbol per `input_block`, keyed on the input's identifier, of kind
  *input*;
- one symbol per `reference_block`, keyed on the reference's identifier,
  of kind *reference*;
- one symbol per **named** `output_block`, keyed on the output's
  identifier, of kind *output*;
- the bare identifier `fees`, of kind *fees* and type `AnyAsset` (§5.7).

Within a single tx scope, names introduced by parameters, locals, inputs,
references, and named outputs **share a flat namespace**. The order of
declaration does not matter for resolution; for example, an output named
`change` MAY be referenced from an `input` block that appears textually
before it.

Inner scopes are created when analysing:

- a `struct_constructor`: an inner scope whose symbols are the variant
  cases of the constructor's type;
- a `variant_case_constructor`: an inner scope whose symbols are the
  record fields of that case;
- a property access `e.p`: an inner scope whose symbols are the properties
  of `e`'s static type (§5.4 or user-defined).

## 6.2 Name resolution and duplicates

Name lookup at a use site `n` proceeds from the current scope outward to
the program scope. The first scope in which `n` is bound determines the
symbol. If `n` is unbound in every enclosing scope and is not a built-in,
the compiler MUST report a *not-in-scope* error.

Within a single scope, an identifier MUST be bound by at most one
definition. Multiple `record_def`/`variant_def`/`alias_def`/`party_def`/
`policy_def`/`asset_def`/`env`-field/parameter/local/input/reference/named
output with the same name are *duplicate definitions* and MUST be
rejected. The error MAY name only the first or last occurrence.

The implicit `Ada` asset definition is added by the compiler; redeclaring
`Ada` at the top level is a duplicate-definition error (§5.8).

## 6.3 Type checking

Every `data_expr` has a *static type* assigned by the analyzer. The static
type is computed bottom-up as follows:

- Literals have the type given in §3.5 (e.g. `42 : Int`,
  `"abc" : Bytes`).
- `()` has type `Unit`.
- `utxo_ref_literal` has type `UtxoRef`.
- An `identifier` `n` has the static type associated with the symbol that
  `n` resolves to: parameter type for *param-var*; the assigned-expression
  type for *local-expr*; the special compiler type `Utxo` for *input*
  (§7.2); the `datum_is` type for *reference* when present (§7.6); the
  output's value type for *output*; `AnyAsset` for `fees` and for `Ada`;
  the underlying type for *env-var*; and so on. Identifiers that resolve
  to *type*, *alias*, *party*, *policy*, *asset*, or *function* symbols
  are not values themselves and may only appear in positions that name
  one of those kinds (§6.4).
- A `struct_constructor` has the type named by its `r#type` identifier.
- A `list_constructor` has type `List<T>` where `T` is the static type of
  the first element. An empty `list_constructor` has type
  `List<Undefined>` (an internal placeholder, see §6.6).
- A `map_constructor` has type `Map<K, V>` where `K` and `V` are the
  static types of the first entry's key and value.
- An `any_asset_constructor` has type `AnyAsset`.
- A `concat_constructor` has the static type of its first argument.
- A `fn_call` to a built-in has the type listed in §5.6.
- An `add_op` or `sub_op` has the static type of its left operand.
- A `negate_op` has the static type of its operand.
- A `property_op` `e.p` has the type of the property `p` on `e`'s type
  (§5.4 / §5.3).
- An `index_op` `e[i]` has type `T` when `e : List<T>`.

A position that *expects* a type `B` accepts an expression whose static
type `A` is type-equivalent to `B` per §5.9. A mismatch is an
*invalid-target-type* error.

The following positions impose type expectations:

| Position                                          | Expected type |
| ------------------------------------------------- | ------------- |
| Policy and asset-name of `asset_def`              | `Bytes`       |
| `metadata` value, when literal                    | byte size ≤ 64 (§6.5) |
| `cardano::withdrawal` `amount`                    | `Int`         |
| `cardano::treasury_donation` `coin`               | `Int`         |
| Argument of `slot_to_time` / `time_to_slot`       | `Int`         |
| Index of `e[i]`                                   | `Int`         |
| Spread base in a `variant_case_constructor`       | type of the constructor's case |

Conforming compilers MAY additionally check that the argument types of
`AnyAsset(p, a, q)` are `(Bytes, Bytes, Int)`, that arithmetic operands
match the patterns of §5.5.1, and that record/variant constructor field
expressions match their declared field types. Such additional checks are
RECOMMENDED.

## 6.4 Identifiers in non-value positions

Several positions in the language name symbols rather than evaluate values:

- The right-hand side of `policy P = …` (a `hex_string`).
- The first argument to `min_utxo(x)` (an *output* identifier).
- The `from` field of an `input_block` and the `to` field of an
  `output_block` (a *party* or *policy* identifier, or an expression of
  type `Address`; see §7.2, §7.3).
- The `r#type` identifier of a `struct_constructor` (a *type* or *alias*
  symbol).
- The case identifier of an `explicit_variant_case_constructor` (a
  *variant-case* symbol within the surrounding type's inner scope).
- The field identifier of a `record_constructor_field` (a *record-field*
  symbol within the surrounding case's inner scope).
- The property identifier of a `data_property` (`.p`).

When a position names a symbol of the wrong kind, the compiler MUST
report an *invalid-symbol* error.

## 6.5 Metadata validation

Inside every `metadata_block`:

- Each entry's *key* expression MUST have static type `Int`. Permissible
  forms in `v1beta0` are an `integer_literal` or an `identifier` that
  resolves to a symbol of type `Int`. Any other key form is a
  *metadata-invalid-key-type* error.
- Each entry's *value*, when it is a `string_literal` or `hex_string`,
  MUST have a byte length ≤ 64. The byte length of a string is the
  number of UTF-8 code units between its quotes; the byte length of a
  hex string is the number of bytes its hex digits encode (half the
  digit count, rounded down). Exceeding 64 bytes is a
  *metadata-size-limit-exceeded* error.

Values that are not literal `string_literal` or `hex_string` (e.g.
identifiers, constructors, arithmetic) are not size-checked at compile
time; the size limit MUST still be respected at resolution time but is
beyond the static analysis's reach.

## 6.6 Other static rules

- An `alias_def` whose right-hand side names an undefined type is a
  *not-in-scope* error.
- An alias chain that does not terminate at a `record_def` or
  `variant_def` (e.g. `type A = B; type B = A;`) is **unresolved**;
  conforming compilers MUST reject any such cyclic chain. Recursive
  references between distinct `type_def`s that ultimately resolve are
  permitted.
- Indexing (`e[i]`) on a non-list type is an *invalid-target-type* error.
- Property access on a value whose type has no such property is an
  *invalid-symbol* or *not-in-scope* error (the analyzer may report
  either).
- An empty `list_constructor` `[]` whose element type cannot be inferred
  from context yields the internal type `List<Undefined>`. A program in
  which `Undefined` reaches a typed position is non-conforming;
  implementations SHOULD report a clearer error than "undefined".
- A `struct_constructor` whose `r#type` resolves to an `alias_def` is
  accepted; the alias is chased to its underlying `record_def` or
  `variant_def` (§5.3.3).

## 6.7 Resolution order

The analyzer establishes scopes in two passes:

1. The program scope is populated with all top-level symbols. Type
   definitions and aliases are then resolved in repeated passes until a
   fixed point is reached (handling forward references between types).
2. For each `tx_def`, the tx scope is populated and the analyzer iterates
   to a fixed point so that mutual references between locals, inputs,
   outputs, and references can be resolved regardless of textual order.

Conforming compilers MAY implement either order or both, but the
observable result MUST be the same: any cyclic dependency between values
not mediated by `datum_is` or by structural recursion is a *not-in-scope*
error.

## 6.8 Block-level validation

In addition to the rules above, each `tx_body_block` is subject to the
per-block constraints in §7 and §8. The following two rules are
established here because they are diagnosed by static analysis:

### 6.8.1 Optional outputs MUST NOT carry a datum

An `output_block` with the `"?"` flag MUST NOT include a `datum` field.
This is an *invalid-optional-output* error.

### 6.8.2 Reference block fields

A `reference_block` MUST include exactly one `ref` field (enforced by the
grammar in §4.6.2). The optional `datum_is` field, when present, MUST be
a type that resolves under §6.6.

## 6.9 Catalogue of static-semantic errors

The error categories below are reserved for the conditions described in
this section. A conforming compiler SHOULD use a stable error identifier
for each so that downstream tooling can match on it. The reference
implementation currently uses the identifiers in the right-hand column.

| Category                          | Reference identifier            |
| --------------------------------- | ------------------------------- |
| not in scope                      | `tx3::not_in_scope`             |
| invalid symbol kind               | `tx3::invalid_symbol`           |
| invalid target type               | `tx3::invalid_type`             |
| duplicate definition              | `tx3::duplicate_definition`     |
| optional output has datum         | `tx3::optional_output_datum`    |
| metadata value > 64 bytes         | `tx3::metadata_size_limit_exceeded` |
| metadata key not an integer       | `tx3::metadata_invalid_key_type` |

Additional diagnostics MAY be emitted for guidance, but a conforming
compiler MUST emit at least one diagnostic from the appropriate category
above for each violation.
