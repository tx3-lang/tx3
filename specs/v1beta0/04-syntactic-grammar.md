# 4. Syntactic grammar

This section gives the complete context-free grammar of Tx3 in EBNF. The
grammar covers syntax only; semantic restrictions (e.g. "metadata keys
MUST be integers") live in §6 and §7.

The notation is as defined in §1.3. Whitespace and comments (§3.2, §3.3) are
implicitly permitted between any two adjacent grammar symbols and are not
written into the productions.

## 4.1 Program

```
program ::= top_level_def*

top_level_def ::=
      env_def
    | asset_def
    | party_def
    | policy_def
    | type_def
    | tx_def
```

Top-level definitions MAY appear in any order. At most one `env_def` SHOULD
appear in a program (§6.2). All other kinds MAY appear any number of times,
subject to the no-duplicate-definitions rule of §6.2.

## 4.2 Top-level definitions

### 4.2.1 Environment

```
env_def     ::= "env" "{" (env_field ",")+ "}"
env_field   ::= identifier ":" type
```

The `env` block declares typed external inputs that are in scope throughout
the program (see §6.1). At least one `env_field` MUST appear.

### 4.2.2 Asset

```
asset_def ::= "asset" identifier "=" data_expr "." data_expr ";"
```

An *asset definition* introduces a name bound to a (policy, asset-name)
pair. The first `data_expr` denotes the policy; the second denotes the asset
name. Both MUST have type `Bytes` after analysis (§6.3).

### 4.2.3 Party

```
party_def ::= "party" identifier ";"
```

A *party definition* introduces a participant name. Party definitions carry
no further structure at the language level; their meaning is supplied at
resolution time.

### 4.2.4 Policy

```
policy_def        ::= "policy" identifier policy_def_value
policy_def_value  ::= policy_def_assign | policy_def_constructor

policy_def_assign ::= "=" hex_string ";"

policy_def_constructor ::= "{" (policy_def_field ",")* "}"
policy_def_field       ::= policy_def_hash | policy_def_script | policy_def_ref
policy_def_hash        ::= "hash"   ":" data_expr
policy_def_script      ::= "script" ":" data_expr
policy_def_ref         ::= "ref"    ":" data_expr
```

A *policy definition* either binds the policy to a literal hex-string
identifier (the assign form) or constructs it from a set of named fields
(the constructor form). The constructor form MAY include any subset of
`hash`, `script`, and `ref`. Field-level semantics are specified in §7.10.

### 4.2.5 Type

```
type_def ::= record_def | variant_def | alias_def

record_def ::= "type" identifier "{" (record_field ",")* "}"
record_field ::= identifier ":" type

variant_def        ::= "type" identifier "{" (variant_case ",")* "}"
variant_case       ::= variant_case_struct | variant_case_tuple | variant_case_unit
variant_case_struct ::= identifier "{" (record_field ",")* "}"
variant_case_tuple  ::= identifier "(" (type ",")* ")"
variant_case_unit   ::= identifier

alias_def ::= "type" identifier "=" type ";"
```

Notes:

- `record_def` and `variant_def` are syntactically disambiguated by the form
  of the items between `{` and `}`: bare `identifier : type` items form a
  record; items that start with a capitalised identifier followed by `{`,
  `(`, or `,`/`}` form a variant. Refer to §5.3 for the semantic
  distinction.
- The trailing comma after the last `record_field`, `variant_case`, or
  variant-case field is required by the grammar above and is conformant
  Tx3 style.

### 4.2.6 Transaction

```
tx_def         ::= "tx" identifier parameter_list "{" tx_body_block* "}"
parameter_list ::= "(" (parameter ("," parameter)* ","?)? ")"
parameter      ::= identifier ":" type
```

A *transaction definition* declares a parameterised template. The
`parameter_list` MAY be empty. Trailing commas are permitted.

## 4.3 Types

```
type ::= primitive_type | list_type | map_type | custom_type

primitive_type ::= "Int" | "Bool" | "Bytes" | "AnyAsset" | "Address" | "UtxoRef"
list_type      ::= "List" "<" type ">"
map_type       ::= "Map"  "<" type "," type ">"
custom_type    ::= identifier
```

A `custom_type` is an identifier that MUST resolve to a `record_def`,
`variant_def`, or `alias_def` (§6).

## 4.4 Data expressions

```
data_expr ::= data_prefix* data_primary data_postfix*
              ( data_infix data_prefix* data_primary data_postfix* )*

data_infix   ::= "+" | "-"
data_prefix  ::= "!"
data_postfix ::= "." identifier
               | "[" data_expr "]"

data_primary ::=
      unit_literal
    | utxo_ref_literal
    | hex_string
    | integer_literal
    | bool_literal
    | string_literal
    | struct_constructor
    | list_constructor
    | map_constructor
    | concat_constructor
    | any_asset_constructor
    | fn_call
    | identifier
    | "(" data_expr ")"

struct_constructor   ::= identifier variant_case_constructor
variant_case_constructor ::= explicit_variant_case_constructor
                           | implicit_variant_case_constructor

explicit_variant_case_constructor ::=
    "::" identifier "{" (record_constructor_field ",")* spread_expression? "}"

implicit_variant_case_constructor ::=
    "{" (record_constructor_field ",")* spread_expression? "}"

record_constructor_field ::= identifier ":" data_expr
spread_expression        ::= "..." data_expr

list_constructor ::= "[" (data_expr ",")* data_expr? "]"

map_constructor ::= "{" (map_field ",")+ "}"
map_field        ::= data_expr ":" data_expr

concat_constructor    ::= "concat" "(" data_expr "," data_expr ")"
any_asset_constructor ::= "AnyAsset" "(" data_expr "," data_expr "," data_expr ")"

fn_call ::= identifier "(" (data_expr ("," data_expr)* ","?)? ")"
```

### 4.4.1 Disambiguation of `{ … }`

A `{ … }` literal in expression position is parsed as:

1. `map_constructor` if at least one comma-separated entry has the form
   `data_expr ":" data_expr` and the left-hand side of the first entry is
   not a bare `identifier` immediately followed by `:` and a `data_expr`
   that is itself parseable as a value;
2. otherwise as an `implicit_variant_case_constructor` (only meaningful
   when the surrounding context is a `struct_constructor`).

In practice, a `map_constructor` MUST contain at least one entry, and an
`implicit_variant_case_constructor` is only valid as the right-hand side of
a `struct_constructor` (§4.4.2). Conforming compilers SHOULD disambiguate
based on whether the enclosing position expects an expression value
(`map_constructor`) versus a record-of-type-fields (`implicit_variant_case_constructor`).

### 4.4.2 Struct constructors

A `struct_constructor` writes:

- `T { f1: e1, f2: e2, }` — implicit case, only valid when `T` is the name
  of a single-case record type.
- `T::Case { f1: e1, ..base, }` — explicit case, valid for any variant
  type.
- `T::Case { }` — explicit case with no fields, valid for `variant_case_unit`
  cases.

The optional `spread_expression` (`...base`) MUST appear last and denotes
"take all fields from `base`, then override with the explicit fields".

### 4.4.3 List and map constructors

- A `list_constructor` MAY be empty (`[]`). A trailing comma is permitted.
- A `map_constructor` MUST contain at least one entry. There is no
  syntactic literal for an empty map in `v1beta0`.

## 4.5 Expression precedence and associativity

Data expressions in §4.4 are parsed with the following precedence, from
tightest binding (highest precedence) to loosest:

| Level | Operators                  | Associativity |
| ----- | -------------------------- | ------------- |
| 1     | `.` `[…]` (postfix)        | left          |
| 2     | `!` (prefix)               | non-assoc     |
| 3     | `+` `-` (infix)            | left          |

Parentheses `( data_expr )` may be used to override precedence.

There are no further operators in `v1beta0`. In particular, there are no
comparison operators, no boolean combinators, no multiplicative or
shift operators, and no ternary form.

## 4.6 Transaction-body blocks

```
tx_body_block ::=
      locals_block
    | reference_block
    | input_block
    | collateral_block
    | burn_block
    | mint_block
    | output_block
    | chain_specific_block
    | signers_block
    | metadata_block
    | validity_block
```

`tx_body_block`s MAY appear in any order; multiple blocks of the same kind
MAY appear in a single `tx_def` (subject to the per-block rules in §7).

### 4.6.1 Input

```
input_block       ::= "input" "*"? identifier "{" (input_block_field ",")* "}"
input_block_field ::=
      "from"       ":" data_expr
    | "datum_is"   ":" type
    | "min_amount" ":" data_expr
    | "redeemer"   ":" data_expr
    | "ref"        ":" data_expr
```

The optional `"*"` marks the input as *many* (see §7.2). The `identifier`
is the input's name (§7.2).

### 4.6.2 Reference

```
reference_block ::= "reference" identifier "{" "ref" ":" data_expr ","
                    ( "datum_is" ":" type "," )? "}"
```

A `reference_block` MUST include a `ref` field. The `datum_is` field is
optional and, if present, MUST be a `type`.

### 4.6.3 Collateral

```
collateral_block       ::= "collateral" "{" (collateral_block_field ",")* "}"
collateral_block_field ::=
      "from"       ":" data_expr
    | "min_amount" ":" data_expr
    | "ref"        ":" data_expr
```

A `collateral_block` has no name. Multiple collateral blocks MAY appear.

### 4.6.4 Output

```
output_block ::= "output" "?"? identifier? "{" (output_block_field ",")* "}"
output_block_field ::=
      "to"     ":" data_expr
    | "amount" ":" data_expr
    | "datum"  ":" data_expr
```

The optional `"?"` flag marks the output as *optional* (§7.3). The
`identifier`, when present, names the output for later reference.

### 4.6.5 Validity

```
validity_block       ::= "validity" "{" (validity_block_field ",")* "}"
validity_block_field ::=
      "until_slot" ":" data_expr
    | "since_slot" ":" data_expr
```

### 4.6.6 Mint and burn

```
mint_block       ::= "mint" "{" (mint_block_field ",")* "}"
burn_block       ::= "burn" "{" (mint_block_field ",")* "}"
mint_block_field ::=
      "amount"   ":" data_expr
    | "redeemer" ":" data_expr
```

`burn_block` shares the field grammar of `mint_block`.

### 4.6.7 Signers

```
signers_block ::= "signers" "{" (data_expr ",")* "}"
```

A `signers_block` is a (possibly empty) comma-separated list of data
expressions, each denoting a required signer (§7.7).

### 4.6.8 Metadata

```
metadata_block       ::= "metadata" "{" (metadata_block_field ",")+ "}"
metadata_block_field ::= data_expr ":" data_expr
```

A `metadata_block` MUST contain at least one entry. Syntactic keys are
arbitrary `data_expr`; §6.5 restricts them to integer-typed expressions.

### 4.6.9 Locals

```
locals_block  ::= "locals" "{" (locals_assign ",")+ "}"
locals_assign ::= identifier ":" data_expr
```

A `locals_block` MUST contain at least one assignment.

### 4.6.10 Chain-specific

```
chain_specific_block ::= cardano_block | bitcoin_block

cardano_block ::= "cardano" "::" cardano_inner

bitcoin_block ::= "bitcoin" "::" identifier
```

The `cardano_inner` productions are given in §8. `bitcoin_block` is
reserved syntax in `v1beta0`; conforming compilers MUST parse it but SHOULD
report a diagnostic indicating that the Bitcoin namespace is not defined by
this version of the specification.

## 4.7 Tokens used elsewhere

The terminal productions `identifier`, `integer_literal`, `bool_literal`,
`string_literal`, `hex_string`, `unit_literal`, and `utxo_ref_literal` are
defined lexically in §3.5.
