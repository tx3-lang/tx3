# 7. Transaction semantics

This section specifies the meaning of each top-level definition kind and
each `tx`-body block. Where a block carries a name, the rules for that
name appear in the block's own subsection.

## 7.1 What a transaction template declares

A `tx_def` declares a *transaction template*: a parameterised description
of how to assemble one concrete transaction. The template fixes the
**shape** of the transaction (which inputs are consumed, which outputs
are produced, etc.), but does not fix the **values** that participate.
Values arrive at resolution time from:

- the user-supplied arguments bound to the `parameter_list`;
- the `env` block, supplied at compile or resolution time;
- the chain state observed by the resolver.

Tx3 does not specify how a resolver computes balances, selects UTxOs, or
calculates fees. It only specifies what the source program guarantees
about the resulting transaction's shape.

## 7.2 `input` blocks

An `input_block` declares a UTxO consumed by the transaction.

### Fields

| Field        | Required | Type    | Meaning                                                                 |
| ------------ | -------- | ------- | ----------------------------------------------------------------------- |
| `from`       | one of `from`/`ref` MUST be present | *party* / *policy* / `Address` | The credential at which the UTxO is held. |
| `ref`        | see above | `UtxoRef` | An explicit UTxO reference. Selects exactly that UTxO. |
| `min_amount` | optional | `AnyAsset` | A lower bound on the value the input must contain. |
| `redeemer`   | optional | any     | A redeemer to attach when the input is script-locked. |
| `datum_is`   | optional | type    | The expected datum type carried by the input (§7.6). |

### Name

Every `input_block` carries a mandatory identifier. The identifier names
the resolved input value within the transaction template and binds a
symbol of kind *input* in the tx scope.

The identifier MAY be used in other expressions in the same tx body as a
value of the special type *Utxo* whose accessible properties are:

- the datum, with the type declared by `datum_is` (if any);
- the asset value, which participates in `+` and `-` against `AnyAsset`
  and other *Utxo* identifiers.

### The `*` flag

An `input` may be marked with a postfix `*` (between `input` and the
identifier) to indicate *many*. A many-input matches any number of UTxOs
(including zero) satisfying the block's predicates. The resolver
aggregates them; the surface language treats `xs` of `input* xs { … }`
as a single aggregate value. The detailed behaviour of `*` is specified
informally in `v1beta0` and refined by the resolver in TRP.

### Field combinations

- `from` and `ref` MAY both be specified; doing so constrains the
  resolution further but does not change the input's static type.
- `redeemer` is only meaningful when the input is script-locked;
  resolvers MAY ignore a redeemer set on a non-script input.

## 7.3 `output` blocks

An `output_block` declares a UTxO produced by the transaction.

### Fields

| Field    | Required | Type     | Meaning                                                                 |
| -------- | -------- | -------- | ----------------------------------------------------------------------- |
| `to`     | required | *party* / *policy* / `Address` | The recipient credential. |
| `amount` | required | `AnyAsset` | The value attached to the output. |
| `datum`  | optional | any (custom type) | A datum to attach. MUST NOT be set on optional outputs (§6.8.1). |

### Name

An `output_block` MAY carry an identifier (after the optional `?` flag).
Named outputs bind a symbol of kind *output* in the tx scope, addressable
by other blocks (most commonly by `min_utxo`).

### The `?` flag

An output marked with `?` is *optional*: if the resolver's value-balancing
pass would otherwise produce a zero-amount output, the optional output
MAY be omitted from the resulting transaction. Optional outputs MUST NOT
carry a `datum` field (§6.8.1).

## 7.4 The `fees` identifier

Inside every `tx` body, the bare identifier `fees` denotes the fee paid
by the transaction. It has type `AnyAsset` (specifically the chain's
native fee asset). It is typically subtracted from a change output:

```tx3
output {
    to: Sender,
    amount: source - Ada(quantity) - fees,
}
```

`fees` is not a parameter; the resolver computes the concrete fee. The
source language guarantees only that the symbol `fees` is in scope and
that the expression's static type matches the surrounding context.

## 7.5 `min_utxo` built-in

`min_utxo(out)` evaluates to the minimum chain-required value (typically
in Ada) needed to make the output named `out` viable. Its single argument
MUST be an identifier that resolves to an *output* symbol within the same
tx scope. Use it as the `amount` field of the named output itself, or
inside a downstream value computation.

```tx3
output target {
    to: Recipient,
    amount: min_utxo(target),
}
```

## 7.6 `reference` blocks

A `reference_block` declares a read-only UTxO referenced (but not
consumed) by the transaction.

### Fields

| Field        | Required | Type    | Meaning |
| ------------ | -------- | ------- | ------- |
| `ref`        | required | `UtxoRef` | The referenced UTxO. |
| `datum_is`   | optional | type    | The expected datum type. When present, property access on the reference identifier (e.g. `oracle.price`) is typed against this type. |

### Name

Every `reference_block` carries a mandatory identifier, binding a symbol
of kind *reference* in the tx scope.

## 7.7 `signers` blocks

A `signers_block` declares additional required signers for the
transaction:

```tx3
signers {
    Authority,
    co_signer,
}
```

Each entry is a `data_expr` that MUST evaluate to an *address-like* value
(a party, a policy with a hash, a hex string, or an expression of type
`Address` / `Bytes`).

The block MAY be empty. At most one `signers_block` per `tx_def` is
RECOMMENDED; the meaning of multiple `signers_block`s is the union of
their entries.

## 7.8 `validity` blocks

A `validity_block` declares the slot range in which the transaction is
valid:

```tx3
validity {
    since_slot: 1000,
    until_slot: 2000,
}
```

Both fields are optional. When present, each is a `data_expr` of type
`Int`. At most one `validity_block` per `tx_def` is RECOMMENDED.

## 7.9 `mint` and `burn` blocks

A `mint_block` declares assets minted by the transaction; a `burn_block`
declares assets burnt. They share the field grammar of `mint_block_field`.

### Fields

| Field      | Required | Type        | Meaning |
| ---------- | -------- | ----------- | ------- |
| `amount`   | required | `AnyAsset`  | The asset value minted or burnt. |
| `redeemer` | optional | any         | A redeemer to attach to the underlying minting policy. |

Multiple `mint_block`s and `burn_block`s MAY appear in a single `tx_def`;
each declares one independent mint/burn directive.

## 7.10 `metadata` blocks

A `metadata_block` attaches metadata entries to the transaction:

```tx3
metadata {
    721: "Some payload",
    42:  0xdeadbeef,
}
```

Each entry's key MUST be an integer expression (§6.5). Each entry's
value, when a literal `string_literal` or `hex_string`, MUST have byte
length ≤ 64 (§6.5). Multiple `metadata_block`s in one `tx_def` are
RECOMMENDED against; their meaning is the union of all entries.

## 7.11 `locals` blocks

A `locals_block` introduces named expressions visible in the rest of the
tx body:

```tx3
locals {
    total: source - fees,
    refund: source - Ada(quantity),
}
```

Each `locals_assign` binds a symbol of kind *local-expr* in the tx scope.
The bound expression is substituted at each use site (modulo
re-evaluation rules at resolution time, which are out of scope for this
specification).

A name introduced by a `locals_assign` MUST NOT collide with any
parameter, input name, named output, or reference name in the same tx
scope (§6.2).

## 7.12 `collateral` blocks

A `collateral_block` declares UTxO(s) supplied as collateral for
script-bearing transactions on chains that require it.

### Fields

| Field        | Required | Type    | Meaning |
| ------------ | -------- | ------- | ------- |
| `from`       | optional | *party* / `Address` | Credential supplying the collateral. |
| `min_amount` | optional | `AnyAsset` | Lower bound on the collateral value. |
| `ref`        | optional | `UtxoRef` | Explicit collateral UTxO. |

A `collateral_block` has no name. Multiple blocks MAY appear in one
`tx_def`; each declares one independent collateral input.

The use of collateral is chain-dependent. On chains that do not require
collateral, conforming compilers SHOULD warn when a `collateral_block`
appears.

## 7.13 Top-level definitions

### 7.13.1 `env`

```tx3
env {
    network: Bytes,
    treasury: Address,
}
```

`env` fields are typed external inputs supplied at compile or resolution
time. They are in scope everywhere except inside type definitions. They
MUST be referenced by name only; arithmetic and property access work on
them according to their declared type.

### 7.13.2 `asset`

```tx3
asset MyToken = 0xabcd. "MyToken";
```

Binds `MyToken` to the asset class `(policy, asset_name)` where both
arguments have type `Bytes`. Used either as a value-producing constructor
(`MyToken(100)` is an `AnyAsset`) or as an identifier in
`AnyAsset`-typed expressions.

When applied as a constructor, an asset identifier `A` takes one argument
of type `Int` and yields a value of type `AnyAsset` whose `policy` and
`asset_name` are those declared by `A`.

### 7.13.3 `party`

```tx3
party Alice;
```

Declares an abstract participant. A `party` is supplied a concrete
address at resolution time. In `tx` bodies, a party identifier is
acceptable wherever an `Address`-typed expression is expected (most
notably in `from`, `to`, `signers`).

### 7.13.4 `policy`

```tx3
policy TimeLock = 0x6b9c...;

policy MyPolicy {
    hash:   0xabcd...,
    script: some_script_value,
    ref:    some_ref,
}
```

A `policy_def_assign` form fixes the policy's hash to a literal hex
string. A `policy_def_constructor` form derives the policy from any
subset of `hash`, `script`, and `ref` expressions.

A `policy` identifier may be used as a *party-like* value (in `from`,
`to`, `signers`), in which case it denotes the script credential
implied by the policy.

The semantics of `script` and `ref` fields are chain-specific and are
described in §8 for Cardano targets.

## 7.14 Cross-block constraints

Within a single `tx_def`, the following requirements apply across blocks:

- Each named symbol (input, reference, named output, local) MUST be
  uniquely named within the tx scope (§6.2).
- At least one `output_block`, `mint_block`, `burn_block`, `signers_block`,
  `metadata_block`, `validity_block`, or chain-specific block SHOULD be
  present; a `tx_def` with no body blocks is syntactically valid but
  rarely useful.
- Value balancing (the sum of inputs and mints equals the sum of outputs,
  burns, and fees) is not enforced by the source language. It is the
  resolver's responsibility (per TRP) and SHOULD be a runtime error of
  the resolver, not a static-semantic error of this specification.
