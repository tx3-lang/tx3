# 8. Cardano extensions

The `cardano::` namespace introduces transaction-body blocks that are
specific to the Cardano family of chains. These constructs are first-class
in `v1beta0` but do not participate in the chain-agnostic core (§7).

A `cardano_block` (per §4.6.10) selects exactly one of the inner forms
below.

## 8.1 Namespace grammar

```
cardano_inner ::=
      cardano_stake_delegation_certificate
    | cardano_vote_delegation_certificate
    | cardano_withdrawal_block
    | cardano_plutus_witness_block
    | cardano_native_witness_block
    | cardano_treasury_donation_block
    | cardano_publish_block
```

Each occurrence is introduced by the prefix `cardano::`.

## 8.2 Conformance scope

A compiler that declares conformance with the Cardano target MUST
accept every construct in this section. A compiler that targets a chain
other than Cardano MAY reject every `cardano::*` block; if it does, it
MUST do so with a clear diagnostic naming the chain mismatch.

## 8.3 Withdrawal

```
cardano_withdrawal_block ::=
    "cardano" "::" "withdrawal" "{" (withdrawal_field ",")+ "}"

withdrawal_field ::=
      "from"     ":" data_expr
    | "amount"   ":" data_expr
    | "redeemer" ":" data_expr
```

| Field      | Required | Type       | Meaning |
| ---------- | -------- | ---------- | ------- |
| `from`     | required | *party* / *policy* / `Address` | The stake credential being withdrawn from. |
| `amount`   | required | `Int`      | The amount, in lovelace, being withdrawn. |
| `redeemer` | optional | any        | Redeemer for a script stake credential. |

A `withdrawal_block` MUST include at least the `from` and `amount`
fields.

## 8.4 Plutus witness

```
cardano_plutus_witness_block ::=
    "cardano" "::" "plutus_witness" "{" (plutus_witness_field ",")+ "}"

plutus_witness_field ::=
      "version" ":" data_expr
    | "script"  ":" data_expr
```

| Field     | Required | Type   | Meaning |
| --------- | -------- | ------ | ------- |
| `version` | optional | `Int`  | The Plutus language version (e.g. 1, 2, 3). |
| `script`  | optional | `Bytes` | Inline Plutus script. |

A `plutus_witness_block` MUST include at least one field. At resolution
time, the supplied fields constitute a script witness attached to
script-using inputs, mints, or certificates that require one.

## 8.5 Native witness

```
cardano_native_witness_block ::=
    "cardano" "::" "native_witness" "{" (native_witness_field ",")+ "}"

native_witness_field ::=
    "script" ":" data_expr
```

| Field    | Required | Type    | Meaning |
| -------- | -------- | ------- | ------- |
| `script` | required | `Bytes` | Inline native (multi-sig) script. |

## 8.6 Treasury donation

```
cardano_treasury_donation_block ::=
    "cardano" "::" "treasury_donation" "{" "coin" ":" data_expr "," "}"
```

| Field  | Required | Type  | Meaning                                   |
| ------ | -------- | ----- | ----------------------------------------- |
| `coin` | required | `Int` | The donation amount in lovelace. |

## 8.7 Stake-delegation certificate

```
cardano_stake_delegation_certificate ::=
    "cardano" "::" "stake_delegation_certificate" "{"
        (record_constructor_field ",")*
    "}"
```

The block syntactically takes a sequence of `record_constructor_field`
entries (§4.4). The fields recognised by `v1beta0` are:

| Field   | Required | Type     | Meaning |
| ------- | -------- | -------- | ------- |
| `pool`  | required | `Bytes` / *policy* / *party* | Pool ID being delegated to. |
| `stake` | required | *party* / *policy* / `Address` | The stake credential delegating. |

Implementations SHOULD reject this block if the required fields are
absent. Lowering of this certificate is implementation-defined in
`v1beta0`; conforming compilers MAY emit a "not yet supported"
diagnostic until the corresponding TIR directive is finalised.

## 8.8 Vote-delegation certificate

```
cardano_vote_delegation_certificate ::=
    "cardano" "::" "vote_delegation_certificate" "{"
        "drep"  ":" data_expr ","
        "stake" ":" data_expr ","
    "}"
```

| Field   | Required | Type     | Meaning |
| ------- | -------- | -------- | ------- |
| `drep`  | required | `Bytes` / *party* / *policy* | The DRep being delegated to. |
| `stake` | required | *party* / *policy* / `Address` | The stake credential delegating. |

Both fields MUST be present (enforced by the grammar above).

## 8.9 Publish (reference script)

```
cardano_publish_block ::=
    "cardano" "::" "publish" identifier? "{" (publish_field ",")* "}"

publish_field ::=
      "to"      ":" data_expr
    | "amount"  ":" data_expr
    | "datum"   ":" data_expr
    | "version" ":" data_expr
    | "script"  ":" data_expr
```

| Field     | Required | Type    | Meaning |
| --------- | -------- | ------- | ------- |
| `to`      | required | *party* / *policy* / `Address` | Recipient of the publish output. |
| `amount`  | optional | `AnyAsset` | Value attached to the publish output. If absent, the resolver SHOULD attach the minimum required value (§7.5). |
| `datum`   | optional | any (custom type) | A datum to attach. |
| `version` | optional | `Int`   | The Plutus language version when publishing a Plutus script. |
| `script`  | required | `Bytes` | The script bytes to be published as a reference script. |

The optional `identifier` names the publish output for later reference,
following the same rules as `output_block` names (§7.3).

A `publish` block produces a single transaction output whose
reference-script slot is set to the supplied `script`. The kind of
script (native vs Plutus, and which Plutus version) is determined by the
presence and value of `version`.

## 8.10 Cross-block interaction

- `cardano::plutus_witness` and `cardano::native_witness` blocks attach
  to the surrounding transaction as a whole; the resolver pairs them
  with the script-using inputs, mints, certificates, or withdrawals
  that require them.
- `cardano::withdrawal` and `cardano::*_delegation_certificate` blocks
  are independent directives; multiple of each MAY appear in one
  `tx_def`.
- A `cardano::publish` block produces a transaction output in addition
  to any declared `output_block`s; it does not replace them.
