# 2. Domain model

This section describes, informally, the abstract entities that a Tx3 program
manipulates. It is non-normative; precise definitions follow in §3–§8. It is
intended to give the reader a mental model before encountering the grammar.

## 2.1 Programs

A Tx3 *program* is a sequence of top-level definitions held in one or more
source files. The top-level definitions, in their declared kinds, are:

- *environment* (`env`): typed external inputs supplied at compile or
  resolution time.
- *asset definitions* (`asset`): named native asset classes.
- *party definitions* (`party`): named participants in a transaction.
- *policy definitions* (`policy`): named minting/spending policies, either
  declared by hash literal or constructed from constituent fields.
- *type definitions* (`type`): records, sum types (variants), and aliases.
- *transaction templates* (`tx`): parameterised templates for transactions.

A program declares at most one `env` block. All other top-level kinds may
appear zero or more times. The order of top-level definitions is not
significant for resolution (see §6.1).

## 2.2 Transaction templates

A *transaction template* is the central artefact of Tx3. A template
declares:

- a name and a list of typed *parameters*;
- a body composed of *blocks* (input, output, mint, burn, reference,
  collateral, validity, signers, metadata, locals, plus chain-specific
  extension blocks).

A template is **not** a transaction. It is a recipe for producing one. Given a
template, a set of argument values for its parameters, an environment, and a
chain snapshot, a resolver (see TRP) produces a concrete transaction in the
TIR wire format, which is then encoded to the target chain's native format.

The act of *declaring* a template is what this specification governs. The act
of *resolving* a template into a transaction is governed by TRP and is out of
scope.

## 2.3 Data expressions

Most positions inside a transaction template that hold a value are filled by
a *data expression*. Data expressions cover:

- literal values (integers, booleans, byte strings, strings, UTxO references,
  the unit value);
- compound constructors (records, variants, lists, maps, asset values);
- identifier references to parameters, locals, parties, policies, assets,
  inputs, references, outputs, type-defined symbols, and a small set of
  built-ins;
- a restricted set of operators: addition (`+`), subtraction (`-`), prefix
  negation (`!`), property access (`.`), and indexing (`[…]`);
- a small set of built-in functions: `min_utxo`, `tip_slot`, `slot_to_time`,
  `time_to_slot`, plus `concat` and the `AnyAsset` constructor.

The expression language is intentionally restricted. It has no control flow,
no user-defined functions, no comparison operators, and no boolean
combinators. Everything that requires computation beyond simple arithmetic
and aggregation is the job of the resolver, not the source language.

## 2.4 Inputs, outputs, references, and named values

A transaction template names some of its component values so they can be
referred to from elsewhere in the same template:

- *Input blocks* carry a required identifier. The identifier denotes the UTxO
  that the input resolves to. Property access (e.g. `source.policy`) and
  arithmetic on input identifiers (e.g. `source - fees`) are common.
- *Output blocks* may carry an optional identifier. A named output can be
  referenced by other blocks within the same template (e.g. as the argument
  to `min_utxo`).
- *Reference blocks* carry a required identifier and denote a read-only UTxO
  reference.
- *Locals blocks* introduce named expressions that abbreviate sub-expressions
  used elsewhere in the template.

Within the body of a `tx`, these names live in a single shared scope (see
§6.1). Names need not be declared before use; in particular, output names may
be referenced from other blocks regardless of textual order.

## 2.5 The compilation pipeline (informative)

A conforming compiler processes a Tx3 program in the following phases. The
spec is normative only over the first two; lowering is described informatively
to set the boundary with TIR.

1. **Lexing and parsing.** The source text is tokenised and parsed into an
   abstract syntax tree per §3 and §4.
2. **Analysis.** The AST is checked for the validation rules in §5 and §6.
   Symbol references are resolved against the surrounding scope.
3. **Lowering.** The analyzed AST is lowered to TIR. Lowering is described by
   the TIR specification, not by this document.

Errors detected in phase 1 are *syntax errors*. Errors detected in phase 2
are *semantic errors*. A conforming compiler MUST emit at least one diagnostic
for any program that violates a rule from §3–§8 (see §9).

## 2.6 Chain genericity

The core of the language (§3–§7) is intended to be chain-agnostic. Constructs
that are specific to a particular target chain live under a dedicated
namespace. The only fully defined namespace in `v1beta0` is `cardano::` (§8).
The token `bitcoin` is reserved for a future Bitcoin namespace and is not
otherwise specified by this version.
