# 9. Conformance

This section defines what it means for a Tx3 source program and a Tx3
compiler to conform to `v1beta0` of this specification.

## 9.1 Conforming source program

A *conforming Tx3 program* is a UTF-8-encoded source file (or set of
source files) that satisfies every MUST and MUST NOT in §3–§8. In
particular, a conforming program:

- contains only tokens defined in §3;
- parses as a `program` under the grammar of §4;
- satisfies the type rules of §5 and the static-semantic rules of §6;
- uses each `tx`-body block according to §7;
- uses each `cardano::*` block according to §8 (when targeting Cardano).

A program that uses `bitcoin::` syntax (§4.6.10) is not a conforming
program under `v1beta0`. Programs MAY use the syntax to anticipate
future Bitcoin support, but they MUST NOT expect that support from a
`v1beta0` compiler.

## 9.2 Conforming compiler

A *conforming Tx3 compiler* is a software artefact that, given a Tx3
source file:

1. **MUST** report no diagnostics, beyond optional warnings, when given a
   conforming program. The program MUST be processed up to and including
   the static-semantic phase (§2.5) without rejection.
2. **MUST** emit at least one diagnostic in the appropriate category
   (§6.9) for any program that violates a MUST or MUST NOT from §3–§8.
3. **MUST** preserve the meaning of every conforming program when it
   produces output. The lowering target (TIR or otherwise) is out of
   scope for `v1beta0`, but the compiler MUST NOT silently alter the
   program's declared shape.

A conforming compiler MAY:

- Emit additional diagnostics, warnings, or lints beyond those required.
- Support source-level features beyond those defined here, provided
  every conforming program continues to be accepted with the meaning
  specified.
- Refuse `cardano::*` blocks if it does not target Cardano (§8.2). It
  MUST emit a clear diagnostic explaining the refusal.

A conforming compiler MUST NOT:

- Accept a non-conforming program as conforming.
- Re-interpret the meaning of an existing construct in a way that
  contradicts §5–§8.
- Use the `bitcoin::` namespace (§4.6.10) to attach behaviour without a
  separate specification authorising it.

## 9.3 Extensions

The specification leaves the following extension points open:

- **The `bitcoin::` namespace.** Reserved; not defined by `v1beta0`.
- **Chain-specific extensions beyond Cardano.** New namespaces MAY be
  introduced by future versions, in dedicated sections analogous to §8.
- **Built-in functions and properties.** Future versions MAY add
  built-ins (§5.6, §5.4). Added built-ins MUST NOT shadow names already
  in use by conforming programs.

A compiler that implements extensions SHOULD gate them behind an
explicit opt-in (a command-line flag, a manifest field, or similar) and
SHOULD reject extension-using programs when the opt-in is not present.

## 9.4 Versioning

Implementations SHOULD report the highest specification version they
conform to. When a future version of this specification introduces a
breaking change relative to `v1beta0`, that future version lives in a
new sibling directory under `specs/` and conformance with it is a
separate, distinct claim.

A compiler MAY conform to more than one version simultaneously, in
which case it SHOULD provide a mechanism (e.g. a per-file directive or a
manifest field) for source programs to select the intended version.

## 9.5 Known divergences from the reference implementation

The following items are tracked divergences between this specification
and the reference implementation in
[`tx3/crates/tx3-lang`](../../crates/tx3-lang/) at the time `v1beta0`
was written. They are listed here so that implementers can predict the
direction of upcoming changes. The specification is normative; the
implementation will be brought into alignment.

1. **Multiple `env`, `signers`, `validity`, or `metadata` blocks.** The
   reference parser accepts more than one of each, with the AST taking
   only one. This specification declares "RECOMMENDED at most one"
   (§7.7, §7.8, §7.10) and leaves any tightening to a future version.
2. **Tuple-form variant constructors.** §5.3.2 forbids construction of
   tuple-form variant cases pending a defined construction syntax. The
   reference parser does not currently provide such syntax either.
3. **Hex-string literals of odd length.** Behaviour is
   implementation-defined (§3.5.3). The reference implementation
   currently accepts them silently.
4. **`bitcoin::` namespace.** The reference parser parses
   `bitcoin :: identifier` but the analyzer has no support for it
   (§4.6.10). This specification reserves the namespace; the
   implementation SHOULD reject use with a clear message.
5. **`Address`-typed primitive literals.** `Address` is a primitive type
   (§5.1) but `v1beta0` defines no literal syntax for it. Values of
   type `Address` are produced by parties, policies, env fields, or
   parameters, or by string/hex coercion at resolution time.
6. **Implicit `Ada` semantics.** The implicit `asset Ada = …;` (§5.8)
   is part of the reference compiler's behaviour. The specific policy
   and asset-name attached to `Ada` are chain-defined and are not part
   of the source language; they are documented in the resolver
   specification.

When a future patch of `v1beta0` is published, items resolved upstream
will be removed from this list.
