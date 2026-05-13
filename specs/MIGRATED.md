# Specs have moved

The TII and TRP specs that used to live here are now maintained in their own repositories:

- **TII** (Transaction Invocation Interface): [tx3-lang/tii](https://github.com/tx3-lang/tii) → `v1beta0/tii.json`
- **TRP** (Transaction Resolver Protocol): [tx3-lang/trp](https://github.com/tx3-lang/trp) → `v1beta0/trp.json`

The previously separate `core.json` `$defs` (`Address`, `UtxoRef`, `TirEnvelope`, `ArgMap`, `EnvMap`) have been inlined into each spec, so both `tii.json` and `trp.json` are now self-contained — no external `$ref` resolution required.

The legacy `v1alpha8/trp/openrpc.json` is not carried forward; it remains accessible through this repository's git history.
