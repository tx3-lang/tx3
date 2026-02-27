# Aspirational Examples

The `.tx3` files in this directory represent real-world Cardano contract
interfaces (JPG Store, Levvy Finance, SplashDEX, etc.) written in Tx3 syntax
that is **not yet supported** by the language.

These examples are **non-functional** and will not pass `trix check`. They exist
to capture desired language features and serve as design references for future
development.

## Why these fail

| File                   | Unsupported features used                                                                                                   |
|------------------------|-----------------------------------------------------------------------------------------------------------------------------|
| `jpg.tx3`              | Old-style policy constructors, array type syntax (`[]`), `outputs ... as` iteration, `required_signers:`, inline `metadata` |
| `levvy.simple.tx3`     | Old-style policy constructors, `PlutusAddress()`, `now()`, `valid_from`/`valid_until`                                       |
| `levvy.tx3`            | All of the above plus `batch foreach`, `filter` lambdas, `let` bindings, `if/else`, `.sum()`, `Address()`                   |
| `splash.tx3`           | Old-style policy constructors, `Option<>`, array type syntax, `IndexOf()`, `Asset()`                                        |
| `plutus_addresses.tx3` | `func` definitions, `return`, `if/else`, `Hash<>` generics, union types (`\|`)                                              |
| `spans.tx3`            | References non-existent fields on inputs; unbalanced transaction                                                            |