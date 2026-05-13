# 5. Type system

Tx3 is statically typed. Every value-bearing expression has a type that is
known after analysis. The type system is nominal, monomorphic, and has no
implicit coercions.

This section defines the types of `v1beta0`. The rules that compute types
for expressions and check them in context are given in §6.

## 5.1 Primitive types

The following types are *primitive* and reserved as keywords (§3.6):

| Type       | Surface keyword | Value space (informal)                                                |
| ---------- | --------------- | --------------------------------------------------------------------- |
| `Int`      | `Int`           | Signed integers; conforming compilers MUST support at least 64-bit two's-complement (§3.5.1). |
| `Bool`     | `Bool`          | The two values `true` and `false`.                                    |
| `Bytes`    | `Bytes`         | Finite-length byte strings.                                           |
| `Address` | `Address`       | A chain address; the concrete representation is chain-defined.        |
| `UtxoRef` | `UtxoRef`       | A reference to a previous transaction output (tx hash + output index). |
| `AnyAsset`| `AnyAsset`      | A `(policy, asset_name, amount)` triple denoting a quantity of one asset class. |

In addition, the type `Unit` exists as the type of the unit literal `()`
(§3.5.5). `Unit` is **not** a surface keyword and cannot be written in a
`type` position; it arises only implicitly from `()` expressions.

## 5.2 Compound types

```
List<T>      ::  list of values, each of type T
Map<K, V>    ::  finite mapping from keys of type K to values of type V
```

Both `List` and `Map` are *parametric type constructors* applied at the
syntactic level. They do not introduce parametric polymorphism into the
expression language: each occurrence of `List<T>` or `Map<K, V>` is a
distinct concrete type.

Constraints:

- `T`, `K`, and `V` MAY be any type, including other compound types and
  user-defined types.
- A `List<T>` constructed from a `list_constructor` (§4.4) takes its
  element type from the type of its first element; an empty
  `list_constructor` MUST appear in a context that supplies the element
  type, or its type is `List<Undefined>` (an internal placeholder; see
  §6.6).
- A `Map<K, V>` constructed from a `map_constructor` takes `K` and `V`
  from the first entry; subsequent entries' key and value types MUST match
  (§6.3).

## 5.3 User-defined types

Three forms of user-defined type are introduced by `type_def` (§4.2.5):

### 5.3.1 Records

A *record* is declared with a single brace-delimited body whose entries are
all `identifier : type` pairs:

```tx3
type State {
    lock_until: Int,
    owner: Bytes,
    beneficiary: Bytes,
}
```

A record is a nominal product type. Its values are constructed with a
single-case implicit `struct_constructor` (§4.4.2):

```tx3
State { lock_until: until, owner: Owner, beneficiary: Beneficiary, }
```

A record is semantically equivalent to a `variant_def` with exactly one
`variant_case_struct` whose name matches the type name.

### 5.3.2 Variants

A *variant* is declared with a single brace-delimited body whose entries are
all `variant_case`s (struct, tuple, or unit form):

```tx3
type Status {
    Pending,
    Active { since: Int, },
    Cancelled(Bytes,),
}
```

A variant value is constructed with an explicit `struct_constructor`
naming the variant type and the chosen case:

```tx3
Status::Pending { }
Status::Active { since: 42, }
```

Each `variant_case` introduces a constructor whose case name MUST be unique
within the variant.

Tuple-form cases (`Foo(Int, Bytes,)`) are accepted by the grammar (§4.2.5)
but `v1beta0` does not define an expression-level construction syntax for
them. Implementations SHOULD reject construction of a tuple-form case until
the construction syntax is specified in a future version.

### 5.3.3 Aliases

An *alias* introduces a new name for an existing type:

```tx3
type Amount = Int;
type Owner  = Bytes;
```

An alias is transparent: at every position, `Amount` is interchangeable with
`Int`. Aliases MAY chain through other aliases; cycles are an error (§6.6).

## 5.4 Built-in compound semantics

Two of the primitive types carry built-in *properties* accessible via the
`.` operator. These are part of the type, not part of any user-defined
shape:

| Type      | Property      | Property type | Meaning                                  |
| --------- | ------------- | ------------- | ---------------------------------------- |
| `AnyAsset`| `policy`      | `Bytes`       | Asset class policy hash.                 |
| `AnyAsset`| `asset_name`  | `Bytes`       | Asset class name.                        |
| `AnyAsset`| `amount`      | `Int`         | Quantity.                                |
| `UtxoRef` | `tx_hash`     | `Bytes`       | Transaction hash component.              |
| `UtxoRef` | `output_index`| `Int`         | Output index component.                  |

Additional built-in properties MAY be added in future versions and MUST NOT
shadow user-defined property names of the same type. Where a user-defined
custom type and a primitive both expose a property of the same name, the
property of the actual value's type takes precedence.

`List<T>` supports indexing (`xs[i]`) where `i` is of type `Int` (§5.5.2).

## 5.5 Operators

The data-expression language supports a small fixed set of operators.

### 5.5.1 Arithmetic

The infix operators `+` and `-` are defined for the following operand-type
pairs, both arities binary, left-associative:

| Left type   | Right type  | Result type | Meaning                          |
| ----------- | ----------- | ----------- | -------------------------------- |
| `Int`       | `Int`       | `Int`       | Integer addition / subtraction. |
| `AnyAsset`  | `AnyAsset`  | `AnyAsset`  | Asset-quantity aggregation.     |

Where the left and right operands have compatible compound types that
contain `Int` or `AnyAsset` components (such as types representing UTxO
values), implementations MAY extend `+` and `-` to those types provided the
extension is consistent across operand pairs. Such extensions are out of
the scope of this version of the specification.

The prefix operator `!` is defined as **arithmetic negation** when applied
to an operand of numeric type:

| Operand type | Result type |
| ------------ | ----------- |
| `Int`        | `Int`       |

`!` is **not** boolean negation. There is no boolean negation operator in
`v1beta0`.

### 5.5.2 Property and index

The postfix `.` operator takes a value of a type that defines properties
(§5.4 or user-defined records / variant struct cases) and yields the value
of the named property. The property MUST exist on the value's static type
or the compiler MUST reject the expression (§6.6).

The postfix `[…]` operator takes a value of type `List<T>` and an index
expression of type `Int`, yielding a value of type `T`. Indexing on
non-list types is a static error (§6.6).

### 5.5.3 `concat`

The built-in `concat(a, b)` yields a value of the same type as `a`. It is
defined for:

| Operand type | Result type |
| ------------ | ----------- |
| `Bytes`      | `Bytes`     |
| `List<T>`    | `List<T>`   |

Both operands MUST be of the same type after analysis.

### 5.5.4 `AnyAsset`

The built-in `AnyAsset(policy, asset_name, amount)` yields a value of type
`AnyAsset`. Its arguments MUST have types `Bytes`, `Bytes`, and `Int`
respectively.

## 5.6 Built-in functions

The following identifiers, when used in `fn_call` position (§4.4), denote
built-in functions provided by every conforming compiler:

| Name             | Argument types                  | Result type | Notes |
| ---------------- | ------------------------------- | ----------- | ----- |
| `min_utxo`       | `(output-name : Identifier)`    | `AnyAsset`  | Computes the minimum lovelace required to make the named output meet the chain's UTxO minimum. The argument is an identifier that MUST resolve to an `output_block` (§7.5). |
| `tip_slot`       | `()`                            | `Int`       | The current chain tip slot at resolution time. |
| `slot_to_time`   | `(slot : Int)`                  | `Int`       | Converts a slot number to a POSIX time. |
| `time_to_slot`   | `(time : Int)`                  | `Int`       | Converts a POSIX time to a slot number. |

`min_utxo` is special: its single argument is treated as an identifier
reference rather than a general data expression. `tip_slot` MAY be invoked
either as `tip_slot()` or as the bare identifier `tip_slot` (the latter
form is accepted but using the call form is RECOMMENDED).

These names occupy the program-global namespace and SHOULD NOT be shadowed
by user-defined identifiers.

## 5.7 Built-in symbols

In addition to the built-in functions above, the following bare identifiers
are implicitly bound inside every `tx` body:

| Identifier | Kind  | Type      | Meaning                                                                 |
| ---------- | ----- | --------- | ----------------------------------------------------------------------- |
| `fees`     | value | `AnyAsset` | The transaction fee. Subtract from inputs / change as needed (§7.4). |
| `Ada`      | asset | (asset)   | The chain's native primary asset, available as an asset constructor (e.g. `Ada(1_000_000)`). Defined by a built-in `asset_def` (§5.8). |

## 5.8 Implicit `Ada` asset

Every program implicitly includes the asset definition equivalent to
`asset Ada = ... . ...;` where the policy and asset-name are chain-defined.
Programs SHOULD NOT define a top-level `asset Ada` themselves; doing so
results in a duplicate-definition error (§6.2).

## 5.9 Type equivalence and assignability

Two types `A` and `B` are *type-equivalent* if they are the same primitive,
the same `List<T>` (with equivalent `T`), the same `Map<K, V>` (with
equivalent `K` and `V`), refer to the same user-defined type definition
(after alias chasing), or denote the same internal `Unit`/`Undefined`
placeholder.

A value of type `A` is *assignable* to a position expecting type `B`
exactly when `A` and `B` are type-equivalent. There are no implicit
coercions in `v1beta0`. In particular:

- `Int` does not implicitly convert to `Bytes` or vice versa.
- `Bytes` does not implicitly convert to `Address`, `UtxoRef`, or
  `AnyAsset`, even when its byte representation would be valid for the
  target type. Conversions, where allowed, are expressed by constructors
  or fields, not by coercion.
