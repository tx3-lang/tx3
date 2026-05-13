# 3. Lexical structure

This section defines how a Tx3 source file is decomposed into tokens.

## 3.1 Source representation

A Tx3 source file is a sequence of Unicode characters encoded as UTF-8.
Conforming compilers MUST accept UTF-8 input. Behaviour on input that is not
well-formed UTF-8 is undefined.

The identifier and literal grammars of `v1beta0` are restricted to ASCII (see
§3.4, §3.5). Non-ASCII Unicode characters outside string literals and
comments are syntax errors. Within string literals and comments, non-ASCII
bytes are permitted but uninterpreted (see §3.5.2).

A *line terminator* is either the single character U+000A (LF) or the
character pair U+000D U+000A (CRLF). A lone U+000D (CR) without a following
U+000A is also accepted and treated as a line terminator. Line terminators
end line comments and otherwise behave as whitespace.

## 3.2 Whitespace

The following characters are *whitespace*:

- U+0020 (SPACE)
- U+0009 (HORIZONTAL TAB)
- U+000A (LINE FEED)
- U+000D (CARRIAGE RETURN)

Whitespace separates tokens and is otherwise insignificant. It MUST appear
between adjacent tokens that would otherwise be lexically merged (e.g.
between two identifiers, or between a keyword and an identifier).

## 3.3 Comments

Tx3 has two kinds of comments. Both are equivalent to whitespace for the
purposes of tokenisation.

- **Line comments** begin with `//` and extend to the next line terminator
  (exclusive).
- **Block comments** begin with `/*` and extend to the next `*/`. Block
  comments **do not nest**: the first `*/` after the opening `/*` ends the
  comment.

A `/*` that is never closed is a syntax error.

## 3.4 Identifiers

An *identifier* matches the regular expression:

```
[A-Za-z] [A-Za-z0-9_]*
```

That is: identifiers start with an ASCII letter and continue with any number
of ASCII letters, ASCII digits, or underscores. Identifiers MUST NOT begin
with an underscore or a digit.

Identifiers are **case-sensitive**: `foo`, `Foo`, and `FOO` are distinct.

Identifiers that match a keyword (§3.6) are not identifiers; they are
keywords. There is no escaping mechanism to use a keyword as an identifier.

By convention (not enforced by this specification):

- Type names and constructor names use `UpperCamelCase`.
- Value names, parameter names, and block-local names use `snake_case`.

## 3.5 Literals

### 3.5.1 Integer literals

An *integer literal* matches:

```
"-"? [0-9]+
```

An optional ASCII minus sign followed by one or more ASCII decimal digits.

Integer literals are interpreted as values of type `Int` (§5.1). A conforming
compiler MUST support, at minimum, the signed 64-bit two's-complement range
(`-2^63` through `2^63 - 1`). Behaviour for literals outside this range is
implementation-defined; conforming compilers SHOULD either reject the literal
or extend the supported range.

### 3.5.2 String literals

A *string literal* is a sequence of bytes enclosed in double quotes:

```
"\"" (any byte except "\"")* "\""
```

There is **no escape syntax** in `v1beta0`. A string literal cannot contain
the double-quote character. The literal denotes the UTF-8 bytes between the
quotes; a string literal has type `Bytes` (§5.1).

Implementations SHOULD warn on string literals that contain bytes outside the
printable ASCII range, but MUST NOT reject them.

### 3.5.3 Hex-string literals

A *hex-string literal* matches:

```
"0x" [0-9a-fA-F]+
```

A hex-string literal denotes a sequence of bytes obtained by interpreting
each pair of hexadecimal digits as one byte. The number of hexadecimal digits
SHOULD be even; behaviour for an odd-length hex-string literal is
implementation-defined.

A hex-string literal has type `Bytes` (§5.1).

### 3.5.4 Boolean literals

A *boolean literal* is one of the keywords `true` or `false`, denoting the
two values of type `Bool` (§5.1).

### 3.5.5 The unit literal

The literal `()` denotes the single value of the `Unit` type (§5.1). A unit
literal is most commonly used as a placeholder for redeemers and other slots
where no payload is required.

### 3.5.6 UTxO-reference literals

A *UTxO-reference literal* matches:

```
"0x" [0-9a-fA-F]+ "#" [0-9]+
```

That is: a hex-string syntactically identical to a hex-string literal,
immediately followed by `#` and a decimal integer. The hex part denotes a
transaction hash; the decimal part denotes a non-negative output index.

A UTxO-reference literal has type `UtxoRef` (§5.1). Implementations SHOULD
warn if the hex part does not contain an even number of digits, or if its
byte length is not 32 (the typical size of a transaction hash on UTxO
chains).

The UTxO-reference syntax takes precedence over the hex-string syntax when
both could match: `0xabcd#0` is a UTxO reference, not a hex string followed
by other tokens.

## 3.6 Keywords

The following identifiers are *reserved keywords* and MUST NOT be used as
user-defined identifiers:

```
env       asset     party     policy    type      tx
input     output    mint      burn      reference collateral
validity  signers   metadata  locals    cardano   bitcoin
true      false
Int       Bool      Bytes     AnyAsset  Address   UtxoRef
List      Map
```

Within blocks, the following *contextual field names* are syntactically
distinguished and act as keywords inside their respective blocks:

```
from        datum_is    min_amount  redeemer    ref
to          amount      datum
until_slot  since_slot
hash        script
drep        stake       pool
version     coin
```

Contextual field names are not reserved at the program level; a top-level
identifier may share the name. Inside a block where the name has a
designated role, the name is consumed as that role.

## 3.7 Operators and punctuation

The following tokens are operators or punctuation:

```
+   -   !
.   [   ]
(   )   {   }
,   ;   :   =
<   >
::   ...   #
?   *
```

Operator precedence and associativity are given in §4.5.

The token `*` is used both as a flag on `input` blocks (`input*`) and is
otherwise reserved; it is **not** a multiplication operator.

The token `?` is used as a flag on `output` blocks (`output?`); it is
otherwise reserved.

The token `#` is used only inside UTxO-reference literals (§3.5.6).

The token `...` is used only inside variant-case constructors as a spread
operator (§4.4).

The token `::` is used both inside chain-specific block prefixes
(`cardano::…`) and inside explicit variant-case constructors (§4.4).

## 3.8 Token disambiguation

Lexing is greedy: at each position, the longest token that matches the input
is consumed. As a special case, a hex-string literal that is immediately
followed by `#` and decimal digits is consumed as a single UTxO-reference
literal (§3.5.6).
