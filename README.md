# `taiwan-id`

[![Latest Release](
  https://img.shields.io/hackage/v/taiwan-id?label=Latest%20Release&color=227755
)](https://hackage.haskell.org/package/taiwan-id)
[![Development Branch](
  https://img.shields.io/badge/Development%20Branch-API%20Documentation-225577
)](https://jonathanknowles.github.io/taiwan-id/)

This package provides both a **Haskell library** and a **command-line tool**
(CLI) for working with identification numbers issued to residents of Taiwan, as
well as the other territories of the Republic of China (ROC), including Kinmen,
Matsu, and Penghu.

# Contents

1. [Background](#background)
2. [Library](#library)
   1. [Usage](#usage)
      1. [Parsing](#parsing)
         1. [Run-time parsing](#run-time-parsing)
         2. [Compile-time parsing](#compile-time-parsing)
      2. [Inspecting attributes](#inspecting-attributes)
   2. [Design philosophy](#design-philosophy)
3. [Command-line tool](#command-line-tool)
   1. [Installation](#installation)
   2. [Usage](#usage-1)
4. [References](#references)

# Background

The ROC government issues identification numbers to individuals on two types of
identification card:

1. **National Identification Cards** (國民身分證), issued by the Household
   Registration Office (戶政事務所);
2. **Resident Certificates** (居留證), issued by the National Immigration
   Agency (移民署).

Although these two card types are distinct, both share a common identification
number format, with numbers assigned under each system occupying disjoint parts
of the same number space.

Each identification number consists of a single uppercase letter followed by
nine decimal digits:

```
A 1 2 3 4 5 6 7 8 9
│ │ └───────────┘ │
│ │ serial number └── checksum
│ └── gender and issuer
└──── region
```

The leading letter encodes the **region** in which the number was issued. The
second digit jointly encodes the holder's **gender** and the **issuing
authority** (either the Household Registration Office or the National
Immigration Agency). The final digit serves as a **checksum**.

# Library

## Usage

### Parsing

At the heart of the library is the ability to parse and validate identification
numbers from textual input, accepting only those that are well-formed according
to the standard.

The library provides two functions for this purpose: `fromText`, which parses
a `Text` value at run time, and `fromSymbol`, which parses a type-level
`Symbol` at compile time.

#### Run-time parsing

To parse an `ID` from `Text`, use the `fromText` function:

```haskell
>>> ID.fromText "A123456789"
Right (ID.fromSymbol @"A123456789")
```

If the supplied `Text` is not a valid identification number, `fromText` returns
a structured error describing why the number is invalid:

```haskell
>>> ID.fromText "A1234"
Left InvalidLength

>>> ID.fromText "A123456780"
Left InvalidChecksum

>>> ID.fromText "A1_3456789"
Left (InvalidChar (CharIndex 2) (CharRange '0' '9'))
```

#### Compile-time parsing

The `fromSymbol` function constructs an `ID` from a type-level `Symbol`,
validated entirely at **compile time**:

```haskell
>>> ID.fromSymbol @"A123456789"
ID.fromSymbol @"A123456789"
```

Instead of run-time errors, invalid symbols are reported as **type errors**:

```haskell
>>> ID.fromSymbol @"A1234"
error:
    An ID must have exactly 10 characters.

>>> ID.fromSymbol @"A123456780"
error:
    ID has invalid checksum.

>>> ID.fromSymbol @"A_23456789"
error:
    "A_23456789"
      ^
    Character at this position must be a digit from the set {1, 2, 8, 9}.
```

### Inspecting attributes

Once you have a valid `ID`, you can inspect its attributes:

```haskell
>>> let i = ID.fromSymbol @"A123456789"

>>> ID.getIssuer i
HouseholdRegistrationOffice

>>> ID.getGender i
Male

>>> ID.Region.toText English (ID.getRegion i)
"Taipei City"
```

## Design philosophy

### Correct-by-construction types

The library is built around the principle that **invalid states should be
unrepresentable**. This applies not just to the top-level `ID` type, but to
every component type used to construct it.

An `ID` value is stored as a record of smaller types, each of which admits
only the values that are structurally valid at its position:

- `c0 :: Letter` — one of the 26 uppercase letters
- `c1 :: Digit1289` — one of the digits `{1, 2, 8, 9}`
- `c2 .. c8 :: Digit` — digits in the range `[0 .. 9]`

The checksum digit is **not stored** in an `ID` value at all. Instead, it is
computed on demand from the other fields, which means a stored `ID` value is
always internally consistent — there is no way to construct one with a
mismatched checksum.

This means that the type of `ID` itself acts as a proof of validity. If you
hold a value of type `ID`, you know — without any further checking — that it
represents a well-formed identification number.

### Lawful `Show` and `Read` instances

The existence of `fromSymbol` makes it possible to offer `Show` and `Read`
instances that are genuinely lawful in a way that naive implementations often
are not.

The `Show` instance produces a valid Haskell expression:

```haskell
>>> show (ID.fromSymbol @"A123456789")
"ID.fromSymbol @\"A123456789\""
```

This output is not merely human-readable — it is a well-typed Haskell
expression that, when evaluated, produces the original value. As a result,
`read . show` roundtrips faithfully:

```haskell
>>> read (show i) == i
True
```

# Command-line tool

## Installation

First, install the Haskell toolchain. One popular way to do this is via
[`ghcup`](https://www.haskell.org/ghcup/).

Then run:

```
cabal install taiwan-id
```

## Usage

The `taiwan-id` command-line tool provides three subcommands.

### `validate`

Checks whether an identification number is valid. Exits with code `0` on
success, code `1` on failure (along with an error message on `stderr`).

```
$ taiwan-id validate P833485645

$ taiwan-id validate N140792413
Invalid checksum.

$ taiwan-id validate I96342
Invalid length.
An identification number must be exactly 10 characters in length.

$ taiwan-id validate C25171445&
Invalid character:
C25171445&
         ^
Character at this position must be a character in the range [0 .. 9].
```

### `decode`

Decodes the attributes of an identification number. Output is available in
English (the default) or Chinese:

```
$ taiwan-id decode H271789449
Issuer: Household Registration Office
Gender: Female
Region: Taoyuan City

$ taiwan-id decode Y175974499 --language=Chinese
核發機關：戶政事務所
性別　　：男性
地區　　：陽明山
```

### `generate`

Generates one or more random identification numbers:

```
$ taiwan-id generate
E218711091

$ taiwan-id generate --count 4
R886773836
C129535585
E892135379
K207816302
```

For deterministic output, you can specify a seed:
```
$ taiwan-id generate --count 4 --seed 888
X207421526
H891911565
K935490929
O198217491
```

# References

- [National identification card (Taiwan)](https://en.wikipedia.org/wiki/National_identification_card_(Taiwan)) — Wikipedia
- [Resident certificate](https://en.wikipedia.org/wiki/Resident_certificate) — Wikipedia
- [中華民國國民身分證](https://zh.wikipedia.org/wiki/中華民國國民身分證) — 維基百科
- [中華民國居留證](https://zh.wikipedia.org/wiki/中華民國居留證) — 維基百科
