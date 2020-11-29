# config-value

[![Hackage](https://img.shields.io/hackage/v/config-value.svg)](https://hackage.haskell.org/package/config-value) [![Build Status](https://api.travis-ci.com/glguy/config-value.svg)](http://travis-ci.com/glguy/config-value)

This package implements a simple, layout-based value definition language
used for supplying configuration values to various applications.

Before starting to use config-value, you probably want to read the documentation for [config-schema](https://github.com/glguy/config-schema) to see the user-friendly way to wrap this library.

Live Demo
--------

The config-value and config-schema packages are available in a [live demo](https://glguy.net/config-demo/).

Example
-------
```
-- Line comments until newline
layout:
  based:
    configuration:
      {} -- empty section

    inline-maps: {key1: value1, key2: value2}

    sections:
     "glguy"

    {- Block comments
       {- nested comments -}
       "O'caml style {- strings in comments"
       so you can comment out otherwise valid
       portions of your config
    -}
    atoms:       yes

    decimal:     -1234
    hexadecimal: 0x1234
    octal:       0o1234
    binary:      0b1010

    floats:      [1e2, 0x3p-5, 24.48]
    underscores: 1_000_000

lists:
   * sections: in-lists
     next-section: still-in-list
   * [ "inline", "lists" ]
   * * "nestable"
     * "layout"
     * "lists"
   * 3

unicode: "standard Haskell format strings (1 ≤ 2)x2228(2 ≤ 3)"

multiline: "haskell style\
           \string gaps"
```

Format
------

The language supports: Strings, Atoms, Integers, Lists, Nested Sections.

Sections are layout based. The contents of a section must be indented further than the section heading.
The whitespace between a section heading and its colon is not significant. Section names must start with
a letter and may contain letters, numbers, dashes (`-`), underscores (`_`), and periods (`.`).

Lists are either layout based with `*` prefixes or inline surrounded by `[` and `]` delimited by `,`

Strings are surrounded by `"` and use Haskell-style escapes.

Numbers support decimal, hexadecimal (`0x`), octal (`0o`), and binary (`0b`).

Atoms follow the same lexical rule as section heading.
