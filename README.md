# config-lang

This package implements a simple, layout-based value definition language which I intend
to be used for supplying configuration values to various applications.

Example
-------
```
-- Line comments until newline
layout:
  based:
    configuration:
      {} -- empty section

    sections:
     "glguy"

    {- Block comments
       {- nested comments -}
       "O'caml style {- strings in comments"
       so you can comment out otherwise valid
       portions of your config
    -}
    booleans   : yes
    complicated: no

    decimal    : -1234
    hexadecimal: 0x1234
    octal      : 0o1234
    binary     : 0b1010

lists:
   * 1
   * [ "inline", "lists" ]
   * * "nestable"
     * "layout"
     * "lists"
   * 3

unicode : "standard Haskell format strings (1 ≤ 2)\x2228(2 ≤ 3)"
```

Format
------

The language supports: Strings, Bools, Integers, Lists, Nested Sections.

Sections are layout based. The contents of a section must be indented further than the section heading.
The whitespace between a section heading and its colon is not significant. Section names must start with
a letter and may contain letters, numbers, and dashes (`-`).

Lists are either layout based with `*` prefixes or inline surrounded by `[` and `]` delimited by `,`

Strings are surrounded by `"` and use Haskell-style escapes.

Numbers support decimal, hexadecimal (`0x`), octal (`0o`), and binary (`0b`).

Booleans are given by `yes` and `no` (and are case-insensitive)
