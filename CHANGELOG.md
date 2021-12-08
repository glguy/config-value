0.8.2
---
* Add `+` and `-` to the set of layout-based list syntax bullets.
  All elements of the list are checked to see that a consistent
  bullet is used. Different bullets might be used to help make
  nested lists more understandable. `-` might be used to make things
  look more like YAML

0.8.1
---
* Allow underscores in number literals
  Copied from <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0076-numeric-underscores.rst#new-syntax-this-proposal>

  Underscores are allowed and ignored
  - in the *middle* of integer-parts of the literal syntax
  - between base-markers (0x, 0o, 0b) and number part
  - before the `eEpP` part of an exponent

0.8
---
* Allow atoms and section names to start with `@` or `$`
* Add `Config.Macro` module

0.7.0.1
---

* Fix pretty-printing of fractional, hexadecimal numbers

0.7.0.0
---
* Updated number representation to preserve fractional part
  and added new `Config.Number` module with operations on
  this new type.

0.6.3.1
---
* Build on GHC 8.4.1

0.6.3
---
* Add `valuePlate`

0.6.2.1
---
* Fixed error output for unexpected floating point literal

0.6.2
---
* Nicer errors on unterminated inline lists and sections.
* Stop enforcing well-formed text files

0.6.1
---
* Add vim syntax highlighting file
* Fix string gaps, they shouldn't require a newline

0.6
---
* Annotate `Value` with file positions
* Derive `Generic1` instances for `Value`

0.5.1
---
* Allow trailing commas in lists and section lists
* Support inline section lists using `{}`
* Add more documentation

0.5
----
* Add support for floating-point numbers

0.4.0.2
----
* Internal lexer and parser improvements
* Added support for `\&` escape sequence

0.4.0.1
----
* Loosen version constraints to build back to GHC 7.4.2
* Remove unused bytestring dependency

0.4
----
* Make `Atom` a newtype to help distinguish it from `Text`
* Add `values` traversal for traversing individual elements of a list

0.3
-----
* Replace `yes` and `no` with generalized atoms
* Add character index to error position
* Add human readable error messages

0.2
-----
* Take `Text` as the input to `parse`

0.1.1
-----
* Added `Config.Lens` module
* Added aligned fields to pretty printer

0.1
-----
* Initial release
