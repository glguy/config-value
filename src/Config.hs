{-# LANGUAGE Safe #-}
{-|
Module      : Config
Description : Configuration file parser and abstract syntax
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

This module parses files using the syntax demonstrated below.
The full lexical syntax is available in the Alex source file.
The full grammar is available in the Happy source file.

Configuration file schemas can be specified using the
<https://hackage.haskell.org/package/config-schema config-schema>
package. This package helps extract application-specific meaning
from a 'Value', and can also generate documentation for the supported
format.

The @config-value@ format offers a simple, layout-based syntax for
specifying configuration information. In addition configuration
values can be pretty-printed back into valid concrete syntax.

= Example

@
-- Line comments until newline
layout:
  based:
    configuration:
      {} -- empty section

    sections:
     "glguy"

    {&#45; Block comments
       {&#45; nested comments &#45;}
       "O'caml style {&#45; strings in comments"
       so you can comment out otherwise valid
       portions of your config
    &#45;}
    atoms      : yes

    decimal    : -1234
    hexadecimal: 0x1234
    octal      : 0o1234
    binary     : 0b1010

lists:
   * sections: in-lists
     next-section: still-in-list
   * [ "inline", "lists" ]
   * * "nestable"
     * "layout"
     * "lists"
   * 3

unicode : "standard Haskell format strings (1 ≤ 2)\\x2228(2 ≤ 3)"
@

= Syntax

A configuration file should contain a single /value/ at the top-level.
Typically this value will be a list of sections (as seen in the example
above).

Unicode character classes are fully supported. The alpha and digit character
classes use the full Unicode range, rather than merely the ASCII ranges.

There are 5 distinct types of values possible in a configuration file:

    * Sections list (list of key-value pairs)

    * Lists

    * Text

    * Numbers

    * Atoms

== Sections list

@
KEY: VALUE
KEY: VALUE
KEY: VALUE
@

Sections lists are lists of key-value pairs. Each key in the list should
start on the same column in the file. The value of the pair should be
indented to the right of the key.

The lexical syntax for section names is identical to the lexical syntax
of /atoms/. Section names are nonempty sequences starting with an /alpha/
character followed by zero or more /alpha/, /digit/, /period/ (.),
underscore (_), or dash (-).

Section lists can be nested.

Section lists can be used inline, without layout, but surrounding them
with @{@ and @}@ and separating the sections with @,@.  The empty sections
list is specified with @{}@.

Examples:

@
key-1 : -- spaces are allowed between the section name and the colon
  key-1.1: value-1.1
  key-1.2: [ value-1.2 ]
key-2: value-2
key-3: {} -- the value for key-3 is the empty sections list
key-4: { red: 1, blue: 2} -- inline syntax for sublist
@

== List

@
* VALUE
* VALUE
* VALUE
@

Lists can be specified using either layout or inline syntax. There is no distinction
between the two syntaxes in the abstract syntax.

Inline lists are surrounded by @[@ and @]@ with elements separated by @,@. The final
list element may be terminated with a trailing comma.

Example: @[1, 2, 3]@

Layout list entries are started with a leading @*@. Each leading @*@ must occur
in the some column of the file. Lists can be nested by starting the new list
on a column to the right of the current list.

Layout based lists can not occur inside inline list syntax. Layout based section lists
can occur inside layout based lists

Example:

@
-- One list element containing an atom
* item-1

-- One list element containing a two element list
* * item-2.1
  * item-2.2

-- One list element containing two key-value pairs
* key-1: value-1
  key-2: value-2
@

== Text

@
"quoted string literals"
@

Text values are specified using the Haskell string literal syntax.

Text values are distinct from /atoms/ described below. This allows
a configuration file to make a distinction between the atom @default@
and the text value @"default"@, for example.

For a detailed description of Haskell string literal syntax, see
<https://www.haskell.org/onlinereport/haskell2010/haskellch2.html#x7-200002.6 Haskell 2010 2.6 Character and String Literals>

== Number

@
123.456
@

Numbers can be written with integer and floating-point literals.

Prefix numbers with @-@ to construct a negative number.

Integer literals support alternate base described below.

Floating-point literals can specify a power-of-10 exponent.

Bases

   * No prefix for decimal (base 10) integer literals

   * Prefix binary (base 2) integer literals with @0b@ or @0B@

   * Prefix octal (base 8) integer literals with @0o@ or @0O@

   * Prefix hexadecimal (base 16) integer literals with @0x@ or @0X@. Upper
     and lower-cased hex digits are supported.

List of examples:

@
[ 0, 42, -42, 123.45, 6E7, 1e+10, 3.4e-5, 0xfF, 0b101010, -0o77 ]
@

== Atom

@
unquoted-string
@

/Atoms/ are unquoted strings that are distinct from normal /text/ values.
This type is intended to represent enumerations in a configuration file.

Atoms are nonempty sequences starting with an /alpha/ character followed by
zero or more /alpha/, /digit/, /period/ (.), underscore (_), or dash (-).

Lexical syntax: @$alpha [$alpha $digit $unidigit \\. _ \\-]*@

List of examples:

@
[ yes, no, default, MODE-61 ]
@

== Comments

Comments are valid white-space.

An ordinary comment begins with @--@ and extends to the following newline.

@
-- This is a comment
@

Use pairs of @{&#45;@ and @&#45;}@ to create comments that can span multiple
lines. These comments can be nested.

@
{&#45; this {&#45; is &#45;}
       a comment &#45;}
@

-}
module Config
  (
  -- * Parsing
    parse

  -- * Pretty-printing
  , pretty

  -- * Types
  , Section(..)
  , Value(..)
  , Atom(..)
  ) where

import Config.Value  (Atom(..), Value(..), Section(..))
import Config.Parser (parseValue)
import Config.Pretty (pretty)
import Config.Lexer  (scanTokens)
import Config.Tokens (Error(..), Position(..), Located(..), layoutPass, Token)
import qualified Config.Tokens as T

import Numeric (showIntAtBase)
import Data.Char (intToDigit)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Parse a configuration file and return the result on the
-- right, or the position of an error on the left.
-- Note: Text file lines are terminated by new-lines.
parse ::
  Text                {- ^ source text                    -} ->
  Either String Value {- ^ error message or parsed value  -}
parse txt =
  case parseValue (layoutPass (scanTokens txt)) of
    Right x -> Right x
    Left (Located posn token) -> Left (explain posn token)

explain :: Position -> Token -> String
explain posn token
   = show (posLine   posn) ++ ":"
  ++ show (posColumn posn) ++ ": "
  ++ case token of
       T.Error e     -> explainError e
       T.Atom atom   -> "parse error: unexpected atom: " ++ Text.unpack atom
       T.String str  -> "parse error: unexpected string: " ++ show (Text.unpack str)
       T.Bullet      -> "parse error: unexpected bullet '*'"
       T.Comma       -> "parse error: unexpected comma ','"
       T.Section s   -> "parse error: unexpected section: " ++ Text.unpack s
       T.Number 2  n -> "parse error: unexpected number: 0b" ++ showIntAtBase 2  intToDigit n ""
       T.Number 8  n -> "parse error: unexpected number: 0o" ++ showIntAtBase 8  intToDigit n ""
       T.Number 16 n -> "parse error: unexpected number: 0x" ++ showIntAtBase 16 intToDigit n ""
       T.Number _  n -> "parse error: unexpected number: "   ++ showIntAtBase 10 intToDigit n ""
       T.OpenList    -> "parse error: unexpected start of list '['"
       T.CloseList   -> "parse error: unexpected end of list ']'"
       T.OpenMap     -> "parse error: unexpected start of section '{'"
       T.CloseMap    -> "parse error: unexpected end of section '}'"
       T.LayoutSep   -> "parse error: unexpected end of block"
       T.LayoutEnd   -> "parse error: unexpected end of block"
       T.EOF         -> "parse error: unexpected end of file"

explainError :: Error -> String
explainError e =
  case e of
    T.UntermComment       -> "lexical error: unterminated comment"
    T.UntermString        -> "lexical error: unterminated string literal"
    T.UntermFile          -> "lexical error: unterminated line"
    T.BadEscape c         -> "lexical error: bad escape sequence: " ++ Text.unpack c
    T.NoMatch c           -> "lexical error at character " ++ show c
