{-# LANGUAGE Safe #-}
-- | This module parses files using the syntax demonstrated below.
-- The full grammar is available in the Happy source file.
--
-- @
-- -- Line comments until newline
-- layout:
--   based:
--     configuration:
--       {} -- empty section
--
--     sections:
--      "glguy"
--
--     booleans   : yes
--     complicated: no
--
--     decimal    : -1234
--     hexadecimal: 0x1234
--     octal      : 0o1234
--     binary     : 0b1010
--
-- lists:
--    * 1
--    * [ "inline", "lists" ]
--    * * "nestable"
--      * "layout"
--      * "lists"
--    * 3
--
-- unicode : "standard Haskell format strings (1 ≤ 2)\\x2228(2 ≤ 3)"
-- @
module Config
  ( Section(..)
  , Value(..)
  , parse
  , pretty
  ) where

import Config.Value  (Value(..), Section(..))
import Config.Parser (parseValue)
import Config.Pretty (pretty)
import Config.Lexer  (scanTokens)
import Config.Tokens (Position(..), Located(..), layoutPass, Token)
import qualified Config.Tokens as T

import Numeric (showIntAtBase)
import Data.Char (showLitChar, intToDigit)
import Data.Text (Text)
import qualified Data.Text as Text

-- | Parse a configuration file and return the result on the
-- right, or the position of an error on the left.
-- Note: Text file lines are terminated by new-lines.
parse ::
  Text                {- ^ Source                      -} ->
  Either String Value {- ^ Either ErrorMessage Result -}
parse txt =
  case parseValue (layoutPass (scanTokens txt)) of
    Right x -> Right x
    Left (Located posn token) -> Left (errorMessage txt posn token)

errorMessage :: Text -> Position -> Token -> String
errorMessage txt posn token
   = show (posLine   posn) ++ ":"
  ++ show (posColumn posn) ++ ": "
  ++ case token of
       T.ErrorUntermComment -> "lexical error: unterminated comment"
       T.ErrorUntermCommentString -> "lexical error: unterminated string in comment"
       T.ErrorUntermString str -> "lexical error: unterminated string: " ++ show str
       T.ErrorUntermFile -> "lexical error: unterminated line"
       T.ErrorEscape c -> "lexical error in string at: " ++ Text.unpack c
       T.ErrorChar c -> "lexical error at character " ++ show c
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
