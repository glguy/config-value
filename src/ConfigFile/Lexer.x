{
module ConfigFile.Lexer
  ( scanTokens
  ) where

import Data.Bits            ((.&.))
import Data.ByteString.Lazy (ByteString)
import Data.Char            (isDigit, digitToInt, isSpace)
import Data.Word            (Word8)
import Numeric              (readInt)
import Text.Read            (readMaybe)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Encoding.Error   as Text

import ConfigFile.Tokens

}

%wrapper "posn-bytestring"

$alpha          = [A-Za-z]
$digit          = [0-9]
$octdigit       = [0-7]
$hexdigit       = [0-9a-fA-F]
$bindigit       = [0-1]
$white_no_nl    = $white # \n
$charesc        = [abfnrtv\\\"'&]
$cntrl          = [A-Z@\[\\\]\^_]

-- Copied from Haskell 2010
@ascii          = \^ $cntrl
                | NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL
                | BS  | HT  | LF  | VT  | FF  | CR  | SO  | SI
                | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB
                | CAN | EM  | SUB | ESC | FS  | GS  | RS  | US
                | SP  | DEL
@decimal        = $digit+
@octal          = $octdigit+
@binary         = $bindigit+
@hexadecimal    = $hexdigit+
@escape         =   $charesc
                |   @ascii
                |   @decimal
                | o @octal
                | x @hexadecimal

@string         = ([^ \" \\ \n] | \\ @escape)*

config :-

$white+;
"--" [^\n]* ;

[Yy][Ee][Ss]            { tok (const Yes)               }
[Nn][Oo]                { tok (const No)                }
"{"                     { tok (const OpenMap)           }
"}"                     { tok (const CloseMap)          }
"["                     { tok (const OpenList)          }
","                     { tok (const Comma)             }
"]"                     { tok (const CloseList)         }
"*"                     { tok (const Bullet)            }
"-"? "0x" @hexadecimal  { tok (number 2 16)             }
"-"?      @decimal      { tok (number 0 10)             }
"-"? "0o" @octal        { tok (number 2  8)             }
"-"? "0b" @binary       { tok (number 2  2)             }
\" @string* \"          { tok string                    }

$alpha [$alpha $digit \-]* $white_no_nl* \: { tok section }


{

-- Helper function for building a PosToken given a token constructor
-- function, a position, and the matched token.
tok ::
  (ByteString -> Token) {- ^ token constructor       -} ->
  AlexPosn              {- ^ token starting position -} ->
  ByteString            {- ^ token bytes             -} ->
  PosToken
tok f (AlexPn _ line column) str = PosToken line column (f str)

-- | Construct a 'Number' token from a token using a
-- given base. This function expect the token to be
-- legal for the given base. This is checked by Alex.
number ::
  Int        {- ^ prefix length      -} ->
  Integer    {- ^ base               -} ->
  ByteString {- ^ sign-prefix-digits -} ->
  Token
number prefixLen base str =
  case readInt base isDigit digitToInt str2 of
    [(n,"")] -> Number (s*n)
    _        -> Error
  where
  str2     = drop prefixLen str1
  (s,str1) = case L8.unpack str of
               '-':rest -> (-1, rest)
               rest     -> ( 1, rest)

-- | Process a section heading token
section ::
  ByteString {- ^ UTF-8 encoded text -} ->
  Token
section
  = Section
  . Text.dropWhileEnd isSpace
  . Text.init
  . Text.decodeUtf8With Text.lenientDecode
  . L8.toStrict

-- | Parse a string literal processing the escapes.
string ::
  ByteString {- ^ UTF-8 encoded text -} ->
  Token
string = maybe Error String
       . readMaybe
       . Text.unpack
       . Text.decodeUtf8With Text.lenientDecode
       . L.toStrict

-- | Attempt to produce a token stream from an input file.
-- In the case of an error the line and column of the error
-- are returned instead.
scanTokens ::
  ByteString                  {- ^ UTF-8 encoded source        -} ->
  Either (Int,Int) [PosToken] {- ^ Either (Line,Column) Tokens -}
scanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return [PosToken (alexLine pos) 0 EOF]
                AlexError ((AlexPn _ line column),_,_) -> Left (line,column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap (act pos (utf8Take len str) :) (go inp')

alexLine :: AlexPosn -> Int
alexLine (AlexPn _ line _) = line

-- | Take the request number of codepoints from a UTF-8 encoded
-- ByteString. UTF-8 is a variable length encoding, so some
-- codepoints will be longer than others.
utf8Take ::
  Int        {- ^ Desired prefix length -} ->
  ByteString {- ^ UTF-8 encoded text    -} ->
  ByteString {- ^ UTF-8 encoded text    -}
utf8Take len bs =
  case drop len (L.findIndices isStartByte bs) of
    []    -> bs
    pos:_ -> L.take pos bs

-- | Return True when applied to the first byte of the UTF-8
-- encoding of a Unicode codepoint.
isStartByte :: Word8 -> Bool
isStartByte x = x .&. 0xc0 /= 0x80

}
