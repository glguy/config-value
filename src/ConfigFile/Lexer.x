{
module ConfigFile.Lexer
  ( scanTokens
  ) where

import Control.Applicative
import Control.Monad
import Data.Bits            ((.&.))
import Data.ByteString.Lazy (ByteString)
import Data.Char            (isDigit, digitToInt, isSpace)
import Data.Int             (Int64)
import Data.Text            (Text)
import Data.Word            (Word8)
import Numeric              (readInt)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Encoding.Error   as Text

import ConfigFile.Tokens

}

%wrapper "posn-bytestring"

$alpha = [A-Za-z]
$digit = [0-9]
$octdigit = [0-7]
$hexdigit = [0-9a-fA-F]
$bindigit = [0-1]
$white_no_nl = $white # \n
$charesc = [abfnrtv\\\"'&]
$cntrl   = [A-Z@\[\\\]\^_]

-- Copied from Haskell 2010
@ascii   = \^ $cntrl
         | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
         | EM | SUB | ESC | FS | GS | RS | US | SP | DEL
@decimal = $digit+
@octal   = $octdigit+
@binary  = $bindigit+
@hexadecimal = $hexdigit+
@escape = \\ ($charesc | @ascii | @decimal | 'o' @octal | 'x' @hexadecimal)


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

$alpha [$alpha $digit \-]* $white_no_nl* \: { tok section }

\" ([^\"\\\n] | @escape)* \" { tok string }

{

tok f (AlexPn _ line column) str = PosToken line column (f str)

-- | Construct a 'Number' token from a token using a
-- given base. This function expect the token to be
-- legal for the given base. This is checked by Alex.
number ::
  Int        {- ^ prefix length -} ->
  Integer    {- ^ base          -} ->
  ByteString {- ^ token         -} ->
  Token
number prefixLen base str = Number $
  case readInt base isDigit digitToInt str2 of
    [(n,"")] -> s*n
    _        -> error "Lexer.number: implementation failure"
  where
  str2     = drop prefixLen str1
  (s,str1) = case L8.unpack str of
               '-':rest -> (-1, rest)
               rest     -> ( 1, rest)


section ::
  ByteString {- ^ UTF-8 encoded text -} ->
  Token
section
  = Section
  . Text.dropWhileEnd isSpace
  . Text.init
  . Text.decodeUtf8With Text.lenientDecode
  . L8.toStrict

string ::
  ByteString {- ^ UTF-8 encoded text -} ->
  Token
string = String
       . read
       . Text.unpack
       . Text.decodeUtf8With Text.lenientDecode
       . L.toStrict

scanTokens :: ByteString -> Either (Int,Int) [PosToken]
scanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return [PosToken (alexLine pos) 0 EOF]
                AlexError ((AlexPn _ line column),_,_) -> Left (line,column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap (act pos (utf8Take len str) :) (go inp')

alexLine :: AlexPosn -> Int
alexLine (AlexPn _ line _) = line

utf8Take :: Int -> ByteString -> ByteString
utf8Take len bs =
  case drop len (L.findIndices isStartByte bs) of
    []    -> bs
    pos:_ -> L.take pos bs

isStartByte :: Word8 -> Bool
isStartByte x = x .&. 0xc0 /= 0x80

}
