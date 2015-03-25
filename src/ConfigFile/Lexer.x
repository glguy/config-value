{
module ConfigFile.Lexer
  ( scanTokens
  ) where

import Debug.Trace
import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import Data.Char            (isDigit, digitToInt)
import Data.Int             (Int64)
import Data.Text            (Text)
import Numeric              (readInt)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Encoding.Error   as Text

import ConfigFile.Tokens

}

%wrapper "posn-bytestring"

$alpha = [A-Za-z]
$digit = [0-9]
$white_no_nl = $white # \n

-- Copied from Haskell 2010
@charesc = [abfnrtv\\\"'&]
@cntrl   = [A-Z@\[\\\]\^_]
@ascii   = \^ @cntrl
         | NUL | SOH | STX | ETX | EOT | ENQ | ACK
         | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
         | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN
         | EM | SUB | ESC | FS | GS | RS | US | SP | DEL
@decimal = [0-9]+
@octal   = [0-7]+
@hexadecimal = [0-9a-fA-F]+
@escape = \\ (@charesc | @ascii | @decimal | 'o' @octal | 'x' @hexadecimal)


config :-

$white+;
\-\- [^\n]* ;

\{\}                       { tok (const EmptyMap) }
\[\]                       { tok (const EmptyList) }
\:                         { tok (const Colon) }
\*                         { tok (const Bullet) }
$alpha [$alpha $digit \-]* { tok atom }
\-?         @decimal       { tok (number 10)}
\-? '0' 'x' @hexadecimal   { tok (number 16)}
\-? '0' 'o' @octal         { tok (number  8)}

\" ($printable # [\"\\] | $white_no_nl | @escape)* \" { tok string }

{

tok f (AlexPn _ line column) str = PosToken line column (f str)

-- | Construct a 'Number' token from a token using a
-- given base. This function expect the token to be
-- legal for the given base. This is checked by Alex.
number ::
  Integer    {- ^ base  -} ->
  ByteString {- ^ token -} ->
  Token
number base str = Number $
  case readInt base isDigit digitToInt (L8.unpack str) of
    [(n,"")] -> n
    _        -> error "Lexer.number: implementation failure"

atom ::
  ByteString {- ^ UTF-8 encoded text -} ->
  Token
atom = Atom . Text.decodeUtf8With Text.lenientDecode . L8.toStrict

string ::
  ByteString {- ^ UTF-8 encoded text -} ->
  Token
string = String
       . read
       . Text.unpack
       . Text.decodeUtf8With Text.lenientDecode
       . L8.toStrict

scanTokens :: ByteString -> Either (Int,Int) [PosToken]
scanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return [PosToken (alexLine pos) 0 EOF]
                AlexError ((AlexPn _ line column),_,_) -> Left (line,column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap (act pos (L8.take (fromIntegral len) str) :) (go inp')

alexLine :: AlexPosn -> Int
alexLine (AlexPn _ line _) = line
}
