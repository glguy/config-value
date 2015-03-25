{
module Lexer
  ( PosToken(..)
  , Token(..)
  , scanTokens
  ) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy as L
import Data.Int (Int64)

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
\{\}                       { tok (const EmptyMap) }
\[\]                       { tok (const EmptyList) }
:                          { tok (const Colon) }
\-                         { tok (const Dash ) }
$alpha [$alpha $digit \-]* { tok Atom }
$digit+                    { tok Number }

\" ($printable # [\"\\] | $white_no_nl | @escape)* \" { tok String }

{

tok f (AlexPn _ line column) str = PosToken line column (f str)

data PosToken = PosToken Int Int Token
  deriving (Show)

data Token
  = Atom ByteString
  | String ByteString
  | Colon
  | OpenString
  | Dash
  | Number ByteString
  | EmptyList
  | EmptyMap
  deriving (Show)

scanTokens :: ByteString -> Either (Int,Int) [PosToken]
scanTokens str = go (alexStartPos,'\n',str)
  where go inp@(pos,_,str) =
          case alexScan inp 0 of
                AlexEOF -> return []
                AlexError ((AlexPn _ line column),_,_) -> Left (line,column)
                AlexSkip  inp' len     -> go inp'
                AlexToken inp' len act -> fmap (act pos (L.take (fromIntegral len) str) :) (go inp')
}
