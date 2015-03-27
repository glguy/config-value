{
{-# OPTIONS_GHC -Wnot #-}
{-# LANGUAGE Trustworthy #-}
module Config.Lexer
  ( scanTokens
  ) where

import Data.Bits            ((.&.))
import Data.ByteString.Lazy (ByteString)
import Data.Char            (digitToInt, isSpace)
import Data.Word            (Word8)
import Numeric              (readInt)
import Text.Read            (readMaybe)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Encoding.Error   as Text

import Config.Tokens

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

<0> {
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
}

<0,comment> "{-"        { startComment }

<comment> {
"-}"                    { endCommentString }
\"                      { startCommentString }
[^\" \- \{]+            ;
\-                      ;
\n                      ;
\{                      ;
}

<commentstring> {
\"                      { endCommentString   }
\n                      { endCommentString   }
\\ \"                   ;
.                       ;
}


{

data LexS
  = InNormal
  | InComment CommentType AlexPosn [AlexPosn]

data CommentType = CommentState | StringState

stateToInt :: LexS -> Int
stateToInt InNormal                     = 0
stateToInt (InComment CommentState _ _) = comment
stateToInt (InComment StringState  _ _) = commentstring

--  token starting position -> token bytes -> lexer state -> (new state, token)
type Action = AlexPosn -> ByteString -> LexS -> (LexS, Maybe PosToken)

-- Helper function for building a PosToken given a token constructor
-- function, a position, and the matched token.
tok ::
  (ByteString -> Token) {- ^ token constructor       -} ->
  Action
tok f (AlexPn _ line column) str st = (st, Just (PosToken line column (f str)))

simpleAction :: (AlexPosn -> ByteString -> LexS -> LexS) -> Action
simpleAction f pos bs st = (f pos bs st, Nothing)

startComment :: Action
startComment = simpleAction $ \posn _ st ->
  case st of
    InNormal    -> InComment CommentState posn []
    InComment _ posn1 posns -> InComment CommentState posn (posn1:posns)

startCommentString :: Action
startCommentString = simpleAction $ \posn _ st ->
  case st of
    InNormal    -> error "startCommentString: Lexer failure"
    InComment _ posn1 posns -> InComment StringState posn (posn1:posns)

endCommentString :: Action
endCommentString = simpleAction $ \_ _ st ->
  case st of
    InNormal                   -> error "endComment: Lexer failure"
    InComment _ _ (posn:posns) -> InComment CommentState posn posns
    InComment _ _ []           -> InNormal

-- | Construct a 'Number' token from a token using a
-- given base. This function expect the token to be
-- legal for the given base. This is checked by Alex.
number ::
  Int        {- ^ prefix length      -} ->
  Int        {- ^ base               -} ->
  ByteString {- ^ sign-prefix-digits -} ->
  Token
number prefixLen base str =
  case readInt (fromIntegral base) (const True) digitToInt str2 of
    [(n,"")] -> Number base (s*n)
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
  ByteString {- ^ UTF-8 encoded source -} ->
  [PosToken] {- ^ Tokens               -}
scanTokens str = go InNormal (alexStartPos,'\n',str)
  where go st inp@(pos,_,str) =
          case alexScan inp (stateToInt st) of
                AlexEOF ->
                  case st of
                    InNormal -> [PosToken (alexLine pos) 0 EOF]
                    InComment _ (AlexPn _ line column) _ -> [PosToken line column Error]
                AlexError (AlexPn _ line column,_,_) -> [PosToken line column Error]
                AlexSkip  inp' len     -> go st inp'
                AlexToken inp' len act ->
                  case act pos (utf8Take len str) st of
                    (st', Nothing) ->     go st' inp'
                    (st', Just x ) -> x : go st' inp'

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
