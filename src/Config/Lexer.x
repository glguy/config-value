{
{-# OPTIONS_GHC -Wnot #-}
{-# LANGUAGE Trustworthy #-}
module Config.Lexer
  ( scanTokens
  ) where

import Config.LexerUtils
import Config.Tokens
import Data.Text (Text)
import qualified Data.Text as Text

}

$uniupper       = \x1
$unilower       = \x2
$unidigit       = \x3
$unisymbol      = \x4
$unispace       = \x5
$uniother       = \x6

$asciialpha     = [A-Z a-z]
$digit          = [0-9]
$octdigit       = [0-7]
$hexdigit       = [0-9a-fA-F]
$bindigit       = [0-1]
$white_no_nl    = $white # \n
$charesc        = [abfnrtv\\\"']
$cntrl          = [A-Z@\[\\\]\^_]

@decimal        = $digit+
@octal          = $octdigit+
@binary         = $bindigit+
@hexadecimal    = $hexdigit+

-- Copied from Haskell 2010
@ascii          = \^ $cntrl
                | NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL
                | BS  | HT  | LF  | VT  | FF  | CR  | SO  | SI
                | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB
                | CAN | EM  | SUB | ESC | FS  | GS  | RS  | US
                | SP  | DEL
@escape         =   $charesc
                |   @ascii
                |   @decimal
                | o @octal
                | x @hexadecimal

@alpha          = $unilower | $uniupper | $asciialpha

config :-

<0> {
$white+                 ;
"--" .*                 ;

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
\"                      { startString                   }

@alpha (@alpha | [$digit $unidigit \. _ \-])* $white_no_nl* \:
                        { tok section                   }
}

<stringlit> {
\"                      { endString                     }
[^ \" \\ ]+             { addString                     }
\\ @escape              { addCharLit                    }
\\ &                    ;
}

<0,comment> {
"{-"                    { startComment                  }
}

<comment> {
"-}"                    { endCommentString              }
\"                      { startCommentString            }
[^ \" \- \{]+           ;
[\- \n \{ ]             ;
}

<commentstring> {
[ \" \n ]               { endCommentString              }
\\ \"                   ;
.                       ;
}


{
-- | Attempt to produce a token stream from an input file.
-- In the case of an error the line and column of the error
-- are returned instead.
scanTokens ::
  Text            {- ^ Source text          -} ->
  [Located Token] {- ^ Tokens with position -}
scanTokens str = go InNormal (Located alexStartPos str)
  where
  go st inp =
    case alexScan inp (stateToInt st) of
      AlexEOF ->
        case st of
          InNormal                  -> [fmap (const EOF) inp]
          InComment _ commentPosn _ -> [Located commentPosn Error]
          InString builder          -> [fmap (const Error) builder]
      AlexError err -> [err { locThing = Error}]
      AlexSkip  inp' len     -> go st inp'
      AlexToken inp' len act ->
        case act (fmap (Text.take len) inp) st of
          (st', Nothing) ->     go st' inp'
          (st', Just x ) -> x : go st' inp'

-- | Compute the Alex state corresponding to a particular 'LexerMode'
stateToInt :: LexerMode -> Int
stateToInt InNormal{}                   = 0
stateToInt (InComment CommentState _ _) = comment
stateToInt (InComment StringState  _ _) = commentstring
stateToInt InString{}                   = stringlit

}
