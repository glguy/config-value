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

@atom           = @alpha (@alpha | [$digit $unidigit \. _ \-])*

config :-

<0> {
$white+                 ;
"--" .*                 ;

"{"                     { token (const OpenMap)         }
"}"                     { token (const CloseMap)        }
"["                     { token (const OpenList)        }
","                     { token (const Comma)           }
"]"                     { token (const CloseList)       }
"*"                     { token (const Bullet)          }
"-"? "0x" @hexadecimal  { token (number 2 16)           }
"-"?      @decimal      { token (number 0 10)           }
"-"? "0o" @octal        { token (number 2  8)           }
"-"? "0b" @binary       { token (number 2  2)           }
@atom                   { token Atom                    }
@atom $white_no_nl* \:  { token section                 }
\"                      { startString                   }
}

<stringlit> {
\"                      { endString                     }
[^ \" \\ ]+             { addString                     }
\\ @escape              { addCharLit                    }
\\ &                    ;
\\ .                    { badEscape                     }
\n                      { untermString                  }
}

<0,comment> {
"{-"                    { startComment                  }
}

<comment> {
"-}"                    { endComment                    }
\"                      { startCommentString            }
.                       ;
\n                      ;
}

<commentstring> {
\"                      { endCommentString              }
\n                      { token (const ErrorUntermCommentString) }
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
          _ | posColumn (locPosition inp) /= 1 -> [Located (locPosition inp) ErrorUntermFile]
          InComment       startPosn _    -> [Located startPosn ErrorUntermComment]
          InCommentString startPosn _    -> [Located startPosn ErrorUntermCommentString]
          InString        startPosn b    -> [Located startPosn (ErrorUntermString (getStringLit b))]
          InNormal                       -> [Located (locPosition inp){posColumn=0} EOF]
      AlexError err -> [fmap (ErrorChar . Text.head) err]
      AlexSkip  inp' len     -> go st inp'
      AlexToken inp' len act ->
        case act (fmap (Text.take len) inp) st of
          (st', Nothing) ->     go st' inp'
          (st', Just x ) -> x : go st' inp'

-- | Compute the Alex state corresponding to a particular 'LexerMode'
stateToInt :: LexerMode -> Int
stateToInt InNormal{}           = 0
stateToInt InComment{}          = comment
stateToInt InCommentString{}    = commentstring
stateToInt InString{}           = stringlit

}
