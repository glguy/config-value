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
$octit          = [0-7]
$hexit          = [0-9a-fA-F]
$binit          = [0-1]
$white_no_nl    = $white # \n
$charesc        = [abfnrtv\\\"'&]
$cntrl          = [A-Z@\[\\\]\^_]
$alpha          = [$unilower $uniupper $asciialpha]

@spacer         = _*

@decimal        = $digit (@spacer $digit)*
@octal          = $octit (@spacer $octit)*
@binary         = $binit (@spacer $binit)*
@hexadecimal    = $hexit (@spacer $hexit)*

-- Copied from Haskell 2010
@ascii          = \^ $cntrl
                | NUL | SOH | STX | ETX | EOT | ENQ | ACK | BEL
                | BS  | HT  | LF  | VT  | FF  | CR  | SO  | SI
                | DLE | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB
                | CAN | EM  | SUB | ESC | FS  | GS  | RS  | US
                | SP  | DEL
@escape         =   $charesc
                |   @ascii
                |   $digit+
                | o $octit+
                | x $hexit+

@atom           = [$alpha \$ \@] [$alpha $digit $unidigit \. _ \-]*

@exponent       = @spacer [Ee] [\-\+]? @decimal
@hexexponent    = @spacer [Pp] [\-\+]? @decimal

config :-

<0> {
$white+                 ;
"--" .*                 ;

"{"                     { token_ OpenMap                }
"}"                     { token_ CloseMap               }
"["                     { token_ OpenList               }
","                     { token_ Comma                  }
"]"                     { token_ CloseList              }
"*"                     { token_ Bullet                 }

"-"? 0 [Xx] @spacer @hexadecimal ("." @hexadecimal?)? @hexexponent? { token number }
"-"? 0 [Oo] @spacer @octal       ("." @octal      ?)?               { token number }
"-"? 0 [Bb] @spacer @binary      ("." @binary     ?)?               { token number }
"-"?                @decimal     ("." @decimal    ?)? @exponent?    { token number }
@atom                   { token Atom                    }
@atom $white_no_nl* :   { token section                 }
\"                      { startString                   }
}

<stringlit> {
\"                      { endMode                       }
"\" @escape             ;
"\" $white+ "\"         ;
"\" .                   { token (Error . BadEscape)     }
.                       ;
\n                      { untermString                  }
}

<0,comment> "{-"        { nestMode InComment            }

<comment> {
"-}"                    { endMode                       }
\"                      { nestMode InCommentString      }
.                       ;
\n                      ;
}

<commentstring> {
\"                      { endMode                       }
\n                      { endMode                       }
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
scanTokens str = go (Located startPos str) InNormal
  where
  go inp st =
    case alexScan inp (stateToInt st) of
      AlexEOF                -> eofAction (locPosition inp) st
      AlexError inp'         -> errorAction inp'
      AlexSkip  inp' _       -> go inp' st
      AlexToken inp' len act -> case act len inp st of
                                  (st', xs) -> xs ++ go inp' st'

-- | Compute the Alex state corresponding to a particular 'LexerMode'
stateToInt :: LexerMode -> Int
stateToInt InNormal{}           = 0
stateToInt InComment{}          = comment
stateToInt InCommentString{}    = commentstring
stateToInt InString{}           = stringlit

}
