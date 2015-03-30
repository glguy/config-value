{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Config.Parser (parse) where

import Control.Applicative
import Control.Monad
import Data.Text (Text)

import Config.Value   (Section(..), Value(..))
import Config.Lexer   (scanTokens)
import Config.Tokens  (Located(..), Position(..), Token, layoutPass)
import qualified Config.Tokens as T

}

%token

SECTION                         { Located _ (T.Section $$)      }
STRING                          { Located _ (T.String $$)       }
ATOM                            { Located _ (T.Atom $$)         }
NUMBER                          { Located _ $$@T.Number{}       }
'*'                             { Located _ T.Bullet            }
'['                             { Located _ T.OpenList          }
','                             { Located _ T.Comma             }
']'                             { Located _ T.CloseList         }
'{'                             { Located _ T.OpenMap           }
'}'                             { Located _ T.CloseMap          }
SEP                             { Located _ T.LayoutSep         }
END                             { Located _ T.LayoutEnd         }

%tokentype                      { Located Token                 }
%lexer { lexerP }               { Located _ T.EOF               }
%monad { ParseM }
%name value value

%%

value ::                        { Value                         }
  : sections END                { Sections (reverse $1)         }
  | list     END                { List     (reverse $1)         }
  | simple                      { $1                            }

simple ::                       { Value                         }
  : NUMBER                      { number $1                     }
  | STRING                      { Text   $1                     }
  | ATOM                        { Atom   $1                     }
  | '{' '}'                     { Sections []                   }
  | '[' inlinelist ']'          { List     $2                   }

sections ::                     { [Section]                     }
  :              section        { [$1]                          }
  | sections SEP section        { $3 : $1                       }

section ::                      { Section                       }
  : SECTION value               { Section $1 $2                 }

list ::                         { [Value]                       }
  :          '*' value          { [$2]                          }
  | list SEP '*' value          { $4 : $1                       }

inlinelist ::                   { [Value]                       }
  :                             { []                            }
  | inlinelist1                 { reverse $1                    }

inlinelist1 ::                  { [Value]                       }
  :                 simple      { [$1]                          }
  | inlinelist1 ',' simple      { $3 : $1                       }



{

number :: Token -> Value
number (T.Number base val) = Number base val
number _                   = error "Config.Parser.number: fatal error"

newtype ParseM a = ParseM
  { runParseM :: Position -> [Located Token] -> Either (Int,Int,Int) (Position,[Located Token], a) }

-- | Parse a configuration file and return the result on the
-- right, or the position of an error on the left.
-- Note: Text file lines are terminated by new-lines.
parse ::
  Text                   {- ^ Source                      -} ->
  Either (Int,Int,Int) Value {- ^ Either (Position,Line,Column) Result -}
parse txt =
  do (_,_,x) <- runParseM value (error "previous token")
              $ layoutPass
              $ scanTokens txt
     return x

instance Functor ParseM where
  fmap          = liftM

instance Applicative ParseM where
  (<*>)         = ap
  pure          = return

instance Monad ParseM where
  return x      = ParseM $ \t ts ->
                     do return (t,ts,x)
  m >>= f       = ParseM $ \t ts ->
                     do (t',ts',x) <- runParseM m t ts
                        runParseM (f x) t' ts'

lexerP :: (Located Token -> ParseM a) -> ParseM a
lexerP k = ParseM $ \_ toks ->
  case toks of
    []      -> error "Unexpected end of token stream"
    t:toks' -> runParseM (k t) (locPosition t) toks'

-- required by 'happy'
happyError :: ParseM a
happyError = ParseM $ \posn _ -> Left (posIndex posn, posLine posn, posColumn posn)

}
