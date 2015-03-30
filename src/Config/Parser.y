{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Config.Parser (parseValue) where

import Control.Applicative
import Control.Monad

import Config.Value   (Section(..), Value(..))
import Config.Tokens  (Located(..), Token)
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
  { runParseM :: Located Token -> [Located Token] -> Either (Located Token) (Located Token,[Located Token], a) }

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
    t:toks' -> runParseM (k t) t toks'

-- required by 'happy'
happyError :: ParseM a
happyError = ParseM $ \prev _ -> Left prev

-- | Attempt to parse a layout annotated token stream or
-- the token that caused the parse to fail.
parseValue ::
  [Located Token]              {- ^ layout annotated token stream -} ->
  Either (Located Token) Value {- ^ token at failure or result -}
parseValue tokens =
  do (_,_,x) <- runParseM value (error "previous token") tokens
     return x

}
