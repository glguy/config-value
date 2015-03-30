{
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.Parser (parseValue) where

import Control.Applicative
import Control.Monad

import Config.Value   (Section(..), Value(..))
import Config.Tokens  (Located(..), Token)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)
import qualified Config.Tokens as T

}

%tokentype                      { Located Token                 }
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

%monad { Parser       }
%lexer { (>>=) lexerP }         { Located _ T.EOF               }
%error { errorP       }

%name value

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

-- | Convert number token to number value. This needs a custom
-- function like this because there are two value matched from
-- the constructor.
number :: Token -> Value
number = \(T.Number base val) -> Number base val

-- | Attempt to parse a layout annotated token stream or
-- the token that caused the parse to fail.
parseValue ::
  [Located Token]              {- ^ layout annotated token stream -} ->
  Either (Located Token) Value {- ^ token at failure or result -}
parseValue = runParser value

------------------------------------------------------------------------
-- Parser monad implementation
------------------------------------------------------------------------

newtype Parser a = Parser (StateT [Located Token] (Either (Located Token)) a)
  deriving (Functor, Applicative, Monad)

runParser :: Parser a -> [Located Token] -> Either (Located Token) a
runParser (Parser m) = evalStateT m

lexerP :: Parser (Located Token)
lexerP = Parser $
  do x:xs <- get
     put xs
     return x

errorP :: Located Token -> Parser a
errorP = Parser . lift . Left

}
