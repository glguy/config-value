{
{-# LANGUAGE Trustworthy #-}

module Config.Parser (parseValue) where

import Config.Value   (Section(..), Value(..))
import Config.ParserUtils (Parser, runParser, lexerP, errorP)
import Config.Tokens  (Located(..), Token)
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

%monad { Parser (Located Token) }
%lexer { (>>=) lexerP }         { Located _ T.EOF               }
%error { errorP       }

%name value

%%

value ::                        { Value                         }
  : sections                    { Sections $1                   }
  | list                        { List     $1                   }
  | simple                      { $1                            }

simple ::                       { Value                         }
  : NUMBER                      { number $1                     }
  | STRING                      { Text   $1                     }
  | ATOM                        { Atom   $1                     }
  | '{' '}'                     { Sections []                   }
  | '[' inlinelist ']'          { List     $2                   }

sections ::                     { [Section]                     }
  : section END                 { [$1]                          }
  | section SEP sections        { $1 : $3                       }

section ::                      { Section                       }
  : SECTION value               { Section $1 $2                 }

list ::                         { [Value]                       }
  : '*' value END               { [$2]                          }
  | '*' value SEP list          { $2 : $4                       }

inlinelist ::                   { [Value]                       }
  :                             { []                            }
  | inlinelist1                 { $1                            }

inlinelist1 ::                  { [Value]                       }
  : simple                      { [$1]                          }
  | simple ',' inlinelist1      { $1 : $3                       }

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

}
