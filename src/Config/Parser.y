{
{-# LANGUAGE Trustworthy #-}

module Config.Parser (parseValue) where

import Config.Value   (Section(..), Value(..), Atom(..))
import Config.Tokens  (Located(..), Token)
import qualified Config.Tokens as T

}

%tokentype                      { Located Token                 }
%token
SECTION                         { Located _ (T.Section $$)      }
STRING                          { Located _ (T.String $$)       }
ATOM                            { Located _ (T.Atom $$)         }
NUMBER                          { Located _ $$@T.Number{}       }
FLOATING                        { Located _ $$@T.Floating{}     }
'*'                             { Located _ T.Bullet            }
'['                             { Located _ T.OpenList          }
','                             { Located _ T.Comma             }
']'                             { Located _ T.CloseList         }
'{'                             { Located _ T.OpenMap           }
'}'                             { Located _ T.CloseMap          }
SEP                             { Located _ T.LayoutSep         }
END                             { Located _ T.LayoutEnd         }
EOF                             { Located _ T.EOF               }

%monad { Either (Located Token) }
%error { errorP }

%name config

%%

config ::                       { Value                         }
  : value EOF                   { $1                            }

value ::                        { Value                         }
  : sections END                { Sections (reverse $1)         }
  | list     END                { List     (reverse $1)         }
  | simple                      { $1                            }

simple ::                       { Value                         }
  : NUMBER                      { number $1                     }
  | FLOATING                    { floating $1                   }
  | STRING                      { Text   $1                     }
  | ATOM                        { Atom (MkAtom $1)              }
  | '{' inlinesections '}'      { Sections (reverse $2)         }
  | '[' inlinelist ']'          { List (reverse $2)             }

sections ::                     { [Section]                     }
  :              section        { [$1]                          }
  | sections SEP section        { $3 : $1                       }

inlinesections ::               { [Section]                     }
  :                             { []                            }
  | inlinesections1             { $1                            }
  | inlinesections1 ','         { $1                            }

inlinesections1 ::              { [Section]                     }
  :                     section { [$1]                          }
  | inlinesections1 ',' section { $3 : $1                       }

section ::                      { Section                       }
  : SECTION value               { Section $1 $2                 }

list ::                         { [Value]                       }
  :          '*' value          { [$2]                          }
  | list SEP '*' value          { $4 : $1                       }

inlinelist ::                   { [Value]                       }
  :                             { []                            }
  | inlinelist1                 { $1                            }
  | inlinelist1 ','             { $1                            }

inlinelist1 ::                  { [Value]                       }
  :                 simple      { [$1]                          }
  | inlinelist1 ',' simple      { $3 : $1                       }

{

-- | Convert number token to number value. This needs a custom
-- function like this because there are two value matched from
-- the constructor.
number :: Token -> Value
number = \(T.Number base val) -> Number base val

-- | Convert floating token to floating value. This needs a custom
-- function like this because there are two value matched from
-- the constructor.
floating :: Token -> Value
floating = \(T.Floating coef expo) -> Floating coef expo

errorP :: [Located Token] -> Either (Located Token) a
errorP xs = Left (head xs)

-- | Attempt to parse a layout annotated token stream or
-- the token that caused the parse to fail.
parseValue ::
  [Located Token]              {- ^ layout annotated token stream -} ->
  Either (Located Token) Value {- ^ token at failure or result -}
parseValue = config

}
