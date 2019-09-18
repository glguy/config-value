{
{-# LANGUAGE Trustworthy #-}

module Config.Parser (parseValue) where

import Config.Value   (Section(..), Value(..), Atom(..))
import Config.Tokens  (Located(..), Token, Position)
import qualified Config.Tokens as T

}

%tokentype                      { Located Token                 }
%token
SECTION                         { Located _ T.Section{}         }
STRING                          { Located _ T.String{}          }
ATOM                            { Located _ T.Atom{}            }
NUMBER                          { Located _ T.Number{}          }
'*'                             { Located $$ T.Bullet            }
'['                             { Located $$ T.OpenList          }
','                             { Located _ T.Comma             }
']'                             { Located _ T.CloseList         }
'{'                             { Located $$ T.OpenMap           }
'}'                             { Located _ T.CloseMap          }
SEP                             { Located _ T.LayoutSep         }
END                             { Located _ T.LayoutEnd         }
EOF                             { Located _ T.EOF               }

%monad { Either (Located Token) }
%error { errorP }

%name config

%%

config ::                       { Value Position                }
  : value EOF                   { $1                            }

value ::                        { Value Position                }
  : sections END                { sections $1                   }
  | '*' list END                { List $1 (reverse $2)          }
  | simple                      { $1                            }

simple ::                       { Value Position                }
  : NUMBER                      { number   $1                   }
  | STRING                      { text     $1                   }
  | ATOM                        { atom     $1                   }
  | '{' inlinesections '}'      { Sections $1 (reverse $2)      }
  | '[' inlinelist ']'          { List     $1 (reverse $2)      }
  | '{' inlinesections term     {% untermSections $1            }
  | '[' inlinelist     term     {% untermList     $1            }

term ::                         { ()                            }
term : EOF                      { ()                            }
     | END                      { ()                            }
     | SEP                      { ()                            }

sections ::                     { [Section Position]            }
  :              section        { [$1]                          }
  | sections SEP section        { $3 : $1                       }

inlinesections ::               { [Section Position]            }
  :                             { []                            }
  | inlinesections1             { $1                            }
  | inlinesections1 ','         { $1                            }

inlinesections1 ::              { [Section Position]            }
  :                     section { [$1]                          }
  | inlinesections1 ',' section { $3 : $1                       }

section ::                      { Section Position              }
  : SECTION value               { section $1 $2                 }

list ::                         { [Value Position]              }
  :              value          { [$1]                          }
  | list SEP '*' value          { $4 : $1                       }

inlinelist ::                   { [Value Position]              }
  :                             { []                            }
  | inlinelist1                 { $1                            }
  | inlinelist1 ','             { $1                            }

inlinelist1 ::                  { [Value Position]              }
  :                 simple      { [$1]                          }
  | inlinelist1 ',' simple      { $3 : $1                       }

{

-- | Convert number token to number value. This needs a custom
-- function like this because there are multiple values matched from
-- the constructor.
number :: Located Token -> Value Position
number = \(Located a (T.Number n)) -> Number a n

section :: Located Token -> Value Position -> Section Position
section = \(Located a (T.Section k)) v -> Section a k v

sections :: [Section Position] -> Value Position
sections xxs = Sections (sectionAnn x) (x:xs)
  where x:xs = reverse xxs

text :: Located Token -> Value Position
text = \(Located a (T.String x)) -> Text a x

atom :: Located Token -> Value Position
atom = \(Located a (T.Atom x)) -> Atom a (MkAtom x)

errorP :: [Located Token] -> Either (Located Token) a
errorP xs = Left (head xs)

untermSections :: Position -> Either (Located Token) a
untermSections p = Left (Located p (T.Error T.UntermSections))

untermList :: Position -> Either (Located Token) a
untermList p = Left (Located p (T.Error T.UntermList))

-- | Attempt to parse a layout annotated token stream or
-- the token that caused the parse to fail.
parseValue ::
  [Located Token]                         {- ^ layout annotated token stream -} ->
  Either (Located Token) (Value Position) {- ^ token at failure or result -}
parseValue = config

}
