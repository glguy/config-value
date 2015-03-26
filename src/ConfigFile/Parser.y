{
{-# LANGUAGE OverloadedStrings #-}

module ConfigFile.Parser (parse) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8

import ConfigFile
import ConfigFile.Lexer   (scanTokens)
import ConfigFile.Tokens  (PosToken(..), Token(..), layoutPass)

}

%token

SECTION                         { PosToken _ _ (Section $$)     }
STRING                          { PosToken _ _ (String $$)      }
NUMBER                          { PosToken _ _ (Number $$)      }
'yes'                           { PosToken _ _ Yes              }
'no'                            { PosToken _ _ No               }
'*'                             { PosToken _ _ Bullet           }
'['                             { PosToken _ _ OpenList         }
','                             { PosToken _ _ Comma            }
']'                             { PosToken _ _ CloseList        }
'{'                             { PosToken _ _ OpenMap          }
'}'                             { PosToken _ _ CloseMap         }
END                             { PosToken _ _ LayoutEnd        }

%tokentype                      { PosToken                      }
%lexer { lexerP }               { PosToken _ _ EOF              }
%monad { ParseM }
%name layout layout

%%

layout ::                       { ConfigValue                   }
  : value END                   { $1                            }

value ::                        { ConfigValue                   }
  : sections                    { ConfigSections (reverse $1)   }
  | list                        { ConfigList     (reverse $1)   }
  | simple                      { $1                            }

simple ::                       { ConfigValue                   }
  : NUMBER                      { ConfigNumber $1               }
  | STRING                      { ConfigText   $1               }
  | 'yes'                       { ConfigBool True               }
  | 'no'                        { ConfigBool False              }
  | '{' '}'                     { ConfigSections []             }
  | '[' inlinelist ']'          { ConfigList     $2             }

sections ::                     { [ConfigSection]               }
  :          section            { [$1]                          }
  | sections section            { $2 : $1                       }

section ::                      { ConfigSection                 }
  : SECTION layout              { ConfigSection $1 $2           }

list ::                         { [ConfigValue]                 }
  :      '*' layout             { [$2]                          }
  | list '*' layout             { $3 : $1                       }

inlinelist ::                   { [ConfigValue]                 }
  :                             { []                            }
  | inlinelist1                 { reverse $1                    }

inlinelist1 ::                  { [ConfigValue]                 }
  :                 simple      { [$1]                          }
  | inlinelist1 ',' simple      { $3 : $1                       }



{

newtype ParseM a = ParseM
  { runParseM :: PosToken -> [PosToken] -> Either (Int,Int) (PosToken,[PosToken], a) }

-- | Parse a configuration file or return the line and column of
-- the first error detected.
parse ::
  ByteString                   {- ^ UTF-8 encoded source        -} ->
  Either (Int,Int) ConfigValue {- ^ Either (Line,Column) Result -}
parse bytes =
  do toks <- scanTokens bytes
     let toks' = layoutPass toks
     (_,_,x) <- runParseM layout (error "previous token") toks'
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

lexerP :: (PosToken -> ParseM a) -> ParseM a
lexerP k = ParseM $ \_ toks ->
  case toks of
    []      -> error "Unexpected end of token stream"
    t:toks' -> runParseM (k t) t toks'

-- required by 'happy'
happyError :: ParseM a
happyError = ParseM $ \(PosToken line column _) _ -> Left (line,column)

}
