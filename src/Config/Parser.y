{
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Trustworthy #-}

module Config.Parser (parse) where

import Control.Applicative
import Control.Monad
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8

import Config.Value   (Section(..), Value(..))
import Config.Lexer   (scanTokens)
import Config.Tokens  (Located(..), Position(..), Token, layoutPass)
import qualified Config.Tokens as T

}

%token

SECTION                         { Located _ (T.Section $$)      }
STRING                          { Located _ (T.String $$)       }
NUMBER                          { Located _ $$@T.Number{}       }
'yes'                           { Located _ T.Yes               }
'no'                            { Located _ T.No                }
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
  | 'yes'                       { Bool True                     }
  | 'no'                        { Bool False                    }
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

number :: T.Token -> Value
number (T.Number base val) = Number base val
number _                   = error "Config.Parser.number: fatal error"

newtype ParseM a = ParseM
  { runParseM :: Position -> [Located Token] -> Either (Int,Int) (Position,[Located Token], a) }

-- | Parse a configuration value and return the result on the
-- right, or the position of an error on the left.
parse ::
  ByteString             {- ^ UTF-8 encoded source        -} ->
  Either (Int,Int) Value {- ^ Either (Line,Column) Result -}
parse bytes =
  do let toks = layoutPass (scanTokens bytes)
     (_,_,x) <- runParseM value (error "previous token") toks
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
happyError = ParseM $ \posn _ -> Left (posLine posn, posColumn posn)

}
