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

'yes'  { PosToken _ _ (Atom "yes") }
'no'   { PosToken _ _ (Atom "no") }
ATOM   { PosToken _ _ (Atom $$) }
STRING { PosToken _ _ (String $$) }
':'    { PosToken _ _ Colon }
'*'    { PosToken _ _ Bullet }
NUMBER { PosToken _ _ (Number $$) }
'[]'   { PosToken _ _ EmptyList  }
'{}'   { PosToken _ _ EmptyMap  }

START  { PosToken _ _ LayoutStart }
END    { PosToken _ _ LayoutEnd }
SEP    { PosToken _ _ LayoutSeparator }

%name value value
%tokentype { PosToken }
%monad     { ParseM   }
%lexer     { lexerP } { PosToken _ _ EOF }

%%

value :: { ConfigValue }
  : START fields END { ConfigSections (reverse $2) }
  | NUMBER           { ConfigNumber $1 }
  | STRING           { ConfigText   $1 }
  | '[]'             { ConfigList     [] }
  | '{}'             { ConfigSections [] }
  | 'yes'            { ConfigBool True   }
  | 'no'             { ConfigBool False  }
  | START list END   { ConfigList (reverse $2) }

fields :: { [ConfigSection] }
  : field            { [$1] }
  | fields SEP field { $3 : $1 }

field :: { ConfigSection }
  : ATOM ':' value   { ConfigSection $1 $3 }

list :: { [ConfigValue] }
  : '*' value  { [$2] }
  | list '*' value { $3 : $1 }

{

newtype ParseM a = ParseM { runParseM :: [PosToken] -> Either (Int,Int) ([PosToken], a) }

-- | Parse a configuration file or return the line and column of
-- the first error detected.
parse :: ByteString -> Either (Int,Int) ConfigValue
parse bytes =
  do toks <- scanTokens bytes
     let toks' = layoutPass [0] toks
     (_,x) <- runParseM value toks'
     return x

instance Functor ParseM where
  fmap = liftM

instance Applicative ParseM where
  (<*>) = ap
  pure  = return

instance Monad ParseM where
  return x = ParseM (\s -> Right (s,x))
  ParseM m >>= f = ParseM $ \toks ->
                     do (toks',x) <- m toks
                        let ParseM n = f x
                        n toks'

lexerP :: (PosToken -> ParseM a) -> ParseM a
lexerP k = ParseM $ \toks ->
  case toks of
    []      -> error "Unexpected end of token stream"
    t:toks' ->
      let ParseM p = k t
      in p toks'

-- required by 'happy'
happyError :: ParseM a
happyError = ParseM $ \toks ->
  case toks of
    [] -> Left (1,1) -- literally empty file in our grammar
    PosToken line column _:_ -> Left (line,column)
}
