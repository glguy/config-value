{-# LANGUAGE CPP #-}
{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Config.ParserUtils
  ( Parser
  , runParser
  , lexerP
  , errorP
  ) where

import Control.Monad.Trans.Class (lift)
import Control.Monad.Trans.State.Strict (StateT, evalStateT, get, put)

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
#endif

------------------------------------------------------------------------
-- Parser monad implementation
------------------------------------------------------------------------

newtype Parser t a = Parser (StateT [t] (Either t) a)
  deriving (Functor, Applicative, Monad)

runParser :: Parser t a -> [t] -> Either t a
runParser (Parser m) = evalStateT m

lexerP :: Parser t t
lexerP = Parser $
  do x:xs <- get
     put xs
     return x

errorP :: t -> Parser t a
errorP = Parser . lift . Left
