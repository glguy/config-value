-- | This module provides the token type used in the lexer and
-- parser and provides the extra pass to insert layout tokens.
module Config.Tokens
  ( Token(..)
  , Located(..)
  , Position(..)
  , layoutPass
  ) where

import Data.Text (Text)

-- | A position in a text file
data Position = Position
  { posLine, posColumn :: !Int }
  deriving (Read, Show)

-- | A value annotated with its text file position
data Located a = Located
  { locPosition :: !Position
  , locThing    :: !a
  }
  deriving (Read, Show)

instance Functor Located where
  fmap f (Located p x) = Located p (f x)

-- | The token type used by "Config.Lexer" and "Config.Parser"
data Token
  = Section Text
  | String Text
  | Bullet
  | Comma
  | Number Int Integer
  | OpenList
  | CloseList
  | OpenMap
  | CloseMap
  | Yes
  | No

  | Error

  -- "Virtual" tokens used by the subsequent layout processor
  | LayoutSep
  | LayoutEnd
  | EOF
  deriving (Show)

-- | Process a list of position-annotated tokens inserting
-- layout end tokens as appropriate.
layoutPass ::
  [Located Token] {- ^ tokens without layout markers -} ->
  [Located Token] {- ^ tokens with    layout markers -}
layoutPass toks = foldr step (\_ -> []) toks [0]

-- | Single step of the layout pass
step :: Located Token -> ([Int] -> [Located Token]) -> ([Int] -> [Located Token])

-- start blocks must be indented
-- tokens before the current layout end the current layout
-- note that EOF occurs on column 1 for properly formatted text files
step t next (col:cols)
  | tokenCol >  col && usesLayout t = t : next (tokenCol:col:cols)
  | tokenCol == col && usesLayout t = t{locThing=LayoutSep} : t : next (col:cols)
  | tokenCol <= col                 = t{locThing=LayoutEnd} : step t next cols
  where
  tokenCol = posColumn (locPosition t)

step t next cols = t : next cols


-- | Return True when a token starts a layout scope.
usesLayout :: Located Token -> Bool
usesLayout t
  | Section{} <- locThing t = True
  | Bullet    <- locThing t = True
  | otherwise               = False
