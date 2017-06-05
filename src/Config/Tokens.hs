-- | This module provides the token type used in the lexer and
-- parser and provides the extra pass to insert layout tokens.
module Config.Tokens
  ( Token(..)
  , Located(..)
  , Position(..)
  , Error(..)
  , layoutPass
  ) where

import Data.Text (Text)

-- | A position in a text file
data Position = Position
  { posIndex, posLine, posColumn :: {-# UNPACK #-} !Int }
  deriving (Read, Show, Ord, Eq)

-- | A value annotated with its text file position
data Located a = Located
  { locPosition :: {-# UNPACK #-} !Position
  , locThing    :: !a
  }
  deriving (Read, Show)

instance Functor Located where
  fmap f (Located p x) = Located p (f x)

-- | The token type used by "Config.Lexer" and "Config.Parser"
data Token
  = Section Text
  | String Text
  | Atom Text
  | Bullet
  | Comma
  | Number Int Integer
  | Floating Integer Integer
  | OpenList
  | CloseList
  | OpenMap
  | CloseMap

  | Error Error

  -- "Virtual" tokens used by the subsequent layout processor
  | LayoutSep
  | LayoutEnd
  | EOF
  deriving (Show)

-- | Types of lexical errors
data Error
  = UntermComment
  | UntermString
  | UntermList
  | UntermSections
  | BadEscape Text
  | NoMatch Char
  deriving (Show)

-- | Process a list of position-annotated tokens inserting
-- layout end tokens as appropriate.
layoutPass ::
  [Located Token] {- ^ tokens without layout markers -} ->
  [Located Token] {- ^ tokens with    layout markers -}
layoutPass toks = foldr step (\_ -> []) toks [Layout (-1)]

data Layout = NoLayout | Layout Int

-- | Single step of the layout pass
step ::
  Located Token                 {- ^ current token            -} ->
  ([Layout] -> [Located Token]) {- ^ continuation             -} ->
  [Layout]                      {- ^ stack of layout scopes   -} ->
  [Located Token]               {- ^ token stream with layout -}

-- start blocks must be indented
-- tokens before the current layout end the current layout
-- note that EOF occurs on column 1 for properly formatted text files
step t next cols =
  case cols of
    NoLayout:cols' | CloseMap <- locThing t -> t : next cols'
    _              | OpenMap  <- locThing t -> t : next (NoLayout : cols)
    Layout col:_     | toCol t == col -> t{locThing=LayoutSep} : t : next cols
    Layout col:cols' | toCol t <  col -> t{locThing=LayoutEnd} : step t next cols'
    Layout{}:_       | usesLayout t   -> t : next (Layout (toCol t) : cols)
    _                                 -> t : next cols

-- | Extract the column number from a located thing.
toCol :: Located a -> Int
toCol = posColumn . locPosition


-- | Return True when a token starts a layout scope.
usesLayout :: Located Token -> Bool
usesLayout t
  | Section{} <- locThing t = True
  | Bullet    <- locThing t = True
  | otherwise               = False
