module ConfigFile.Tokens
  ( Token(..)
  , PosToken(..)
  , layoutPass
  ) where

import Data.Text (Text)

data PosToken = PosToken
  { posLine   :: Int
  , posColumn :: Int
  , posToken  :: Token
  }
  deriving (Show)

data Token
  = Section Text
  | String Text
  | Bullet
  | Comma
  | Number Integer
  | OpenList
  | CloseList
  | OpenMap
  | CloseMap
  | Yes
  | No

  | Error

  -- "Virtual" tokens used by the subsequent layout processor
  | LayoutEnd
  | EOF
  deriving (Show)

-- | Process a list of position-annotated tokens inserting
-- layout end tokens as appropriate.
layoutPass ::
  [PosToken] {- ^ tokens without layout markers -} ->
  [PosToken] {- ^ tokens with    layout markers -}
layoutPass = layoutPass' [1]

-- | Worker function for 'layoutPass'. This function keeps
-- a stack of the current layout scopes. New scopes are
-- opened for layout herald tokens and the scopes are closed
-- when a token occurs at or before the marked column.
layoutPass' ::
  [Int]      {- ^ layout column stack -} ->
  [PosToken] {- ^ remaining tokens    -} ->
  [PosToken]

layoutPass' _ []  = []

-- Layout ends when a token occurs BEFORE the current layout scope.
layoutPass' (col:cols) (t:ts)
  | posColumn t < col
  = t { posToken = LayoutEnd } : layoutPass' cols (t:ts)

-- Layout FAILS when a token occurs AFTER the current layout scope
-- This is quite restrictive but the language is simple enough
-- that it's OK.
layoutPass' (col:_) (t:ts)
  | col < posColumn t = t { posToken = Error } : t : ts

-- Layout extends out to the token after token.
layoutPass' stack (t1:t2:ts)
  | isLayoutHerald t1
  = if posColumn t1 < posColumn t2
     then t1 : layoutPass' (posColumn t2:stack) (t2:ts)
     else t1 : t2 { posToken = Error } : t2 : ts

layoutPass' stack (t:ts) = t : layoutPass' stack ts



-- | Return True when a token starts a layout scope.
isLayoutHerald :: PosToken -> Bool
isLayoutHerald t
  | Section{} <- posToken t = True
  | Bullet    <- posToken t = True
  | otherwise               = False
