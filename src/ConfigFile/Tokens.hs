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
layoutPass = layoutPass' [0]

-- | Worker function for 'layoutPass'. This function keeps
-- a stack of the current layout scopes. New scopes are
-- opened for layout herald tokens and the scopes are closed
-- when a token occurs at or before the marked column.
layoutPass' ::
  [Int]      {- ^ layout column stack -} ->
  [PosToken] {- ^ remaining tokens    -} ->
  [PosToken]

layoutPass' _ []  = []

layoutPass' (col:cols) (t:ts)
  | posColumn t <= col
  = t { posToken = LayoutEnd } : layoutPass' cols (t:ts)

layoutPass' stack (t:ts)
  | isLayoutHerald t
  = t : layoutPass' (posColumn t:stack) ts

layoutPass' stack (t:ts) = t : layoutPass' stack ts



-- | Return True when a token starts a layout scope.
isLayoutHerald :: PosToken -> Bool
isLayoutHerald t
  | Section{} <- posToken t = True
  | Bullet    <- posToken t = True
  | otherwise               = False
