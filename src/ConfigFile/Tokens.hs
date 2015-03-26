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
layoutPass toks = foldr step (\_ _ -> []) toks False [1]

step :: PosToken -> (Bool -> [Int] -> [PosToken]) -> Bool -> [Int] -> [PosToken]

-- start blocks must be indented
-- tokens before the current layout end the current layout
step t next startBlock (col:cols)
  | posColumn t <= col && startBlock = [t {posToken = Error}]
  | posColumn t < col = t {posToken = LayoutEnd} : step t next startBlock cols

-- non-start layout needs to land on the current block
step t _ False (col:_)
  | usesLayout t && posColumn t /= col = [t {posToken = Error}]

step t next startBlock cols =
  t : next (usesLayout t) cols'
  where
  cols' | startBlock = posColumn t : cols
        | otherwise  = cols


-- | Return True when a token starts a layout scope.
usesLayout :: PosToken -> Bool
usesLayout t
  | Section{} <- posToken t = True
  | Bullet    <- posToken t = True
  | otherwise               = False
