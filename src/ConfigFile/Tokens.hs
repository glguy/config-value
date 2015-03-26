module ConfigFile.Tokens
  ( Token(..)
  , PosToken(..)
  , layoutPass
  ) where

import Data.Text            (Text)
import qualified Data.ByteString.Lazy.Char8 as L8

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

  -- "Virtual" tokens used by the subsequent layout processor
  | LayoutEnd
  | EOF
  deriving (Show)

layoutPass ::
  [Int]    {- ^ layout column stack -} ->
  [PosToken] {- ^ remaining tokens -} ->
  [PosToken]

layoutPass _ []  = []

layoutPass (col:cols) (t:ts)
  | posColumn t <= col
  = t { posToken = LayoutEnd }
  : layoutPass cols (t:ts)

layoutPass stack (t:ts)
  | isLayoutHerald t
  = t : layoutPass (posColumn t:stack) ts

layoutPass stack (t:ts) = t : layoutPass stack ts




isLayoutHerald :: PosToken -> Bool
isLayoutHerald t
  | Section{} <- posToken t = True
  | Bullet    <- posToken t = True
  | otherwise               = False
