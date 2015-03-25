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
  = Atom   Text
  | String Text
  | Colon
  | Bullet
  | Number Integer
  | EmptyList
  | EmptyMap

  -- "Virtual" tokens used by the subsequent layout processor
  | LayoutStart
  | LayoutEnd
  | LayoutSeparator
  | EOF
  deriving (Show)

layoutPass ::
  [Int]    {- ^ layout column stack -} ->
  [PosToken] {- ^ remaining tokens -} ->
  [PosToken]

layoutPass _ []  = []

layoutPass (col:cols) (t:ts)
  | posColumn t < col = t { posToken = LayoutEnd }
                      : layoutPass cols (t:ts)

layoutPass stack@(col:_) (t1:t2:ts)
  | Atom{} <- posToken t1
  , Colon  <- posToken t2
  , posColumn t1 < posColumn t2 =

      let (stack',newToken)
           | col == posColumn t1 = (             stack, LayoutSeparator)
           | otherwise           = (posColumn t1:stack, LayoutStart    )

      in t1 { posToken = newToken }
       : t1 : t2
       : layoutPass stack' ts

layoutPass stack@(col:_) (t:ts)
  | Bullet <- posToken t
  , col < posColumn t =

     t { posToken = LayoutStart } : t
     : layoutPass (posColumn t : stack) ts

layoutPass stack (t:ts) = t : layoutPass stack ts
