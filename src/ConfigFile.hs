module ConfigFile where

import Control.Applicative
import Data.Map (Map)
import Data.ByteString.Lazy (ByteString)
import qualified Data.Map as Map

newtype ConfigSection = ConfigSection (Map String ConfigValue)
  deriving (Read, Show)

data ConfigValue
  = Subsection ConfigSection
  | ConfigNumber Integer
  | ConfigString String
  | ConfigBool   Bool
  | ConfigList   [ConfigValue]
  deriving (Read, Show)
