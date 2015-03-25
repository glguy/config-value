module ConfigFile where

import Control.Applicative
import Data.ByteString.Lazy (ByteString)
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as Map

data ConfigSection = ConfigSection
  { sectionName  :: Text
  , sectionValue :: ConfigValue
  }
  deriving (Read, Show)

data ConfigValue
  = ConfigSections [ConfigSection]
  | ConfigNumber Integer
  | ConfigText   Text
  | ConfigBool   Bool
  | ConfigList   [ConfigValue]
  deriving (Read, Show)
