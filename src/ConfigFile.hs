{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

module ConfigFile
  ( ConfigSection(..)
  , ConfigValue(..)
  ) where

import Data.Text (Text)
import Data.Data (Data, Typeable)
import GHC.Generics (Generic)

data ConfigSection = ConfigSection
  { sectionName  :: Text
  , sectionValue :: ConfigValue
  }
  deriving (Read, Show, Typeable, Data, Generic)

data ConfigValue
  = ConfigSections [ConfigSection]
  | ConfigNumber Integer
  | ConfigText   Text
  | ConfigBool   Bool
  | ConfigList   [ConfigValue]
  deriving (Read, Show, Typeable, Data, Generic)
