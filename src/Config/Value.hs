{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- | This module provides the types used in this package for configuration.
-- Visit "ConfigFile.Parser" to parse values of this type in a convenient
-- layout based notation.
module Config.Value
  ( Section(..)
  , Value(..)
  ) where

import Data.Text    (Text)
import Data.Data    (Data, Typeable)
import GHC.Generics (Generic)

-- | A single section of a 'Value'
data Section = Section
  { sectionName  :: Text
  , sectionValue :: Value
  }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

-- | Sum type of the values supported by this language.
data Value
  = Sections [Section]
  | Number   Int Integer -- ^ base number
  | Text     Text
  | Atom     Text
  | List     [Value]
  deriving (Eq, Read, Show, Typeable, Data, Generic)
