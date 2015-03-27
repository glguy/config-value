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

data Section = Section
  { sectionName  :: Text
  , sectionValue :: Value
  }
  deriving (Eq, Read, Show, Typeable, Data, Generic)

data Value
  = Sections [Section]
  | Number   Int Integer -- ^ base number
  | Text     Text
  | Bool     Bool
  | List     [Value]
  deriving (Eq, Read, Show, Typeable, Data, Generic)
