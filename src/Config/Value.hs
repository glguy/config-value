{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE DeriveDataTypeable #-}

#ifndef MIN_VERSION_base
#define MIN_VERSION_base(x,y,z) 1
#endif

-- | This module provides the types used in this package for configuration.
-- Visit "ConfigFile.Parser" to parse values of this type in a convenient
-- layout based notation.
module Config.Value
  ( Section(..)
  , Value(..)
  , Atom(..)
  ) where

import Data.Text    (Text)
import Data.Data    (Data, Typeable)
import Data.String  (IsString(..))

#if MIN_VERSION_base(4,6,0)
import GHC.Generics (Generic)
#endif

-- | A single section of a 'Value'
data Section = Section
  { sectionName  :: Text
  , sectionValue :: Value
  }
  deriving (Eq, Read, Show, Typeable, Data
#if MIN_VERSION_base(4,6,0)
           , Generic
#endif
           )

-- | Wrapper to distinguish 'Atom' from 'Text' by
-- type in a configuration.
newtype Atom = MkAtom { atomName :: Text }
  deriving (Eq, Ord, Show, Read, Typeable, Data
#if MIN_VERSION_base(4,6,0)
           , Generic
#endif
           )

instance IsString Atom where
  fromString = MkAtom . fromString

-- | Sum type of the values supported by this language.
data Value
  = Sections [Section]
  | Number   Int Integer -- ^ base number
  | Floating Integer Integer -- ^ coef exponent: coef * 10 ^ exponent
  | Text     Text
  | Atom     Atom
  | List     [Value]
  deriving (Eq, Read, Show, Typeable, Data
#if MIN_VERSION_base(4,6,0)
           , Generic
#endif
           )
