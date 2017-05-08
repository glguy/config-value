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
--
-- Example:
--
--    * @my-key: my-value@ is @'Section' ('Atom' "my-key") ('Atom' "my-value")@
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
-- type in a configuration. Atoms can be constructed
-- using the @OverloadedStrings@ extension.
newtype Atom = MkAtom { atomName :: Text }
  deriving (Eq, Ord, Show, Read, Typeable, Data
#if MIN_VERSION_base(4,6,0)
           , Generic
#endif
           )

instance IsString Atom where
  fromString = MkAtom . fromString

-- | Sum type of the values supported by this language.
--
-- The first field of the 'Number' constructor is the based used in the concrete
-- syntax of the configuration value.
--
-- The 'Floating' constructor stores the coefficient and power-of-10 exponent used in
-- the concrete syntax. This allows representing numbers that would
-- otherwise overflow a 'Double'.
--
-- Examples:
--
--    * @0xff@ is @'Number' 16 255@
--
--    * @123@  is @'Number' 10 123@
--
--    * @123e10@ is @'Floating' 123 10@
--    * @123.45@ is @'Floating' 12345 (-2)@
data Value
  = Sections [Section] -- ^ lists of key-value pairs
  | Number   Int Integer -- ^ integer literal base (2, 8, 10, or 16) and integer value
  | Floating Integer Integer -- ^ coef exponent: coef * 10 ^ exponent
  | Text     Text -- ^ quoted strings
  | Atom     Atom -- ^ unquoted strings
  | List     [Value] -- ^ lists
  deriving (Eq, Read, Show, Typeable, Data
#if MIN_VERSION_base(4,6,0)
           , Generic
#endif
           )
