{-# Language DeriveGeneric, DeriveTraversable, DeriveDataTypeable #-}

-- | This module provides the types used in this package for configuration.
-- Visit "Config.Parser" to parse values of this type in a convenient
-- layout based notation.
module Config.Value
  ( Section(..)
  , Value(..)
  , Atom(..)
  , valueAnn
  ) where

import Data.Text    (Text)
import Data.Data    (Data, Typeable)
import Data.String  (IsString(..))
import GHC.Generics (Generic, Generic1)

-- | A single section of a 'Value'
--
-- Example:
--
--    * @my-key: my-value@ is @'Section' ('Atom' "my-key") ('Atom' "my-value")@
data Section a = Section
  { sectionAnn   :: a
  , sectionName  :: Text
  , sectionValue :: Value a
  }
  deriving ( Eq, Read, Show, Typeable, Data
           , Functor, Foldable, Traversable
           , Generic, Generic1
           )

-- | Wrapper to distinguish 'Atom' from 'Text' by
-- type in a configuration. Atoms can be constructed
-- using the @OverloadedStrings@ extension.
newtype Atom = MkAtom { atomName :: Text }
  deriving ( Eq, Ord, Show, Read, Typeable, Data
           , Generic
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
-- 'Value' is parameterized over an annotation type indented to be used for
-- file position or other application specific information.
--
-- Examples:
--
--    * @0xff@ is @'Number' 16 255@
--
--    * @123@  is @'Number' 10 123@
--
--    * @123e10@ is @'Floating' 123 10@
--    * @123.45@ is @'Floating' 12345 (-2)@
data Value a
  = Sections a [Section a] -- ^ lists of key-value pairs
  | Number   a Int Integer -- ^ integer literal base (2, 8, 10, or 16) and integer value
  | Floating a Integer Integer -- ^ coef exponent: coef * 10 ^ exponent
  | Text     a Text -- ^ quoted strings
  | Atom     a Atom -- ^ unquoted strings
  | List     a [Value a] -- ^ lists
  deriving ( Eq, Read, Show, Typeable, Data
           , Functor, Foldable, Traversable
           , Generic, Generic1
           )

-- | Returns the annotation for a value.
valueAnn :: Value a -> a
valueAnn v =
  case v of
    Sections a _   -> a
    Number   a _ _ -> a
    Floating a _ _ -> a
    Text     a _   -> a
    Atom     a _   -> a
    List     a _   -> a
