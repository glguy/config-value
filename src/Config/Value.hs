{-# Language DeriveGeneric, DeriveTraversable, DeriveDataTypeable, Safe #-}

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

import Config.Number (Number)

-- | A single section of a 'Value'
--
-- Example:
--
--    * @my-key: my-value@ is @'Section' _ "my-key" ('Atom' _ "my-value")@
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
-- 'Value' is parameterized over an annotation type indented to be used for
-- file position or other application specific information. When no
-- annotations are needed, '()' is a fine choice.
data Value a
  = Sections a [Section a] -- ^ lists of key-value pairs
  | Number   a Number      -- ^ numbers
  | Text     a Text        -- ^ quoted strings
  | Atom     a Atom        -- ^ unquoted strings
  | List     a [Value a]   -- ^ lists
  deriving ( Eq, Read, Show, Typeable, Data
           , Functor, Foldable, Traversable
           , Generic, Generic1
           )

-- | Returns the annotation for a value.
valueAnn :: Value a -> a
valueAnn v =
  case v of
    Sections a _ -> a
    Number   a _ -> a
    Text     a _ -> a
    Atom     a _ -> a
    List     a _ -> a
