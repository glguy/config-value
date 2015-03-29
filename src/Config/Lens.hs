{-# LANGUAGE CPP #-}

-- | Optics for compatibility with the lens package
module Config.Lens
  ( key
  , text
  , number
  , atom
  , list
  , sections
  ) where

import Config

import Data.Text

#if !MIN_VERSION_base(4,8,0)
import Control.Applicative
import Data.Traversable
#endif

-- | Apply a function to the subsections of the given value when
-- that value is a @Sections@ and the subsection name matches the
-- given section name 
key ::
  Applicative f =>
  Text {- ^ section name -} ->
  (Value -> f Value) -> Value -> f Value
key i f (Sections xs) = Sections <$> traverse (section i f) xs
key _ _ v             = pure v

-- | Apply a function to the 'Value' contained inside the given
-- 'Value' when it is a section name matches the given name.
section ::
  Applicative f =>
  Text {- ^ section name -} ->
  (Value -> f Value) -> Section -> f Section
section i f s@(Section j v) | i == j = Section j <$> f v
                            | otherwise = pure s

-- | Apply a function to the ['Section'] contained inside the given
-- 'Value' when it is a @Sections@.
sections :: Applicative f => ([Section] -> f [Section]) -> Value -> f Value
sections f (Sections xs) = Sections <$> f xs
sections _ v             = pure v

-- | Apply a function to the 'Text' contained inside the given
-- 'Value' when it is a @Text@.
text :: Applicative f => (Text -> f Text) -> Value -> f Value
text f (Text t) = Text <$> f t
text _ v        = pure v

-- | Apply a function to the 'Text' contained inside the given
-- 'Value' when it is a @Text@. This traversal is only valid
-- if the output atom is a valid atom!
atom :: Applicative f => (Text -> f Text) -> Value -> f Value
atom f (Atom t) = Atom <$> f t
atom _ v        = pure v

-- | Apply a function to the 'Integer' contained inside the given
-- 'Value' when it is a @Number@.
number :: Applicative f => (Integer -> f Integer) -> Value -> f Value
number f (Number b n) = Number b <$> f n
number _ v            = pure v

-- | Apply a function to the ['Value'] contained inside the given
-- 'Value' when it is a @List@.
list :: Applicative f => ([Value] -> f [Value]) -> Value -> f Value
list f (List xs) = List <$> f xs
list _ v         = pure v
