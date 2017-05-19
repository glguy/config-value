{-# LANGUAGE CPP #-}

-- | Optics for compatibility with the lens package
module Config.Lens
  ( key
  , text
  , number
  , atom
  , list
  , values
  , sections
  , ann
  , valuePlate
  ) where

import Config.Value

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
  (Value a -> f (Value a)) -> Value a -> f (Value a)
key i = sections . traverse . section i

-- | Apply a function to the 'Value' contained inside the given
-- 'Value' when it is a section name matches the given name.
section ::
  Applicative f =>
  Text {- ^ section name -} ->
  (Value a -> f (Value a)) -> Section a -> f (Section a)
section i f s@(Section a j v) | i == j    = Section a j <$> f v
                              | otherwise = pure s

-- | Apply a function to the ['Section'] contained inside the given
-- 'Value' when it is a @Sections@.
sections :: Applicative f => ([Section a] -> f [Section a]) -> Value a -> f (Value a)
sections f (Sections a xs) = Sections a <$> f xs
sections _ v               = pure v

-- | Apply a function to the 'Text' contained inside the given
-- 'Value' when it is a @Text@.
text :: Applicative f => (Text -> f Text) -> Value a -> f (Value a)
text f (Text a t) = Text a <$> f t
text _ v          = pure v

-- | Apply a function to the 'Text' contained inside the given
-- 'Value' when it is a @Text@. This traversal is only valid
-- if the output atom is a valid atom!
atom :: Applicative f => (Atom -> f Atom) -> Value a -> f (Value a)
atom f (Atom a t) = Atom a <$> f t
atom _ v          = pure v

-- | Apply a function to the 'Integer' contained inside the given
-- 'Value' when it is a @Number@.
number :: Applicative f => (Integer -> f Integer) -> Value a -> f (Value a)
number f (Number a b n) = Number a b <$> f n
number _ v              = pure v

-- | Apply a function to the ['Value'] contained inside the given
-- 'Value' when it is a @List@.
list :: Applicative f => ([Value a] -> f [Value a]) -> Value a -> f (Value a)
list f (List a xs) = List a <$> f xs
list _ v           = pure v

-- | Apply a function to any of the immediate values in a list or
-- a sections list. This is intended to be used with Control.Lens.Plated.
valuePlate :: Applicative f => (Value a -> f (Value a)) -> Value a -> f (Value a)
valuePlate f (List     a xs) = List     a <$> traverse             f  xs
valuePlate f (Sections a xs) = Sections a <$> traverse (sectionVal f) xs
valuePlate _ v               = pure v

sectionVal :: Functor f => (Value a -> f (Value a)) -> Section a -> f (Section a)
sectionVal f (Section a k v) = Section a k <$> f v

-- | Apply a function to the 'Value' elements inside the given
-- 'Value' when it is a @List@.
--
-- > values = list . traverse
values :: Applicative f => (Value a -> f (Value a)) -> Value a -> f (Value a)
values = list . traverse


ann :: Functor f => (a -> f a) -> Value a -> f (Value a)
ann f v =
  case v of
    Sections a x   -> (\a' -> Sections a' x  ) <$> f a
    Number   a x y -> (\a' -> Number   a' x y) <$> f a
    Floating a x y -> (\a' -> Floating a' x y) <$> f a
    Text     a x   -> (\a' -> Text     a' x  ) <$> f a
    Atom     a x   -> (\a' -> Atom     a' x  ) <$> f a
    List     a x   -> (\a' -> List     a' x  ) <$> f a
