{-|
Module      : Config.Lens
Description : Lenses and traversals for manipulating 'Value' values.
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com

Lenses and traversals for compatibility with the lens package
-}
module Config.Lens
  ( key
  , text
  , atom
  , number
  , list
  , values
  , sections
  , ann
  , valuePlate
  ) where

import Config.Number
import Config.Value
import Data.Text

-- | Traversal for the subsections of the given 'Value' when
-- that value is a 'Sections' and the section name matches the
-- given name.
key ::
  Applicative f =>
  Text {- ^ section name -} ->
  (Value a -> f (Value a)) -> Value a -> f (Value a)
key i = sections . traverse . section i

-- | Traversal for the 'Value' contained inside the given
-- 'Section' when its section name matches the given name.
section ::
  Applicative f =>
  Text {- ^ section name -} ->
  (Value a -> f (Value a)) -> Section a -> f (Section a)
section i f s@(Section a j v) | i == j    = Section a j <$> f v
                              | otherwise = pure s

-- | Traversal for the ['Section'] contained inside the given
-- 'Value' when it is a 'Sections'.
sections :: Applicative f => ([Section a] -> f [Section a]) -> Value a -> f (Value a)
sections f (Sections a xs) = Sections a <$> f xs
sections _ v               = pure v

-- | Traversal for the 'Text' contained inside the given 'Value'.
text :: Applicative f => (Text -> f Text) -> Value a -> f (Value a)
text f (Text a t) = Text a <$> f t
text _ v          = pure v

-- | Traversal for the 'Atom' contained inside the given 'Value'.
atom :: Applicative f => (Atom -> f Atom) -> Value a -> f (Value a)
atom f (Atom a t) = Atom a <$> f t
atom _ v          = pure v

-- | Traversal for the 'Number' contained inside the given 'Value'.
number :: Applicative f => (Number -> f Number) -> Value a -> f (Value a)
number f (Number a n) = Number a <$> f n
number _ v            = pure v

-- | Traversal for the ['Value'] contained inside the given
-- 'Value' when it is a 'List'.
list :: Applicative f => ([Value a] -> f [Value a]) -> Value a -> f (Value a)
list f (List a xs) = List a <$> f xs
list _ v           = pure v

-- | Traversal for the immediate values in a list or a sections list.
--
-- This is intended to be used with "Control.Lens.Plated".
valuePlate :: Applicative f => (Value a -> f (Value a)) -> Value a -> f (Value a)
valuePlate f (List     a xs) = List     a <$> traverse             f  xs
valuePlate f (Sections a xs) = Sections a <$> traverse (sectionVal f) xs
valuePlate _ v               = pure v

sectionVal :: Functor f => (Value a -> f (Value a)) -> Section a -> f (Section a)
sectionVal f (Section a k v) = Section a k <$> f v

-- | Traversal for the 'Value' elements inside the given
-- 'Value' when it is a 'List'.
--
-- @
-- 'values' = 'list' . 'traverse'
-- @
values :: Applicative f => (Value a -> f (Value a)) -> Value a -> f (Value a)
values = list . traverse


-- | Lens for the annotation component of a 'Value'
ann :: Functor f => (a -> f a) -> Value a -> f (Value a)
ann f v =
  case v of
    Sections a x -> (\a' -> Sections a' x) <$> f a
    Number   a x -> (\a' -> Number   a' x) <$> f a
    Text     a x -> (\a' -> Text     a' x) <$> f a
    Atom     a x -> (\a' -> Atom     a' x) <$> f a
    List     a x -> (\a' -> List     a' x) <$> f a
