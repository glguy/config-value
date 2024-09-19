{-# Language DeriveDataTypeable, DeriveGeneric, Safe #-}
{-|
Module      : Config.Number
Description : Scientific-notation numbers with explicit radix
Copyright   : (c) Eric Mertens, 2019
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a representation of numbers in scientific
notation.
-}
module Config.Number
  ( Number(..)
  , Radix(..)
  , radixToInt
  , numberToRational
  , numberToInteger
  , integerToNumber
  , rationalToNumber
  ) where

import Data.Ratio (numerator, denominator)
import Data.Data (Data)
import GHC.Generics (Generic)

-- | Numbers are represented as base, coefficient, and exponent.
--
-- The most convenient way to get numbers into and out of this form
-- is to use one of: 'numberToRational', 'numberToInteger',
-- 'rationalToNumber', or 'integerToNumber'.
--
-- This representation is explicit about the radix and exponent
-- used to facilitate better pretty-printing. By using explicit
-- exponents extremely large numbers can be represented compactly.
-- Consider that it is easy to write `1e100000000` which would use
-- a significant amount of memory if realized as an 'Integer'. This
-- representation allows concerned programs to check bounds before
-- converting to a representation like 'Integer'.
data Number = MkNumber
  { numberRadix       :: !Radix
  , numberCoefficient :: !Rational
  }
  deriving (Eq, Ord, Read, Show, Data, Generic)

-- | Radix used for a number. Some radix modes support an exponent.
data Radix
  = Radix2           -- ^ binary, base 2
  | Radix8           -- ^ octal, base 8
  | Radix10 !Integer -- ^ decimal, base 10, exponent base 10
  | Radix16 !Integer -- ^ hexdecimal, base 16, exponent base 2
  deriving (Eq, Ord, Read, Show, Data, Generic)

-- | Returns the radix as an integer ignoring any exponent.
radixToInt :: Radix -> Int
radixToInt r =
  case r of
    Radix2 {} ->  2
    Radix8 {} ->  8
    Radix10{} -> 10
    Radix16{} -> 16

-- | Convert a number to a 'Rational'. Warning: This can use a
-- lot of memory in the case of very large exponent parts.
numberToRational :: Number -> Rational
numberToRational (MkNumber r c) =
  case r of
    Radix2    -> c
    Radix8    -> c
    Radix10 e -> c * 10 ^^ e
    Radix16 e -> c *  2 ^^ e

-- | Convert a number to a 'Integer'. Warning: This can use a
-- lot of memory in the case of very large exponent parts.
numberToInteger :: Number -> Maybe Integer
numberToInteger n
  | denominator r == 1 = Just $! numerator r
  | otherwise          = Nothing
  where
    r = numberToRational n

-- | 'Integer' to a radix 10 'Number' with no exponent
integerToNumber :: Integer -> Number
integerToNumber = rationalToNumber . fromInteger

-- | 'Rational' to a radix 10 'Number' with no exponent
rationalToNumber :: Rational -> Number
rationalToNumber = MkNumber (Radix10 0)
