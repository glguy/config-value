{-# Language DeriveDataTypeable #-}
module Config.Number
  ( Number(..)
  , Radix(..)
  , radixToInt
  , numberToRational
  , numberToInteger
  ) where

import Data.Ratio (numerator, denominator)
import Data.Data (Data)

-- | Numbers are represented as base, coefficient, and exponent.
data Number = MkNumber
  { numberRadix       :: !Radix
  , numberCoefficient :: !Rational
  }
  deriving (Eq, Ord, Read, Show, Data)

-- | Radix used for a number. Some radix modes support an
-- exponent.
data Radix
  = Radix2  -- ^ binary, base 2
  | Radix8  -- ^ octal, base 8
  | Radix10 !Integer -- ^ decimal, base 10, exponent base 10
  | Radix16 !Integer -- ^ hexdecimal, base 16, exponent base 2
  deriving (Eq, Ord, Read, Show, Data)

radixToInt :: Radix -> Int
radixToInt r =
  case r of
    Radix2 {} ->  2
    Radix8 {} ->  8
    Radix10{} -> 10
    Radix16{} -> 16

-- | Convert a number to a 'Rational'. Warning: This can use a
-- lot of member in the case of very large exponent parts.
numberToRational :: Number -> Rational
numberToRational (MkNumber r c) =
  case r of
    Radix2    -> c
    Radix8    -> c
    Radix10 e -> c * 10 ^^ e
    Radix16 e -> c * 2  ^^ e

-- | Convert a number to a 'Integer'. Warning: This can use a
-- lot of member in the case of very large exponent parts.
numberToInteger :: Number -> Maybe Integer
numberToInteger n
  | denominator r == 1 = Just $! numerator r
  | otherwise          = Nothing
  where
    r = numberToRational n
