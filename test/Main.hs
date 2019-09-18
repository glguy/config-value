{-# Language OverloadedStrings #-}
{-|
Module      : Main
Description : Unit tests for config-schema
Copyright   : (c) Eric Mertens, 2017
License     : ISC
Maintainer  : emertens@gmail.com
-}
module Main (main) where

import           Config
import           Config.Number
import           Control.Monad (unless)
import           Data.Foldable
import           Data.Text (Text)
import qualified Data.Text as Text

parseTest ::
  Value () {- ^ expected value -} ->
  [Text]   {- ^ input lines    -} ->
  IO ()
parseTest expected txts =
  case parse (Text.unlines txts) of
    Left e -> fail (show e)
    Right v ->
      unless ((() <$ v) == expected) (fail (show (expected, () <$ v)))

main :: IO ()
main = sequenceA_

  [ parseTest (Number () (MkNumber (Radix10 0) 42))     ["42"]
  , parseTest (Number () (MkNumber (Radix10 56) 42))    ["42e56"]
  , parseTest (Number () (MkNumber (Radix10 56) 42.34)) ["42.34e56"]
  , parseTest (Number () (MkNumber (Radix10 0) 42.34))  ["42.34"]
  , parseTest (Number () (MkNumber (Radix10 0) 42))     ["42."]
  , parseTest (Number () (MkNumber (Radix10 0) 42))     ["042"]

  , parseTest (Number () (MkNumber (Radix16 0) 42))     ["0x2a"]
  , parseTest (Number () (MkNumber (Radix16 56) 42))    ["0x2ap56"]
  , parseTest (Number () (MkNumber (Radix16 56) (0x2a + (0x34 / 16^(2::Int))))) ["0x2a.34p56"]
  , parseTest (Number () (MkNumber (Radix16 0)  (0x2a + (0x3f / 16^(2::Int))))) ["0x2a.3f"]
  , parseTest (Number () (MkNumber (Radix16 0) 42))     ["0x2a."]
  , parseTest (Number () (MkNumber (Radix16 0) 42))     ["0x02a"]

  , parseTest (Number () (MkNumber Radix2 42))     ["0b101010"]
  , parseTest (Number () (MkNumber Radix2 4))     ["0b0100"]
  , parseTest (Number () (MkNumber Radix2 4))     ["0b0100."]
  , parseTest (Number () (MkNumber Radix2 (4 + (22 / 2^(6::Int))))) ["0b100.010110"]

  , parseTest (Number () (MkNumber Radix8 55))     ["0o67"]
  , parseTest (Number () (MkNumber Radix8 55))     ["0o67."]
  , parseTest (Number () (MkNumber Radix8 55))     ["0o067"]
  , parseTest (Number () (MkNumber Radix8 (55 + (10 / 64)))) ["0o67.12"]
  ]
