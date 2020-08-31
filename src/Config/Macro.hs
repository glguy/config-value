{-# LANGUAGE Safe, OverloadedStrings, DeriveDataTypeable, DeriveGeneric, DeriveTraversable #-}
{-|
Module      : Config.Macro
Description : Configuration pre-processor adding support for aliases and common sections
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

This module provides a processing pass for configuration files to assign
local variables used to reduce duplication.

= Sigils

* @$@ starts a variable.
* @\@@ starts a directive.

= Variables

Variables are atoms that start with a @$@ sigil. Variables are defined by
setting a variable as a section name. This variable will remain in
scope for the remainder of the sections being defined.

Variables used in a value position will be replaced with their previously
defined values.

@
$example: 42
field1: $example
field2: [0, $example]
@

expands to

@
field1: 42
field2: [0, 42]
@

Later variable definitions will shadow previous definitions.

= Sections inclusion

One sections value can be spliced into another sections value using the @\@include@
directive.

@
$xy: { x: 0, y: 1 }
example:
  \@include: $xy
  z: 2
@

expands to

@
example:
  x: 0
  y: 1
  z: 2
@

-}
module Config.Macro (MacroError(..), expandMacros, expandMacros') where

import Data.Text (Text)
import qualified Data.Text as Text
import Config
import Data.Map (Map)
import Data.Data
import GHC.Generics
import qualified Data.Map as Map

data MacroError a
  = UndeclaredVariable a Text -- ^ Variable used with a defintion
  | UnknownDirective a Text   -- ^ Unknown directive
  | BadInclude a              -- ^ Incorrect use of @\@include@
  deriving
  ( Eq, Read, Show, Typeable, Data
  , Functor, Foldable, Traversable
  , Generic, Generic1
  )

data Special = Plain | Variable Text | Include

processAtom :: a -> Text -> Either (MacroError a) Special
processAtom a txt =
  case Text.uncons txt of
    Just ('@',"include") -> pure Include
    Just ('@',t        ) -> Left (UnknownDirective a t)
    Just ('$',t        ) -> pure (Variable t)
    _                    -> pure Plain

-- | Expand macros in a configuration value.
expandMacros :: Value a -> Either (MacroError a) (Value a)
expandMacros = expandMacros' Map.empty

-- | Expand macros in a configuration value using a pre-populated environment.
expandMacros' :: Map Text (Value a) -> Value a -> Either (MacroError a) (Value a)
expandMacros' env v =
  case v of
    Number a n -> pure (Number a n)
    Text   a t -> pure (Text a t)
    List   a l -> fmap (List a) (traverse (expandMacros' env) l)
    Atom a x ->
      do x' <- processAtom a (atomName x)
         case x' of
           Plain -> pure (Atom a x)
           Include -> Left (BadInclude a)
           Variable var ->
             case Map.lookup var env of
               Nothing -> Left (UndeclaredVariable a var)
               Just y  -> pure y
    Sections a xs -> fmap (Sections a) (elaborateSections env xs)

elaborateSections :: Map Text (Value a) -> [Section a] -> Either (MacroError a) [Section a]
elaborateSections _ [] = pure []
elaborateSections env (Section a k v : xs) =
  do special <- processAtom a k
     v' <- expandMacros' env v
     case special of
       Plain ->
         do xs' <- elaborateSections env xs
            pure (Section a k v' : xs')
       Variable var ->
         elaborateSections (Map.insert var v' env) xs
       Include ->
         case v' of
           Sections _ ys -> elaborateSections env (ys ++ xs)
           _             -> Left (BadInclude a)
