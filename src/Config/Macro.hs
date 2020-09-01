{-# LANGUAGE Safe, OverloadedStrings, DeriveTraversable, RankNTypes #-}
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

= Sections splicing

One sections value can be spliced into another sections value using the @\@spilce@
directive. It is an error to splice a value that is not a key-value sections.

@
$xy: { x: 0, y: 1 }
example:
  \@splice: $xy
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
module Config.Macro (
  -- * Macro expansion primitives
  MacroError(..),
  expandMacros,
  expandMacros',

  -- * File loader with inclusion
  LoadFileError(..),
  FilePosition(..),
  loadFileWithMacros
  ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as Text
import Control.Exception
import Config
import Data.Map (Map)
import Data.Typeable (Typeable)
import qualified Data.Map as Map

-- | Errors from macro expansion.
data MacroError a
  = UndeclaredVariable a Text -- ^ Variable used with a defintion
  | UnknownDirective a Text   -- ^ Unknown directive
  | BadSplice a               -- ^ Incorrect use of @\@splice@
  | BadLoad a                 -- ^ Incorrect use of @\@load@
  deriving
  (Eq, Read, Show, Functor, Foldable, Traversable)

instance (Typeable a, Show a) => Exception (MacroError a)

data Special = Plain | Variable Text | Splice | Load

processAtom :: a -> Text -> Either (MacroError a) Special
processAtom a txt =
  case Text.uncons txt of
    Just ('@',"splice") -> Right Splice
    Just ('@',"load"  ) -> Right Load
    Just ('@',t       ) -> Left (UnknownDirective a t)
    Just ('$',t       ) -> Right (Variable t)
    _                   -> Right Plain

-- | Expand macros in a configuration value.
--
-- @\@load@ not supported and results in a 'BadLoad' error.
expandMacros :: Value a -> Either (MacroError a) (Value a)
expandMacros = expandMacros' Left (\a _ -> Left (BadLoad a)) Map.empty

-- | Expand macros in a configuration value using a pre-populated environment.
expandMacros' ::
  Monad m =>
  (forall b. MacroError a -> m b) {- ^ failure                       -} ->
  (a -> Text -> m (Value a))      {- ^ @\@load@ implementation       -} ->
  Map Text (Value a)              {- ^ variable environment          -} ->
  Value a                         {- ^ value to expand               -} ->
  m (Value a)                     {- ^ expanded value                -}
expandMacros' failure load = go
  where
    proc a txt = either failure pure (processAtom a txt)

    go env v =
      case v of
        Number   a x -> pure (Number a x)
        Text     a x -> pure (Text a x)
        List     a x -> fmap (List a) (traverse (go env) x)
        Sections _ [Section a "@load" arg] ->
          do arg' <- go env arg
             case arg' of
               Text _ path -> load a path
               _           -> failure (BadLoad a)
        Sections a ss -> fmap (Sections a) (elaborateSections env x)
        Atom     a x ->
          do x' <- proc a (atomName x)
             case x' of
               Plain  -> pure (Atom a x)
               Splice -> failure (BadSplice a)
               Load   -> failure (BadLoad a)
               Variable var ->
                 case Map.lookup var env of
                   Nothing -> failure (UndeclaredVariable a var)
                   Just y  -> pure y

    elaborateSections _ [] = pure []
    elaborateSections env (Section a k v : xs) =
      do special <- proc a k
         v' <- go env v
         case special of
           Plain ->
             do xs' <- elaborateSections env xs
                pure (Section a k v' : xs')
           Variable var ->
             elaborateSections (Map.insert var v' env) xs
           Load -> failure (BadLoad a)
           Splice ->
             case v' of
               Sections _ ys -> elaborateSections env (ys ++ xs)
               _             -> failure (BadSplice a)

-- | A pair of filepath and position
data FilePosition = FilePosition FilePath Position
  deriving (Read, Show, Ord, Eq)

-- | Errors thrown by 'loadFileWithMacros'
data LoadFileError
  = LoadFileParseError FilePath ParseError -- ^ failure to parse a file
  | LoadFileMacroError (MacroError FilePosition) -- ^ failure to expand macros
  deriving (Eq, Read, Show)

instance Exception LoadFileError

-- | Load a configuration value from a given file path.
--
-- @\@load@ will compute included file path from the given function given the
-- load argument and current configuration file path.
--
-- Throws `IOError` from file loads and `LoadFileError`
loadFileWithMacros ::
  (Text -> FilePath -> FilePath) {- ^ inclusion path resolution -} ->
  FilePath                       {- ^ starting file path -} ->
  IO (Value FilePosition)        {- ^ macro-expanded config value -}
loadFileWithMacros toPath = go
  where
    go path =
      do txt <- Text.readFile path
         v1 <- case parse txt of
                 Left e -> throwIO (LoadFileParseError path e)
                 Right v -> pure v
         let v2 = fmap (FilePosition path) v1
         expandMacros' (throwIO . LoadFileMacroError) (\_ str -> go (toPath str path)) Map.empty v2
