{-# LANGUAGE Safe, OverloadedStrings, DeriveTraversable, RankNTypes #-}
{-|
Module      : Config.Macro
Description : Configuration pre-processor adding support for aliases and common sections
Copyright   : (c) Eric Mertens, 2020
License     : ISC
Maintainer  : emertens@gmail.com

This module provides assigns meaning to atoms and section names that start with @\@@
and @$@. It provides processing pass for configuration to use local variables and
inclusion to better structure configuration.

= Sigils

* @$@ starts a variable.
* @\@@ starts a directive.

Merge key-value mappings using @\@splice@.

Load external configuration with @\@load@.

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

Later variable definitions will shadow earlier definitions.

@
{ $x: 1, $x: 2, k: $x }
@

expands to

@
{ k: 2 }
@

Scoping examples:

@
top1:
  a:  $x                     -- BAD: $x not defined yet
  $x: 42                     -- $x is now defined to be 42
  b:  $x                     -- OK: $x was defined above
  c:  {sub1: $x, sub2: [$x]} -- OK: $x in scope in subsections
                             -- note: $x now goes out of scope
top2: $x                     -- BAD: $x no longer in scope
@

Macros are expanded at there definition site. All variables are resolved before
adding the new variable into the environment. Variables are lexically scoped
rather than dynamically scoped.

Allowed:

@
$x: 1
$y: $x -- OK, y is now 1
@

Not allowed:

@
$y: $x -- BAD: $x was not in scope
$x: 1
z:  $y
@

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

= File loading

The @\@load@ directive is intended including configuration from other sources.
'loadFileWithMacros' provides an interpretation of this directive that loads
other files. An arbitrary interpretation can be defined with 'expandMacros''

To load a value define a key-value mapping with a single @\@load@ key with a
value specifying the location to load from.

@
x: @load: "fourty-two.cfg"
@

could expand to

@
x: 42
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

-- | Errors from macro expansion annotated with the 'valueAnn' from
-- the 'Value' nearest to the problem (typically a file position).
data MacroError a
  = UndeclaredVariable a Text -- ^ Variable used before its defintion
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
expandMacros = expandMacros' Left (Left . BadLoad . valueAnn) Map.empty

-- | Expand macros in a configuration value using a pre-populated environment.
expandMacros' ::
  Monad m =>
  (forall b. MacroError a -> m b) {- ^ failure                       -} ->
  (Value a -> m (Value a))        {- ^ @\@load@ implementation       -} ->
  Map Text (Value a)              {- ^ variable environment          -} ->
  Value a                         {- ^ value to expand               -} ->
  m (Value a)                     {- ^ expanded value                -}
expandMacros' failure load = go
  where
    proc a txt = either failure pure (processAtom a txt)

    go env v =
      case v of
        Number a x -> pure (Number a x)
        Text a x -> pure (Text a x)
        List a x -> List a <$> traverse (go env) x

        Sections _ [Section _ "@load" arg] -> load =<< go env arg
        Sections a x -> Sections a <$> elaborateSections env x

        Atom a x ->
          do x' <- proc a (atomName x)
             case x' of
               Plain -> pure (Atom a x)
               Splice -> failure (BadSplice a)
               Load -> failure (BadLoad a)
               Variable var ->
                 case Map.lookup var env of
                   Nothing -> failure (UndeclaredVariable a var)
                   Just y -> pure y

    elaborateSections _ [] = pure []
    elaborateSections env (Section a k v : xs) =
      do special <- proc a k
         v' <- go env v
         case special of
           Load -> failure (BadLoad a)
           Variable var -> elaborateSections (Map.insert var v' env) xs
           Plain -> (Section a k v' :) <$> elaborateSections env xs
           Splice ->
             case v' of
               Sections _ ys -> (ys++) <$> elaborateSections env xs
               _ -> failure (BadSplice a)

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
-- Valid @\@load@ arguments are string literals use as arguments to
-- the path resolution function.
--
-- Throws `IOError` from file loads and `LoadFileError`
loadFileWithMacros ::
  (Text -> FilePath -> IO FilePath) {- ^ inclusion path resolution -} ->
  FilePath                          {- ^ starting file path -} ->
  IO (Value FilePosition)           {- ^ macro-expanded config value -}
loadFileWithMacros findPath = go
  where
    go path =
      do txt <- Text.readFile path
         v1 <- case parse txt of
                 Left e -> throwIO (LoadFileParseError path e)
                 Right v -> pure v
         let v2 = FilePosition path <$> v1
         let loadImpl pathVal =
               case pathVal of
                 Text _ str -> go =<< findPath str path
                 _ -> throwIO (LoadFileMacroError (BadLoad (valueAnn pathVal)))
         expandMacros' (throwIO . LoadFileMacroError) loadImpl Map.empty v2
