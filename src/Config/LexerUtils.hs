{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
-- | This module is separate from the Lexer.x input to Alex
-- to segregate the automatically generated code from the
-- hand written code. The automatically generated code
-- causes lots of warnings which mask the interesting warnings.
module Config.LexerUtils where

import Data.Char            (GeneralCategory(..), generalCategory, digitToInt,
                             isAscii, isSpace, readLitChar, ord)
import Data.Monoid          ((<>))
import Data.Text            (Text)
import Data.Text.Lazy.Builder (Builder)
import Data.Word            (Word8)
import Numeric              (readInt)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as Builder

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
import Data.Functor ((<$))
#endif

import Config.Tokens

------------------------------------------------------------------------
-- Custom Alex wrapper
------------------------------------------------------------------------

type AlexInput = Located Text

alexStartPos :: Position
alexStartPos = Position { posLine = 1, posColumn = 1 }

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (Located p cs)
  = do (c,!cs') <- Text.uncons cs
       let !p' = alexMove p c
           !b = byteForChar c
       return (b, Located p' cs')

alexMove :: Position -> Char -> Position
alexMove (Position line column) c =
  case c of
    '\t' -> Position line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> Position (line + 1) 1
    _    -> Position line (column + 1)

------------------------------------------------------------------------
-- Lexer Modes
------------------------------------------------------------------------

data LexerMode
  = InNormal
  | InComment       !Position !LexerMode -- ^ Start of comment and return mode
  | InCommentString !Position !LexerMode -- ^ Start of string and return mode
  | InString        !Position !Builder   -- ^ Start of string and accumulated text

--  token starting position -> token bytes -> lexer state -> (new state, token)
type Action = Located Text -> LexerMode -> (LexerMode, Maybe (Located Token))

-- Helper function for building an Action given a token constructor
-- function, a position, and the matched token.
token :: (Text -> Token) -> Action
token f match st = (st, Just (fmap f match))

modeChange :: (Located Text -> LexerMode -> LexerMode) -> Action
modeChange f match st = (f match st, Nothing)

------------------------------------------------------------------------
-- Comment state
------------------------------------------------------------------------

startComment :: Action
startComment = modeChange (InComment . locPosition)

endComment :: Action
endComment = modeChange $ \_ (InComment _ st) -> st

------------------------------------------------------------------------
-- Comment string state
------------------------------------------------------------------------

startCommentString :: Action
startCommentString = modeChange (InCommentString . locPosition)

endCommentString :: Action
endCommentString = modeChange $ \_ (InCommentString _ st) -> st

------------------------------------------------------------------------
-- String state
------------------------------------------------------------------------

-- | Enter the string literal lexer
startString :: Action
startString = modeChange $ \match _ -> InString (locPosition match) mempty

-- | Emit completed string literal, exit string literal lexer and return to
-- Normal mode.
endString :: Action
endString _ = \(InString posn builder) ->
   let !t = LText.toStrict (Builder.toLazyText builder)
   in (InNormal, Just (Located posn (String t)))

-- | Add region of text to current string literal state. Escapes are handled
-- separately.
addString :: Action
addString = modeChange $ \match (InString posn builder) ->
  InString posn (builder <> Builder.fromText (locThing match))

-- | Handle character escapes in string literal mode
addCharLit :: Action
addCharLit = modeChange $ \match (InString posn builder) ->
  case readLitChar (Text.unpack (locThing match)) of
    [(c,"")] -> InString posn (builder <> Builder.singleton c)
    _        -> error "addCharLit: Lexer failure"

------------------------------------------------------------------------
-- Token builders
------------------------------------------------------------------------

-- | Construct a 'Number' token from a token using a
-- given base. This function expect the token to be
-- legal for the given base. This is checked by Alex.
number ::
  Int  {- ^ prefix length      -} ->
  Int  {- ^ base               -} ->
  Text {- ^ sign-prefix-digits -} ->
  Token
number prefixLen base str =
  case readInt (fromIntegral base) (const True) digitToInt str2 of
    [(n,"")] -> Number base (s*n)
    _        -> Error
  where
  str2     = drop prefixLen str1
  (s,str1) = case Text.unpack str of
               '-':rest -> (-1, rest)
               rest     -> ( 1, rest)

-- | Process a section heading token
section :: Text -> Token
section = Section . Text.dropWhileEnd isSpace . Text.init

------------------------------------------------------------------------
-- Embed all of unicode, kind of, in a single byte!
------------------------------------------------------------------------

byteForChar :: Char -> Word8
byteForChar c
  | c <= '\6' = non_graphic
  | isAscii c = fromIntegral (ord c)
  | otherwise = case generalCategory c of
                  LowercaseLetter       -> lower
                  OtherLetter           -> lower
                  UppercaseLetter       -> upper
                  TitlecaseLetter       -> upper
                  DecimalNumber         -> digit
                  OtherNumber           -> digit
                  ConnectorPunctuation  -> symbol
                  DashPunctuation       -> symbol
                  OtherPunctuation      -> symbol
                  MathSymbol            -> symbol
                  CurrencySymbol        -> symbol
                  ModifierSymbol        -> symbol
                  OtherSymbol           -> symbol
                  Space                 -> space
                  ModifierLetter        -> other
                  NonSpacingMark        -> other
                  SpacingCombiningMark  -> other
                  EnclosingMark         -> other
                  LetterNumber          -> other
                  OpenPunctuation       -> other
                  ClosePunctuation      -> other
                  InitialQuote          -> other
                  FinalQuote            -> other
                  _                     -> non_graphic
  where
  non_graphic     = 0
  upper           = 1
  lower           = 2
  digit           = 3
  symbol          = 4
  space           = 5
  other           = 6
