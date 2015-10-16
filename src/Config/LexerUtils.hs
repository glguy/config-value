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
import Data.Word            (Word8)
import Numeric              (readInt)
import qualified Data.Text                  as Text
import qualified Data.Text.Lazy             as LText

#if !MIN_VERSION_base(4,8,0)
import Data.Functor ((<$))
import Data.Monoid (mempty)
#endif

import Config.Tokens

------------------------------------------------------------------------
-- Custom Alex wrapper
------------------------------------------------------------------------

type AlexInput = Located Text

alexStartPos :: Position
alexStartPos = Position { posIndex = 0, posLine = 1, posColumn = 1 }

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (Located p cs)
  = do (c,!cs') <- Text.uncons cs
       let !p' = alexMove p c
           !b = byteForChar c
       return (b, Located p' cs')

alexMove :: Position -> Char -> Position
alexMove (Position ix line column) c =
  case c of
    '\t' -> Position (ix + 1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> Position (ix + 1) (line + 1) 1
    _    -> Position (ix + 1) line (column + 1)

------------------------------------------------------------------------
-- Lexer Modes
------------------------------------------------------------------------

data LexerMode
  = InNormal
  | InComment       !Position !LexerMode -- ^ Start of comment and return mode
  | InCommentString !Position !LexerMode -- ^ Start of string and return mode
  | InString        !Position !Text      -- ^ Start of string and input text

-- match length -> token starting position -> token bytes -> lexer state ->
-- (new state, token)
type Action = Int -> Located Text -> LexerMode ->
              (LexerMode, Maybe (Located Token))

-- Helper function for building an Action given a token constructor
-- function, a position, and the matched token.
token :: (Text -> Token) -> Action
token f len match st = (st, Just (fmap (f . Text.take len) match))

token_ :: Token -> Action
token_ = token . const

modeChange :: (Position -> LexerMode -> LexerMode) -> Action
modeChange f _ match st = (f (locPosition match) st, Nothing)

------------------------------------------------------------------------
-- Comment state
------------------------------------------------------------------------

startComment :: Action
startComment = modeChange InComment

endComment :: Action
endComment = modeChange (\_ (InComment _ st) -> st)

------------------------------------------------------------------------
-- Comment string state
------------------------------------------------------------------------

startCommentString :: Action
startCommentString = modeChange InCommentString

endCommentString :: Action
endCommentString = modeChange (\_ (InCommentString _ st) -> st)

------------------------------------------------------------------------
-- String state
------------------------------------------------------------------------

-- | Enter the string literal lexer
startString :: Action
startString _ (Located posn text) _ = (InString posn text, Nothing)

-- | Emit completed string literal, exit string literal lexer and return to
-- Normal mode.
endString :: Action
endString len (Located endPosn match) = \(InString startPosn input) ->
   let n = posIndex endPosn - posIndex startPosn + len
       t = Text.pack (read (Text.unpack (Text.take n input)))
   in (InNormal, Just (Located startPosn (String t)))

-- | Action for unterminated string constant
untermString :: Action
untermString _ _ = \(InString posn _) ->
  (InNormal, Just (Located posn (Error UntermString)))

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
    _        -> error "number: Lexer failure"
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
