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
import qualified Data.Text      as Text
import qualified Data.Text.Lazy as LText

import Config.Tokens

------------------------------------------------------------------------
-- Custom Alex wrapper - these functions are used by generated code
------------------------------------------------------------------------

-- | The generated code expects the lexer input type to be named 'AlexInput'
type AlexInput = Located Text

-- | Get the next characteristic byte from the input source.
alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (Located p cs)
  = do (c,cs') <- Text.uncons cs
       let !b = byteForChar c
           !inp = Located (move p c) cs'
       return (b, inp)

------------------------------------------------------------------------

-- | The initial 'Position' for the start of a file
startPos :: Position
startPos = Position { posIndex = 0, posLine = 1, posColumn = 1 }

-- | Advance the position according to the kind of character lexed.
move :: Position -> Char -> Position
move (Position ix line column) c =
  case c of
    '\t' -> Position (ix + 1) line (((column + 7) `div` 8) * 8 + 1)
    '\n' -> Position (ix + 1) (line + 1) 1
    _    -> Position (ix + 1) line (column + 1)

-- | Action to perform upon end of file. Produce errors if EOF was unexpected.
eofAction :: Position -> LexerMode -> [Located Token]
eofAction eofPosn st =
  case st of
    _ | posColumn eofPosn /= 1 -> [Located eofPosn (Error UntermFile)]
    InComment       posn _     -> [Located posn    (Error UntermComment)]
    InCommentString posn _     -> [Located posn    (Error UntermCommentString)]
    InString        posn _     -> [Located posn    (Error UntermString)]
    InNormal                   -> [Located eofPosn{posColumn=0} EOF]

-- | Action to perform when lexer gets stuck. Emits an error.
errorAction :: AlexInput -> [Located Token]
errorAction inp = [fmap (Error . NoMatch . Text.head) inp]

------------------------------------------------------------------------
-- Lexer Modes
------------------------------------------------------------------------

-- | The lexer can be in any of four modes which determine which rules
-- are active.
data LexerMode
  = InNormal
  | InComment       !Position !LexerMode -- ^ Start of comment and return mode
  | InCommentString !Position !LexerMode -- ^ Start of string and return mode
  | InString        !Position !Text      -- ^ Start of string and input text

-- | Type of actions used by lexer upon matching a rule
type Action =
  Int                          {- ^ match length                       -} ->
  Located Text                 {- ^ current input                      -} ->
  LexerMode                    {- ^ lexer mode                         -} ->
  (LexerMode, [Located Token]) {- ^ updated lexer mode, emitted tokens -}

-- | Helper function for building an 'Action' using the lexeme
token :: (Text -> Token) -> Action
token f len match st = (st, [fmap (f . Text.take len) match])

-- | Helper function for building an 'Action' where the lexeme is unused.
token_ :: Token -> Action
token_ = token . const

------------------------------------------------------------------------
-- Alternative modes
------------------------------------------------------------------------

-- | Used to enter one of the nested modes
nestMode :: (Position -> LexerMode -> LexerMode) -> Action
nestMode f _ match st = (f (locPosition match) st, [])

-- | Enter the string literal lexer
startString :: Action
startString _ (Located posn text) _ = (InString posn text, [])

-- | Successfully terminate the current mode and emit tokens as needed
endMode :: Action
endMode len (Located endPosn match) mode =
  case mode of
    InNormal                 -> (InNormal, [])
    InCommentString _ st     -> (st, [])
    InComment       _ st     -> (st, [])
    InString startPosn input ->
      let n = posIndex endPosn - posIndex startPosn + len
          t = Text.pack (read (Text.unpack (Text.take n input)))
      in (InNormal, [Located startPosn (String t)])

-- | Action for unterminated string constant
untermString :: Action
untermString _ _ = \(InString posn _) ->
  (InNormal, [Located posn (Error UntermString)])

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

-- | Alex is driven by looking up elements in a 128 element array.
-- This function maps each ASCII character to its ASCII encoding
-- and it maps non-ASCII code-points to a character class (0-6)
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
