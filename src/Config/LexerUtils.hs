{-# LANGUAGE CPP #-}
{-# LANGUAGE BangPatterns #-}
-- | This module is separate from the Lexer.x input to Alex
-- to segregate the automatically generated code from the
-- hand written code. The automatically generated code
-- causes lots of warnings which mask the interesting warnings.
module Config.LexerUtils where

import Data.Bits            ((.&.))
import Data.ByteString.Lazy (ByteString)
import Data.Char            (digitToInt, isSpace, readLitChar)
import Data.Monoid          ((<>))
import Data.Word            (Word8)
import Numeric              (readInt)
import qualified Data.ByteString.Lazy       as L
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text                  as Text
import qualified Data.Text.Encoding         as Text
import qualified Data.Text.Encoding.Error   as Text
import qualified Data.Text.Lazy             as LText
import qualified Data.Text.Lazy.Builder     as BText
import qualified Data.Text.Lazy.Encoding    as LText

#if !MIN_VERSION_base(4,8,0)
import Data.Monoid (mempty)
import Data.Functor ((<$))
#endif

import Config.Tokens

------------------------------------------------------------------------
-- Custom Alex wrapper
------------------------------------------------------------------------

type AlexInput = Located ByteString

alexStartPos :: Position
alexStartPos = Position { posLine = 1, posColumn = 1 }

alexGetByte :: AlexInput -> Maybe (Word8,AlexInput)
alexGetByte (Located p cs)
  = do (!b,!cs') <- L.uncons cs
       let !p' = alexMove p (L8.head cs)
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
  | InComment !CommentType !Position [Position]
  | InString !(Located BText.Builder)

data CommentType = CommentState | StringState

--  token starting position -> token bytes -> lexer state -> (new state, token)
type Action = Located ByteString -> LexerMode -> (LexerMode, Maybe (Located Token))

-- Helper function for building an Action given a token constructor
-- function, a position, and the matched token.
tok ::
  (ByteString -> Token) {- ^ token constructor -} ->
  Action
tok f match st = (st, Just (fmap f match))

simpleAction :: (Located ByteString -> LexerMode -> LexerMode) -> Action
simpleAction f match st = (f match st, Nothing)

startComment :: Action
startComment = simpleAction $ \match st ->
  case st of
    InNormal                -> InComment CommentState (locPosition match) []
    InComment _ posn1 posns -> InComment CommentState (locPosition match) (posn1:posns)
    _                       -> error "startComment: Lexer failure"

startCommentString :: Action
startCommentString = simpleAction $ \match st ->
  case st of
    InComment _ posn1 posns -> InComment StringState (locPosition match) (posn1:posns)
    _                       -> error "startCommentString: Lexer failure"

endCommentString :: Action
endCommentString = simpleAction $ \_ st ->
  case st of
    InComment _ _ (posn:posns) -> InComment CommentState posn posns
    InComment _ _ []           -> InNormal
    _                          -> error "endComment: Lexer failure"

-- | Enter the string literal lexer
startString :: Action
startString = simpleAction $ \match _ -> InString (mempty <$ match)

-- | Emit completed string literal, exit string literal lexer and return to
-- Normal mode.
endString :: Action
endString _ st =
  case st of
    InString builder ->
      let !t = fmap (LText.toStrict . BText.toLazyText) builder
      in (InNormal, Just (fmap String t))
    _ -> error "endString: Lexer failure"

-- | Add region of text to current string literal state. Escapes are handled
-- separately.
addString :: Action
addString match st =
  case st of
    InString builder ->
      case LText.decodeUtf8' (locThing match) of
        Left{}    -> (InString builder, Just (Error <$ match))
        Right txt -> (InString (fmap (<> BText.fromLazyText txt) builder), Nothing)
    _ -> error "addString: Lexer failure"

-- | Handle character escapes in string literal mode
addCharLit :: Action
addCharLit = simpleAction $ \match st ->
  case (st, readLitChar (L8.unpack (locThing match))) of
    (InString builder, [(c,"")]) ->
         InString (fmap (<> BText.singleton c) builder)
    _ -> error "addCharLit: Lexer failure"

-- | Construct a 'Number' token from a token using a
-- given base. This function expect the token to be
-- legal for the given base. This is checked by Alex.
number ::
  Int        {- ^ prefix length      -} ->
  Int        {- ^ base               -} ->
  ByteString {- ^ sign-prefix-digits -} ->
  Token
number prefixLen base str =
  case readInt (fromIntegral base) (const True) digitToInt str2 of
    [(n,"")] -> Number base (s*n)
    _        -> Error
  where
  str2     = drop prefixLen str1
  (s,str1) = case L8.unpack str of
               '-':rest -> (-1, rest)
               rest     -> ( 1, rest)

-- | Process a section heading token
section ::
  ByteString {- ^ UTF-8 encoded text -} ->
  Token
section
  = Section
  . Text.dropWhileEnd isSpace
  . Text.init
  . Text.decodeUtf8With Text.lenientDecode
  . L8.toStrict

-- | Take the request number of codepoints from a UTF-8 encoded
-- ByteString. UTF-8 is a variable length encoding, so some
-- codepoints will be longer than others.
utf8Take ::
  Int        {- ^ Desired prefix length -} ->
  ByteString {- ^ UTF-8 encoded text    -} ->
  ByteString {- ^ UTF-8 encoded text    -}
utf8Take len bs =
  case drop len (L.findIndices isStartByte bs) of
    []    -> bs
    pos:_ -> L.take pos bs

-- | Return True when applied to the first byte of the UTF-8
-- encoding of a Unicode codepoint.
isStartByte :: Word8 -> Bool
isStartByte x = x .&. 0xc0 /= 0x80
