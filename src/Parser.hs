module Parser where

import Control.Applicative
import Control.Monad
import Data.Char
import Text.Parsec hiding ((<|>),many)
import Text.Parsec.Pos
import Text.Parsec.Error
import Lexer
import ConfigFile
import Data.Map (Map)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text

type P = Parsec [PosToken] ParserState

data ParserState = ParserState
  { indentColumn :: !Column
  }

layout :: P a -> P a
layout p =
  do hold <- indentColumn <$> getState
     here <- sourceColumn <$> getPosition
     modifyState (\s -> s { indentColumn = here })
     x <- p
     modifyState (\s -> s { indentColumn = hold })
     return x

indentCheck ::
  (Column -> Column -> Bool) {- ^ (marked -> current -> Bool) -} ->
  P ()
indentCheck (?) =
  do mark <- indentColumn <$> getState
     here <- sourceColumn <$> getPosition
     guard (here ? mark) <?> show (mark,here)

align :: P ()
align = indentCheck (==) <?> "Block (same indentation)"

indented :: P ()
indented = indentCheck (>) -- <?> "Block (indented)"

posTokenToSourcePos :: PosToken -> SourcePos
posTokenToSourcePos (PosToken line column _) = newPos "" line column

mytoken :: (Token -> Maybe a) -> P a
mytoken f =
  token
    (\(PosToken _ _ t) -> prettyToken t)
    posTokenToSourcePos
    (\(PosToken _ _ t) -> f t)

prettyToken :: Token -> String
prettyToken t = case t of
  String{} -> "string"
  Atom{}   -> "atom"
  Colon{}  -> "colon"
  Dash{}   -> "dash"
  Number{} -> "number"

numberToken :: P Integer
numberToken = mytoken $ \t ->
  case t of
    Number n ->
      case L8.readInteger n of
        Just (i, rest) | L8.null rest -> Just i
        Nothing      -> error "numberToken: fatal lexer error"
    _ -> Nothing

stringToken :: P String
stringToken = mytoken $ \t ->
  case t of
    String n -> Just (read (Text.unpack (Text.decodeUtf8 n))) -- TODO: make efficient
    _        -> Nothing

colonToken :: P ()
colonToken = mytoken $ \t ->
  case t of
    Colon -> Just ()
    _     -> Nothing

dashToken :: P ()
dashToken = mytoken $ \t ->
  case t of
    Dash -> Just ()
    _    -> Nothing

boolToken :: P Bool
boolToken = mytoken $ \t ->
  case t of
    Atom x | L8.map toLower x == L8.pack "yes" -> Just True
           | L8.map toLower x == L8.pack "no"  -> Just False
    _                                  -> Nothing

atomToken :: P String
atomToken = mytoken $ \t ->
  case t of
    Atom x -> Just (Text.unpack (Text.decodeUtf8 x))
    _      -> Nothing

emptyListToken :: P ()
emptyListToken = mytoken $ \t ->
  case t of
    EmptyList -> Just ()
    _         -> Nothing

emptyMapToken :: P ()
emptyMapToken = mytoken $ \t ->
  case t of
    EmptyMap -> Just ()
    _        -> Nothing

parseSection :: P ConfigSection
parseSection = fmap ConfigSection
             ( Map.empty <$ emptyMapToken
           <|> layout (Map.fromList <$> many1 (align *> parseSectionEntry)) )
           <?> "config section"

parseSectionEntry :: P (String, ConfigValue)
parseSectionEntry =
  do k <- try (atomToken <* colonToken)
     indented
     v <- parseValue
     return (k,v)

parseList :: P [ConfigValue]
parseList = [] <$ emptyListToken
        <|> layout (many1 (align *> parseListEntry))

parseListEntry :: P ConfigValue
parseListEntry =
  do _ <- dashToken
     indented
     parseValue

parseValue :: P ConfigValue
parseValue
    = fmap Subsection   parseSection
  <|> fmap ConfigBool   boolToken
  <|> fmap ConfigNumber numberToken
  <|> fmap ConfigString stringToken
  <|> fmap ConfigList   parseList
  <?> "value"

parseConfigFile :: SourceName -> ByteString -> Either ParseError [ConfigSection]
parseConfigFile name bytes =
  case scanTokens bytes of
    Right toks -> runParser (many1 parseSection <* eof) (ParserState 1) name toks
    Left (line,column) ->
      Left (newErrorMessage
              (Message "Lexical error")
              (newPos name line column))

main :: IO ()
main =
  do bs <- L8.readFile "demo.txt"
     print (parseConfigFile "demo.txt" bs)
