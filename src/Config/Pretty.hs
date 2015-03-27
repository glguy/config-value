module Config.Pretty where

import           Data.Char (isPrint, isDigit,intToDigit)
import           Data.List (mapAccumL)
import qualified Data.Text as Text
import           Text.PrettyPrint
import           Numeric(showIntAtBase)

import Config.Value

-- | Pretty-print a 'Value' as shown in the example.
pretty :: Value -> Doc
pretty value =
  case value of
    Sections [] -> text "{}"
    Sections xs -> vcat (map prettySection xs)
    Number b n  -> prettyNum b n
    Text t      -> prettyText (Text.unpack t)
    Bool b      -> if b then text "yes" else text "no"
    List []     -> text "[]"
    List xs     -> vcat [ char '*' <+> pretty x | x <- xs ]


prettyNum :: Int -> Integer -> Doc
prettyNum b n
  | b == 16   = pref <> text "0x" <> num
  | b ==  8   = pref <> text "0o" <> num
  | b ==  2   = pref <> text "0b" <> num
  | otherwise = integer n
  where
  pref = if n < 0 then char '-' else empty
  num  = text (showIntAtBase (fromIntegral b) intToDigit (abs n) "")



prettyText :: String -> Doc
prettyText = doubleQuotes . cat . snd . mapAccumL ppChar True

  where ppChar s x
          | isDigit x = (True, if not s then text "\\&" <> char x else char x)
          | isPrint x = (True, char x)
          | otherwise = (False, char '\\' <> int (fromEnum x))


prettySection :: Section -> Doc
prettySection s
  | isBig val = lab $$ nest 2 (pretty val)
  | otherwise = lab <+> pretty val

  where
  lab = text (Text.unpack (sectionName s)) <> colon
  val = sectionValue s


isBig :: Value -> Bool
isBig (Sections (_:_))  = True
isBig (List (_:_))      = True
isBig _                 = False



