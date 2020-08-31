-- | Pretty-printing implementation for 'Value'
module Config.Pretty (pretty) where

import           Data.Char (isPrint, isDigit,intToDigit)
import           Data.List (mapAccumL)
import           Data.Ratio (denominator)
import qualified Data.Text as Text
import           Text.PrettyPrint
import           Numeric(showIntAtBase)
import           Prelude hiding ((<>))

import           Config.Value
import           Config.Number

-- | Pretty-print a 'Value' as shown in the example.
-- Sections will nest complex values underneath with
-- indentation and simple values will be rendered on
-- the same line as their section.
pretty :: Value a -> Doc
pretty value =
  case value of
    Sections _ [] -> text "{}"
    Sections _ xs -> prettySections xs
    Number _ n    -> prettyNumber n
    Text _ t      -> prettyText (Text.unpack t)
    Atom _ t      -> text (Text.unpack (atomName t))
    List _ []     -> text "[]"
    List _ xs     -> vcat [ char '*' <+> pretty x | x <- xs ]

prettyNumber :: Number -> Doc
prettyNumber (MkNumber r c) =
  case r of
    Radix16 e -> pref <> text "0x" <> num <> expPart 'p' e
    Radix10 e -> pref <>              num <> expPart 'e' e
    Radix8    -> pref <> text "0o" <> num
    Radix2    -> pref <> text "0b" <> num
  where
    radix = radixToInt r
    pref = if c < 0 then char '-' else empty
    num  = text (showIntAtBase (fromIntegral radix) intToDigit whole "")
           <> fracPart
    (whole,frac) = properFraction (abs c) :: (Integer, Rational)
    expPart _ 0 = text ""
    expPart p i = text (p : show i)
    fracPart
      | 0 == frac = text ""
      | otherwise = text ('.' : showFrac radix frac)

showFrac :: Int -> Rational -> String
showFrac _ 0 = ""
showFrac radix x = intToDigit w : rest
  where
    (w,f) = properFraction (x * fromIntegral radix)
    rest
      | denominator f < denominator x = showFrac radix f
      | otherwise = ""

prettyText :: String -> Doc
prettyText = doubleQuotes . cat . snd . mapAccumL ppChar True

  where ppChar s x
          | isDigit x = (True, if not s then text "\\&" <> char x else char x)
          | isPrint x = (True, char x)
          | otherwise = (False, char '\\' <> int (fromEnum x))


prettySections :: [Section a] -> Doc
prettySections ss = prettySmallSections small $$ rest
  where
  (small,big) = break (isBig . sectionValue) ss
  rest        = case big of
                  []     -> empty
                  b : bs -> prettyBigSection b $$ prettySections bs

prettyBigSection :: Section a -> Doc
prettyBigSection s =
  text (Text.unpack (sectionName s)) <> colon
  $$ nest 2 (pretty (sectionValue s))

prettySmallSections :: [Section a] -> Doc
prettySmallSections ss = vcat (map pp annotated)
  where
  annotate s = (Text.length (sectionName s), s)
  annotated  = map annotate ss
  indent     = 1 + maximum (0 : map fst annotated)
  pp (l,s)   = prettySmallSection (indent - l) s

prettySmallSection :: Int -> Section a -> Doc
prettySmallSection n s =
  text (Text.unpack (sectionName s)) <> colon <>
    text (replicate n ' ') <> pretty (sectionValue s)

isBig :: Value a -> Bool
isBig (Sections _ (_:_)) = True
isBig (List _ (_:_))     = True
isBig _                  = False
