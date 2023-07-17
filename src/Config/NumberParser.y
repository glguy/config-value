{
{-# LANGUAGE Trustworthy #-}

module Config.NumberParser where

import Data.List (foldl')
import Config.Number

}

%tokentype
    { Char}
%token
'+' { '+' }
'-' { '-' }
'.' { '.' }
'0' { '0' }
'1' { '1' }
'2' { '2' }
'3' { '3' }
'4' { '4' }
'5' { '5' }
'6' { '6' }
'7' { '7' }
'8' { '8' }
'9' { '9' }
'A' { 'A' }
'B' { 'B' }
'C' { 'C' }
'D' { 'D' }
'E' { 'E' }
'F' { 'F' }
'O' { 'O' }
'P' { 'P' }
'X' { 'X' }

%name number

%%

number ::                               { Number        }
  : '-' unsigned_number                 { negNum $2     }
  | '+' unsigned_number                 {        $2     }
  |     unsigned_number                 {        $1     }

unsigned_number
  : '0' 'X' hexadecimal fracpart(hexadecimal) exppart('P')
                                        { mkNum (Radix16 $5) $3 $4 }
  |         decimal     fracpart(decimal    ) exppart('E')
                                        { mkNum (Radix10 $3) $1 $2 }
  | '0' 'O' octal       fracpart(octal      )
                                        { mkNum Radix8 $3 $4 }
  | '0' 'B' binary      fracpart(binary     )
                                        { mkNum Radix2 $3 $4 }

fracpart(p) ::                          { [Int]         }
  :                                     { []            }
  | '.'                                 { []            }
  | '.' p                               { $2            }

exppart(p) ::                           { Integer       }
  :                                     { 0             }
  | p expnum                            { $2            }


expnum ::                               { Integer       }
  : '+' decimal                         {   toInt 10 $2 }
  | '-' decimal                         { - toInt 10 $2 }
  |     decimal                         {   toInt 10 $1 }

hexadecimal ::                          { [Int]         }
  :             hexdigit                { [$1]          }
  | hexadecimal hexdigit                { $2 : $1       }

decimal ::                              { [Int]         }
  :         decdigit                    { [$1]          }
  | decimal decdigit                    { $2 : $1       }

octal ::                                { [Int]         }
  :       octdigit                      { [$1]          }
  | octal octdigit                      { $2 : $1       }

binary ::                               { [Int]         }
  :        bindigit                     { [$1]          }
  | binary bindigit                     { $2 : $1       }

hexdigit :: { Int }
  : decdigit { $1 }
  | 'A' {10} | 'B' {11} | 'C' {12}
  | 'D' {13} | 'E' {14} | 'F' {15}

decdigit :: { Int }
  : octdigit {$1}
  | '8' { 8} | '9' { 9}

octdigit :: { Int }
  : bindigit { $1 }
  | '2' { 2} | '3' { 3} | '4' { 4}
  | '5' { 5} | '6' { 6} | '7' { 7}

bindigit :: { Int }
  : '0' { 0} | '1' { 1}

{

mkNum :: Radix -> [Int] -> [Int] -> Number
mkNum radix coef frac =
  MkNumber radix (fromInteger (toInt base coef) + toFrac base frac)
  where
    base = radixToInt radix

negNum :: Number -> Number
negNum n = n { numberCoefficient = - numberCoefficient n }

toInt :: Int -> [Int] -> Integer
toInt base = foldl' (\acc i -> acc*base' + fromIntegral i) 0 . reverse
  where base' = fromIntegral base

toFrac :: Int -> [Int] -> Rational
toFrac base = foldl' (\acc i -> (fromIntegral i+acc)/base') 0
  where base' = fromIntegral base

happyError [] = error "Unexpected EOF"
happyError (c:_) = error ("Unexpected: "++[c])
}
