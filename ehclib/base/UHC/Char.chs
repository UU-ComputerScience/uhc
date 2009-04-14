%%[99
module UHC.Char
  ( intToDigit
  )
  where

import UHC.Base
%%]

%%[99
intToDigit :: Int -> Char
intToDigit i
 | i >= 0 && i <= 9 = toEnum (fromEnum '0' + i)
 | i >= 10 && i <= 15 = toEnum (fromEnum 'a' + i - 10)
 | otherwise = error "Char.intToDigit: not a digit"
%%]
