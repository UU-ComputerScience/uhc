module Char (
    isAscii, isLatin1, isControl, isPrint, isSpace, isUpper, isLower, 
    isAlpha, isDigit, isOctDigit, isHexDigit, isAlphaNum, 
    digitToInt, intToDigit,
    toUpper, toLower,
    ord, chr,
    readLitChar, showLitChar, lexLitChar,

-- ...and what the Prelude exports
    Char, String
  ) where

import Data.Char
