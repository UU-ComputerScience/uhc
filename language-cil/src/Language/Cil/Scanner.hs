-- |
-- Language.Cil Scanner to List of Tokens
-- Uses the Parser combinators from the Haskell Utrecht Tools.
--

module Language.Cil.Scanner (
    scan
  ) where

import UU.Scanner.Position (Pos (..), initPos)
import UU.Scanner.Token (Token)
import UU.Scanner.TokenParser
import qualified UU.Scanner as S (scan)

scan :: FilePath -> String -> [Token]
scan path =
  S.scan
    [ "assembly", "extern"
    , "class", "public"
    , "field", "static"
    , "void", "bool", "char", "int8", "int16", "int32", "int64", "uint8"
    , "uint16", "uint32", "uint64", "unsigned", "float32", "float64", "string"
    , "object"
    , "method", "hidebysig", "static", "cil", "managed"
    , "add", "and"
    ]
    [ ""
    ]
    ".{}()/"
    ""
    (initPos path)

