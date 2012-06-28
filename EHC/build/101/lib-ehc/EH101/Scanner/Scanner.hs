module EH101.Scanner.Scanner
( module EH101.Scanner.Machine, module EH101.Scanner.Token, module EH101.Scanner.TokenParser, module UU.Scanner.Position )
where
import Data.List
import EH101.Scanner.Machine
import EH101.Scanner.Token
import EH101.Scanner.TokenParser
import UU.Scanner.Position
import UU.Scanner.GenTokenOrd ()
import UU.Scanner.GenTokenSymbol ()

{-# LINE 32 "src/ehc/Scanner/Scanner.chs" #-}
instance Show Token where
  showsPrec _ token
    = showString
       (case token of
         Reserved key      pos -> "symbol "      ++ key ++ maybeshow pos
         ValToken tp val   pos -> show tp ++ " " ++ concat (intersperse "." val) ++ maybeshow pos
       )

instance Show EnumValToken where
 show tp = case tp of
  TkVarid            -> "lower case identifier"
  TkConid            -> "upper case identifier"
  TkOp               -> "operator"
  TkConOp            -> "con operator"
  TkOpUnboxed        -> "operator (unboxed)"
  TkVaridUnboxed     -> "lower case identifier (unboxed)"
  TkConidUnboxed     -> "upper case identifier (unboxed)"
  TkConOpUnboxed     -> "con operator (unboxed)"
  TkQOp              -> "qualified operator"
  TkQVarid           -> "lower case qualified identifier"
  TkQConid           -> "upper case qualified identifier"
  TkQConOp           -> "qualified con operator"
  TkString           -> "string"
  TkChar             -> "character"
  TkInteger8         -> "octal integer"
  TkInteger10        -> "decimal Integer"
  TkInteger16        -> "hexadecimal integer"
  TkFraction         -> "fraction (float,...)"
  TkTextnm           -> "text name"
  TkTextln           -> "text lines"
  TkError            -> "error in scanner:"

maybeshow :: Pos -> String
maybeshow (Pos l c fn) | l <= 0 || c <= 0 =  ""
                       | otherwise        =  " at line " ++ show l
                                          ++ ", column " ++ show c
                                          ++ " of file " ++ show fn
