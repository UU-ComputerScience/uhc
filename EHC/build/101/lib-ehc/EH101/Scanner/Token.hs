module EH101.Scanner.Token
( module UU.Scanner.GenToken
, reserved, valueToken, errToken, tokTpIsInt
, Token, ValTokenVal
, EnumValToken (..)
, tokenVals, tokenVal, tokenMap
, tokTpIsId
, tokTpUnboxed
, tokTpQual )
where
import UU.Scanner.Position
import UU.Scanner.GenToken
import qualified Data.Set as Set



{-# LINE 27 "src/ehc/Scanner/Token.chs" #-}
-- | The value of a Token is a [String], of length > 1 only when it encodes qualified identifiers
type ValTokenVal = [String]
type Token = GenToken String EnumValToken ValTokenVal

{-# LINE 33 "src/ehc/Scanner/Token.chs" #-}
-- | The kind of tokens
data EnumValToken
  = TkVarid
  | TkConid
  | TkOp
  | TkConOp
  | TkString
  | TkChar
  | TkInteger8
  | TkInteger10
  | TkInteger16
  | TkFraction
  | TkTextnm
  | TkTextln
  | TkVaridUnboxed
  | TkConidUnboxed
  | TkOpUnboxed
  | TkConOpUnboxed
  | TkQVarid			-- qualified variants
  | TkQConid
  | TkQOp
  | TkQConOp
  | TkError
  deriving (Eq, Ord)

{-# LINE 64 "src/ehc/Scanner/Token.chs" #-}
-- | Extract the parts of a qualified identifier
tokenVals :: Token -> [String]
tokenVals (ValToken _ v _) = v
tokenVals (Reserved   v _) = [v]

-- | Extract the value as a flattened string
tokenVal :: Token -> String
tokenVal = concat . tokenVals

tokenMap :: (String->String) -> Token -> Token
tokenMap f (ValToken t v p) = ValToken t (map f v) p
tokenMap f (Reserved   k p) = Reserved   (f k) p

{-# LINE 79 "src/ehc/Scanner/Token.chs" #-}
reserved                :: String -> Pos -> Token
reserved                =  Reserved

valueToken              :: EnumValToken -> String -> Pos -> Token
valueToken t s p        =  ValToken t [s] p

errToken                :: String -> Pos -> Token
errToken                =  valueToken TkError

{-# LINE 90 "src/ehc/Scanner/Token.chs" #-}
tokTpIsInt :: EnumValToken -> Bool
tokTpIsInt tp = tp == TkInteger8 || tp == TkInteger10 || tp == TkInteger16

{-# LINE 95 "src/ehc/Scanner/Token.chs" #-}
tokTpIsId :: EnumValToken -> Bool
tokTpIsId
  = (`Set.member` ts)
  where ts = Set.fromList
  			   [TkVarid,TkConid,TkOp,TkConOp
  			   ,TkVaridUnboxed,TkConidUnboxed,TkOpUnboxed,TkConOpUnboxed
  			   ,TkQVarid,TkQConid,TkQOp,TkQConOp
  			   ]

{-# LINE 110 "src/ehc/Scanner/Token.chs" #-}
tokTpUnboxed :: EnumValToken -> EnumValToken
tokTpUnboxed TkVarid = TkVaridUnboxed
tokTpUnboxed TkConid = TkConidUnboxed
tokTpUnboxed TkOp    = TkOpUnboxed
tokTpUnboxed TkConOp = TkConOpUnboxed
tokTpUnboxed t       = t

{-# LINE 119 "src/ehc/Scanner/Token.chs" #-}
-- | Qualified equivalents of token kinds representing unqualified tokens
tokTpQual :: EnumValToken -> EnumValToken
tokTpQual TkVarid = TkQVarid
tokTpQual TkConid = TkQConid
tokTpQual TkOp    = TkQOp
tokTpQual TkConOp = TkQConOp
tokTpQual t       = t
