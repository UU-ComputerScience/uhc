%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5 module {%{EH}Scanner.Token} import(UU.Scanner.Position, UU.Scanner.GenToken) export(module UU.Scanner.GenToken)
%%]

%%[5 export(reserved,valueToken,errToken,tokTpIsInt)
%%]

%%[8 import(qualified Data.Set as Set) export(tokTpIsId)
%%]

%%[5 export(Token, ValTokenVal,EnumValToken(..))
type ValTokenVal = [String]
type Token = GenToken String EnumValToken ValTokenVal

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
%%[[18
  | TkVaridUnboxed
  | TkConidUnboxed
  | TkOpUnboxed
  | TkConOpUnboxed
%%]]
%%[[20
  | TkQVarid
  | TkQConid
  | TkQOp
  | TkQConOp
%%]]
  | TkError
  deriving (Eq, Ord)
%%]

%%[5 export(tokenVals,tokenVal,tokenMap)
tokenVals :: Token -> [String]
tokenVals (ValToken _ v _) = v
tokenVals (Reserved   v _) = [v]

tokenVal :: Token -> String
tokenVal = concat . tokenVals

tokenMap :: (String->String) -> Token -> Token
tokenMap f (ValToken t v p) = ValToken t (map f v) p
tokenMap f (Reserved   k p) = Reserved   (f k) p
%%]

%%[5
reserved                :: String -> Pos -> Token
reserved                =  Reserved 

valueToken              :: EnumValToken -> String -> Pos -> Token
valueToken t s p        =  ValToken t [s] p

errToken                :: String -> Pos -> Token
errToken                =  valueToken TkError 
%%]

%%[5
tokTpIsInt :: EnumValToken -> Bool
tokTpIsInt tp = tp == TkInteger8 || tp == TkInteger10 || tp == TkInteger16
%%]

%%[8
tokTpIsId :: EnumValToken -> Bool
tokTpIsId
  = (`Set.member` ts)
  where ts = Set.fromList
  			   [TkVarid,TkConid,TkOp,TkConOp
%%[[18
  			   ,TkVaridUnboxed,TkConidUnboxed,TkOpUnboxed,TkConOpUnboxed
%%]]
%%[[20
  			   ,TkQVarid,TkQConid,TkQOp,TkQConOp
%%]]
  			   ]
%%]

%%[18 export(tokTpUnboxed)
tokTpUnboxed :: EnumValToken -> EnumValToken
tokTpUnboxed TkVarid = TkVaridUnboxed
tokTpUnboxed TkConid = TkConidUnboxed
tokTpUnboxed TkOp    = TkOpUnboxed
tokTpUnboxed TkConOp = TkConOpUnboxed
tokTpUnboxed t       = t
%%]

%%[20 export(tokTpQual)
tokTpQual :: EnumValToken -> EnumValToken
tokTpQual TkVarid = TkQVarid
tokTpQual TkConid = TkQConid
tokTpQual TkOp    = TkQOp
tokTpQual TkConOp = TkQConOp
tokTpQual t       = t
%%]
