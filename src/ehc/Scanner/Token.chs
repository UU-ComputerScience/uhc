%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5 module {%{EH}Scanner.Token} import(UU.Scanner.Position, UU.Scanner.GenToken) export(Token,EnumValToken(..), module UU.Scanner.GenToken)
%%]

%%[5 export(reserved,valueToken,errToken,tokTpIsInt)
%%]

%%[8 import(Data.Set) export(tokTpIsId)
%%]

%%[20 export(tokTpQual)
%%]

%%[5
type Token = GenToken String EnumValToken String

data EnumValToken
  = TkVarid
  | TkConid
  | TkOp
  | TkConOp
%%[[20
  | TkQVarid
  | TkQConid
  | TkQOp
  | TkQConOp
%%]
  | TkString
  | TkChar
  | TkInteger8
  | TkInteger10
  | TkInteger16
  | TkFraction
  | TkTextnm
  | TkTextln 
  | TkError
  deriving (Eq, Ord)
%%]

%%[5
reserved                :: String -> Pos -> Token
reserved                =  Reserved 

valueToken              :: EnumValToken -> String -> Pos -> Token
valueToken              =  ValToken 

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
  = (`member` ts)
  where ts = fromList
  			   [TkVarid,TkConid,TkOp,TkConOp
%%[[20
  			   ,TkQVarid,TkQConid,TkQOp,TkQConOp
%%]
  			   ]
%%]

%%[20
tokTpQual :: EnumValToken -> EnumValToken
tokTpQual TkVarid = TkQVarid
tokTpQual TkConid = TkQConid
tokTpQual TkOp    = TkQOp
tokTpQual TkConOp = TkQConOp
tokTpQual t       = t
%%]
