%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5 module {%{EH}Base.Scanner.Token} import(UU.Scanner.Position, UU.Scanner.GenToken) export(Token,EnumValToken(..), module UU.Scanner.GenToken)
%%]

%%[5 export(reserved,valueToken,errToken,tokTpIsInt)
%%]

%%[5
type Token = GenToken String EnumValToken String

data EnumValToken
  = TkQVarid
  | TkQConid
  | TkVarid
  | TkConid
  | TkString
  | TkChar
  | TkInteger8
  | TkInteger10
  | TkInteger16
  | TkFraction
  | TkTextnm
  | TkTextln 
  | TkQOp
  | TkQConOp
  | TkOp
  | TkConOp
  | TkError
  deriving (Eq, Ord)

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