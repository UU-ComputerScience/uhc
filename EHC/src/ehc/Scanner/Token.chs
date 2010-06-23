%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
A |Token| is the result of scanning, the lexeme coming out of |scan|.
A |Token| is a specialisation of |GenToken|, available from the UU scanning library.
The value variant represents its values as @[String]@ in order to preserve qualification info.
This is only relevant when modules are available.
%%]

%%[5 module {%{EH}Scanner.Token} import(UU.Scanner.Position, UU.Scanner.GenToken) export(module UU.Scanner.GenToken)
%%]

%%[5 export(reserved,valueToken,errToken,tokTpIsInt)
%%]

%%[8 import(qualified Data.Set as Set) export(tokTpIsId)
%%]

%%[5 export(Token, ValTokenVal)
-- | The value of a Token is a [String], of length > 1 only when it encodes qualified identifiers
type ValTokenVal = [String]
type Token = GenToken String EnumValToken ValTokenVal
%%]

%%[5 export(EnumValToken(..))
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
%%[[18
  | TkVaridUnboxed
  | TkConidUnboxed
  | TkOpUnboxed
  | TkConOpUnboxed
%%]]
%%[[20
  | TkQVarid			-- qualified variants
  | TkQConid
  | TkQOp
  | TkQConOp
%%]]
  | TkError
  deriving (Eq, Ord)
%%]

%%[5 export(tokenVals,tokenVal,tokenMap)
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
-- | Qualified equivalents of token kinds representing unqualified tokens
tokTpQual :: EnumValToken -> EnumValToken
tokTpQual TkVarid = TkQVarid
tokTpQual TkConid = TkQConid
tokTpQual TkOp    = TkQOp
tokTpQual TkConOp = TkQConOp
tokTpQual t       = t
%%]
