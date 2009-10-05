%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TauPhi Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[5 module {%{EH}TauPhi.Common} import({%{EH}Base.Common})
%%]

%%[5 hs export(Strictness(..))
%%]

%%[5
data Strictness
  = Strict
  | NonStrict
  | Var HsName

instance Show Strictness where
  show Strict    = "strict"
  show NonStrict = "nonStrict"
  show (Var n)   = "strictness:" ++ show n

%%]

