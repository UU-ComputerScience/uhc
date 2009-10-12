%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TauPhi Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 tauphi) module {%{EH}TauPhi.Common} import({%{EH}Base.Common})
%%]

%%[(8 tauphi) hs export(Strictness(..))
%%]

%%[(8 tauphi)
data Strictness
  = Strict
  | NonStrict
  | Var HsName

instance Show Strictness where
  show Strict    = "strict"
  show NonStrict = "nonStrict"
  show (Var n)   = "strictness:" ++ show n

%%]

