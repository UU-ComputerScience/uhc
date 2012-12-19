%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Pred}
-- Force generation of module header
%%]

%%[9 import(UHC.Util.Pretty)
%%]

%%[9999 import({%{EH}Base.Builtin},Data.Maybe,Data.List,qualified Data.Map as Map,qualified Data.Set as Set,UHC.Util.Pretty,{%{EH}Gam.Full},{%{EH}Base.Common})
%%]

%%[(9999 hmtyinfer) import({%{EH}Opts.Base})
%%]

%%[(9999 hmtyinfer) import({%{EH}Ty},{%{EH}Ty.Pretty},{%{EH}Ty.FitsInCommon},{%{EH}Ty.Trf.Quantify},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(9999 codegen hmtyinfer) import({%{EH}Core},{%{EH}Core.Pretty},{%{EH}Core.Subst},{%{EH}Core.Utils})
%%]

%%[(9999 hmtyinfer) import({%{EH}Base.Debug})
%%]

%%[(9999 hmtyinfer) import({%{EH}Error})
%%]

%%[(9999 hmtyinfer) import({%{EH}Ty.Ftv})
%%]

%%[(50 hmtyinfer) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[(9999 hmtyinfer) import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functional dependency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(15 hmtyinfer) export(ClsFuncDep(..))
data ClsFuncDep = ClsFuncDep [Int] [Int] deriving Show

instance PP ClsFuncDep where
  pp (ClsFuncDep f t) = ppBracketsCommas f >|< "->" >|< ppBracketsCommas t
%%]


