%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Pred} import(Data.Maybe,Data.List,qualified Data.Map as Map,qualified Data.Set as Set,EH.Util.Pretty)
%%]

%%[9 import({%{EH}Ty},{%{EH}Ty.Pretty},{%{EH}Ty.FitsInCommon},{%{EH}Ty.Trf.Quantify},{%{EH}Core},{%{EH}Core.Pretty},{%{EH}Core.Subst},{%{EH}Core.Utils},{%{EH}Base.Builtin},{%{EH}Base.CfgPP},{%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Gam},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[9 import({%{EH}Base.Debug})
%%]

%%[9 import({%{EH}Error})
%%]

%%[9 export(ClGamInfo(..),ClGam,emptyCLGI)
%%]

%%[9 import({%{EH}Ty.Ftv})
%%]

%%[15 export(ClsFuncDep(..))
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functional dependency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[15
data ClsFuncDep = ClsFuncDep [Int] [Int] deriving Show

instance PP ClsFuncDep where
  pp (ClsFuncDep f t) = ppBracketsCommas f >|< "->" >|< ppBracketsCommas t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for intro rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ClGamInfo
  =  ClGamInfo
       { clgiPrToEvidTy     :: !Ty
       , clgiRuleTy         :: !Ty
       , clgiDfltDictNm     :: !HsName
       } deriving Show

type ClGam     = Gam HsName ClGamInfo

emptyCLGI = ClGamInfo Ty_Any Ty_Any hsnUnknown

instance PP ClGamInfo where
  pp clgi = pp (clgiDfltDictNm clgi) >#< "::" >#< ppTy (clgiRuleTy clgi) >#< "::" >#< ppTy (clgiPrToEvidTy clgi)
%%]

%%[9 export(initClGam)
initClGam
  = assocLToGam
      [ (hsnPrArrow,    emptyCLGI)
      ]
%%]

%%[99
instance ForceEval ClGamInfo where
  forceEval x@(ClGamInfo e r n) | forceEval e `seq` forceEval r `seq` forceEval n `seq` True = x
%%[[101
  fevCount (ClGamInfo e r n) = cm1 "ClGamInfo" `cmUnion` fevCount e `cmUnion` fevCount r `cmUnion` fevCount n
%%]]
%%]

