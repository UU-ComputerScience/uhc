%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Pred} import({%{EH}Base.Builtin},Data.Maybe,Data.List,qualified Data.Map as Map,qualified Data.Set as Set,EH.Util.Pretty,{%{EH}Gam.Full},{%{EH}Base.Common})
%%]

%%[(9 hmtyinfer) import({%{EH}Base.CfgPP},{%{EH}Base.Opts})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty},{%{EH}Ty.Pretty},{%{EH}Ty.FitsInCommon},{%{EH}Ty.Trf.Quantify},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(9 codegen hmtyinfer) import({%{EH}Core},{%{EH}Core.Pretty},{%{EH}Core.Subst},{%{EH}Core.Utils})
%%]

%%[(9 hmtyinfer) import({%{EH}Base.Debug})
%%]

%%[(9 hmtyinfer) import({%{EH}Error})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty.Ftv})
%%]

%%[(15 hmtyinfer) export(ClsFuncDep(..))
%%]

%%[(99 hmtyinfer) import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functional dependency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(15 hmtyinfer)
data ClsFuncDep = ClsFuncDep [Int] [Int] deriving Show

instance PP ClsFuncDep where
  pp (ClsFuncDep f t) = ppBracketsCommas f >|< "->" >|< ppBracketsCommas t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for intro rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ClGamInfo(..),ClGam,emptyCLGI)
data ClGamInfo
  =  ClGamInfo
%%[[(9 hmtyinfer)
       { clgiPrToEvidTy     :: !Ty
       , clgiRuleTy         :: !Ty
       , clgiDfltDictNm     :: !HsName
       -- , clgiSupClsFldNmL   :: ![HsName]
       }
%%]]
       deriving Show

type ClGam     = Gam HsName ClGamInfo

emptyCLGI
  = ClGamInfo
%%[[(9 hmtyinfer)
      Ty_Any Ty_Any hsnUnknown
%%]]
%%]

%%[(9 hmtyinfer)
instance PP ClGamInfo where
  pp clgi = pp (clgiDfltDictNm clgi) >#< "::" >#< ppTy (clgiRuleTy clgi) >#< "::" >#< ppTy (clgiPrToEvidTy clgi)
%%]

%%[9 export(initClGam)
initClGam
  = assocLToGam
      [ (hsnPrArrow,    emptyCLGI)
      ]
%%]

%%[(99 hmtyinfer)
instance ForceEval ClGamInfo where
  forceEval x@(ClGamInfo e r n) | forceEval e `seq` forceEval r `seq` forceEval n `seq` True = x
%%[[102
  fevCount (ClGamInfo e r n) = cm1 "ClGamInfo" `cmUnion` fevCount e `cmUnion` fevCount r `cmUnion` fevCount n
%%]]
%%]

