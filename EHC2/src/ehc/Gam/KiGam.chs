%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 module {%{EH}Gam.KiGam}
%%]

%%[6 import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[6 hs import ({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]
%%[6 hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
%%]
%%[6 hs import ({%{EH}Gam})
%%]
%%[6 hs import({%{EH}Error}) 
%%]

%%[(6 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[(6 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(6 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Sort of kind" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 export(KiGam, KiGamInfo(..))
data KiGamInfo
  = KiGamInfo
%%[[(6 hmtyinfer || hmtyast)
      { kgiKi :: Ty }
%%]]
      deriving Show

type KiGam = Gam HsName KiGamInfo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of kiGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 export(initKiGam)
initKiGam :: KiGam
initKiGam
  = assocLToGam
%%[[(6 hmtyinfer || hmtyast)
      [ (hsnArrow   ,   KiGamInfo (Ty_Con hsnArrow))
      , (hsnKindStar,   KiGamInfo kiStar)
%%][6
      [ (hsnArrow   ,   KiGamInfo)
      , (hsnKindStar,   KiGamInfo)
%%]]
%%[[(7 hmtyinfer || hmtyast)
      , (hsnKindRow ,   KiGamInfo kiRow)
%%][7
      , (hsnKindRow ,   KiGamInfo)
%%]]
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 hmtyinfer || hmtyast)
instance ForceEval KiGamInfo where
  forceEval x@(KiGamInfo k) | forceEval k `seq` True = x
%%[[102
  fevCount (KiGamInfo x) = cm1 "KiGamInfo" `cmUnion` fevCount x
%%]]
%%]
