%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization: Class environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 module {%{EH}Gam.ClGam}
%%]

%%[9 import(UHC.Util.Pretty,UHC.Util.Utils)
%%]

%%[9 hs import ({%{EH}Base.Common},{%{EH}Base.TermLike},{%{EH}Base.HsName.Builtin})
%%]
%%[(9 hmtyast || hmtyinfer) hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
%%]
%%[9 hs import ({%{EH}Gam})
%%]
%%[6999 hs import({%{EH}Error}) 
%%]

%%[(6999 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[(6999 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[50 import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for intro rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ClGamInfo(..),ClGam,emptyCLGI)
-- If this changes, also change {%{EH}ConfigInternalVersions}
data ClGamInfo
  =  ClGamInfo
%%[[(9 hmtyinfer)
       { clgiPrToEvidRecTy      :: !Ty                  -- mapping from predicate type -> dictionary structure record, encoded as function
       , clgiPrToEvidDataTy     :: !Ty              	-- mapping from predicate type -> datatype, encoded as function
       , clgiRuleTy             :: !Ty              	-- predicate context + class head as function type
       , clgiDfltDictNm         :: !HsName              -- dictionary name of default instance fields constructing function
       , clgiDictTag            :: !CTag                -- tag of dictionary
%%[[92
       , clgiGenerDerivableL    :: [(HsName,HsName)]    -- list of fields with default value which can be derived using generic deriving
%%]]
       }
%%]]
       deriving (Show, Generic)

type ClGam     = Gam HsName ClGamInfo

emptyCLGI
  = ClGamInfo
%%[[(9 hmtyinfer)
      Ty_Any Ty_Any Ty_Any hsnUnknown CTagRec
%%[[92
      []
%%]]
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
deriving instance Typeable ClGamInfo
%%]

%%[(9 hmtyinfer)
instance PP ClGamInfo where
  pp clgi = "dictnm=" >#< pp (clgiDfltDictNm clgi) >-< indent 2 (
        "rule=" >#< ppTy (clgiRuleTy clgi)
    >-< "eviddataty=" >#< ppTy (clgiPrToEvidDataTy clgi)
    >-< "evidrecty=" >#< ppTy (clgiPrToEvidRecTy clgi)
%%[[92
    >-< "clgiGenerDerivableL=" >#< ppAssocL (clgiGenerDerivableL clgi)
%%]]
    )
%%]

%%[9 export(initClGam)
initClGam
  = assocLToGam
      [ (hsnPrArrow,    emptyCLGI)
      ]
%%]

%%[(9999 hmtyinfer)
instance ForceEval ClGamInfo where
  forceEval x@(ClGamInfo e r n) | forceEval e `seq` forceEval r `seq` forceEval n `seq` True = x
%%[[102
  fevCount (ClGamInfo e r n) = cm1 "ClGamInfo" `cmUnion` fevCount e `cmUnion` fevCount r `cmUnion` fevCount n
%%]]
%%]

%%[(50 hmtyinfer)
instance Serialize ClGamInfo
%%]

%%[(5050 hmtyinfer)
instance Serialize ClGamInfo where
%%[[50
  sput (ClGamInfo a b c d e) = sput a >> sput b >> sput c >> sput d >> sput e
%%][92
  sput (ClGamInfo a b c d e f) = sput a >> sput b >> sput c >> sput d >> sput e >> sput f
%%]]
%%[[50
  sget = liftM5 ClGamInfo sget sget sget sget sget
%%][92
  sget = liftM6 ClGamInfo sget sget sget sget sget sget
%%]]
%%]

