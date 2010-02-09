%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 module {%{EH}Gam.PolGam}
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

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Polarity gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(17 hmtyinfer || hmtyast) export(PolGamInfo(..), PolGam, mapPolGam,mkPGI)
data PolGamInfo = PolGamInfo { pgiPol :: Polarity } deriving Show

mkPGI :: Ty -> PolGamInfo
mkPGI t = PolGamInfo t

emptyPGI :: PolGamInfo
emptyPGI = mkPGI Ty_Any

type PolGam = Gam HsName PolGamInfo


mapPolGam :: (Ty -> Ty) -> PolGam -> PolGam
mapPolGam f
  = fst . gamMapThr (\(nm, PolGamInfo ty) thr -> ((nm, PolGamInfo $ f ty), thr)) ()
%%]

%%[(17 hmtyinfer || hmtyast) export(polGamLookup,polGamLookupErr)
polGamLookup :: HsName -> PolGam -> Maybe PolGamInfo
polGamLookup = gamLookup

polGamLookupErr :: HsName -> PolGam -> (PolGamInfo,ErrL)
polGamLookupErr n g
  = case polGamLookup n g of
      Nothing  -> (emptyPGI,[rngLift emptyRange mkErr_NamesNotIntrod "polarity" [n]])
      Just i   -> (i,[])
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of tyKiGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(17 hmtyinfer || hmtyast) export(initPolGam)
initPolGam :: PolGam
initPolGam
  = assocLToGam
      [ (hsnArrow       	, mkPGI $ quant $ [mkPolNegate var, var] `mkArrow` var)
      , (hsnInt         	, mkPGI   quantvar)
      , (hsnChar        	, mkPGI   quantvar)
      , (hsnRec         	, mkPGI $ quant $ [var] `mkArrow` var)
%%[[18
      , (hsnRecUnboxed  	, mkPGI $ quant $ [var] `mkArrow` var)
      , (hsnIntUnboxed  	, mkPGI quantvar)
%%]]
%%[[97
      , (hsnInteger     	, mkPGI quantvar)
      , (hsnInt8Unboxed  	, mkPGI quantvar)
      , (hsnInt16Unboxed  	, mkPGI quantvar)
      , (hsnInt32Unboxed  	, mkPGI quantvar)
      , (hsnInt64Unboxed  	, mkPGI quantvar)
      , (hsnWordUnboxed  	, mkPGI quantvar)
      , (hsnWord8Unboxed  	, mkPGI quantvar)
      , (hsnWord16Unboxed  	, mkPGI quantvar)
      , (hsnWord32Unboxed  	, mkPGI quantvar)
      , (hsnWord64Unboxed  	, mkPGI quantvar)
%%]]
%%[[99
      , (hsnAddrUnboxed  	, mkPGI quantvar)
%%]]
      ]
  where
    u     = uidStart
    quant = mkTyQu tyQu_Forall [(u,kiStar)]	-- TBD
    var   = mkPolVar u
    quantvar = quant var
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(17 hmtyinfer || hmtyast).Substitutable.inst.PolGamInfo
instance Substitutable PolGamInfo TyVarId VarMp where
  s |=> pgi  = pgi { pgiPol = s |=> pgiPol pgi }
  s |==> pgi = substLift pgiPol (\i x -> i {pgiPol = x}) (|==>) s pgi
  ftvSet pgi = ftvSet (pgiPol pgi)
%%]

%%[(17 hmtyinfer || hmtyast)
instance PP PolGamInfo where
  pp i = ppTy (pgiPol i)
%%]

%%[(99 hmtyinfer)
instance ForceEval PolGamInfo where
  forceEval x@(PolGamInfo p) | forceEval p `seq` True = x
%%[[102
  fevCount (PolGamInfo p) = cmUnions [cm1 "PolGamInfo",fevCount p]
%%]]
%%]

