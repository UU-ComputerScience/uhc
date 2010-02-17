%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam.ValGam}
%%]

%%[1 import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[1 hs import ({%{EH}Base.Common},{%{EH}Base.Builtin})
%%]
%%[1 hs import ({%{EH}Ty},{%{EH}Ty.Pretty})
%%]
%%[1 hs import ({%{EH}Gam})
%%]
%%[1 hs import({%{EH}Error}) 
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[(2 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(3 hmtyinfer) import({%{EH}Ty.Trf.Quantify})
%%]

%%[(20 hmtyinfer) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Type of value" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ValGam.Base export(ValGamInfo(..),ValGam)
data ValGamInfo
  = ValGamInfo
%%[[(1 hmtyinfer || hmtyast)
      { vgiTy :: Ty }		-- strictness has negative mem usage effect. Why??
%%]]
      deriving Show

type ValGam = Gam HsName ValGamInfo
%%]

%%[(20 hmtyinfer)
deriving instance Typeable ValGamInfo
deriving instance Data ValGamInfo
%%]

%%[8 export(vgiGetSet)
vgiGetSet = (vgiTy,(\x i -> i {vgiTy = x}))
%%]

%%[1.valGamLookup export(valGamLookup)
valGamLookup :: HsName -> ValGam -> Maybe ValGamInfo
valGamLookup = gamLookup
%%]

%%[(1 hmtyinfer || hmtyast).valGamLookupTy export(valGamLookupTy)
valGamLookupTy :: HsName -> ValGam -> (Ty,ErrL)
valGamLookupTy n g
  =  case valGamLookup n g of
       Nothing    ->  (Ty_Any,[rngLift emptyRange mkErr_NamesNotIntrod "value" [n]])
       Just vgi   ->  (vgiTy vgi,[])
%%]

%%[4.valGamLookup -1.valGamLookup export(valGamLookup)
valGamLookup :: HsName -> ValGam -> Maybe ValGamInfo
valGamLookup nm g
  =  case gamLookup nm g of
       Nothing
%%[[(4 hmtyinfer || hmtyast)
         |  hsnIsProd nm
                 -> let pr = mkPr nm in mkRes (tyProdArgs pr `mkArrow` pr)
         |  hsnIsUn nm && hsnIsProd (hsnUnUn nm)
                 -> let pr = mkPr (hsnUnUn nm) in mkRes ([pr] `mkArrow` pr)
         where  mkPr nm  = mkTyFreshProd (hsnProdArity nm)
                mkRes t  = Just (ValGamInfo (tyQuantifyClosed t))
%%][4
         |  hsnIsProd nm
                 -> Just ValGamInfo
         |  hsnIsUn nm && hsnIsProd (hsnUnUn nm)
                 -> Just ValGamInfo
%%]]
       Just vgi  -> Just vgi
       _         -> Nothing
%%]

%%[(3 hmtyinfer || hmtyast).valGamMapTy export(valGamMapTy)
valGamMapTy :: (Ty -> Ty) -> ValGam -> ValGam
valGamMapTy f = gamMapElts (\vgi -> vgi {vgiTy = f (vgiTy vgi)})
%%]

%%[(8 hmtyinfer || hmtyast).valGamDoWithVarMp export(valGamDoWithVarMp)
-- Do something with each ty in a ValGam.
valGamDoWithVarMp :: (HsName -> (Ty,VarMp) -> VarMp -> thr -> (Ty,VarMp,thr)) -> VarMp -> thr -> ValGam -> (ValGam,VarMp,thr)
valGamDoWithVarMp = gamDoTyWithVarMp vgiGetSet
%%]

%%[66_4.valGamCloseExists
valGamCloseExists :: ValGam -> ValGam
valGamCloseExists = valGamMapTy (\t -> tyQuantify (not . tvIsEx (tyFtvMp t)) t)
%%]

%%[(7 hmtyinfer || hmtyast) export(valGamTyOfDataCon)
valGamTyOfDataCon :: HsName -> ValGam -> (Ty,Ty,ErrL)
valGamTyOfDataCon conNm g
  = (t,rt,e)
  where (t,e) = valGamLookupTy conNm g
        (_,rt) = tyArrowArgsRes t
%%]

%%[(7 hmtyinfer || hmtyast) export(valGamTyOfDataFld)
valGamTyOfDataFld :: HsName -> ValGam -> (Ty,Ty,ErrL)
valGamTyOfDataFld fldNm g
  | null e    = (t,rt,e)
  | otherwise = (t,Ty_Any,e)
  where (t,e) = valGamLookupTy fldNm g
        ((rt:_),_) = tyArrowArgsRes t
%%]

%%[(6 hmtyinfer || hmtyast)
%%]
-- restrict the kinds of tvars bound to value identifiers to kind *
valGamRestrictKiVarMp :: ValGam -> VarMp
valGamRestrictKiVarMp g = varmpIncMetaLev $ assocTyLToVarMp [ (v,kiStar) | vgi <- gamElts g, v <- maybeToList $ tyMbVar $ vgiTy vgi ]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).Substitutable.inst.ValGamInfo
instance Substitutable ValGamInfo TyVarId VarMp where
  s |=>  vgi         =   vgi { vgiTy = s |=> vgiTy vgi }
%%[[4
  s |==> vgi         =   substLift vgiTy (\i x -> i {vgiTy = x}) (|==>) s vgi
%%]]
  ftvSet vgi         =   ftvSet (vgiTy vgi)
%%]

%%[(1 hmtyinfer || hmtyast).PP.ValGamInfo
instance PP ValGamInfo where
  pp vgi = ppTy (vgiTy vgi)
%%]

%%[(99 hmtyinfer || hmtyast)
instance ForceEval ValGamInfo where
  forceEval x@(ValGamInfo t) | forceEval t `seq` True = x
%%[[102
  fevCount (ValGamInfo x) = cm1 "ValGamInfo" `cmUnion` fevCount x
%%]]
%%]

%%[(20 hmtyinfer || hmtyast)
instance Serialize ValGamInfo where
  sput (ValGamInfo a) = sput a
  sget = liftM ValGamInfo sget
%%]

