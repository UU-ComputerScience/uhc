%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam specialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 module {%{EH}Gam.TyKiGam}
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
%%% "Kind of type variable/name" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 export(TyKiGamInfo(..),TyKiGam,emptyTKGI)
data TyKiGamInfo
  = TyKiGamInfo
%%[[(6 hmtyinfer || hmtyast)
      { tkgiKi :: !Ty }
%%]]
      deriving Show

emptyTKGI :: TyKiGamInfo
emptyTKGI
  = TyKiGamInfo
%%[[(6 hmtyinfer || hmtyast)
      kiStar
%%]]

type TyKiGam = Gam TyKiKey TyKiGamInfo
%%]

%%[8 export(tkgiGetSet)
tkgiGetSet = (tkgiKi,(\x i -> i {tkgiKi = x}))
%%]

%%[6 export(tyKiGamLookupByTyVar)
tyKiGamLookupByTyVar :: TyVarId -> TyKiGam -> Maybe TyKiGamInfo
tyKiGamLookupByTyVar v g = gamLookup (TyKiKey_TyVar v) g
%%]

%%[6 export(tyKiGamLookupByName)
tyKiGamLookupByName :: HsName -> TyKiGam -> Maybe TyKiGamInfo
tyKiGamLookupByName n g
  = case gamLookup (TyKiKey_Name n) g of
      Nothing
        | hsnIsProd n
%%[[(6 hmtyinfer || hmtyast)
            -> Just (TyKiGamInfo (replicate (hsnProdArity n) kiStar `mkArrow` kiStar))
%%][6
            -> Just TyKiGamInfo
%%]]
      x     -> x
%%]

%%[(6 hmtyinfer || hmtyast) export(tyKiGamLookup)
tyKiGamLookup :: Ty -> TyKiGam -> Maybe TyKiGamInfo
tyKiGamLookup t g
  = case tyMbVar t of
      Just v  -> tyKiGamLookupByTyVar v g
      Nothing ->
                 case tyMbCon t of
                   Just n -> tyKiGamLookupByName n g
                   _      -> Nothing
%%]

%%[(6 hmtyinfer || hmtyast) export(tyKiGamLookupErr,tyKiGamLookupKi)
tyKiGamLookupErr :: Ty -> TyKiGam -> (TyKiGamInfo,ErrL)
tyKiGamLookupErr t g
  = case tyKiGamLookup t g of
      Nothing -> (emptyTKGI,[rngLift emptyRange mkErr_NamesNotIntrod "kind" [mkHNm $ show t]])
      Just i  -> (i,[])

tyKiGamLookupKi :: TyKiGam -> Ty -> Ty
tyKiGamLookupKi g t = tkgiKi $ fst $ tyKiGamLookupErr t g
%%]

%%[(6 hmtyinfer || hmtyast) export(tyKiGamLookupByNameErr)
tyKiGamLookupByNameErr :: HsName -> TyKiGam -> (TyKiGamInfo,ErrL)
tyKiGamLookupByNameErr n g = tyKiGamLookupErr (semCon n) g
%%]

%%[(6 hmtyinfer || hmtyast) export(tyKiGamVarSingleton)
tyKiGamVarSingleton :: TyVarId -> TyKiGamInfo -> TyKiGam
tyKiGamVarSingleton v k = gamSingleton (TyKiKey_TyVar v) k
%%]

%%[6 export(tyKiGamNameSingleton)
tyKiGamNameSingleton :: HsName -> TyKiGamInfo -> TyKiGam
tyKiGamNameSingleton n k = gamSingleton (TyKiKey_Name n) k
%%]

%%[(6 hmtyinfer || hmtyast) export(tyKiGamSingleton)
tyKiGamSingleton :: Ty -> TyKiGamInfo -> TyKiGam
tyKiGamSingleton t k
  = case tyMbVar t of
      Just v  -> tyKiGamVarSingleton v k
      Nothing -> case tyMbCon t of
                   Just n -> tyKiGamNameSingleton n k
                   _      -> panic "Gam.tyKiGamSingleton"
%%]

%%[(8 hmtyinfer || hmtyast) export(tyKiGamDoWithVarMp)
-- Do something with each kind in a TyKiGam.
tyKiGamDoWithVarMp :: (TyKiKey -> (Ty,VarMp) -> VarMp -> thr -> (Ty,VarMp,thr)) -> VarMp -> thr -> TyKiGam -> (TyKiGam,VarMp,thr)
tyKiGamDoWithVarMp = gamDoTyWithVarMp tkgiGetSet
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Determinining the kind of a type variable, after being used in type inference
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

The tricky point is to get from substituted tyvars back to the original one,
because that is what is stored in the mapping for type variables to kinds.
It is the responsibility to prepare tvKiVarMp that it contains the correct mapping,
possibly using varmpMapTyVarKey.
Defaults to * (kiStar).

%%[(6 hmtyinfer || hmtyast) export(tvarKi)
tvarKi :: TyKiGam -> VarMp -> VarMp -> TyVarId -> Ty
tvarKi tyKiGam tvKiVarMp _ tv
  = case tyKiGamLookup tv' tyKiGam of
      Just tkgi -> tvKiVarMp |=> tkgiKi tkgi
      _         -> tvKiVarMp |=> tv'
  where tv' = {- tyVarMp |=> -} mkTyVar tv
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of tyKiGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6 export(initTyKiGam)
initTyKiGam :: TyKiGam
initTyKiGam
%%[[(6 hmtyinfer || hmtyast)
  = gamUnions
      [ (tyKiGamNameSingleton hsnArrow      (TyKiGamInfo ([kiStar,kiStar] `mkArrow` kiStar)))
      , gamUnions
          (zipWith tyKiGamNameSingleton
               [ hsnInt, hsnChar
%%[[97
               , hsnInteger
%%]]
               ]
               (repeat star)
          )
%%[[18
      , gamUnions
          (zipWith tyKiGamNameSingleton
               [ hsnIntUnboxed
%%[[97
               , hsnInt8Unboxed, hsnInt16Unboxed, hsnInt32Unboxed, hsnInt64Unboxed
               , hsnWordUnboxed
               , hsnWord8Unboxed, hsnWord16Unboxed, hsnWord32Unboxed, hsnWord64Unboxed
%%]]
%%[[99
               , hsnAddrUnboxed
%%]]
               ]
               (repeat unbx)
          )
%%]]
%%[[7
      , (tyKiGamNameSingleton hsnRow        (TyKiGamInfo kiRow))
      , (tyKiGamNameSingleton hsnRec        (TyKiGamInfo ([kiRow] `mkArrow` kiStar)))
      , (tyKiGamNameSingleton hsnSum        (TyKiGamInfo ([kiRow] `mkArrow` kiStar)))
%%]]
%%[[9
      , (tyKiGamNameSingleton hsnPrArrow    (TyKiGamInfo ([kiStar,kiStar] `mkArrow` kiStar)))
%%]]
%%[[18
      , (tyKiGamNameSingleton hsnRecUnboxed (TyKiGamInfo ([kiRow] `mkArrow` kiUnboxed)))
%%]]
      ]
  where star = TyKiGamInfo kiStar
%%[[18
        unbx = TyKiGamInfo kiUnboxed
%%]]
%%][6
  = gamMap (\(k,_) -> (k,TyKiGamInfo)) initTyGam
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(6 hmtyinfer || hmtyast).Substitutable.inst.TyKiGamInfo
instance Substitutable TyKiGamInfo TyVarId VarMp where
  s |=>  tkgi         =   tkgi { tkgiKi = s |=> tkgiKi tkgi }
  s |==> tkgi         =   substLift tkgiKi (\i x -> i {tkgiKi = x}) (|==>) s tkgi
  ftvSet tkgi         =   ftvSet (tkgiKi tkgi)
%%]

%%[(6 hmtyinfer || hmtyast)
instance PP TyKiGamInfo where
  pp i = ppTy (tkgiKi i)
%%]

%%[(99 hmtyinfer || hmtyast)
instance ForceEval TyKiGamInfo where
  forceEval x@(TyKiGamInfo k) | forceEval k `seq` True = x
%%[[102
  fevCount (TyKiGamInfo x) = cm1 "TyKiGamInfo" `cmUnion` fevCount x
%%]]
%%]
