% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma (aka Assumptions, Environment)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 import(List,EHCommon) export(Gam,emptyGam,gamLookup, gamPushNew, gamPop, gamAddGam, gamUnit, gamAdd, gamPushGam, gamToAssocL, assocLToGam)
%%]

%%[1 import(EHTy,EHError) export(ValGam, ValGamInfo(..), valGamLookup,valGamLookupTy)
%%]

%%[1 export(TyGam, TyGamInfo(..), tyGamLookup)
%%]

%%[1 import(UU.Pretty,EHTyPretty) export(ppGam)
%%]

%%[2 import(EHCnstr,EHSubstitutable)
%%]

%%[3 import(EHTyQuantify) export(valGamQuantify, gamMap,gamMapElts,valGamMapTy)
%%]

%%[4 import(EHOpts,EHTyInstantiate) export(valGamInst1Exists)
%%]

%%[4 import(EHTyFitsInCommon) export(AppSpineGam, appSpineGam, asGamLookup)
%%]

%%[4 export(gamMapThr)
%%]

%%[4 export(gamTop)
%%]

%%[4_2 export(valGamQuantifyWithCnstr,valGamInst1ExistsWithCnstr)
%%]

%%[6 export(tyGamQuantify, tyGamInst1Exists,gamUnzip)
%%]

%%[6 export(KiGam, KiGamInfo(..))
%%]

%%[6 export(mkTGI)
%%]

%%[7 export(mkTGIData)
%%]

%%[8 import(Maybe,FiniteMap,EHCore) export(gamUpd,DataTagMp)
%%]

%%[9 import(EHDebug,EHCoreSubst,EHTyFitsInCommon) export(gamUpdAdd,gamLookupAll,gamSubstTop,gamElts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Base.sigs
newtype Gam k v     =   Gam [AssocL k v]  deriving Show

emptyGam            ::  Gam k v
gamUnit             ::  k -> v      -> Gam k v
gamLookup           ::  Eq k => k   -> Gam k v -> Maybe v
gamToAssocL         ::  Gam k v     -> AssocL k v
gamPushNew          ::  Gam k v     -> Gam k v
gamPushGam          ::  Gam k v     -> Gam k v -> Gam k v
gamAddGam           ::  Gam k v     -> Gam k v -> Gam k v
gamAdd              ::  k -> v      -> Gam k v -> Gam k v
%%]

%%[1.Base.funs
emptyGam                            = Gam [[]]
gamUnit         k v                 = Gam [[(k,v)]]
gamLookup       k (Gam ll)          = foldr  (\l mv -> maybe mv Just (lookup k l))
                                             Nothing ll
gamToAssocL     (Gam ll)            = concat ll
gamPushNew      (Gam ll)            = Gam ([]:ll)
gamPushGam g1   (Gam ll2)           = Gam (gamToAssocL g1 : ll2)
gamAddGam       g1 (Gam (l2:ll2))   = Gam ((gamToAssocL g1 ++ l2):ll2)
gamAdd          k v                 = gamAddGam (k `gamUnit` v)
%%]

%%[1.Rest.sigs
gamPop              ::  Gam k v     -> (Gam k v,Gam k v)
assocLToGam         ::  AssocL k v  -> Gam k v
%%]

%%[1.Rest.funs
gamPop          (Gam (l:ll))        = (Gam [l],Gam ll)
assocLToGam     l                   = Gam [l]
%%]

%%[3.gamMap
gamMap :: ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap f (Gam ll) = Gam (map (map f) ll)

gamMapElts :: (v -> v') -> Gam k v -> Gam k v'
gamMapElts f = gamMap (\(n,v) -> (n,f v))
%%]

%%[4
gamMapThr :: ((k,v) -> t -> ((k',v'),t)) -> t -> Gam k v -> (Gam k' v',t)
gamMapThr f thr (Gam ll)
  =  let (ll',thr')
           =  (foldr  (\l (ll,thr)
                        ->  let  (l',thr')
                                    =  foldr  (\kv (l,thr)
                                                ->  let (kv',thr') = f kv thr in (kv':l,thr')
                                              ) ([],thr) l
                            in   (l':ll,thr')
                      ) ([],thr) ll
              )
     in  (Gam ll',thr')
%%]

%%[4
gamTop ::  Gam k v -> Gam k v
gamTop  (Gam (l:ll)) = Gam [l]
%%]

%%[8.gamUpd
gamMbUpd :: Eq k => k -> (k -> v -> v) -> Gam k v -> Maybe (Gam k v)
gamMbUpd k upd (Gam ll)
  =  let u ((kv@(k',v):ls):lss)
             | k' == k    = Just (((k',upd k v):ls):lss)
             | otherwise  = maybe Nothing (\(ls:lss) -> Just ((kv:ls):lss)) (u (ls:lss))
         u ([]:lss)       = maybe Nothing (\lss -> Just ([] : lss)) (u lss)
         u []             = Nothing
     in  fmap Gam (u ll)

gamUpd :: Eq k => k -> (k -> v -> v) -> Gam k v -> Gam k v
gamUpd k upd = fromJust . gamMbUpd k upd
%%]

gamMbUpd :: Eq k => k -> (k -> v -> v) -> Gam k v -> Maybe (Gam k v)
gamMbUpd k upd (Gam ll)
  =  let  (didUpd,ll')
            = foldr  (\l (didUpd,ll')
                         ->  let  (didUpd',l')
                                    = foldr  (\kv@(k',v) (didUpd,l')
                                               -> if k' == k then (True,(k,upd k v) : l') else (didUpd,kv : l')
                                             ) (didUpd,[]) l
                             in   (didUpd',l' : ll')
                     ) (False,[]) ll
     in   if didUpd then Just (Gam ll') else Nothing

%%[9
gamElts :: Gam k v -> [v]
gamElts = assocLElts . gamToAssocL

gamLookupAll :: Eq k => k -> Gam k v -> [v]
gamLookupAll k (Gam ll) = catMaybes (map (lookup k) ll)

gamUpdAdd :: Eq k => k -> v -> (k -> v -> v) -> Gam k v -> Gam k v
gamUpdAdd k v upd g = maybe (gamAdd k v g) id (gamMbUpd k upd g)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Type of value" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ValGam.Base
data ValGamInfo = ValGamInfo { vgiTy :: Ty } deriving Show

type ValGam = Gam HsName ValGamInfo
%%]

%%[1.valGamLookup
valGamLookup :: HsName -> ValGam -> Maybe ValGamInfo
valGamLookup = gamLookup
%%]

%%[1.valGamLookupTy
valGamLookupTy :: HsName -> ValGam -> (Ty,ErrL)
valGamLookupTy n g
  =  case valGamLookup n g of
       Nothing    ->  (Ty_Any,[Err_NamesNotIntrod [n]])
       Just vgi   ->  (vgiTy vgi,[])
%%]

%%[4.valGamLookup -1.valGamLookup
valGamLookup :: HsName -> ValGam -> Maybe ValGamInfo
valGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
                 -> let pr = mkPr nm in mkRes (tyProdArgs pr `mkTyArrow` pr)
         |  hsnIsUn nm && hsnIsProd (hsnUnUn nm)
                 -> let pr = mkPr (hsnUnUn nm) in mkRes ([pr] `mkTyArrow` pr)
         where  mkPr nm  = mkTyFreshProd (hsnProdArity nm)
                mkRes t  = Just (ValGamInfo (tyQuantifyClosed t))
       Just vgi  -> Just vgi
       _         -> Nothing
%%]

%%[3.valGamMapTy
valGamMapTy :: (Ty -> Ty) -> ValGam -> ValGam
valGamMapTy f = gamMapElts (\vgi -> vgi {vgiTy = f (vgiTy vgi)})
%%]

%%[3.valGamQuantify
valGamQuantify :: TyVarIdL -> ValGam -> ValGam
valGamQuantify globTvL = valGamMapTy (\t -> tyQuantify (`elem` globTvL) t)
%%]

%%[4_2.valGamDoWithCnstr
valGamDoWithCnstr :: (Ty -> thr -> (Ty,thr)) -> Cnstr -> thr -> ValGam -> (ValGam,Cnstr)
valGamDoWithCnstr f gamCnstr thr gam
  =  let  (g,(_,c))
            =  gamMapThr
                    (\(n,vgi) (thr,c)
                        ->  let  t = vgiTy vgi
                                 (t',thr') = f (gamCnstr |=> t) thr
                                 (tg,cg) =  case t of
                                                Ty_Var v _ -> (t,v `cnstrTyUnit` t')
                                                _ -> (t',emptyCnstr)
                            in   ((n,vgi {vgiTy = tg}),(thr',cg `cnstrPlus` c))
                    )
                    (thr,emptyCnstr) gam
     in   (g,c)
%%]

%%[4_2.valGamQuantifyWithCnstr
valGamQuantifyWithCnstr :: Cnstr -> TyVarIdL -> ValGam -> (ValGam,Cnstr)
valGamQuantifyWithCnstr = valGamDoWithCnstr (\t globTvL -> (tyQuantify (`elem` globTvL) t,globTvL))
%%]
valGamQuantifyWithCnstr gamCnstr globTvL
  =  gamMapThr
        (\(n,vgi) c
            ->  let  t = vgiTy vgi
                     tq = tyQuantify (`elem` globTvL) (gamCnstr |=> t)
                     (tg,cg) =  case t of
                                    Ty_Var v _ -> (t,v `cnstrTyUnit` tq)
                                    _ -> (tq,emptyCnstr)
                in   ((n,vgi {vgiTy = tg}),cg `cnstrPlus` c)
        )
        emptyCnstr

%%[6
gamUnzip :: Gam k (v1,v2) -> (Gam k v1,Gam k v2)
gamUnzip (Gam ll)
  =  let  (ll1,ll2) = unzip . map (unzip . map (\(n,(v1,v2)) -> ((n,v1),(n,v2)))) $ ll
     in   (Gam ll1,Gam ll2)
%%]

%%[9.valGamQuantify -3.valGamQuantify
valGamQuantify :: TyVarIdL -> [PredOcc] -> ValGam -> (ValGam,Gam HsName TyQuOut)
valGamQuantify globTvL prL g
  =  let  g' = gamMapElts  (\vgi ->  let  tqo = tyQuantifyPr defaultTyQuOpts (`elem` globTvL) TyQu_Forall prL (vgiTy vgi)
                                     in   (vgi {vgiTy = tqoTy tqo},tqo)
                           ) g
     in   gamUnzip g'
%%]

%%[4.valGamInst1Exists
gamInst1Exists :: (v -> Ty,v -> Ty -> v) -> UID -> Gam k v -> Gam k v
gamInst1Exists (extr,upd) u
  =  let  ex = foldr  (\(n,t) (u,ts)
                          ->  let  (u',ue) = mkNewLevUID u
                              in   (u', (n,upd t (tyInst1Exists ue (extr t))) : ts)
                      )
                      (u,[])
     in   assocLToGam . snd . ex . gamToAssocL

valGamInst1Exists :: UID -> ValGam -> ValGam
valGamInst1Exists = gamInst1Exists (vgiTy,(\vgi t -> vgi {vgiTy=t}))
%%]

%%[4_2.valGamInst1ExistsWithCnstr
valGamInst1ExistsWithCnstr :: Cnstr -> UID -> ValGam -> (ValGam,Cnstr)
valGamInst1ExistsWithCnstr
  =  valGamDoWithCnstr
        (\t u ->  let  (u',ue) = mkNewLevUID u
                  in   (tyInst1Exists ue t,u')
        )
%%]

%%[6
tyGamInst1Exists :: UID -> TyGam -> TyGam
tyGamInst1Exists = gamInst1Exists (tgiKi,(\tgi k -> tgi {tgiKi=k}))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Kind of type" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.TyGamInfo
data TyGamInfo = TyGamInfo { tgiTy :: Ty } deriving Show
%%]

%%[1.tyGamLookup
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing | hsnIsProd nm   -> Just (TyGamInfo (Ty_Con nm))
       Just tgi                 -> Just tgi
       _                        -> Nothing
%%]

%%[1.TyGam
type TyGam = Gam HsName TyGamInfo
%%]

%%[6.TyGamInfo -1.TyGamInfo
data TyGamInfo = TyGamInfo { tgiTy :: Ty, tgiKi :: Ty } deriving Show

mkTGI :: Ty -> Ty -> TyGamInfo
mkTGI t k = TyGamInfo t k
%%]

%%[7.TyGamInfo -6.TyGamInfo
data TyGamInfo = TyGamInfo { tgiTy :: Ty, tgiKi :: Ty, tgiData :: Ty } deriving Show

mkTGIData :: Ty -> Ty -> Ty -> TyGamInfo
mkTGIData t k d = TyGamInfo t k d

mkTGI :: Ty -> Ty -> TyGamInfo
mkTGI t k = mkTGIData t k Ty_Any
%%]

%%[8.TyGamInfo -7.TyGamInfo
type DataTagMp = FiniteMap HsName CTag

data TyGamInfo = TyGamInfo { tgiTy :: Ty, tgiKi :: Ty, tgiData :: Ty, tgiDataTagMp :: DataTagMp }

instance Show TyGamInfo where
  show _ = "TyGamInfo"

mkTGIData :: Ty -> Ty -> Ty -> DataTagMp -> TyGamInfo
mkTGIData t k d m = TyGamInfo t k d m

mkTGI :: Ty -> Ty -> TyGamInfo
mkTGI t k = mkTGIData t k Ty_Any emptyFM
%%]

%%[6.tyGamLookup -1.tyGamLookup
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
                 -> Just (TyGamInfo  (Ty_Con nm)
                                     (replicate (hsnProdArity nm) kiStar `mkTyArrow` kiStar))
       Just tgi  -> Just tgi
       _         -> Nothing
%%]

%%[7.tyGamLookup -6.tyGamLookup
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup = gamLookup
%%]

%%[6.tyGamQuantify
tyGamQuantify :: TyVarIdL -> TyGam -> TyGam
tyGamQuantify globTvL
  = gamMap (\(n,k) -> (n,k {tgiKi = kiQuantify (`elem` globTvL) (tgiKi k)}))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Ty app spine" gam, to be merged with tyGam in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.AppSpineGam
data AppSpineGamInfo = AppSpineGamInfo { asgiInfoL :: [AppSpineInfo] }

type AppSpineGam = Gam HsName AppSpineGamInfo

asGamLookup :: HsName -> AppSpineGam -> [AppSpineInfo]
asGamLookup nm g
  =  case gamLookup nm g of
        Just ccgi                ->  asgiInfoL ccgi
        Nothing | hsnIsProd nm   ->  take (hsnProdArity nm) prodAppSpineInfoL
        _                        ->  unknownAppSpineInfoL
%%]

%%[4.appSpineGam
appSpineGam :: AppSpineGam
appSpineGam =  assocLToGam [(hsnArrow, AppSpineGamInfo arrowAppSpineInfoL)]
%%]

%%[7.appSpineGam -4.appSpineGam
appSpineGam :: AppSpineGam
appSpineGam =  assocLToGam
                 [ (hsnArrow,    AppSpineGamInfo arrowAppSpineInfoL)
                 , (hsnRec,      AppSpineGamInfo (take 1 prodAppSpineInfoL))
                 ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Sort of kind" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
data KiGamInfo = KiGamInfo { kgiKi :: Ty } deriving Show

type KiGam = Gam HsName KiGamInfo
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances for Substitutable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2
instance Substitutable v => Substitutable (Gam k v) where
  s |=> (Gam ll)    =   Gam (map (assocLMapSnd (s |=>)) ll)
  ftv   (Gam ll)    =   unionL . map ftv . map snd . concat $ ll

instance Substitutable ValGamInfo where
  s |=> vgi         =   vgi { vgiTy = s |=> vgiTy vgi }
  ftv   vgi         =   ftv (vgiTy vgi)
%%]

%%[6
instance Substitutable TyGamInfo where
  s |=> tgi         =   tgi { tgiKi = s |=> tgiKi tgi }
  ftv   tgi         =   ftv (tgiKi tgi)
%%]

%%[9
gamSubstTop :: Substitutable v => Cnstr -> Gam k v -> Gam k v
gamSubstTop s g
  =  let  (h,t) = gamPop g
     in   (s |=> h) `gamPushGam` t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ppGam
ppGam :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam g = ppAssocL (gamToAssocL g)

instance (PP k, PP v) => PP (Gam k v) where
  pp = ppGam

instance PP ValGamInfo where
  pp vgi = ppTy (vgiTy vgi)
%%]

%%[4.PP.TyGamInfo
instance PP TyGamInfo where
  pp tgi = ppTy (tgiTy tgi)
%%]

%%[6.PP.TyGamInfo -4.PP.TyGamInfo
instance PP TyGamInfo where
  pp tgi = ppTy (tgiTy tgi) >|< "/" >|< ppTy (tgiKi tgi)
%%]
