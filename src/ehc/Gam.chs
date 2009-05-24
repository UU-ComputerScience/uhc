%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma (aka Assumptions, Environment)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam} import(Data.List,EH.Util.Utils,{%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}NameAspect}) export(Gam,emptyGam,gamMap,gamLookup,gamLookupDup, gamPushNew, gamPop, gamTop, gamAddGam, gamAdd, gamPushGam, gamToAssocL, gamToAssocDupL, assocLToGam, assocDupLToGam,gamKeys)
%%]

%%[(1 hmtyinfer || hmtyast).Ty import({%{EH}Ty})
%%]

%%[4 -1.Ty import({%{EH}Ty})
%%]

%%[1 import({%{EH}Error}) 
%%]

%%[1 export(TyGam, TyGamInfo(..), tyGamLookup, initTyGam)
%%]

%%[1 export(FixityGam, FixityGamInfo(..), defaultFixityGamInfo)
%%]

%%[1 import(EH.Util.Pretty) export(ppGam,ppGamDup)
%%]

%%[(1 hmtyinfer || hmtyast) import({%{EH}Ty.Pretty})
%%]

%%[1 export(gamSingleton,gamInsert,gamUnion,gamUnions,gamFromAssocL)
%%]

%%[1 export(IdDefOccGam,IdDefOccAsc)
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[(2 hmtyinfer || hmtyast) import({%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(3 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.Quantify})
%%]

%%[3 export(gamPartition)
%%]

%%[4 import({%{EH}Base.Opts})
%%]

%%[(4 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.Instantiate})
%%]

%%[(4 hmtyinfer) import({%{EH}Ty.FitsInCommon})
%%]

%%[4 export(gamMapThr)
%%]

%%[4_2 export(ErrGam)
%%]

%%[4_2 export(valGamInst1ExistsWithVarMp)
%%]

%%[6 export(gamUnzip)
%%]

%%[7 import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map) export(gamNoDups)
%%]

%%[(8 codegen) import({%{EH}Core})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty.FitsInCommon})
%%]

%%[9.ScopeMapGam import({%{EH}Gam.ScopeMapGam})
%%]

%%[9 import({%{EH}Base.Debug})
%%]

%%[(9 codegen) import({%{EH}Core.Subst})
%%]
%%[(9 hmtyinfer || hmtyast) hs import({%{EH}VarLookup})
%%]

%%[(9 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.MergePreds})
%%]

%%[9 export(idDefOccGamPartitionByKind)
%%]

%%[20 export(idDefOccGamByKind)
%%]

%%[99 import({%{EH}Base.ForceEval})
%%]

%%[(99 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Base.type
newtype Gam k v     =   Gam [AssocL k v]  deriving Show
%%]

%%[9.Base.type -1.Base.type
type Gam k v        =   SGam k v
%%]

%%[9999 -(9.Base.type 1.Base.type)
type Gam k v        =   SGam k v
%%]

%%[1.Base.sigs
emptyGam            ::            Gam k v
gamSingleton        ::            k -> v        -> Gam k v
gamLookup           ::  Ord k =>  k -> Gam k v  -> Maybe v
gamToAssocL         ::  Ord k =>  Gam k v       -> AssocL k v
gamPushNew          ::            Gam k v       -> Gam k v
gamPushGam          ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamPop              ::  Ord k =>  Gam k v       -> (Gam k v,Gam k v)
gamAddGam           ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamAdd              ::  Ord k =>  k -> v        -> Gam k v -> Gam k v
%%]

%%[1.Base.funs
emptyGam                            = Gam [[]]
gamSingleton    k v                 = Gam [[(k,v)]]
gamLookup       k (Gam ll)          = foldr  (\l mv -> maybe mv Just (lookup k l))
                                             Nothing ll
gamToAssocL     (Gam ll)            = concat ll
gamPushNew      (Gam ll)            = Gam ([]:ll)
gamPushGam      g1 (Gam ll2)        = Gam (gamToAssocL g1 : ll2)
gamPop          (Gam (l:ll))        = (Gam [l],Gam ll)
gamAddGam       g1 (Gam (l2:ll2))   = Gam ((gamToAssocL g1 ++ l2):ll2)
gamAdd          k v g               = gamAddGam (k `gamSingleton` v) g
%%]

%%[9
gamToAssocL     g                   = [ (k,v) | (k,(v:_)) <- gamToAssocDupL g ]
gamLookup       k g                 = fmap head $ gamLookupDup k g
assocLToGam                         = gamUnions . map (uncurry gamSingleton)
%%]

%%[9.Base.funs -1.Base.funs
emptyGam                            = emptySGam
gamSingleton                        = sgamSingleton
gamPushNew      g                   = sgamPushNew g
gamPushGam      g1 g2               = sgamPushGam g1 g2
gamPop          g                   = sgamPop g
gamAddGam       g1 g2               = sgamUnion g1 g2
gamAdd          k v g               = sgamUnion (sgamSingleton k v) g
%%]

%%[9999 -(9.Base.funs 1.Base.funs)
emptyGam                            = emptySGam
gamSingleton                        = sgamSingleton
gamPushNew      g                   = sgamPushNew g
gamPushGam      g1 g2               = sgamPushGam g1 g2
gamPop          g                   = sgamPop g
gamAddGam       g1 g2               = sgamUnion g1 g2
gamAdd          k v g               = sgamUnion (sgamSingleton k v) g
%%]

%%[1.Rest.sigs
gamTop              ::  Ord k =>  Gam k v     -> Gam k v
assocLToGam         ::  Ord k =>  AssocL k v  -> Gam k v
%%]

%%[1.Rest.funs
gamTop                              = fst . gamPop
assocLToGam     l                   = Gam [l]
%%]

%%[9.Rest.funs -1.Rest.funs
gamTop                              = sgamTop
%%]

%%[9999 -(9.Rest.funs 1.Rest.funs)
gamTop                              = sgamTop
%%]

%%[1.assocDupLToGam
assocDupLToGam :: Ord k => AssocL k [v] -> Gam k v
assocDupLToGam = assocLToGam . concat . map (\(k,vs) -> zip (repeat k) vs)
%%]

%%[9.assocDupLToGam -1.assocDupLToGam
assocDupLToGam :: Ord k => AssocL k [v] -> Gam k v
assocDupLToGam = sgamFromAssocDupL
%%]

%%[9999 -(9.assocDupLToGam 1.assocDupLToGam)
assocDupLToGam :: Ord k => AssocL k [v] -> Gam k v
assocDupLToGam = sgamFromAssocDupL
%%]

%%[1 export(assocLToGamWithDups)
assocLToGamWithDups :: Ord k => AssocL k v -> Gam k v
assocLToGamWithDups = assocDupLToGam . assocLGroupSort
%%]

%%[1.gamToAssocDupL
gamToAssocDupL :: Ord k => Gam k v -> AssocL k [v]
gamToAssocDupL = assocLGroupSort . gamToAssocL
%%]

%%[9.gamToAssocDupL -1.gamToAssocDupL
gamToAssocDupL :: Ord k => Gam k v -> AssocL k [v]
gamToAssocDupL g = sgamToAssocDupL g
%%]

%%[9999 -(9.gamToAssocDupL 1.gamToAssocDupL)
gamToAssocDupL :: Ord k => Gam k v -> AssocL k [v]
gamToAssocDupL g = sgamToAssocDupL g
%%]

%%[1.gamToOnlyDups export(gamToOnlyDups)
gamToOnlyDups :: Ord k => Gam k v -> AssocL k [v]
gamToOnlyDups g = [ x | x@(n,(_:_:_)) <- gamToAssocDupL g ]
%%]

%%[1.gamNoDups
gamNoDups :: Ord k => Gam k v -> Gam k v
gamNoDups (Gam ll) = Gam (map (nubBy (\(k1,_) (k2,_) -> k1 == k2)) ll)
%%]

%%[9.gamNoDups -1.gamNoDups
gamNoDups :: Ord k => Gam k v -> Gam k v
gamNoDups g = sgamNoDups g
%%]

%%[9999 -(9.gamNoDups 1.gamNoDups)
gamNoDups :: Ord k => Gam k v -> Gam k v
gamNoDups g = sgamNoDups g
%%]

%%[1.gamMap
gamMap :: ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap f (Gam ll) = Gam (map (map f) ll)
%%]

%%[9.gamMap -1.gamMap
gamMap :: (Ord k,Ord k') => ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap = sgamMap
%%]

%%[9999 -(9.gamMap 1.gamMap)
gamMap :: (Ord k,Ord k') => ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap = sgamMap
%%]

%%[3.gamMapElts export(gamMapElts)
gamMapElts :: Ord k => (v -> v') -> Gam k v -> Gam k v'
gamMapElts f = gamMap (\(n,v) -> (n,f v))
%%]

%%[3.gamPartition
gamPartition :: (k -> v -> Bool) -> Gam k v -> (Gam k v,Gam k v)
gamPartition f (Gam ll)
  = (Gam ll1,Gam ll2)
  where (ll1,ll2)
           = unzip $ map (partition (\(k,v) -> f k v)) $ ll
%%]

%%[9.gamPartition -3.gamPartition
gamPartition :: Ord k => (k -> v -> Bool) -> Gam k v -> (Gam k v,Gam k v)
gamPartition = sgamPartitionWithKey
%%]

%%[9999 -(9.gamPartition 3.gamPartition)
gamPartition :: Ord k => (k -> v -> Bool) -> Gam k v -> (Gam k v,Gam k v)
gamPartition = sgamPartitionWithKey
%%]

%%[4.gamMapThr
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

%%[9.gamMapThr -4.gamMapThr
gamMapThr :: (Ord k,Ord k') => ((k,v) -> t -> ((k',v'),t)) -> t -> Gam k v -> (Gam k' v',t)
gamMapThr = sgamMapThr
%%]

%%[9999 -(9.gamMapThr 4.gamMapThr)
gamMapThr :: (Ord k,Ord k') => ((k,v) -> t -> ((k',v'),t)) -> t -> Gam k v -> (Gam k' v',t)
gamMapThr = sgamMapThr
%%]

%%[1
gamKeys :: Ord k => Gam k v -> [k]
gamKeys = assocLKeys . gamToAssocL
%%]

%%[9 export(gamElts)
gamElts :: Ord k => Gam k v -> [v]
gamElts = assocLElts . gamToAssocL
%%]

%%[1.gamLookupDup
gamLookupDup :: Ord k => k -> Gam k v -> Maybe [v]
gamLookupDup k (Gam ll) = foldr (\l mv -> case filter ((==k) . fst) l of {[] -> mv; lf -> Just (assocLElts lf)}) Nothing ll
%%]

%%[9.gamLookupDup -1.gamLookupDup
gamLookupDup :: Ord k => k -> Gam k v -> Maybe [v]
gamLookupDup k g = sgamLookupDup k g
%%]

%%[9999 -(9.gamLookupDup 1.gamLookupDup)
gamLookupDup :: Ord k => k -> Gam k v -> Maybe [v]
gamLookupDup k g = sgamLookupDup k g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Aliases, for future compliance with naming conventions of (e.g.) Data.Map
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
gamInsert :: Ord k => k -> v -> Gam k v -> Gam k v
gamInsert = gamAdd

gamUnion :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnion = gamAddGam

gamFromAssocL ::  Ord k =>  AssocL k v  -> Gam k v
gamFromAssocL = assocLToGam
%%]

%%[1
gamUnions :: Ord k => [Gam k v] -> Gam k v
gamUnions [] = emptyGam
gamUnions gs = foldr1 gamUnion gs
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Error" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4_2
type ErrGam = Gam HsName ErrL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fixity gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
data FixityGamInfo = FixityGamInfo { fgiPrio :: !Int, fgiFixity :: !Fixity } deriving Show

defaultFixityGamInfo = FixityGamInfo fixityMaxPrio Fixity_Infixl
%%]

%%[1.FixityGam
type FixityGam = Gam HsName FixityGamInfo
%%]

%%[1 export(fixityGamLookup)
fixityGamLookup :: HsName -> FixityGam -> FixityGamInfo
fixityGamLookup nm fg = maybe defaultFixityGamInfo id $ gamLookup nm fg
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

%%[(3 hmtyinfer || hmtyast).valGamQuantify export(valGamQuantify)
valGamQuantify :: Set.Set TyVarId -> ValGam -> ValGam
valGamQuantify globTvS = valGamMapTy (\t -> tyQuantify (`Set.member` globTvS) t)
%%]

%%[(4_2 hmtyinfer || hmtyast).valGamDoWithVarMp
valGamDoWithVarMp :: (Ty -> thr -> (Ty,thr)) -> VarMp -> thr -> ValGam -> (ValGam,VarMp)
valGamDoWithVarMp f gamVarMp thr gam
  =  let  (g,(_,c))
            =  gamMapThr
                    (\(n,vgi) (thr,c)
                        ->  let  t = vgiTy vgi
                                 (t',thr') = f (gamVarMp |=> t) thr
                                 (tg,cg) =  case t of
                                                Ty_Var v _ -> (t,v `varmpTyUnit` t')
                                                _ -> (t',emptyVarMp)
                            in   ((n,vgi {vgiTy = tg}),(thr',cg `varmpPlus` c))
                    )
                    (thr,emptyVarMp) gam
     in   (g,c)
%%]

%%[(4_2 hmtyinfer || hmtyast).valGamQuantifyWithVarMp export(valGamQuantifyWithVarMp)
valGamQuantifyWithVarMp :: VarMp -> TyVarIdL -> ValGam -> (ValGam,VarMp)
valGamQuantifyWithVarMp = valGamDoWithVarMp (\t globTvL -> (tyQuantify (`elem` globTvL) t,globTvL))
%%]

%%[6.gamUnzip
gamUnzip :: Gam k (v1,v2) -> (Gam k v1,Gam k v2)
gamUnzip (Gam ll)
  =  let  (ll1,ll2) = unzip . map (unzip . map (\(n,(v1,v2)) -> ((n,v1),(n,v2)))) $ ll
     in   (Gam ll1,Gam ll2)
%%]

%%[9.gamUnzip -6.gamUnzip
gamUnzip :: Ord k => Gam k (v1,v2) -> (Gam k v1,Gam k v2)
gamUnzip g = sgamUnzip g
%%]

%%[9999 -(9.gamUnzip 6.gamUnzip)
gamUnzip :: Ord k => Gam k (v1,v2) -> (Gam k v1,Gam k v2)
gamUnzip g = sgamUnzip g
%%]

%%[(9 hmtyinfer || hmtyast).valGamQuantify -3.valGamQuantify export(valGamQuantify)
valGamQuantify :: Set.Set TyVarId -> [PredOcc] -> ValGam -> (ValGam,Gam HsName TyMergePredOut)
valGamQuantify globTvS prL g
  =  let  g' = gamMapElts  (\vgi ->  let  tmpo = tyMergePreds prL (vgiTy vgi)
                                          ty   = tyQuantify (`Set.member` globTvS) (tmpoTy tmpo)
                                     in   (vgi {vgiTy = ty},tmpo {tmpoTy = ty})
                           ) g
     in   gamUnzip g'
%%]

%%[(4 hmtyinfer).valGamInst1Exists export(gamInst1Exists,valGamInst1Exists)
gamInst1Exists :: Ord k => (v -> Ty,v -> Ty -> v) -> UID -> Gam k v -> Gam k v
gamInst1Exists (extr,upd) u
  =  fst . gamMapThr (\(n,t) u -> let (u',ue) = mkNewLevUID u in ((n,upd t (tyInst1Exists ue (extr t))),u')) u

valGamInst1Exists :: UID -> ValGam -> ValGam
valGamInst1Exists = gamInst1Exists (vgiTy,(\vgi t -> vgi {vgiTy=t}))
%%]

%%[66_4.valGamCloseExists
valGamCloseExists :: ValGam -> ValGam
valGamCloseExists = valGamMapTy (\t -> tyQuantify (not . tvIsEx (tyFtvMp t)) t)
%%]

%%[(4_2 hmtyinfer).valGamInst1ExistsWithVarMp
valGamInst1ExistsWithVarMp :: VarMp -> UID -> ValGam -> (ValGam,VarMp)
valGamInst1ExistsWithVarMp
  =  valGamDoWithVarMp
        (\t u ->  let  (u',ue) = mkNewLevUID u
                  in   (tyInst1Exists ue t,u')
        )
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Type of type" and "Kind of type" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.TyGamInfo
data TyGamInfo
  = TyGamInfo
%%[[(1 hmtyinfer || hmtyast)
      { tgiTy :: !Ty
      }
%%]]
      deriving Show
%%]

%%[(6 hmtyinfer || hmtyast).mkTGIData export(mkTGIData)
mkTGIData :: Ty -> Ty -> TyGamInfo
mkTGIData t _ = TyGamInfo t
%%]

%%[(6 hmtyinfer || hmtyast) export(mkTGI)
mkTGI :: Ty -> TyGamInfo
mkTGI t = mkTGIData t Ty_Any
%%]

%%[1.emtpyTGI export(emtpyTGI)
emtpyTGI :: TyGamInfo
emtpyTGI
  = TyGamInfo
%%[[(1 hmtyinfer || hmtyast)
      Ty_Any
%%]]
%%]

%%[1.TyGam
type TyGam = Gam HsName TyGamInfo
%%]

%%[1.tyGamLookup
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing | hsnIsProd nm 
%%[[(1 hmtyinfer || hmtyast) 
                 -> Just (TyGamInfo (Ty_Con nm))
%%][1
                 -> Just emtpyTGI
%%]]
       Just tgi  -> Just tgi
       _         -> Nothing
%%]

%%[1 export(tyGamLookupErr)
tyGamLookupErr :: HsName -> TyGam -> (TyGamInfo,ErrL)
tyGamLookupErr n g
  = case tyGamLookup n g of
      Nothing  -> (emtpyTGI,[rngLift emptyRange mkErr_NamesNotIntrod "type" [n]])
      Just tgi -> (tgi,[])
%%]

%%[6.tyGamLookup -1.tyGamLookup
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
%%[[(6 hmtyinfer || hmtyast) 
                 -> Just (TyGamInfo (Ty_Con nm))
%%][6
                 -> Just emtpyTGI
%%]]
       Just tgi  -> Just tgi
       _         -> Nothing
%%]

%%[7.tyGamLookup -6.tyGamLookup
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup = gamLookup
%%]

%%[(6 hmtyinfer || hmtyast) export(tyKiGamQuantify)
tyKiGamQuantify :: TyVarIdS -> TyKiGam -> TyKiGam
tyKiGamQuantify globTvS
  = gamMap (\(n,k) -> (n,k {tkgiKi = kiQuantify (`Set.member` globTvS) (tkgiKi k)}))
%%]

%%[(6 hmtyinfer) export(tyKiGamInst1Exists)
tyKiGamInst1Exists :: UID -> TyKiGam -> TyKiGam
tyKiGamInst1Exists = gamInst1Exists (tkgiKi,(\i k -> i {tkgiKi=k}))
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
      Just v  -> gamLookup (TyKiKey_TyVar v) g
      Nothing ->
                 case tyMbCon t of
                   Just n -> tyKiGamLookupByName n g
                   _      -> Nothing
%%]

%%[(6 hmtyinfer || hmtyast) export(tyKiGamLookupErr)
tyKiGamLookupErr :: Ty -> TyKiGam -> (TyKiGamInfo,ErrL)
tyKiGamLookupErr t g
  = case tyKiGamLookup t g of
      Nothing -> (emptyTKGI,[rngLift emptyRange mkErr_NamesNotIntrod "kind" [mkHNm $ show t]])
      Just i  -> (i,[])
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data tag/etc info gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(7 hmtyinfer) export(DataFldMp,DataFldInfo(..),emptyDataFldInfo)
data DataFldInfo
  = DataFldInfo
%%[[8
      { dfiOffset 	:: !Int
      }
%%]]
      deriving Show

type DataFldMp = Map.Map HsName DataFldInfo

emptyDataFldInfo
  = DataFldInfo
%%[[8
      (-1)
%%]]
%%]

%%[(7 hmtyinfer) export(DataTagInfo(..),emptyDataTagInfo,DataConstrTagMp)
data DataTagInfo
  = DataTagInfo
      { dtiFldMp    		:: !DataFldMp
      , dtiConNm			:: !HsName
%%[[8
      , dtiCTag 			:: !CTag
%%]]
%%[[95
      , dtiMbFixityPrio 	:: !(Maybe Int)
%%]]
      } deriving Show

type DataConstrTagMp = Map.Map HsName DataTagInfo

emptyDataTagInfo
  = DataTagInfo
      Map.empty hsnUnknown
%%[[8
      emptyCTag
%%]]
%%[[95
      Nothing
%%]]
%%]

%%[(8 hmtyinfer) export(dtiOffsetOfFld)
dtiOffsetOfFld :: HsName -> DataTagInfo -> Int
dtiOffsetOfFld fldNm dti = dfiOffset $ panicJust "dtiOffsetOfFld" $ Map.lookup fldNm $ dtiFldMp dti
%%]

%%[(8 hmtyinfer) export(DataFldInConstr(..),DataFldInConstrMp)
data DataFldInConstr
  = DataFldInConstr
      { dficInTagMp	:: !(Map.Map CTag Int)
      }

type DataFldInConstrMp = Map.Map HsName DataFldInConstr
%%]

%%[(7 hmtyinfer) export(DataGam,DataGamInfo(..),mkDGI)
data DataGamInfo
  = DataGamInfo
      { dgiTyNm      		:: !HsName
      , dgiDataTy 			:: !Ty
%%[[20
      , dgiConstrNmL 		:: ![HsName]
%%]]
      , dgiConstrTagMp 		:: !DataConstrTagMp
%%[[8
      , dgiFldInConstrMp	:: !DataFldInConstrMp
%%[[8
      , dgiIsNewtype 		:: !Bool
%%][94
      , dgiMbNewtype 		:: !(Maybe Ty)			-- the type lambda corresponding to a newtype
%%]]
      , dgiMaxConstrArity   :: !Int
%%]]
      }


type DataGam = Gam HsName DataGamInfo

instance Show DataGamInfo where
  show _ = "DataGamInfo"

%%[[7
mkDGI :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> Bool -> DataGamInfo
%%][94
mkDGI :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> Maybe Ty -> DataGamInfo
%%]]
mkDGI tyNm dty cNmL m nt
  = DataGamInfo
      tyNm
      dty
%%[[20
      cNmL
%%]]
      m
%%[[8
      fm nt mx
  where fm = Map.map DataFldInConstr $ Map.unionsWith Map.union
             $ [ Map.singleton f (Map.singleton (dtiCTag ci) (dfiOffset fi)) | ci <- Map.elems m, (f,fi) <- Map.toList $ dtiFldMp ci ]
        mx = if Map.null m then (-1) else (ctagMaxArity $ dtiCTag $ head $ Map.elems m)
%%]]
%%]

%%[7 export(mkDGIPlain)
mkDGIPlain :: HsName -> Ty -> [HsName] -> DataConstrTagMp -> DataGamInfo
mkDGIPlain tyNm dty cNmL m
  = mkDGI tyNm dty cNmL m
%%[[7
          False
%%][94
          Nothing
%%]]

%%]

%%[94 export(dgiIsNewtype)
dgiIsNewtype :: DataGamInfo -> Bool
dgiIsNewtype = isJust . dgiMbNewtype
%%]

%%[(7 hmtyinfer) export(emptyDataGamInfo,emptyDGI)
emptyDataGamInfo, emptyDGI :: DataGamInfo
emptyDataGamInfo = mkDGI hsnUnknown Ty_Any [] Map.empty
%%[[7
                         False
%%][94
                         Nothing
%%]]
emptyDGI = emptyDataGamInfo
%%]

%%[(20 hmtyinfer) export(dgiConstrTagAssocL)
dgiConstrTagAssocL :: DataGamInfo -> AssocL HsName DataTagInfo
dgiConstrTagAssocL dgi = [ (cn,panicJust "dgiConstrTagAssocL" $ Map.lookup cn $ dgiConstrTagMp dgi) | cn <- dgiConstrNmL dgi ]
%%]

%%[(7 hmtyinfer) export(dgiDtiOfCon)
dgiDtiOfCon :: HsName -> DataGamInfo -> DataTagInfo
dgiDtiOfCon conNm dgi = panicJust "dgiDtiOfCon" $ Map.lookup conNm $ dgiConstrTagMp dgi
%%]

%%[(7 hmtyinfer) export(dataGamLookup,dataGamLookupErr)
dataGamLookup :: HsName -> DataGam -> Maybe DataGamInfo
dataGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm							-- ??? should not be necessary, in variant 7 where tuples are represented by records
                 -> Just emptyDataGamInfo
       Just dgi  -> Just dgi
       _         -> Nothing

dataGamLookupErr :: HsName -> DataGam -> (DataGamInfo,ErrL)
dataGamLookupErr n g
  = case dataGamLookup n g of
      Nothing  -> (emptyDGI,[rngLift emptyRange mkErr_NamesNotIntrod "data" [n]])
      Just tgi -> (tgi,[])
%%]

%%[(7 hmtyinfer) export(dataGamDgiOfTy)
dataGamDgiOfTy :: Ty -> DataGam -> Maybe DataGamInfo
dataGamDgiOfTy conTy dg = dataGamLookup (tyAppFunConNm conTy) dg
%%]

%%[(8 hmtyinfer) export(dataGamDTIsOfTy)
dataGamDTIsOfTy :: Ty -> DataGam -> Maybe [DataTagInfo]
dataGamDTIsOfTy t g
  = fmap
%%[[8
      (Map.elems . dgiConstrTagMp)
%%][95
      (assocLElts . dgiConstrTagAssocL)
%%]]
    $ gamLookup (tyAppFunConNm t)
    $ g
%%]

%%[(8 hmtyinfer) export(dataGamTagsOfTy)
dataGamTagsOfTy :: Ty -> DataGam -> Maybe [CTag]
dataGamTagsOfTy t g
  = fmap (map dtiCTag) (dataGamDTIsOfTy t g)
%%]

Lookup by constructor name:

%%[8
%%]
valDataGamLookup :: HsName -> ValGam -> DataGam -> Maybe DataGamInfo
valDataGamLookup nm vg dg
  = do { vgi <- valGamLookup nm vg
       ; dgi <- dataGamDgiOfTy (vgiTy vgi) dg
       }

Is datatype an enum? I.e. has no field for any constructor.

%%[(8 codegen) export(dgiIsEnumable)
dgiIsEnumable :: DataGamInfo -> Bool
dgiIsEnumable dgi = dgiMaxConstrArity dgi == 0
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Ty app spine" gam, to be merged with tyGam in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(4 hmtyinfer).AppSpineGam export(AppSpineGam, asGamLookup)
type AppSpineGam = Gam HsName AppSpineInfo

asGamLookup :: HsName -> AppSpineGam -> Maybe AppSpineInfo
asGamLookup nm g
  = case gamLookup nm g of
      j@(Just _)             -> j
      Nothing | hsnIsProd nm -> Just $ emptyAppSpineInfo {asgiVertebraeL = take (hsnProdArity nm) prodAppSpineVertebraeInfoL}
      _                      -> Nothing

%%]

%%[(4 hmtyinfer).appSpineGam export(appSpineGam)
appSpineGam :: AppSpineGam
appSpineGam =  assocLToGam [(hsnArrow, emptyAppSpineInfo {asgiVertebraeL = arrowAppSpineVertebraeInfoL})]
%%]

%%[(7 hmtyinfer).appSpineGam -4.appSpineGam export(appSpineGam)
appSpineGam :: AppSpineGam
appSpineGam
  = assocLToGam
      [ (hsnArrow       , emptyAppSpineInfo {asgiVertebraeL = arrowAppSpineVertebraeInfoL})
      , (hsnRec         , emptyAppSpineInfo {asgiVertebraeL = take 1 prodAppSpineVertebraeInfoL})
%%[[18
      , (hsnRecUnboxed  , emptyAppSpineInfo {asgiVertebraeL = take 1 prodAppSpineVertebraeInfoL})
%%]]
      ]
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
%%% Identifier definition occurrence gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
type IdDefOccGam = Gam    IdOcc  IdDefOcc
type IdDefOccAsc = AssocL IdOcc [IdDefOcc]
%%]

%%[9
idDefOccGamPartitionByKind :: [IdOccKind] -> IdDefOccGam -> (IdDefOccAsc,IdDefOccAsc)
idDefOccGamPartitionByKind ks
  = partition (\(IdOcc n k',_) -> k' `elem` ks) . gamToAssocDupL
%%]

%%[20
idDefOccGamByKind :: IdOccKind -> IdDefOccGam -> AssocL HsName IdDefOcc
idDefOccGamByKind k g = [ (n,head i) | (IdOcc n _,i) <- fst (idDefOccGamPartitionByKind [k] g) ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identifier definition additional aspect gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

not used

%%[1 export(IdDefAspGam,IdDefAspAsc)
type IdDefAspGam = Gam    IdOcc  IdDefAsp
type IdDefAspAsc = AssocL IdOcc [IdDefAsp]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identifier unqualified to qualified gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(IdQualGam)
type IdQualGam = Gam IdOcc HsName
%%]

%%[20 export(idGam2QualGam,idQualGamReplacement)
idGam2QualGam :: IdDefOccGam -> IdQualGam
idGam2QualGam = gamMap (\(iocc,docc) -> (iocc {ioccNm = hsnQualified $ ioccNm iocc},ioccNm $ doccOcc $ docc))

idQualGamReplacement :: IdQualGam -> IdOccKind -> HsName -> HsName
idQualGamReplacement g k n = maybe n id $ gamLookup (IdOcc n k) g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances for Substitutable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2 hmtyinfer || hmtyast).Substitutable.Gam
instance (Eq k,Eq tk,Substitutable vv k subst) => Substitutable (Gam tk vv) k subst where
  s |=>  (Gam ll)    =   Gam (map (assocLMapElt (s |=>)) ll)
%%[[4
  s |==> (Gam ll)    =   (Gam ll',varmpUnions $ map (varmpUnions . assocLElts) m)
                     where (ll',m) = unzip $ map (assocLMapUnzip . assocLMapElt (s |==>)) ll
%%]]
  ftvSet (Gam ll)    =   Set.unions . map ftvSet . map snd . concat $ ll
%%]

%%[(9 hmtyinfer || hmtyast).Substitutable.SGam -2.Substitutable.Gam
instance (Ord tk,Ord k,Substitutable vv k subst) => Substitutable (SGam tk vv) k subst where
  s |=>  g    =   gamMapElts (s |=>) g
%%[[4
  s |==> g    =   (g',varmpUnions $ gamElts gm)
              where (g',gm) = sgamUnzip $ gamMapElts (s |==>) g
%%]]
  ftvSet g    =   Set.unions $ map ftvSet $ gamElts g
%%]

%%[(9999 hmtyinfer || hmtyast).Substitutable.SGam -(9.Substitutable.SGam 2.Substitutable.Gam)
instance (Ord tk,Ord k,Substitutable vv k subst) => Substitutable (SGam tk vv) k subst where
  s |=>  g    =   gamMapElts (s |=>) g
%%[[4
  s |==> g    =   (g',varmpUnions $ gamElts gm)
              where (g',gm) = sgamUnzip $ gamMapElts (s |==>) g
%%]]
  ftvSet g    =   Set.unions $ map ftvSet $ gamElts g
%%]

%%[(2 hmtyinfer || hmtyast).Substitutable.inst.ValGamInfo
instance Substitutable ValGamInfo TyVarId VarMp where
  s |=>  vgi         =   vgi { vgiTy = s |=> vgiTy vgi }
%%[[4
  s |==> vgi         =   substLift vgiTy (\i x -> i {vgiTy = x}) (|==>) s vgi
%%]]
  ftvSet vgi         =   ftvSet (vgiTy vgi)
%%]

%%[(2 hmtyinfer || hmtyast).Substitutable.inst.TyGamInfo
instance Substitutable TyGamInfo TyVarId VarMp where
  s |=>  tgi         =   tgi { tgiTy = s |=> tgiTy tgi }
%%[[4
  s |==> tgi         =   substLift tgiTy (\i x -> i {tgiTy = x}) (|==>) s tgi
%%]]
  ftvSet tgi         =   ftvSet (tgiTy tgi)
%%]

%%[(6 hmtyinfer || hmtyast).Substitutable.inst.TyKiGamInfo
instance Substitutable TyKiGamInfo TyVarId VarMp where
  s |=>  tkgi         =   tkgi { tkgiKi = s |=> tkgiKi tkgi }
%%[[4
  s |==> tkgi         =   substLift tkgiKi (\i x -> i {tkgiKi = x}) (|==>) s tkgi
%%]]
  ftvSet tkgi         =   ftvSet (tkgiKi tkgi)
%%]

%%[(17 hmtyinfer || hmtyast).Substitutable.inst.PolGamInfo
instance Substitutable PolGamInfo TyVarId VarMp where
  s |=> pgi  = pgi { pgiPol = s |=> pgiPol pgi }
  s |==> pgi = substLift pgiPol (\i x -> i {pgiPol = x}) (|==>) s pgi
  ftvSet pgi = ftvSet (pgiPol pgi)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ppGam
ppGam :: (Ord k, PP k, PP v) => Gam k v -> PP_Doc
ppGam g = ppAssocL (gamToAssocL g)
%%]

%%[1
ppGamDup :: (Ord k,PP k, PP v) => Gam k v -> PP_Doc
ppGamDup g = ppAssocL $ map (\(k,v) -> (k,ppBracketsCommas v)) $ gamToAssocDupL $ g
%%]

%%[1.PP.Gam
instance (Ord k, PP k, PP v) => PP (Gam k v) where
  pp = ppGam
%%]

%%[9.PP.Gam -1.PP.Gam
instance (Ord k, PP k, PP v) => PP (SGam k v) where
  pp g = ppGam g
%%]

%%[9999 -(9.PP.Gam 1.PP.Gam)
instance (Ord k, PP k, PP v) => PP (SGam k v) where
  pp g = ppGam g
%%]

%%[(1 hmtyinfer || hmtyast).PP.ValGamInfo
instance PP ValGamInfo where
  pp vgi = ppTy (vgiTy vgi)
%%]

%%[(1 hmtyinfer || hmtyast).PP.TyGamInfo
instance PP TyGamInfo where
  pp tgi = ppTy (tgiTy tgi)
%%]

%%[(6 hmtyinfer || hmtyast)
instance PP TyKiGamInfo where
  pp i = ppTy (tkgiKi i)
%%]

%%[(17 hmtyinfer || hmtyast)
instance PP PolGamInfo where
  pp i = ppTy (pgiPol i)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer)
instance ForceEval DataFldInfo
%%[[102
  where
    fevCount (DataFldInfo x) = cm1 "DataFldInfo" `cmUnion` fevCount x
%%]]

instance ForceEval DataTagInfo where
  forceEval x@(DataTagInfo m n t p) | forceEval m `seq` forceEval p `seq` True = x
%%[[102
  fevCount (DataTagInfo m n t p) = cmUnions [cm1 "DataTagInfo",fevCount m,fevCount n,fevCount t,fevCount p]
%%]]

instance ForceEval DataFldInConstr where
  forceEval x@(DataFldInConstr m) | forceEval m `seq` True = x
%%[[102
  fevCount (DataFldInConstr x) = cm1 "DataFldInConstr" `cmUnion` fevCount x
%%]]

instance ForceEval DataGamInfo where
  forceEval x@(DataGamInfo n t nl tm cm nt mx) | forceEval nl `seq` forceEval tm `seq` forceEval cm `seq` True = x
%%[[102
  fevCount (DataGamInfo n t nl tm cm nt mx) = cmUnions [cm1 "DataGamInfo",fevCount n,fevCount t,fevCount nl,fevCount tm,fevCount cm,fevCount nt,fevCount mx]
%%]]

instance ForceEval PolGamInfo where
  forceEval x@(PolGamInfo p) | forceEval p `seq` True = x
%%[[102
  fevCount (PolGamInfo p) = cmUnions [cm1 "PolGamInfo",fevCount p]
%%]]
%%]

%%[(99 hmtyinfer || hmtyast)
instance ForceEval ValGamInfo where
  forceEval x@(ValGamInfo t) | forceEval t `seq` True = x
%%[[102
  fevCount (ValGamInfo x) = cm1 "ValGamInfo" `cmUnion` fevCount x
%%]]

instance ForceEval KiGamInfo where
  forceEval x@(KiGamInfo k) | forceEval k `seq` True = x
%%[[102
  fevCount (KiGamInfo x) = cm1 "KiGamInfo" `cmUnion` fevCount x
%%]]

instance ForceEval TyKiGamInfo where
  forceEval x@(TyKiGamInfo k) | forceEval k `seq` True = x
%%[[102
  fevCount (TyKiGamInfo x) = cm1 "TyKiGamInfo" `cmUnion` fevCount x
%%]]

instance ForceEval TyKiKey
%%[[102
  where
    fevCount (TyKiKey_Name  n) = cm1 "TyKiKey_Name"  `cmUnion` fevCount n
    fevCount (TyKiKey_TyVar v) = cm1 "TyKiKey_TyVar" `cmUnion` fevCount v
%%]]

instance ForceEval TyGamInfo where
  forceEval x@(TyGamInfo t) | forceEval t `seq` True = x
%%[[102
  fevCount (TyGamInfo x) = cm1 "TyGamInfo" `cmUnion` fevCount x
%%]]

%%]

%%[99
instance ForceEval FixityGamInfo
%%[[102
  where
    fevCount (FixityGamInfo p f) = cm1 "FixityGamInfo" `cmUnion` fevCount p `cmUnion` fevCount f
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of tyGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.initTyGam
initTyGam :: TyGam
initTyGam
  = assocLToGam
%%[[(1 hmtyinfer || hmtyast)
      [ (hsnArrow,  TyGamInfo (Ty_Con hsnArrow))
      , (hsnInt,    TyGamInfo tyInt)
      , (hsnChar,   TyGamInfo tyChar)
      ]
%%][1
      [ (hsnArrow,  emtpyTGI)
      , (hsnInt,    emtpyTGI)
      , (hsnChar,   emtpyTGI)
      ]
%%]]
%%]

%%[6.initTyGam -1.initTyGam
initTyGam :: TyGam
initTyGam
  = assocLToGam
%%[[(6 hmtyinfer || hmtyast)
      [ (hsnArrow			, mkTGI (Ty_Con hsnArrow))
      , (hsnInt				, mkTGI tyInt)
      , (hsnChar			, mkTGI tyChar)
%%[[7
      , (hsnRow				, mkTGI (Ty_Con hsnUnknown))
      , (hsnRec				, mkTGI (Ty_Con hsnRec))
      , (hsnSum				, mkTGI (Ty_Con hsnSum))
%%]]
%%[[9
      , (hsnPrArrow			, mkTGI (Ty_Con hsnPrArrow))
%%]]
%%[[18
      , (hsnRecUnboxed		, mkTGI (Ty_Con hsnRecUnboxed))
      , (hsnIntUnboxed		, mkTGI tyIntUnboxed)
%%]]
%%[[97
      , (hsnInteger			, mkTGI tyInteger		)
      , (hsnInt8Unboxed  	, mkTGI (Ty_Con hsnInt8Unboxed  )	)
      , (hsnInt16Unboxed 	, mkTGI (Ty_Con hsnInt16Unboxed )	)
      , (hsnInt32Unboxed 	, mkTGI (Ty_Con hsnInt32Unboxed )	)
      , (hsnInt64Unboxed 	, mkTGI (Ty_Con hsnInt64Unboxed )	)
      , (hsnWordUnboxed  	, mkTGI (Ty_Con hsnWordUnboxed  )	)
      , (hsnWord8Unboxed 	, mkTGI (Ty_Con hsnWord8Unboxed )	)
      , (hsnWord16Unboxed	, mkTGI (Ty_Con hsnWord16Unboxed)	)
      , (hsnWord32Unboxed	, mkTGI (Ty_Con hsnWord32Unboxed)	)
      , (hsnWord64Unboxed	, mkTGI (Ty_Con hsnWord64Unboxed)	)
%%]]
%%[[99
      , (hsnAddrUnboxed		, mkTGI (Ty_Con hsnAddrUnboxed  )	)
%%]]  
      ]
%%][6
      zip [ hsnArrow, hsnInt, hsnChar
%%[[7
          , hsnRow, hsnRec, hsnSum
%%]]
%%[[9
          , hsnPrArrow
%%]]
%%[[18
          , hsnIntUnboxed, hsnRecUnboxed
%%]]
%%[[97
          , hsnInteger
          , tyInt8Unboxed, tyInt16Unboxed, tyInt32Unboxed, tyInt64Unboxed
          , tyWordUnboxed
          , tyWord8Unboxed, tyWord16Unboxed, tyWord32Unboxed, tyWord64Unboxed
%%]]
%%[[99
          , hsnAddrUnboxed
%%]]
          ]
          (repeat emtpyTGI)
%%]]
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
%%% Polarity gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(17 hmtyinfer || hmtyast) export(PolGamInfo(..), PolGam, initPolGam, mapPolGam, quantifyPolGam,mkPGI)
data PolGamInfo = PolGamInfo { pgiPol :: Polarity } deriving Show

mkPGI :: Ty -> PolGamInfo
mkPGI t = PolGamInfo t

emptyPGI :: PolGamInfo
emptyPGI = mkPGI Ty_Any

type PolGam = Gam HsName PolGamInfo

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
    quant = mkTyQu TyQu_Forall [u]
    var   = mkPolVar u
    quantvar = quant var

mapPolGam :: (Ty -> Ty) -> PolGam -> PolGam
mapPolGam f
  = fst . gamMapThr (\(nm, PolGamInfo ty) thr -> ((nm, PolGamInfo $ f ty), thr)) ()

quantifyPolGam :: PolGam -> PolGam
quantifyPolGam gam
  = let fvs = ftv gam
        notElemFtvs tv = not $ elem tv fvs
     in mapPolGam (tyQuantify notElemFtvs) gam
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


