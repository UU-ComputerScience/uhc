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

%%[(3333 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.Quantify})
%%]

%%[3 export(gamPartition)
%%]

%%[4 import({%{EH}Base.Opts})
%%]

%%[(4444 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.Instantiate})
%%]

%%[(4444 hmtyinfer) import({%{EH}Ty.FitsInCommon})
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

%%[(9999 hmtyinfer) import({%{EH}Ty.FitsInCommon})
%%]

%%[9.ScopeMapGam import({%{EH}Gam.ScopeMapGam})
%%]

%%[9 import({%{EH}Base.Debug})
%%]

%%[(9 codegen) import({%{EH}Core.Subst})
%%]
%%[(9 hmtyinfer || hmtyast) hs import({%{EH}VarLookup})
%%]


%%[9 export(idDefOccGamPartitionByKind)
%%]

%%[20 export(idDefOccGamByKind)
%%]

%%[9999 import({%{EH}Base.ForceEval})
%%]

%%[(9999 hmtyinfer || hmtyast) import({%{EH}Ty.Trf.ForceEval})
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

%%[6 export(gamElts)
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

%%[(8 hmtyinfer || hmtyast) export(gamDoTyWithVarMp)
-- Do something with each Ty in a Gam.
-- The global VarMp is kept separately so a new tyvar binding can be computed, which is threaded separatedly and also returned.
-- This allows the retainment of the original tyvar in the Gam, which is required when used twice with different VarMp's.
gamDoTyWithVarMp
  :: Ord key =>
     (info -> Ty,Ty -> info -> info)									-- get/set from/into info in Gam
     																	-- do whatever must be done given
     -> (key															-- 	 name in gam
         -> (Ty,VarMp)													--   Ty + cycles
         -> VarMp														--   new subst
         -> thr															--   thread
         -> (Ty,VarMp,thr))												--   result: new Ty, new subst, thread
     -> VarMp															-- subst for Gam entries
     -> thr																-- initial value for thread
     -> Gam key info													-- the Gam (env)
     -> (Gam key info,VarMp,thr)										-- result: new gam, new subst, thread
gamDoTyWithVarMp (get,set) f gamVarMp thr gam
  = (g,c,thr')
  where (g,(thr',c))
           = gamMapThr
               (\(n,gi) (thr,c)
                   -> let t = get gi
                          (t',c',thr') = f n (gamVarMp |==> t) c thr
                          (tg,cg)      = case (tyUnAnn t,tyUnAnn t') of
                                           (Ty_Var v1 _  ,Ty_Var v2 _) | v1 == v2
                                             -> dflt
                                           (Ty_Var v  cat,_          ) | not (tvCatIsFixed cat)
                                             -> (t ,v `varmpTyUnit` t')
                                           _ -> dflt
                                       where dflt = (t',emptyVarMp)
                      in  ((n,set ({- tr "gamDoTyWithVarMp.set" (ppTy tg) $ -} tg) gi),(thr',{- (\v -> tr "gamDoTyWithVarMp" (pp v) v) $ -} cg `varmpPlus` c'))
               )
               (thr,emptyVarMp) gam
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
%%% "XX of sort" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(SoGam, SoGamInfo(..))
data SoGamInfo
  = SoGamInfo
      { sgiSo :: Ty }
      deriving Show

type SoGam = Gam HsName SoGamInfo
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


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9999 hmtyinfer || hmtyast)
instance ForceEval TyKiKey
%%[[102
  where
    fevCount (TyKiKey_Name  n) = cm1 "TyKiKey_Name"  `cmUnion` fevCount n
    fevCount (TyKiKey_TyVar v) = cm1 "TyKiKey_TyVar" `cmUnion` fevCount v
%%]]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of soGam, only used by TyCore
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(initSoGam)
initSoGam :: SoGam
initSoGam
  = assocLToGam
      [ (hsnKindStar,   SoGamInfo kiStar)
      ]
%%]


