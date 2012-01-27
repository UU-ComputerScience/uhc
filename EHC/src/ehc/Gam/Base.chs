%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma (aka Assumptions, Environment)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam.Base} import(Data.List,EH.Util.Utils,{%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}NameAspect}) export(emptyGam,gamMap,gamLookup,gamLookupDup, gamPushNew, gamPop, gamTop, gamAddGam, gamAdd, gamPushGam, gamToAssocL, gamToAssocDupL, assocLToGam, assocDupLToGam,gamKeys)
%%]

%%[1 export(gamSingleton,gamInsert,gamUnion,gamUnions,gamFromAssocL)
%%]

%%[(2 hmtyinfer || hmtyast) import(qualified Data.Set as Set)
%%]

%%[3 export(gamPartition)
%%]

%%[4 import({%{EH}Opts.Base})
%%]

%%[4 export(gamMapThr)
%%]

%%[6 export(gamUnzip)
%%]

%%[7 import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map) export(gamNoDups)
%%]

%%[9.ScopeMapGam import({%{EH}Gam.ScopeMapGam})
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Base.type export(Gam(Gam))
newtype Gam k v     =   Gam [AssocL k v]  deriving Show
%%]

%%[9.Base.type -1.Base.type export(Gam)
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

