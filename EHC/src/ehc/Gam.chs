%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma (aka Assumptions, Environment)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Gam} import(Data.List,EH.Util.Utils,{%{EH}Base.Builtin},{%{EH}Base.Common},{%{EH}NameAspect}) export(Gam,emptyGam,gamMap,gamLookup,gamLookupDup, gamPushNew, gamPop, gamTop, gamAddGam, gamUnit, gamAdd, gamPushGam, gamToAssocL, gamToAssocDupL, assocLToGam, assocDupLToGam,gamKeys)
%%]

%%[1 import({%{EH}Ty},{%{EH}Error}) export(ValGam, ValGamInfo(..), valGamLookup,valGamLookupTy)
%%]

%%[1 export(TyGam, TyGamInfo(..), tyGamLookup, initTyGam)
%%]

%%[1 export(FixityGam, FixityGamInfo(..), defaultFixityGamInfo)
%%]

%%[1 import(UU.Pretty,EH.Util.PPUtils,{%{EH}Ty.Pretty}) export(ppGam,ppGamDup)
%%]

%%[1 export(gamSingleton,gamInsert,gamUnion,gamUnions,gamFromAssocL)
%%]

%%[1 export(IdDefOccGam,IdDefOccAsc)
%%]

%%[2 import({%{EH}Cnstr},{%{EH}Substitutable})
%%]

%%[3 import({%{EH}Ty.Quantify}) export(valGamQuantify,gamMapElts,valGamMapTy)
%%]

%%[3 export(gamPartition)
%%]

%%[4 import({%{EH}Base.Opts},{%{EH}Ty.Instantiate}) export(valGamInst1Exists)
%%]

%%[4 import({%{EH}Ty.FitsInCommon}) export(AppSpineGam, appSpineGam, asGamLookup)
%%]

%%[4 export(gamMapThr)
%%]

%%[4_2 export(ErrGam)
%%]

%%[4_2 export(valGamQuantifyWithCnstr,valGamInst1ExistsWithCnstr)
%%]

%%[6 export(tyGamQuantify, tyGamInst1Exists,gamUnzip)
%%]

%%[6 export(KiGam, KiGamInfo(..),initKiGam)
%%]

%%[6 export(mkTGI)
%%]

%%[7 import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map) export(gamNoDups)
%%]

%%[7777 export(mkTGIData)
%%]

%%[8 import({%{EH}Core}) export(gamUpd)
%%]

%%[9 import({%{EH}Base.Debug},{%{EH}Core.Subst},{%{EH}Ty.FitsInCommon}) export(gamUpdAdd,gamLookupAll,gamSubstTop,gamElts)
%%]

%%[9 export(TreeGam,emptyTGam,tgamSingleton,tgamLookup,tgamLookupAll,tgamLookupAllEmp,tgamElts,tgamMap,tgamPushNew,tgamAddGam,tgamPushGam,tgamAdd,tgamPop,tgamUpdAdd,tgamUpd,tgamMbUpd,tgamInScopes,tgamIsInScope,tgamToAssocL)
%%]

%%[9 export(ppTGam)
%%]

%%[9 export(idDefOccGamPartitionByKind)
%%]

%%[12 export(idDefOccGamByKind)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.Base.type
newtype Gam k v     =   Gam [AssocL k v]  deriving Show
%%]

%%[9.Base.type -1.Base.type
type Gam k v        =   TreeGam Int k v
%%]

%%[1.Base.sigs
emptyGam            ::            Gam k v
gamUnit             ::            k -> v        -> Gam k v
gamLookup           ::  Ord k =>  k -> Gam k v  -> Maybe v
gamToAssocL         ::            Gam k v       -> AssocL k v
gamPushNew          ::            Gam k v       -> Gam k v
gamPushGam          ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamPop              ::            Gam k v       -> (Gam k v,Gam k v)
gamAddGam           ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamAdd              ::  Ord k =>  k -> v        -> Gam k v -> Gam k v
%%]

%%[1.Base.funs
emptyGam                            = Gam [[]]
gamUnit         k v                 = Gam [[(k,v)]]
gamLookup       k (Gam ll)          = foldr  (\l mv -> maybe mv Just (lookup k l))
                                             Nothing ll
gamToAssocL     (Gam ll)            = concat ll
gamPushNew      (Gam ll)            = Gam ([]:ll)
gamPushGam      g1 (Gam ll2)        = Gam (gamToAssocL g1 : ll2)
gamPop          (Gam (l:ll))        = (Gam [l],Gam ll)
gamAddGam       g1 (Gam (l2:ll2))   = Gam ((gamToAssocL g1 ++ l2):ll2)
gamAdd          k v g               = gamAddGam (k `gamUnit` v) g
%%]

%%[9.Base.funs -1.Base.funs
emptyGam                            = emptyTGam 1 
gamUnit                             = tgamSingleton 1
gamLookup       k g                 = tgamLookup (tgamSize1 g) k g
gamToAssocL     g                   = tgamToAssocL (tgamSize1 g) g
gamPushNew      g                   = let sz = tgamSize1 g in tgamPushNew sz (sz+1) g
gamPushGam      g1 g2               = let sz = tgamSize1 g2 in tgamPushGam (tgamSize1 g1) sz (sz+1) g1 g2
gamPop          g                   = let (g1,_,g2) = tgamPop (tgamSize1 g) 1 g in (g1,g2)
gamAddGam       g1 g2               = tgamAddGam (tgamSize1 g1) (tgamSize1 g2) g1 g2
gamAdd          k v g               = tgamAdd (tgamSize1 g) k v g
%%]

%%[1.Rest.sigs
gamTop              ::            Gam k v     -> Gam k v
assocLToGam         ::  Ord k =>  AssocL k v  -> Gam k v
%%]

%%[1.Rest.funs
gamTop                              = fst . gamPop
assocLToGam     l                   = Gam [l]
%%]

%%[9.Rest.funs -1.Rest.funs
gamTop                              = fst . gamPop
assocLToGam                         = assocLToTGam 1 
%%]

%%[1.assocDupLToGam
assocDupLToGam :: Ord k => AssocL k [v] -> Gam k v
assocDupLToGam = assocLToGam . concat . map (\(k,vs) -> zip (repeat k) vs)
%%]

%%[9 -1.assocDupLToGam
assocDupLToGam :: Ord k => AssocL k [v] -> Gam k v
assocDupLToGam = assocDupLToTGam 1
%%]

%%[1.gamToAssocDupL
gamToAssocDupL :: Ord k => Gam k v -> AssocL k [v]
gamToAssocDupL = map (foldr (\(k,v) (_,vs) -> (k,v:vs)) (undefined,[])) . groupSortOn fst . gamToAssocL
%%]

%%[9 -1.gamToAssocDupL
gamToAssocDupL :: Ord k => Gam k v -> AssocL k [v]
gamToAssocDupL g = tgamToAssocDupL (tgamSize1 g) g
%%]

%%[1.gamToOnlyDups export(gamToOnlyDups)
gamToOnlyDups :: Ord k => Gam k v -> AssocL k [v]
gamToOnlyDups g = [ x | x@(n,(_:_:_)) <- gamToAssocDupL g ]
%%]

%%[1.gamNoDups
gamNoDups :: Ord k => Gam k v -> Gam k v
gamNoDups (Gam ll) = Gam (map (nubBy (\(k1,_) (k2,_) -> k1 == k2)) ll)
%%]

%%[9 -1.gamNoDups
gamNoDups :: Ord k => Gam k v -> Gam k v
gamNoDups g = tgamMap2 (tgamSize1 g) (\(k,v:_) -> (k,[v])) g
%%]

%%[1.gamMap
gamMap :: ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap f (Gam ll) = Gam (map (map f) ll)
%%]

%%[9.gamMap -1.gamMap
gamMap :: (Ord k,Ord k') => ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap f g = tgamMap (tgamSize1 g) f g
%%]

%%[3.gamMapElts
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

%%[9 -3.gamPartition
gamPartition :: Ord k => (k -> v -> Bool) -> Gam k v -> (Gam k v,Gam k v)
gamPartition = tgamPartition
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
gamMapThr f thr g = tgamMapThr (tgamSize1 g) (\k v t -> let ((k',v'),t') = f (k,v) t in (k',v',t')) thr g
%%]

%%[8.gamMbUpd
gamMbUpd :: Ord k => k -> (k -> v -> v) -> Gam k v -> Maybe (Gam k v)
gamMbUpd k upd (Gam ll)
  =  let u ((kv@(k',v):ls):lss)
             | k' == k    = Just (((k',upd k v):ls):lss)
             | otherwise  = maybe Nothing (\(ls:lss) -> Just ((kv:ls):lss)) (u (ls:lss))
         u ([]:lss)       = maybe Nothing (\lss -> Just ([] : lss)) (u lss)
         u []             = Nothing
     in  fmap Gam (u ll)
%%]

%%[9.gamMbUpd -8.gamMbUpd
gamMbUpd :: Ord k => k -> (k -> v -> v) -> Gam k v -> Maybe (Gam k v)
gamMbUpd k upd g = tgamMbUpd (tgamSize1 g) k upd g
%%]

%%[8.gamUpd
gamUpd :: Ord k => k -> (k -> v -> v) -> Gam k v -> Gam k v
gamUpd k upd = panicJust "gamUpd" . gamMbUpd k upd
%%]

%%[9.gamUpd -8.gamUpd
gamUpd :: Ord k => k -> (k -> v -> v) -> Gam k v -> Gam k v
gamUpd k upd g = tgamUpd (tgamSize1 g) k upd g
%%]

%%[1
gamKeys :: Ord k => Gam k v -> [k]
gamKeys = assocLKeys . gamToAssocL
%%]

%%[9
gamElts :: Ord k => Gam k v -> [v]
gamElts = assocLElts . gamToAssocL
%%]

%%[9
gamLookupAll :: Ord k => k -> Gam k v -> [v]
gamLookupAll k g = tgamLookupAll (tgamSize1 g) k g
%%]

%%[1.gamLookupDup
gamLookupDup :: Ord k => k -> Gam k v -> Maybe [v]
gamLookupDup k (Gam ll) = foldr (\l mv -> case filter ((==k) . fst) l of {[] -> mv; lf -> Just (assocLElts lf)}) Nothing ll
%%]

%%[9 -1.gamLookupDup
gamLookupDup :: Ord k => k -> Gam k v -> Maybe [v]
gamLookupDup k g = tgamLookupDup (tgamSize1 g) k g
%%]

%%[9
gamUpdAdd :: Ord k => k -> v -> (k -> v -> v) -> Gam k v -> Gam k v
gamUpdAdd k v upd g = tgamUpdAdd (tgamSize1 g) k v upd g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Aliases, for future compliance with naming conventions of (e.g.) Data.Map
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1
gamSingleton :: k -> v -> Gam k v
gamSingleton = gamUnit

gamInsert :: Ord k => k -> v -> Gam k v -> Gam k v
gamInsert = gamAdd

gamUnion :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnion = gamAddGam

gamUnions :: Ord k => [Gam k v] -> Gam k v
gamUnions = foldr1 gamUnion

gamFromAssocL ::  Ord k =>  AssocL k v  -> Gam k v
gamFromAssocL = assocLToGam
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tree Gam, a Gam with multiple (search) entry points
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data TreeGam i k v
  =  TreeGam
        { tgamEntriesOf :: Map.Map i (Maybe i,Map.Map k [v])
        }

instance Show (TreeGam i k v) where
  show _ = "TreeGam"

emptyTGam1 :: TreeGam i k v
emptyTGam1 = TreeGam Map.empty

emptyTGam :: Ord i => i -> TreeGam i k v
emptyTGam i = TreeGam (i `Map.singleton` (Nothing,Map.empty))

tgamSize1 :: TreeGam i k v -> Int
tgamSize1 = Map.size . tgamEntriesOf

tgamPartition :: (Ord i,Ord k) => (k -> v -> Bool) -> TreeGam i k v -> (TreeGam i k v,TreeGam i k v)
tgamPartition f g
  = (g {tgamEntriesOf = Map.unions ms1},g {tgamEntriesOf = Map.unions ms2})
  where (ms1,ms2)
          = unzip $ map (\(i,(mi,m)) -> let (ms1,ms2) = xx m in (mk i mi ms1,mk i mi ms2)) $ Map.toList $ tgamEntriesOf g
          where mk i mi m = Map.singleton i (mi,Map.unions m)
        xx m = unzip [ (mk k vs1,mk k vs2) | (k,vs) <- Map.toList m, let (vs1,vs2) = yy k vs ]
             where mk k l = if null l then Map.empty else Map.singleton k l
        yy k = partition (f k)

tgamSingleton :: i -> k -> v -> TreeGam i k v
tgamSingleton i k v = TreeGam (i `Map.singleton` (Nothing,k `Map.singleton` [v]))

tgamFoldr1 :: Ord i => i -> (i -> Maybe i -> Map.Map k [v] -> r -> r) -> r -> TreeGam i k v -> r
tgamFoldr1 i fr r g
  =  case Map.lookup i (tgamEntriesOf g) of
        Just (n,e)  -> fr i n e (maybe r (\i' -> tgamFoldr1 i' fr r g) n)
        Nothing     -> r

tgamFoldr2 :: Ord i => i -> (k -> [v] -> r -> r) -> r -> TreeGam i k v -> r
tgamFoldr2 i fr r g = tgamFoldr1 i (\_ _ e r -> Map.foldWithKey fr r e) r g

tgamFoldr :: Ord i => i -> (k -> v -> r -> r) -> r -> TreeGam i k v -> r
tgamFoldr i fr r g = tgamFoldr2 i (\k (v:_) r -> fr k v r) r g

tgamMapThr1 :: Ord i => i -> (Map.Map k [v] -> t -> (Map.Map k' [v'],t)) -> t -> TreeGam i k v -> (TreeGam i k' v',t)
tgamMapThr1 i f thr
  =  tgamFoldr1 i  (\i n e (g,t) ->  let  (e',t') = f e t
                                     in   (g {tgamEntriesOf = Map.insert i (n,e') (tgamEntriesOf g)},t')
                   )
                   (emptyTGam1,thr)

tgamMapThr2 :: (Ord i,Ord k') => i -> (k -> [v] -> t -> (k',[v'],t)) -> t -> TreeGam i k v -> (TreeGam i k' v',t)
tgamMapThr2 i f
  =  tgamMapThr1 i  (\e t -> Map.foldWithKey (\k vs (e,t) -> let (k',vs',t') = f k vs t in (Map.insert k' vs' e,t'))
                                             (Map.empty,t) e
                    )

tgamMapThr :: (Ord i,Ord k') => i -> (k -> v -> t -> (k',v',t)) -> t -> TreeGam i k v -> (TreeGam i k' v',t)
tgamMapThr i f = tgamMapThr2 i (\k (v:vs) t -> let (k',v',t') = f k v t in (k',(v':map (\v -> snd3 (f k v t)) vs),t'))

tgamMap2 :: (Ord i,Ord k') => i -> ((k,[v]) -> (k',[v'])) -> TreeGam i k v -> TreeGam i k' v'
tgamMap2 i f = fst . tgamMapThr2 i (\k vs _ -> let (k',vs') = f (k,vs) in (k',vs',())) ()

tgamMap :: (Ord i,Ord k') => i -> ((k,v) -> (k',v')) -> TreeGam i k v -> TreeGam i k' v'
tgamMap i f = fst . tgamMapThr i (\k v _ -> let (k',v') = f (k,v) in (k',v',())) ()

tgamUnzip :: (Ord i,Ord k) => i -> TreeGam i k (v1,v2) -> (TreeGam i k v1,TreeGam i k v2)
tgamUnzip i
  =  tgamFoldr1 i  (\i n e (g1,g2)
                        ->  let (e1,e2) = Map.foldWithKey (\k ((v1,v2):_) (e1,e2) -> (Map.insert k [v1] e1,Map.insert k [v2] e2)) (Map.empty,Map.empty) e
                            in  (g1 {tgamEntriesOf = Map.insert i (n,e1) (tgamEntriesOf g1)}
                                ,g2 {tgamEntriesOf = Map.insert i (n,e2) (tgamEntriesOf g2)}
                                )
                   )
                   (emptyTGam1,emptyTGam1)

tgamLookupAllDupEmp :: (Ord i,Ord k) => i -> k -> TreeGam i k v -> [[v]]
tgamLookupAllDupEmp i k g = tgamFoldr1 i (\_ _ e r -> maybe ([]:r) (:r) (Map.lookup k e)) [] g

tgamLookupAllDup :: (Ord i,Ord k) => i -> k -> TreeGam i k v -> [[v]]
tgamLookupAllDup i k = filter (not . null) . tgamLookupAllDupEmp i k

tgamLookupAllEmp :: (Ord i,Ord k) => i -> k -> v -> TreeGam i k v -> [v]
tgamLookupAllEmp i k v = map (\l -> if null l then v else head l) . tgamLookupAllDupEmp i k

tgamLookupAll :: (Ord i,Ord k) => i -> k -> TreeGam i k v -> [v]
tgamLookupAll i k = map head . tgamLookupAllDup i k

tgamLookupDup :: (Ord i,Ord k) => i -> k -> TreeGam i k v -> Maybe [v]
tgamLookupDup i k g = tgamFoldr1 i (\_ _ e r -> maybe r Just (Map.lookup k e)) Nothing g

tgamLookup :: (Ord i,Ord k) => i -> k -> TreeGam i k v -> Maybe v
tgamLookup i k = fmap head . tgamLookupDup i k

tgamToFM1 :: (Ord i,Ord k) => i -> TreeGam i k v -> Map.Map k [v]
tgamToFM1 i = tgamFoldr1 i (\_ _ e e' -> e `Map.union` e') Map.empty

tgamToFM :: (Ord i,Ord k) => i -> TreeGam i k v -> Map.Map k v
tgamToFM i = Map.map (\(v:_) -> v) . tgamToFM1 i

tgamToAssocDupL :: Ord i => i -> TreeGam i k v -> AssocL k [v]
tgamToAssocDupL i = tgamFoldr2 i (\k vs kvs -> (k,vs) : kvs) []

tgamToAssocL :: Ord i => i -> TreeGam i k v -> AssocL k v
tgamToAssocL i = tgamFoldr i (\k v kvs -> (k,v) : kvs) []

assocLToTGam :: Ord k => i -> AssocL k v -> TreeGam i k v
assocLToTGam i l = TreeGam (i `Map.singleton` (Nothing,Map.fromList . assocLMapElt (:[]) $ l))

assocDupLToTGam :: Ord k => i -> AssocL k [v] -> TreeGam i k v
assocDupLToTGam i l = TreeGam (i `Map.singleton` (Nothing,Map.fromList l))

tgamPushNew :: Ord i => i -> i -> TreeGam i k v -> TreeGam i k v
tgamPushNew i iNew g
  | i /= iNew = g {tgamEntriesOf = Map.insert iNew (Just i,Map.empty) (tgamEntriesOf g)}
  | otherwise = g

tgamAddGam :: (Ord i,Ord k) => i -> i -> TreeGam i k v -> TreeGam i k v -> TreeGam i k v
tgamAddGam i2 i1 g2 g1
  =  case Map.lookup i1 (tgamEntriesOf g1) of
        Just (n,e)  -> g1 {tgamEntriesOf = Map.insert i1 (n,Map.unionWith (++) (tgamToFM1 i2 g2) e) (tgamEntriesOf g1)}
        Nothing     -> g1

tgamPushGam :: (Ord i,Ord k) => i -> i -> i -> TreeGam i k v -> TreeGam i k v -> TreeGam i k v
tgamPushGam i1 i2 iNew g1 g2 = tgamAddGam i1 iNew g1 (tgamPushNew i2 iNew g2)

tgamAdd :: (Ord i,Ord k) => i -> k -> v -> TreeGam i k v -> TreeGam i k v
tgamAdd i k v g = tgamAddGam i i (tgamSingleton i k v) g

tgamPop :: Ord i => i -> i -> TreeGam i k v -> (TreeGam i k v,Maybe i,TreeGam i k v)
tgamPop i iPop g
  =  case Map.lookup i (tgamEntriesOf g) of
        Just (n,e)  ->  (TreeGam (iPop `Map.singleton` (Nothing,e))
                        ,n
                        ,g {tgamEntriesOf = Map.delete i (tgamEntriesOf g)}
                        )
        Nothing     ->  (emptyTGam iPop,Nothing,g)

tgamTop :: Ord i => i -> i -> TreeGam i k v -> TreeGam i k v
tgamTop i iTop g = let (g',_,_) = tgamPop i iTop g in g'

tgamMbUpd :: (Ord i,Ord k) => i -> k -> (k -> v -> v) -> TreeGam i k v -> Maybe (TreeGam i k v)
tgamMbUpd i k f g
  =  tgamFoldr1 i  (\i n e mg -> case Map.lookup k e of
                                   Just (v:_)   -> Just (g {tgamEntriesOf = Map.insert i (n,Map.insert k [f k v] e) (tgamEntriesOf g)})
                                   Nothing      -> mg
                   )
                   Nothing g

tgamUpd :: (Ord i,Ord k) => i -> k -> (k -> v -> v) -> TreeGam i k v -> TreeGam i k v
tgamUpd i k f = panicJust "tgamUpd" . tgamMbUpd i k f

tgamUpdAdd :: (Ord i,Ord k) => i -> k -> v -> (k -> v -> v) -> TreeGam i k v -> TreeGam i k v
tgamUpdAdd i k v upd g = maybe (tgamAdd i k v g) id (tgamMbUpd i k upd g)

tgamIsInScope :: Ord i => i -> i -> TreeGam i k v -> Bool
tgamIsInScope i iQuery g = iQuery `elem` tgamInScopes i g

tgamInScopes :: Ord i => i -> TreeGam i k v -> [i]
tgamInScopes i = tgamFoldr1 i (\i _ _ r -> i : r) []

tgamElts :: Ord i => i -> TreeGam i k v -> [v]
tgamElts i = assocLElts . tgamToAssocL i
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
data FixityGamInfo = FixityGamInfo { fgiPrio :: Int, fgiFixity :: Fixity } deriving Show

defaultFixityGamInfo = FixityGamInfo 9 Fixity_Infixl
%%]

%%[1.FixityGam
type FixityGam = Gam HsName FixityGamInfo
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
       Nothing    ->  (Ty_Any,[mkErr_NamesNotIntrod "value" [n]])
       Just vgi   ->  (vgiTy vgi,[])
%%]

%%[4.valGamLookup -1.valGamLookup
valGamLookup :: HsName -> ValGam -> Maybe ValGamInfo
valGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
                 -> let pr = mkPr nm in mkRes (tyProdArgs pr `mkArrow` pr)
         |  hsnIsUn nm && hsnIsProd (hsnUnUn nm)
                 -> let pr = mkPr (hsnUnUn nm) in mkRes ([pr] `mkArrow` pr)
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

%%[6.gamUnzip
gamUnzip :: Gam k (v1,v2) -> (Gam k v1,Gam k v2)
gamUnzip (Gam ll)
  =  let  (ll1,ll2) = unzip . map (unzip . map (\(n,(v1,v2)) -> ((n,v1),(n,v2)))) $ ll
     in   (Gam ll1,Gam ll2)
%%]

%%[9.gamUnzip -6.gamUnzip
gamUnzip :: Ord k => Gam k (v1,v2) -> (Gam k v1,Gam k v2)
gamUnzip g = tgamUnzip (tgamSize1 g) g
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

%%[4_2.valGamInst1ExistsWithCnstr
valGamInst1ExistsWithCnstr :: Cnstr -> UID -> ValGam -> (ValGam,Cnstr)
valGamInst1ExistsWithCnstr
  =  valGamDoWithCnstr
        (\t u ->  let  (u',ue) = mkNewLevUID u
                  in   (tyInst1Exists ue t,u')
        )
%%]

%%[7 export(valGamTyOfDataCon)
valGamTyOfDataCon :: HsName -> ValGam -> (Ty,Ty,ErrL)
valGamTyOfDataCon conNm g
  = (t,rt,e)
  where (t,e) = valGamLookupTy conNm g
        (_,rt) = tyArrowArgsRes t
%%]

%%[7 export(valGamTyOfDataFld)
valGamTyOfDataFld :: HsName -> ValGam -> (Ty,Ty,ErrL)
valGamTyOfDataFld fldNm g
  | null e    = (t,rt,e)
  | otherwise = (t,Ty_Any,e)
  where (t,e) = valGamLookupTy fldNm g
        ((rt:_),_) = tyArrowArgsRes t
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
data TyGamInfo = TyGamInfo { tgiTy :: Ty, tgiKi :: Ty } deriving Show

mkTGIData :: Ty -> Ty -> Ty -> TyGamInfo
mkTGIData t k _ = TyGamInfo t k

mkTGI :: Ty -> Ty -> TyGamInfo
mkTGI t k = mkTGIData t k Ty_Any
%%]

%%[6.tyGamLookup -1.tyGamLookup
tyGamLookup :: HsName -> TyGam -> Maybe TyGamInfo
tyGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
                 -> Just (TyGamInfo  (Ty_Con nm)
                                     (replicate (hsnProdArity nm) kiStar `mkArrow` kiStar))
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

%%[6
tyGamInst1Exists :: UID -> TyGam -> TyGam
tyGamInst1Exists = gamInst1Exists (tgiKi,(\tgi k -> tgi {tgiKi=k}))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Data tag/etc info gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7 export(DataFldMp,DataFldInfo(..),emptyDataFldInfo)
data DataFldInfo
  = DataFldInfo
%%[[8
      { dfiOffset 	:: Int
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

%%[7 export(DataTagInfo(..),emptyDataTagInfo,DataConstrTagMp)
data DataTagInfo
  = DataTagInfo
      { dtiFldMp    :: DataFldMp
      , dtiConNm	:: HsName
%%[[8
      , dtiCTag 	:: CTag
%%]]
      } deriving Show

type DataConstrTagMp = Map.Map HsName DataTagInfo

emptyDataTagInfo
  = DataTagInfo
      Map.empty hsnUnknown
%%[[8
      emptyCTag
%%]]
%%]

%%[8 export(dtiOffsetOfFld)
dtiOffsetOfFld :: HsName -> DataTagInfo -> Int
dtiOffsetOfFld fldNm dti = dfiOffset $ panicJust "dtiOffsetOfFld" $ Map.lookup fldNm $ dtiFldMp dti
%%]

%%[8 export(DataFldInConstr(..),DataFldInConstrMp)
data DataFldInConstr
  = DataFldInConstr
      { dficInTagMp	:: Map.Map CTag Int
      }

type DataFldInConstrMp = Map.Map HsName DataFldInConstr
%%]

%%[7 export(DataGam,DataGamInfo(..),mkDGI,emptyDataGamInfo)
data DataGamInfo
  = DataGamInfo
      { dgiTyNm      		:: HsName
      , dgiConstrTagMp 		:: DataConstrTagMp
%%[[8
      , dgiFldInConstrMp	:: DataFldInConstrMp
      , dgiIsNewtype 		:: Bool
%%]]
      }

type DataGam = Gam HsName DataGamInfo

instance Show DataGamInfo where
  show _ = "DataGamInfo"

mkDGI :: HsName -> DataConstrTagMp -> Bool -> DataGamInfo
mkDGI tyNm m nt
  = DataGamInfo
      tyNm m
%%[[8
      fm nt
  where fm = Map.map DataFldInConstr $ Map.unionsWith Map.union
             $ [ Map.singleton f (Map.singleton (dtiCTag ci) (dfiOffset fi)) | ci <- Map.elems m, (f,fi) <- Map.toList $ dtiFldMp ci ]
%%]]

emptyDataGamInfo :: DataGamInfo
emptyDataGamInfo = mkDGI hsnUnknown Map.empty False
%%]

%%[7 export(dgiDtiOfCon)
dgiDtiOfCon :: HsName -> DataGamInfo -> DataTagInfo
dgiDtiOfCon conNm dgi = panicJust "dgiDtiOfCon" $ Map.lookup conNm $ dgiConstrTagMp dgi
%%]

%%[7 export(dataGamLookup)
dataGamLookup :: HsName -> DataGam -> Maybe DataGamInfo
dataGamLookup nm g
  =  case gamLookup nm g of
       Nothing
         |  hsnIsProd nm
                 -> Just emptyDataGamInfo
       Just dgi  -> Just dgi
       _         -> Nothing
%%]

%%[7 export(dataGamDgiOfTy)
dataGamDgiOfTy :: Ty -> DataGam -> Maybe DataGamInfo
dataGamDgiOfTy conTy dg = dataGamLookup (tyAppFunConNm conTy) dg
%%]

%%[8 export(dataGamTagsOfTy)
dataGamTagsOfTy :: Ty -> DataGam -> Maybe [CTag]
dataGamTagsOfTy t g = fmap (map dtiCTag . Map.elems . dgiConstrTagMp) $ gamLookup (tyAppFunConNm t) $ g
%%]

%%[8
%%]
dataGamTagOfCon :: HsName -> Ty -> DataGam -> Maybe CTag
dataGamTagOfCon conNm conTy dg
  = case dataGamDgiOfTy conTy dg of
      Just dgi
        -> Just $ dtiCTag $ dgiDtiOfCon conNm dgi
      _ -> Nothing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Ty app spine" gam, to be merged with tyGam in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.AppSpineGam
data AppSpineGamInfo = AppSpineGamInfo { asgiInfoL :: [AppSpineInfo] }

type AppSpineGam = Gam HsName AppSpineGamInfo

asGamLookup :: AppSpineGam -> HsName -> [AppSpineInfo]
asGamLookup g nm
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

%%[12
idDefOccGamByKind :: IdOccKind -> IdDefOccGam -> AssocL HsName IdDefOcc
idDefOccGamByKind k g = [ (n,head i) | (IdOcc n _,i) <- fst (idDefOccGamPartitionByKind [k] g) ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Identifier unqualified to qualified gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[12 export(IdQualGam)
type IdQualGam = Gam IdOcc HsName
%%]

%%[12 export(idGam2QualGam,idQualGamReplacement)
idGam2QualGam :: IdDefOccGam -> IdQualGam
idGam2QualGam = gamMap (\(iocc,docc) -> (iocc {ioccNm = hsnQualified $ ioccNm iocc},ioccNm $ doccOcc $ docc))

idQualGamReplacement :: IdQualGam -> IdOccKind -> HsName -> HsName
idQualGamReplacement g k n = maybe n id $ gamLookup (IdOcc n k) g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances for Substitutable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[2.Substitutable.Gam
instance (Eq k,Eq tk,Substitutable k v vv) => Substitutable k v (Gam tk vv) where
  s |=> (Gam ll)    =   Gam (map (assocLMapElt (s |=>)) ll)
  ftv   (Gam ll)    =   unions . map ftv . map snd . concat $ ll
%%]

%%[9.Substitutable.TreeGam -2.Substitutable.Gam
instance (Ord tk,Ord k,Substitutable k v vv) => Substitutable k v (TreeGam i tk vv) where
  s |=> g    =   g {tgamEntriesOf = Map.map (\(n,e) -> (n,Map.map (s |=>) e)) (tgamEntriesOf g)}
  ftv   g    =   unions . map (ftv . map head . Map.elems . snd) . Map.elems . tgamEntriesOf $ g
%%]

%%[2.Substitutable.inst.ValGamInfo
instance Substitutable TyVarId Ty ValGamInfo where
%%]
%%[9 -2.Substitutable.inst.ValGamInfo
instance Substitutable TyVarId (CnstrInfo Ty) ValGamInfo where
%%]
%%[2
  s |=> vgi         =   vgi { vgiTy = s |=> vgiTy vgi }
  ftv   vgi         =   ftv (vgiTy vgi)
%%]

%%[6.Substitutable.inst.TyGamInfo
instance Substitutable TyVarId Ty TyGamInfo where
%%]
%%[9 -6.Substitutable.inst.TyGamInfo
instance Substitutable TyVarId (CnstrInfo Ty) TyGamInfo where
%%]
%%[6
  s |=> tgi         =   tgi { tgiKi = s |=> tgiKi tgi }
  ftv   tgi         =   ftv (tgiKi tgi)
%%]

%%[9
gamSubstTop :: (Ord k,Substitutable k v vv) => Cnstr' k v -> Gam k vv -> Gam k vv
gamSubstTop s g
  =  let  (h,t) = gamPop g
     in   gamPushGam (s |=> h) t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ppGam
ppGam :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam g = ppAssocL (gamToAssocL g)
%%]

%%[1
ppGamDup :: (Ord k,PP k, PP v) => Gam k v -> PP_Doc
ppGamDup g = ppAssocL $ map (\(k,v) -> (k,ppBracketsCommas v)) $ gamToAssocDupL $ g
%%]

%%[9.ppTGam
ppTGam :: (Ord i, PP k, PP v) => i -> TreeGam i k v -> PP_Doc
ppTGam i g = ppAssocL (tgamToAssocL i g)
%%]

%%[1.PP.Gam
instance (PP k, PP v) => PP (Gam k v) where
  pp = ppGam
%%]

%%[9.PP.Gam -1.PP.Gam
instance (PP i, PP k, PP v) => PP (TreeGam i k v) where
  pp g = ppAssocLV . assocLMapElt (\(n,e) -> pp n >|< ":" >|< (ppAssocL . Map.toList $ e)) . Map.toList . tgamEntriesOf $ g
%%]
  pp g = ppAssocLV . assocLMapElt (\(n,e) -> pp n >|< ":" >|< (ppAssocL . Map.toList $ e)) . Map.toList . tgamEntriesOf $ g
  pp g = ppAssocL . assocLMapElt (\(n,e) -> pp n >|< ":" >|< (ppAssocL . Map.toList $ e)) . Map.toList . tgamEntriesOf $ g

%%[1.PP.ValGamInfo
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of tyGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.initTyGam
initTyGam :: TyGam
initTyGam
  = assocLToGam
      [ (hsnArrow,  TyGamInfo (Ty_Con hsnArrow))
      , (hsnInt,    TyGamInfo tyInt)
      , (hsnChar,   TyGamInfo tyChar)
      ]
%%]

%%[6.initTyGam -1.initTyGam
initTyGam :: TyGam
initTyGam
  = assocLToGam
      [ (hsnArrow,      mkTGI (Ty_Con hsnArrow) ([kiStar,kiStar] `mkArrow` kiStar))
      , (hsnInt,        mkTGI tyInt kiStar)
      , (hsnChar,       mkTGI tyChar kiStar)
%%[[7
      , (hsnRow,        mkTGI (Ty_Con hsnUnknown) kiRow)
      , (hsnRec,        mkTGI (Ty_Con hsnRec) ([kiRow] `mkArrow` kiStar))
      , (hsnSum,        mkTGI (Ty_Con hsnSum) ([kiRow] `mkArrow` kiStar))
%%]
%%[[8
      , (hsnFloat  ,    mkTGI tyFloat   kiStar)
%%]
%%[[99
      , (hsnInteger,    mkTGI tyInteger kiStar)
      , (hsnDouble ,    mkTGI tyDouble  kiStar)
%%]
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Init of kiGam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[6
initKiGam :: KiGam
initKiGam
  = assocLToGam
      [ (hsnArrow,  KiGamInfo (Ty_Con hsnArrow))
      , (hsnStar,   KiGamInfo kiStar)
%%[[7
      , (hsnRow,    KiGamInfo kiRow)
%%]
      ]
%%]

