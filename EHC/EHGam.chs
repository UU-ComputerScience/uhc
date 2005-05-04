% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma (aka Assumptions, Environment)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 import(Data.List,EHCommon) export(Gam,emptyGam,gamLookup, gamPushNew, gamPop, gamAddGam, gamUnit, gamAdd, gamPushGam, gamToAssocL, assocLToGam, gamToDups)
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

%%[4_2 export(ErrGam)
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

%%[8 import(Data.Maybe,qualified Data.Map as Map,EHCore) export(gamUpd,DataTagMp)
%%]

%%[8 export(DataGam,DataGamInfo(..),mkDGI)
%%]

%%[9 import(EHDebug,EHCoreSubst,EHTyFitsInCommon) export(gamUpdAdd,gamLookupAll,gamSubstTop,gamElts)
%%]

%%[9 export(TreeGam,emptyTGam,tgamUnit,tgamLookup,tgamLookupAll,tgamElts,tgamPushNew,tgamAddGam,tgamPushGam,tgamAdd,tgamPop,tgamUpdAdd,tgamUpd,tgamMbUpd,tgamInScopes,tgamIsInScope)
%%]

%%[9 export(ppTGam)
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
gamLookup           ::  Ord k =>  Gam k v  -> k -> Maybe v
gamToAssocL         ::            Gam k v       -> AssocL k v
gamPushNew          ::            Gam k v       -> Gam k v
gamPushGam          ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamAddGam           ::  Ord k =>  Gam k v       -> Gam k v -> Gam k v
gamAdd              ::  Ord k =>  Gam k v  -> k -> v       -> Gam k v
%%]

%%[1.Base.funs
emptyGam                            = Gam [[]]
gamUnit         k v                 = Gam [[(k,v)]]
gamLookup       (Gam ll) k          = foldr  (\l mv -> maybe mv Just (lookup k l))
                                             Nothing ll
gamToAssocL     (Gam ll)            = concat ll
gamPushNew      (Gam ll)            = Gam ([]:ll)
gamPushGam      (Gam ll2)  g1       = Gam (gamToAssocL g1 : ll2)
gamAddGam       (Gam (l2:ll2)) g1   = Gam ((gamToAssocL g1 ++ l2):ll2)
gamAdd          g k v               = gamAddGam g (k `gamUnit` v)
%%]

%%[9.Base.funs -1.Base.funs
emptyGam                            = emptyTGam 1 
gamUnit                             = tgamUnit 1
gamLookup       g                   = tgamLookup (tgamSize1 g) g
gamToAssocL     g                   = tgamToAssocL (tgamSize1 g) g
gamPushNew      g                   = let sz = tgamSize1 g in tgamPushNew sz (sz+1) g
gamPushGam      g1 g2               = let sz = tgamSize1 g1 in tgamPushGam sz (tgamSize1 g2) (sz+1) g1 g2
gamAddGam       g1 g2               = tgamAddGam (tgamSize1 g1) (tgamSize1 g2) g1 g2
gamAdd          g                   = tgamAdd (tgamSize1 g) g
%%]

%%[1.Rest.sigs
gamPop              ::            Gam k v     -> (Gam k v,Gam k v)
assocLToGam         ::  Ord k =>  AssocL k v  -> Gam k v
%%]

%%[1.Rest.funs
gamPop          (Gam (l:ll))        = (Gam [l],Gam ll)
assocLToGam     l                   = Gam [l]
%%]

%%[9.Rest.funs -1.Rest.funs
gamPop          g                   = let (g1,_,g2) = tgamPop (tgamSize1 g) 1 g in (g1,g2)
assocLToGam                         = assocLToTGam 1 
%%]

%%[1.gamToDups
gamToDups :: Ord k => Gam k v -> [k]
gamToDups g = [ n | ns@(n:_) <- group . sort . assocLKeys . gamToAssocL $ g, length ns > 1 ]
%%]

%%[9.gamToDups -1.gamToDups
gamToDups :: Ord k => Gam k v -> [k]
gamToDups g = [ n | (n,vs) <- tgamToAssocL2 (tgamSize1 g) g, length vs > 1 ]
%%]

%%[3.gamMap
gamMap :: ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap f (Gam ll) = Gam (map (map f) ll)
%%]

%%[9.gamMap -3.gamMap
gamMap :: (Ord k,Ord k') => ((k,v) -> (k',v')) -> Gam k v -> Gam k' v'
gamMap f g = tgamMap (tgamSize1 g) f g
%%]

%%[3.gamMapElts
gamMapElts :: Ord k => (v -> v') -> Gam k v -> Gam k v'
gamMapElts f = gamMap (\(n,v) -> (n,f v))
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

%%[4.gamTop
gamTop ::  Gam k v -> Gam k v
gamTop = fst . gamPop
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
gamUpd k upd = fromJust . gamMbUpd k upd
%%]

%%[9.gamUpd -8.gamUpd
gamUpd :: Ord k => k -> (k -> v -> v) -> Gam k v -> Gam k v
gamUpd k upd g = tgamUpd (tgamSize1 g) k upd g
%%]

%%[9
gamElts :: Ord k => Gam k v -> [v]
gamElts = assocLElts . gamToAssocL
%%]

%%[9
gamLookupAll :: Ord k => Gam k v -> k -> [v]
gamLookupAll g = tgamLookupAll (tgamSize1 g) g
%%]

%%[9
gamUpdAdd :: Ord k => k -> v -> (k -> v -> v) -> Gam k v -> Gam k v
gamUpdAdd k v upd g = tgamUpdAdd (tgamSize1 g) k v upd g
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

tgamUnit :: i -> k -> v -> TreeGam i k v
tgamUnit i k v = TreeGam (i `Map.singleton` (Nothing,k `Map.singleton` [v]))

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

tgamLookupAll1 :: (Ord i,Ord k) => i -> TreeGam i k v -> k -> [[v]]
tgamLookupAll1 i g k = tgamFoldr1 i (\_ _ e r -> maybe r (:r) (Map.lookup k e)) [] g

tgamLookupAll :: (Ord i,Ord k) => i -> TreeGam i k v -> k -> [v]
tgamLookupAll i g = map head . tgamLookupAll1 i g

tgamLookup1 :: (Ord i,Ord k) => i -> TreeGam i k v -> k -> Maybe [v]
tgamLookup1 i g k = tgamFoldr1 i (\_ _ e r -> maybe r Just (Map.lookup k e)) Nothing g

tgamLookup :: (Ord i,Ord k) => i -> TreeGam i k v -> k -> Maybe v
tgamLookup i g = fmap head . tgamLookup1 i g

tgamToFM1 :: (Ord i,Ord k) => i -> TreeGam i k v -> Map.Map k [v]
tgamToFM1 i = tgamFoldr1 i (\_ _ e e' -> e `Map.union` e') Map.empty

tgamToFM :: (Ord i,Ord k) => i -> TreeGam i k v -> Map.Map k v
tgamToFM i = Map.map (\(v:_) -> v) . tgamToFM1 i

tgamToAssocL2 :: Ord i => i -> TreeGam i k v -> AssocL k [v]
tgamToAssocL2 i = tgamFoldr2 i (\k vs kvs -> (k,vs) : kvs) []

tgamToAssocL :: Ord i => i -> TreeGam i k v -> AssocL k v
tgamToAssocL i = tgamFoldr i (\k v kvs -> (k,v) : kvs) []

tgamPushNew :: Ord i => i -> i -> TreeGam i k v -> TreeGam i k v
tgamPushNew i iNew g = g {tgamEntriesOf = Map.insert iNew (Just i,Map.empty) (tgamEntriesOf g)}

tgamAddGam :: (Ord i,Ord k) => i -> i -> TreeGam i k v -> TreeGam i k v -> TreeGam i k v
tgamAddGam i1 i2 g1 g2
  =  case Map.lookup i1 (tgamEntriesOf g1) of
        Just (n,e)  -> g1 {tgamEntriesOf = Map.insert i1 (n,Map.unionWith (++) (tgamToFM1 i2 g2) e) (tgamEntriesOf g1)}
        Nothing     -> g1

tgamPushGam :: (Ord i,Ord k) => i -> i -> i -> TreeGam i k v -> TreeGam i k v -> TreeGam i k v
tgamPushGam i1 i2 iNew g1 g2 = tgamAddGam iNew i2 (tgamPushNew i1 iNew g1) g2

tgamAdd :: (Ord i,Ord k) => i -> TreeGam i k v -> k -> v -> TreeGam i k v
tgamAdd i g k v = tgamAddGam i i g (tgamUnit i k v)

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

assocLToTGam :: Ord k => i -> AssocL k v -> TreeGam i k v
assocLToTGam i l = TreeGam (i `Map.singleton` (Nothing,Map.fromList . assocLMapElt (:[]) $ l))

tgamMbUpd :: (Ord i,Ord k) => i -> k -> (k -> v -> v) -> TreeGam i k v -> Maybe (TreeGam i k v)
tgamMbUpd i k f g
  =  tgamFoldr1 i  (\i n e mg -> case Map.lookup k e of
                                   Just (v:_)   -> Just (g {tgamEntriesOf = Map.insert i (n,Map.insert k [f k v] e) (tgamEntriesOf g)})
                                   Nothing      -> mg
                   )
                   Nothing g

tgamUpd :: (Ord i,Ord k) => i -> k -> (k -> v -> v) -> TreeGam i k v -> TreeGam i k v
tgamUpd i k f = fromJust . tgamMbUpd i k f

tgamUpdAdd :: (Ord i,Ord k) => i -> k -> v -> (k -> v -> v) -> TreeGam i k v -> TreeGam i k v
tgamUpdAdd i k v upd g = maybe (tgamAdd i g k v) id (tgamMbUpd i k upd g)

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
%%% "Type of value" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ValGam.Base
data ValGamInfo = ValGamInfo { vgiTy :: Ty } deriving Show

type ValGam = Gam HsName ValGamInfo
%%]

%%[1.valGamLookup
valGamLookup :: ValGam -> HsName -> Maybe ValGamInfo
valGamLookup = gamLookup
%%]

%%[1.valGamLookupTy
valGamLookupTy :: ValGam -> HsName -> (Ty,ErrL)
valGamLookupTy g n
  =  case valGamLookup g n of
       Nothing    ->  (Ty_Any,[Err_NamesNotIntrod [n]])
       Just vgi   ->  (vgiTy vgi,[])
%%]

%%[4.valGamLookup -1.valGamLookup
valGamLookup :: ValGam -> HsName -> Maybe ValGamInfo
valGamLookup g nm
  =  case gamLookup g nm of
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Kind of type" gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.TyGamInfo
data TyGamInfo = TyGamInfo { tgiTy :: Ty } deriving Show
%%]

%%[1.tyGamLookup
tyGamLookup :: TyGam -> HsName -> Maybe TyGamInfo
tyGamLookup g nm
  =  case gamLookup g nm of
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

%%[8.DataTagMp
type DataTagMp = Map.Map HsName CTag
%%]

%%[8.TyGamInfo
%%]

data TyGamInfo = TyGamInfo { tgiTy :: Ty, tgiKi :: Ty, tgiData :: Ty, tgiDataTagMp :: DataTagMp }

instance Show TyGamInfo where
  show _ = "TyGamInfo"

mkTGIData :: Ty -> Ty -> Ty -> DataTagMp -> TyGamInfo
mkTGIData t k d m = TyGamInfo t k d m

mkTGI :: Ty -> Ty -> TyGamInfo
mkTGI t k = mkTGIData t k Ty_Any Map.empty

%%[6.tyGamLookup -1.tyGamLookup
tyGamLookup :: TyGam -> HsName -> Maybe TyGamInfo
tyGamLookup g nm
  =  case gamLookup g nm of
       Nothing
         |  hsnIsProd nm
                 -> Just (TyGamInfo  (Ty_Con nm)
                                     (replicate (hsnProdArity nm) kiStar `mkArrow` kiStar))
       Just tgi  -> Just tgi
       _         -> Nothing
%%]

%%[7.tyGamLookup -6.tyGamLookup
tyGamLookup :: TyGam -> HsName -> Maybe TyGamInfo
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
%%% Data tag info gam
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8.DataGamInfo
data DataGamInfo = DataGamInfo { dgiDataTagMp :: DataTagMp }

type DataGam = Gam HsName DataGamInfo

instance Show DataGamInfo where
  show _ = "DataGamInfo"

mkDGI :: DataTagMp -> DataGamInfo
mkDGI m = DataGamInfo m
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% "Ty app spine" gam, to be merged with tyGam in the future
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[4.AppSpineGam
data AppSpineGamInfo = AppSpineGamInfo { asgiInfoL :: [AppSpineInfo] }

type AppSpineGam = Gam HsName AppSpineGamInfo

asGamLookup :: AppSpineGam -> HsName -> [AppSpineInfo]
asGamLookup g nm
  =  case gamLookup g nm of
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

%%[2.Substitutable.Gam
instance Substitutable v => Substitutable (Gam k v) where
  s |=> (Gam ll)    =   Gam (map (assocLMapElt (s |=>)) ll)
  ftv   (Gam ll)    =   unionL . map ftv . map snd . concat $ ll
%%]

%%[9.Substitutable.TreeGam -2.Substitutable.Gam
instance Substitutable v => Substitutable (TreeGam i k v) where
  s |=> g    =   g {tgamEntriesOf = Map.map (\(n,e) -> (n,Map.map (s |=>) e)) (tgamEntriesOf g)}
  ftv   g    =   unionL . map (ftv . map head . Map.elems . snd) . Map.elems . tgamEntriesOf $ g
%%]

%%[2
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
gamSubstTop :: (Ord k,Substitutable v) => Cnstr -> Gam k v -> Gam k v
gamSubstTop s g
  =  let  (h,t) = gamPop g
     in   t `gamPushGam` (s |=> h)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1.ppGam
ppGam :: (PP k, PP v) => Gam k v -> PP_Doc
ppGam g = ppAssocL (gamToAssocL g)
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
