% $Id$

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 import(Maybe,List,FiniteMap,Set,UU.Pretty)
%%]

%%[9 import(EHTy,EHTyPretty,EHTyFitsInCommon,EHTyQuantify,EHCore,EHCorePretty,EHCoreSubst,EHCommon,EHGam,EHCnstr,EHSubstitutable)
%%]

%%[9 import(EHDebug)
%%]

%%[9 import(EHError) export(ProofState(..))
%%]

%%[9 export(ProofState2(..))
%%]

%%[9 export(ProvenNode(..),prvnIsAnd)
%%]

%%[9 export(ProvenGraph(..),prvgAddPrNd,prvgAddPrUids,prvgAddNd,prvgCode,prvgBackToOrig,prvgReachableFrom,prvgReachableTo,prvgIsFact,prvgFactL)
%%]

%%[9 export(prvgCxBindLMap,prvgIntroBindL,prvgLetHoleCSubst)
%%]

%%[9 export(prvg2ReachableFrom)
%%]

%%[9 export(Rule(..),emptyRule,mkInstElimRule)
%%]

%%[9 export(ProofCost(..),mkCost,mkCostAvailImpl,costVeryMuch,costAvailArg,costZero,costNeg,costAdd,costALot,costBase,costAddHi,costMulBy)
%%]

%%[9 export(ClsFuncDep(..))
%%]

%%[9 export(PrIntroGamInfo(..),PrIntroGam,emptyPIGI)
%%]

%%[9 export(PrElimGamInfo(..))
%%]

%%[9 export(PrElimGam,peGamAdd,peGamDel,peGamAddKnPr,peGamAddKnPrL)
%%]

%%[9 export(PrElimTGam,peTGamAdd,peTGamDel,peTGamAddKnPr,peTGamAddKnPrL,peTGamPoiS)
%%]

%%[9 export(ProofCtxt(..),emptyProofCtxt)
%%]

%%[9 import(EHTyFtv) export(prvgArgLeaves,prvgSatisfiedNodeS,prvgOrigs,prvgAppPoiSubst,prvg2BackMp,prvgShareMp)
%%]

%%[9 export(PoiSubst(..),PoiSubstitutable(..))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitution for PredOccId
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
newtype PoiSubst = PoiSubst {poisMp :: FiniteMap PredOccId PredOccId}
%%]

%%[9
infixr `poisApp`

class PoiSubstitutable a where
  poisApp :: PoiSubst -> a -> a

instance PoiSubstitutable PredOccId where
  poisApp (PoiSubst m) poi = maybe poi id . lookupFM m $ poi

instance PoiSubstitutable a => PoiSubstitutable [a] where
  poisApp s = map (poisApp s)

instance PoiSubstitutable PoiSubst where
  poisApp (PoiSubst m1) (PoiSubst m2)
    = PoiSubst (mapFM (\_ i -> maybe i id (lookupFM m1 i)) m2 `plusFM` m1)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cost
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofCost  = Cost {costHi, costLo :: Int}
                deriving (Eq,Show)

instance Ord ProofCost where
  (Cost h1 l1) `compare` (Cost h2 l2)
    | hcmp == EQ  = l1 `compare` l2
    | otherwise   = hcmp
    where hcmp = h1 `compare` h2

mkCost :: Int -> ProofCost
mkCost c = Cost 0 c

costVeryMuch 	= Cost 100 0
costZero 		= mkCost 0
costBase  		= mkCost 1
costAvailArg 	= mkCost 2
costNeg 		= Cost (-2) 0

mkCostAvailImpl :: Int -> ProofCost
mkCostAvailImpl c = Cost (-1) c

costALot :: Int
costALot  = 100

infixr 2 `costAdd`, `costAddHi`
infixr 3 `costMulBy`

costMulBy :: ProofCost -> Int -> ProofCost
c1@(Cost h l) `costMulBy` i = Cost h (l * i)

costAdd :: ProofCost -> ProofCost -> ProofCost
c1@(Cost h1 l1) `costAdd` c2@(Cost h2 l2) = Cost (h1 + h2) (l1 + l2)

costAddHi :: ProofCost -> Int -> ProofCost
(Cost h l) `costAddHi` i = Cost (h+i) l

instance PP ProofCost where
  pp (Cost h l) = "C:" >|< pp h >|< ":" >|< l
%%]
data ProofCost  = CostAvailImpl {costCost :: Int} | CostInt {costCost :: Int}
                deriving (Eq,Show,Ord)

costAdd :: ProofCost -> ProofCost -> ProofCost
costAdd (CostInt c1)            (CostInt c2)            = CostInt (c1 + c2)
costAdd (CostAvailImpl c1)      (CostAvailImpl c2)      = CostAvailImpl (c1 + c2)
costAdd (CostAvailImpl _ )      c2                      = c2
costAdd c1                      _                       = c1

instance PP ProofCost where
  pp (CostInt c)       = pp c
  pp (CostAvailImpl c) = pp "-Inf:" >|< pp c

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Proof context
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofCtxt
  =  ProofCtxt
        { prcxElimGam   ::  PrElimGam
        }

emptyProofCtxt
  =  ProofCtxt
        { prcxElimGam   =   emptyGam
        }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver, proven nodes in proven graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProvenNode
  =  ProvenOr       { prvnPred           :: Pred            , prvnEdges         :: [PredOccId]
                    , prvnCost           :: ProofCost
                    }
  |  ProvenAnd      { prvnPred           :: Pred
                    , prvnEdges          :: [PredOccId]     , prvnLamEdges      :: [PredOccId]
                    , prvnCost           :: ProofCost       , prvnEvid          :: CExpr
                    }
  |  ProvenShare    { prvnPred           :: Pred            , prvnEdge          :: PredOccId    }
  |  ProvenArg      { prvnPred           :: Pred            , prvnCost          :: ProofCost    }
  |  ProvenLcl      { prvnPred           :: Pred            , prvnEvid          :: CExpr        }

prvnIsAnd :: ProvenNode -> Bool
prvnIsAnd (ProvenAnd _ _ _ _ _) = True
prvnIsAnd _                     = False

prvnIsArg :: ProvenNode -> Bool
prvnIsArg (ProvenArg _ _) = True
prvnIsArg _               = False

instance Show ProvenNode where
  show _ = ""

instance PP ProvenNode where
  pp (ProvenOr  pr es c         )   = "OR:"    >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es                                      >#< "cost=" >|< pp c
  pp (ProvenAnd pr es les c ev  )   = "AND:"   >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es >#< "lamedges=" >|< ppCommaList les  >#< "cost=" >|< pp c >#< "evid=" >|< pp ev
  pp (ProvenShare pr e          )   = "SHARE:" >#< "pr=" >|< pp pr >#< "edge="  >|< ppCommaList [e]
  pp (ProvenArg pr c            )   = "ARG:"   >#< "pr=" >|< pp pr                                                                      >#< "cost=" >|< pp c
  pp (ProvenLcl pr ev           )   = "LCL:"   >#< "pr=" >|< pp pr                                                                      >#< "evid=" >|< pp ev
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver, proven graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProvenGraph
  =  ProvenGraph
       { prvgIdNdMp             :: FiniteMap PredOccId ProvenNode
       , prvgPrIdMp             :: FiniteMap Pred [PredOccId]
       , prvgPrOrigIdMp         :: FiniteMap Pred [PredOccId]
       , prvgPrUsedFactIdMp     :: FiniteMap Pred [PredOccId]
       }

prvgAddPrUids :: Pred -> [PredOccId] -> ProvenGraph -> ProvenGraph
prvgAddPrUids pr uidL g@(ProvenGraph _ p2i _ _)
  =  g {prvgPrIdMp = addToFM_C (++) p2i pr uidL}

prvgAddNd :: PredOccId -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddNd uid nd g@(ProvenGraph i2n _ _ _)
  =  g {prvgIdNdMp = addToFM i2n uid nd}

prvgAddPrNd :: Pred -> [PredOccId] -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddPrNd pr uidL@(uid:_) nd
  =  prvgAddPrUids pr uidL . prvgAddNd uid nd

instance Show ProvenGraph where
  show _ = ""

instance PP ProvenGraph where
  pp (ProvenGraph i2n p2i p2oi p2fi)
    =      "Pr->Ids  :" >#< pp2i p2i
       >-< "Id->Nd   :" >#< ppAssocLV (fmToList i2n)
       >-< "Pr->Orig :" >#< pp2i p2oi
       >-< "Pr->Facts:" >#< pp2i p2fi
    where pp2i = ppAssocLV . assocLMapElt ppCommaList . fmToList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver, state of prover
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofState
  =  ProofState     { prfsProvenGraph        :: ProvenGraph     , prfsUniq               :: UID
                    , prfsPredsToProve       :: [PredOcc]       , prfsPredsOrigToProve   :: [Pred]
                    , prfsPrElimGam          :: PrElimGam       , prfsErrL               :: [Err]
                    }
                    deriving Show

instance PP ProofState where
  pp s = "PrfSt:" >#< (pp (prfsProvenGraph s)
                       >-< pp (prfsUniq s)
                       >-< ppListV (prfsPredsToProve s)
                       >-< ppListV (prfsPredsOrigToProve s))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver, state of prover (tgam version)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofState2
  =  ProofState2    { prfs2ProvenGraph        :: ProvenGraph     , prfs2Uniq               :: UID
                    , prfs2PredsToProve       :: [PredOcc]
                    , prfs2PrElimTGam         :: PrElimTGam      , prfs2ErrL               :: [Err]
                    }

instance Show ProofState2 where
  show _ = "ProofState2"

instance PP ProofState2 where
  pp s = "PrfSt2:" >#< (pp (prfs2ProvenGraph s)
                       >-< pp (prfs2Uniq s)
                       >-< ppCommaList (prfs2PredsToProve s)
                       )
%%]
                       >-< (ppCommaList . setToList . prfs2PredsOrigToProve $ s)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code gen for a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgSplitIntoSubstBind :: ProvenGraph -> AssocL PredOccId ProvenNode -> (CSubst,AssocL PredOccId CExpr)
prvgSplitIntoSubstBind g@(ProvenGraph _ p2i _ _) provenNdL
  = (provenAsCSubst,provenForBind)
  where   i2i  = listToFM [ (i,ii) | (_,ii) <- fmToList p2i, i <- ii ]
          canAsSubst (_,CExpr_Var _)  = True
          canAsSubst (_,CExpr_Int _)  = True
          canAsSubst _                = False
          (provenAsCSubst,(_,provenForBind))
            = head
              . dropWhile (\(_,(l,_)) -> not (null l))
              . iterate
                  (\(s,(asS,asB))
                      -> let asS' = concat . map (\(i,n) -> zip (maybe [] id . lookupFM i2i $ i) (repeat n)) $ asS
                             s' = s `cSubstApp` poiCExprLToCSubst asS'
                         in  (s',partition canAsSubst (s' `cSubstApp` asB))
                  )
              $ (emptyCSubst
                ,partition canAsSubst (assocLMapElt prvnEvid provenNdL)
                )

prvgCode :: [PredOcc] -> ProvenGraph -> (CBindLMap,CSubst,Set PredOccId,PP_DocL)
prvgCode prL g@(ProvenGraph i2n p2i _ p2fi)
  =  let  i2nL = fmToList i2n
          p2iL = fmToList p2i
          (provenNdL,argNdL,overlapNdL)
            = (concat p, concat a, concat o)
            where  spl n@(_,ProvenAnd _ _ _ _ _) = ([n],[],[])
                   spl n@(_,ProvenLcl _ _      ) = ([n],[],[])
                   spl n@(_,ProvenArg _ _      ) = ([],[n],[])
                   spl n                         = ([],[],[n])
                   (p,a,o) = unzip3 . map spl $ i2nL
          (provenAsCSubst,provenForBind) = prvgSplitIntoSubstBind g provenNdL
          provenRefToBoundCSubst = poiCExprLToCSubst [ (i,CExpr_Var (poiHNm i)) | (i,_) <- provenForBind ]
          aliasRefCSubst
            = poiCExprLToCSubst
             . concat
             . map (\(p,(i:iL))
                        -> let allI = (iL `List.union` maybe [] id (lookupFM p2fi p)) \\ [i]
                           in  zip allI (repeat (mkCExprPrHole i))
                   )
             $ p2iL
          allCSubst = provenAsCSubst `cSubstApp` provenRefToBoundCSubst `cSubstApp` aliasRefCSubst
          bindMp
            = let s p = let rAll = prvgReachableTo g [poPrId p]
                            r = rAll `delFromSet` poPrId p
                        in  (poPrId p
                            ,(rAll
                             ,[ CBind_Bind (poiHNm i) ev | (i,ev) <- provenForBind, i `elementOf` r ]
                            ))
              in  map s . map (\(p,(i:_)) -> PredOcc p (maybe i head . lookupFM p2fi $ p)) $ p2iL
          remIdL = concat [ uidL | (_,uidL) <- p2iL, any (`elem` map fst argNdL) uidL ]
     in   (listToFM bindMp ,allCSubst,mkSet remIdL
          ,[]
          )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code gen for bindings required by a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgCxBindLMap :: ProvenGraph -> CxBindLMap
prvgCxBindLMap g@(ProvenGraph i2n _ _ p2fi)
  =  CxBindLMap
     .  listToFM
     .  map  (\i -> let dpdS = prvgReachableTo' True g [i]
                    in  (i,[ b | (i,b) <- binds, i `elementOf` dpdS ])
             )
     $  leaveL
  where   factPoiL = concat (eltsFM p2fi)
          leaveL = prvgAllLeaves g
          leaveS = mkSet leaveL
          binds  = [ (i,(CBind_Bind (poiHNm i) ev,i,(r `minusSet` mkSet les) `Set.intersect` leaveS))
                   | (i,ProvenAnd _ _ les _ ev) <- fmToList i2n
                   , not (i `elem` factPoiL)
                   , let (r,_) = prvg2ReachableFrom g [i]
                   ]
%%]

%%[9
prvgIntroBindL :: (PredOccId -> Bool) -> [PredOccId] -> ProvenGraph -> CBindL
prvgIntroBindL poiIsGlob topSort g@(ProvenGraph i2n _ _ _)
  = [ CBind_Bind (poiHNm i) ev
    | (i,ProvenAnd _ _ _ _ ev) <- introNoDpd ++ introGlob, not (isFact i)
    ]
  where  introNoDpd     = [ nn | nn@(i,ProvenAnd _ es les _ _) <- fmToList i2n, null es, null les ]
         introGlob      = [ (i,n) | i <- setToList (prvgSatisfiedNodeS True poiIsGlob g topSort)
                                  , n <- maybeToList (lookupFM i2n i)
                          ]
         isFact         = prvgIsFact g
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code gen for let holes in a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgLetHoleCSubst :: ProvenGraph -> PrElimTGam -> CxBindLMap -> CSubst
prvgLetHoleCSubst g@(ProvenGraph i2n _ _ _) peTGam cxBindLM
  = uidCBindLLToCSubst sBinds
  where  sBinds
           = [ (poiId iHole,mkCxBindLForPoiL (availPoiS i iArgs) cxBindLM iArgs)
             | (i,ProvenAnd _ _ (iHole:iArgs) _ _) <- fmToList i2n
             ]
         availPoiS i iArgs
           = peTGamPoiS (poiCxId i) peTGam `Set.union` mkSet iArgs `delFromSet` i
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reachable nodes in a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgReachableFrom :: ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableFrom (ProvenGraph i2n _ _ _)
  =  let  r uid reachSet
            =  if uid `elementOf` reachSet
               then  reachSet
               else  let  reachSet' = addToSet reachSet uid
                     in   case lookupFM i2n uid of
                                Just (ProvenAnd _ es _ _ _ ) -> rr reachSet' es
                                Just (ProvenOr  _ es _     ) -> rr reachSet' es
                                _                            -> reachSet'
          rr = foldr r
     in   rr emptySet
%%]

%%[9
prvg2ReachableFrom' :: Bool -> ProvenGraph -> [PredOccId] -> (Set PredOccId,[PredOccId])
prvg2ReachableFrom' hideLamArg (ProvenGraph i2n _ _ _) poiL
  =  let  allPoiS = mkSet (keysFM i2n)
          nextOf poi
            =  case lookupFM i2n poi of
                  Just (ProvenAnd _ es les _ _ ) -> mkSet es `minusSet` les'
                                                 where les' = if hideLamArg then emptySet else mkSet les
                  Just (ProvenOr  _ es _     )   -> mkSet es
                  _                              -> emptySet
          rr r@(reachSet,revTopSort) poiL
            =  let  newPoiL = setToList newPoiS
                    newPoiS = (unionManySets . map nextOf $ poiL) `minusSet` reachSet
               in   if isEmptySet newPoiS
                    then r
                    else rr (reachSet `Set.union` newPoiS,newPoiL ++ revTopSort) newPoiL
          (s,l) = rr (mkSet poiL,poiL) poiL
     in   (s `Set.intersect` allPoiS,filter (`elementOf` allPoiS) l)
%%]

%%[9
prvg2ReachableFrom :: ProvenGraph -> [PredOccId] -> (Set PredOccId,[PredOccId])
prvg2ReachableFrom = prvg2ReachableFrom' False
%%]
prvg2ReachableFrom :: ProvenGraph -> [PredOccId] -> (Set PredOccId,[PredOccId])
prvg2ReachableFrom (ProvenGraph i2n _ _ _) poiL
  =  let  nextOf poi
            =  case lookupFM i2n poi of
                  Just (ProvenAnd _ es _ _ _ ) -> es
                  Just (ProvenOr  _ es _     ) -> es
                  _                            -> []
          rr r@(reachSet,revTopSort) poiL
            =  let  newPoiL = filter (not . (`elementOf` reachSet)) . concat . map nextOf $ poiL
               in   if null newPoiL
                    then r
                    else rr (reachSet `Set.union` mkSet newPoiL,newPoiL ++ revTopSort) newPoiL
     in   rr (mkSet poiL,poiL) poiL

%%[9
prvgReachableTo' :: Bool -> ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableTo' hideLamArg (ProvenGraph i2n _ _ _)
  =  let  allNd = eltsFM i2n
          r uid reachSet
            =  if uid `elementOf` reachSet
               then  reachSet
               else  let  reachSet' = addToSet reachSet uid
                          reachFrom fromNd toUid
                            =  case fromNd of
                                 ProvenAnd _ es les _ _
                                    | hideLamArg
                                        -> inEs && toUid `notElem` les
                                    | otherwise
                                        -> inEs
                                    where inEs = toUid `elem` es
                                 ProvenOr  _ es _
                                    -> toUid `elem` es
                                 _  -> False
                     in   rr reachSet' (keysFM . filterFM (\i n -> reachFrom n uid) $ i2n)
          rr = foldr r
     in   rr emptySet
%%]

%%[9
prvgReachableTo :: ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableTo = prvgReachableTo' False
%%]
prvgReachableTo :: ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableTo (ProvenGraph i2n _ _ _)
  =  let  allNd = eltsFM i2n
          r uid reachSet
            =  if uid `elementOf` reachSet
               then  reachSet
               else  let  reachSet' = addToSet reachSet uid
                          reachFrom fromNd toUid
                            =  case fromNd of
                                 ProvenAnd _ es _ _ _  | toUid `elem` (filter (\i -> case lookupFM i2n i of {Just (ProvenLcl _ _) -> False ; _ -> True}) es)
                                                                          -> True
                                 ProvenOr  _ es _      | toUid `elem` es  -> True
                                 _                                        -> False
                     in   rr reachSet' (keysFM . filterFM (\i n -> reachFrom n uid) $ i2n)
          rr = foldr r
     in   rr emptySet

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Original nodes, fact nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgOrigs :: ProvenGraph -> [PredOccId]
prvgOrigs (ProvenGraph _ _ p2oi _)
  =  concat (eltsFM p2oi)
%%]

%%[9
prvgFactL :: ProvenGraph -> [PredOccId]
prvgFactL (ProvenGraph _ _ _ p2fi)
  =  concat (eltsFM p2fi)

prvgIsFact :: ProvenGraph -> PredOccId -> Bool
prvgIsFact g = (`elem` prvgFactL g)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Computation along topsort order (bottom to top),
%%% by def each node visited 1x (topsort order should guarantee)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgTopsortCompute
  ::  Ord val =>  (val -> ProvenNode -> Set val -> res -> (Maybe val,Maybe upd)
                  ,PredOccId -> Maybe ProvenNode -> Maybe val
                  ,Set val -> val -> Set val,res -> upd -> res,res
                  )
                  -> ProvenGraph -> [PredOccId] -> res
prvgTopsortCompute (sat,valOf,updDone,updRes,stRes) g@(ProvenGraph i2n _ p2oi _) topSort
  = res
  where  (_,res)
           = foldr  (\poi rd@(done,res)
                        ->  let  mbN = lookupFM i2n poi
                                 mbPoiVal = valOf poi mbN
                            in   case mbN of
                                   Just n | isJust mbPoiVal && not (poiVal `elementOf` done)
                                       ->  (maybe done (updDone done) mbVal
                                           ,maybe res (updRes res) mbRes
                                           )
                                       where  (mbVal,mbRes) = sat poiVal n done res
                                              poiVal = fromJust mbPoiVal
                                   _   ->  rd
                    )
                    (emptySet,stRes)
                    topSort
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Nodes which do not depend on others
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgAllLeaves :: ProvenGraph -> [PredOccId]
prvgAllLeaves g@(ProvenGraph i2n _ p2oi _)
  = keysFM (filterFM (const isLf) i2n) ++ prvgFactL g
  where  isLf (ProvenArg _ _) = True
         isLf _               = False
%%]
prvgAllLeaves :: ProvenGraph -> [PredOccId]
prvgAllLeaves g@(ProvenGraph i2n _ p2oi _)
  = keysFM (filterFM (\i n -> not (isFact i) && isLf n) i2n)
  where  isLf (ProvenArg _ _       )  = True
         isLf (ProvenAnd _ es _ _ _)  = null es || all isFact es
         isLf _                       = False
         isFact = prvgIsFact g

%%[9
prvgArgLeaves :: ProvenGraph -> [PredOccId] -> [Pred]
prvgArgLeaves g@(ProvenGraph i2n _ p2oi _) topSort
  =  filter (any (not . tvCatIsFixed) . eltsFM . tyFtvMp . predTy) l
  where  provenAnds pr = [ n | n@(ProvenAnd ndPr _ _ _ _) <- eltsFM i2n, ndPr == pr ]
         l = prvgTopsortCompute (sat,\_ mn -> fmap prvnPred mn,addToSet,flip (:),[]) g topSort
         isFact = prvgIsFact g
         dpdPrsOf (ProvenAnd _ es _ _ _)
           = catMaybes . map (fmap prvnPred . lookupFM i2n) . filter (not . isFact) $ es
         sat pr n prDoneS _
           =  case n of
                ProvenAnd _ es _ _ _
                    | not isOrig && needed n
                        ->  (Just pr,Nothing)
                    | not isOrig
                        ->  (Nothing,Nothing)
                _   | isOrig
                        ->  let  ands = provenAnds pr
                            in   if null ands || all (not . needed) ands
                                 then (Just pr,Just pr)
                                 else (Nothing,Nothing)
                    | otherwise
                        ->  (Nothing,Nothing)
           where  isOrig = pr `elemFM` p2oi
                  needed n = any (`elementOf` prDoneS) (dpdPrsOf n)
%%]

%%[9
%%]
prvgArgLeaves :: ProvenGraph -> [PredOccId] -> [Pred]
prvgArgLeaves g@(ProvenGraph i2n _ p2oi _) topSort
  =  filter (any (not . tvCatIsFixed) . eltsFM . tyFtvMp . predTy) l
  where  provenAnds pr = [ n | n@(ProvenAnd ndPr _ _ _ _) <- eltsFM i2n, ndPr == pr ]
         isFact = prvgIsFact g
         (l,_)
           = foldr  (\poi sl@(l,d) ->
                        let  n = fromJust . lookupFM i2n $ poi
                             pr = prvnPred n
                             isOrig = pr `elemFM` p2oi
                             done = pr `elementOf` d
                             dpdPrsOf (ProvenAnd _ es _ _ _)
                               = catMaybes . map (fmap prvnPred . lookupFM i2n) . filter (not . isFact) $ es
                             needed n = any (`elementOf` d) (dpdPrsOf n)
                        in   case n of
                                 ProvenAnd _ es _ _ _ | not isOrig && not done
                                       ->  if needed n
                                           then (l,addToSet d pr)
                                           else sl
                                 _ | isOrig && not done
                                       ->  let  ands = provenAnds pr
                                           in   if null ands || all (not . needed) ands
                                                then (pr:l,addToSet d pr)
                                                else sl
                                   | otherwise
                                       ->  sl
                    )
                    ([],emptySet)
                    topSort

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% All nodes only (indirectly) satisfying a condition
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgSatisfiedNodeS :: Bool -> (PredOccId -> Bool) -> ProvenGraph -> [PredOccId] -> Set PredOccId
prvgSatisfiedNodeS hideLamArg satPoi
  =  prvgTopsortCompute (sat,\i _ -> Just i,addToSet,addToSet,emptySet)
  where  sat poi n _ res
           =  (Just poi,r)
           where  r =  case n of
                         ProvenAnd _ es les _ _
                           | satPoi poi || not (null es)
                                           && all (\i -> i `elementOf` res || satPoi i || hideLamArg && i `elem` les) es
                             ->  Just poi
                         _   ->  Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Back to orginal nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgBackToOrig :: ProvenGraph -> ProvenGraph
prvgBackToOrig g@(ProvenGraph i2n p2i _ p2fi)
  =  let  backL         =  [ (uid,o)  | (p,uidL@(i:is)) <- fmToList p2i
                                      , let {mo = lookupFM p2fi p; o = maybe i head mo}
                                      , uid <- maybe is (const uidL) mo
                           ]
          backFM        =  listToFM backL
          backUid uid   =  maybe uid id (lookupFM backFM uid)
          backCSubst    =  poiCExprLToCSubst . assocLMapElt mkCExprPrHole $ backL
          backN n       =  case n of
                             ProvenAnd pr es les c ev    -> ProvenAnd pr (map backUid es) (map backUid les) c (backCSubst `cSubstApp` ev)
                             ProvenOr pr es c            -> ProvenOr pr (map backUid es) c
                             ProvenShare pr e            -> ProvenShare pr (backUid e)
                             _                           -> n
     in   g  { prvgIdNdMp = listToFM . map (\(i,n) -> (backUid i,backN n)) . fmToList $ i2n
             , prvgPrIdMp = mapFM (\p l -> maybe l (\(i:_) -> [i]) (lookupFM p2fi p)) $ p2i
             }
%%]

%%[9
prvgShareMp :: ProvenGraph -> [PredOccId] -> PoiSubst
prvgShareMp g@(ProvenGraph _ p2i p2oi _) topSort
  =  PoiSubst m
  where  (m,_) = prvgTopsortCompute (sat,\i _ -> Just i,addToSet,upd,(emptyFM,emptyFM)) g topSort
         upd (i2i,p2ii) (i1,i2,Just (pr,ii))  = (addToFM i2i i1 i2,addToFM p2ii pr (i2,ii))
         upd (i2i,p2ii) (i1,i2,Nothing)       = (addToFM i2i i1 i2,p2ii)
         sat i n _ res@(i2i,p2ii)
           =  (Just i,r)
           where r =  case n of
                        ProvenArg pr _
                            | isJust mbSh
                                ->  Just (i,shI,Nothing)
                            | otherwise
                                ->  Just (i,i,Just (pr,[]))
                        ProvenAnd pr es _ _ _
                            | null es
                                ->  Just (i,i,Just (pr,[]))
                            | isJust mbSh && shEs == esShL
                                ->  Just (i,shI,Nothing)
                            | otherwise
                                ->  Just (i,i,Just (pr,esShL))
                            where  esShL = map (\i -> maybe i id (lookupFM i2i i)) es
                        _   ->  Nothing
                   where  mbSh        = lookupFM p2ii (prvnPred n)
                          (shI,shEs)  = fromJust mbSh
%%]

%%[9
prvg2BackMp :: ProvenGraph -> FiniteMap PredOccId PredOccId
prvg2BackMp g@(ProvenGraph i2n p2i p2oi _)
  =  let  isFact         =  prvgIsFact g
          argPoiS        =  mkSet [ i | (i,n) <- fmToList i2n, prvnIsArg n ]
          isArg          =  (`elementOf` argPoiS)
          mkBack b       =  listToFM [ (bp,p) | (pr,(p:pL)) <- b, bp <- maybe [] id (lookupFM p2i pr) ++ pL, not (isFact bp) ]
          backL          =  fmToList p2oi
          backOrigMp     =  mkBack backL
          backIntermMp   =  mkBack (fmToList p2i)
     in   backIntermMp `plusFM` backOrigMp
%%]

%%[9
prvgAppPoiSubst :: PoiSubst -> ProvenGraph -> ProvenGraph
prvgAppPoiSubst pois g@(ProvenGraph i2n p2i p2oi p2fi)
  =  let  backPoi       =  (pois `poisApp`)
          backPoiL      =  nub . map backPoi
          backPrFM      =  mapFM (\_ l -> backPoiL l)
          backN n       =  case n of
                             ProvenAnd pr es les c ev    -> ProvenAnd pr (backPoiL es) (backPoiL les) c ev
                             ProvenOr pr es c            -> ProvenOr pr (backPoiL es) c
                             ProvenShare pr e            -> ProvenShare pr (backPoi e)
                             _                           -> n
     in   g  { prvgIdNdMp = listToFM . map (\(i,n) -> (backPoi i,backN n)) . fmToList $ i2n
             , prvgPrIdMp = backPrFM p2i
             , prvgPrOrigIdMp = backPrFM p2oi
             }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functional dependency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ClsFuncDep = ClsFuncDep [Int] [Int] deriving Show

instance PP ClsFuncDep where
  pp (ClsFuncDep f t) = ppCommaList f >|< "->" >|< ppCommaList t
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data Rule
  =  Rule
       { rulRuleTy          :: Ty
       , rulMkEvid          :: [CExpr] -> CExpr
       , rulNmEvid          :: HsName
       , rulId              :: PredOccId
       , rulCost            :: ProofCost
       , rulFuncDeps        :: [ClsFuncDep]
       }

emptyRule = Rule Ty_Any head hsnUnknown (mkPrId uidStart uidStart) (mkCost costALot) []

mkInstElimRule :: HsName -> PredOccId -> Int -> Ty -> Rule
mkInstElimRule n i sz ctxtToInstTy
  =  let  r =  Rule  { rulRuleTy    = ctxtToInstTy
                     , rulMkEvid    = \ctxt -> CExpr_Var n `mkCExprApp` ctxt
                     , rulNmEvid    = n
                     , rulId        = i
                     , rulCost      = costBase `costAdd` (costBase `costMulBy` 2 * sz)
                     , rulFuncDeps  = []
                     }
     in   r

instance Substitutable Rule where
  s |=>  r = r { rulRuleTy = s |=> rulRuleTy r }
  ftv    r = ftv (rulRuleTy r)

instance Show Rule where
  show r = show (rulNmEvid r) ++ "::" ++ show (rulRuleTy r)

instance PP Rule where
  pp r = pp (rulRuleTy r) >#< ":>" >#< pp (rulNmEvid r) >|< "/" >|< pp (rulId r) >#< "|" >|< ppCommaList (rulFuncDeps r)
%%]
instance PP Rule where
  pp r = ppTyPr (rulRuleTy r) >#< ":>" >#< pp (rulNmEvid r) >|< "/" >|< pp (rulId r) >#< "|" >|< ppCommaList (rulFuncDeps r)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for intro rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data PrIntroGamInfo
  =  PrIntroGamInfo
       { pigiPrToEvidTy     :: Ty
       , pigiKi             :: Ty
       , pigiRule           :: Rule
       } deriving Show

type PrIntroGam     = Gam HsName PrIntroGamInfo

emptyPIGI = PrIntroGamInfo Ty_Any Ty_Any emptyRule

instance PP PrIntroGamInfo where
  pp pigi = pp (pigiRule pigi) >#< "::" >#< ppTy (pigiPrToEvidTy pigi) >#< ":::" >#< ppTy (pigiKi pigi)

instance Substitutable PrIntroGamInfo where
  s |=> pigi        =   pigi { pigiKi = s |=> pigiKi pigi }
  ftv   pigi        =   ftv (pigiKi pigi)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Info for Gamma for elim rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data PrElimGamInfo
  =  PrElimGamInfo
       { pegiRuleL          :: [Rule]
       } deriving Show

instance Substitutable PrElimGamInfo where
  s |=>  pegi = pegi { pegiRuleL = s |=> pegiRuleL pegi }
  ftv    pegi = ftv (pegiRuleL pegi)

instance PP PrElimGamInfo where
  pp pegi = ppCommaList (pegiRuleL pegi)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for elim rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type PrElimGam      = Gam HsName PrElimGamInfo

peGamAdd :: HsName -> Rule -> PrElimGam -> PrElimGam
peGamAdd n r g
  =  let  (h,t) = gamPop g
          h' = gamUpdAdd n (PrElimGamInfo [r]) (\_ p -> p {pegiRuleL = r : pegiRuleL p}) h
     in   t `gamPushGam` h'

peGamDel :: HsName -> Rule -> PrElimGam -> PrElimGam
peGamDel n r g
  =  let  (h,t) = gamPop g
          h' = gamUpd n (\_ p -> p {pegiRuleL = deleteBy (\r1 r2 -> rulId r1 == rulId r2) r . pegiRuleL $ p}) h
     in   t `gamPushGam` h'

peGamAddKnPr :: HsName -> PredOccId -> Pred -> PrElimGam -> PrElimGam
peGamAddKnPr n i p
  = peGamAdd (predMatchNm p) (mkInstElimRule n i 0 (mkTyPr p))

peGamAddKnPrL :: PrfCtxtId -> UID -> [Pred] -> PrElimGam -> (PrElimGam,[HsName],[PredOccId])
peGamAddKnPrL ci i prL g
  =  foldr  (\(i,p) (g,nL,idL) -> let n = poiHNm i in (peGamAddKnPr n i p g,n:nL,i:idL))
            (g,[],[])
            (zip (map (mkPrId ci) . mkNewUIDL (length prL) $ i) prL)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for elim rules (tgam version)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type PrElimTGam = TreeGam PrfCtxtId HsName PrElimGamInfo

peTGamAdd :: PrfCtxtId -> HsName -> Rule -> PrElimTGam -> PrElimTGam
peTGamAdd ci n r g
  =  let  (h,mnci,t) = tgamPop ci ci g
          h' = tgamUpdAdd ci n (PrElimGamInfo [r]) (\_ p -> p {pegiRuleL = r : pegiRuleL p}) h
     in   maybe h' (\nci -> tgamPushGam nci ci ci t h') mnci

peTGamDel :: PrfCtxtId -> HsName -> Rule -> PrElimTGam -> PrElimTGam
peTGamDel ci n r g
  =  maybe g id . tgamMbUpd ci n (\_ p -> p {pegiRuleL = deleteBy (\r1 r2 -> rulId r1 == rulId r2) r . pegiRuleL $ p}) $ g

peTGamAddKnPr :: PrfCtxtId -> HsName -> PredOccId -> Pred -> PrElimTGam -> PrElimTGam
peTGamAddKnPr ci n i p
  = peTGamAdd ci (predMatchNm p) (mkInstElimRule n i 0 (mkTyPr p))

peTGamAddKnPrL :: PrfCtxtId -> UID -> [Pred] -> PrElimTGam -> (PrElimTGam,[HsName],[PredOccId])
peTGamAddKnPrL ci i prL g
  =  foldr  (\(i,p) (g,nL,idL) -> let n = poiHNm i in (peTGamAddKnPr ci n i p g,n:nL,i:idL))
            (g,[],[])
            (zip (map (mkPrId ci) . mkNewUIDL (length prL) $ i) prL)

peTGamPoiS :: PrfCtxtId -> PrElimTGam -> Set PredOccId
peTGamPoiS ci = unionManySets . map (mkSet . map rulId . pegiRuleL) . tgamElts ci
%%]
peTGamDel :: PrfCtxtId -> HsName -> Rule -> PrElimTGam -> PrElimTGam
peTGamDel ci n r g
  =  let  (h,mnci,t) = tgamPop ci ci g
          h' = maybe h id . tgamMbUpd ci n (\_ p -> p {pegiRuleL = deleteBy (\r1 r2 -> rulId r1 == rulId r2) r . pegiRuleL $ p}) $ h
     in   maybe h' (\nci -> tgamPushGam nci ci ci t h') mnci


