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

%%[9 export(ProvenGraph(..),prvgAddPrNd,prvgAddPrUids,prvgAddNd,prvgCode,prvgCxBindLMap,prvgBackToOrig,prvgReachableFrom,prvgReachableTo,prvgIsFact)
%%]

%%[9 export(prvg2ReachableFrom)
%%]

%%[9 export(Rule(..),emptyRule,mkInstElimRule)
%%]

%%[9 export(ProofCost(..),mkCost,mkCostAvailImpl,costVeryMuch,costZero,costNeg,costAdd,costALot,costBase,costAddHi)
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

%%[9 import(EHTyFtv) export(prvgArgLeaves,prvgOrigs,prvg2BackToOrig,prvg2BackMp)
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

costVeryMuch = Cost 100 0
costZero = mkCost 0
costNeg = Cost (-2) 0

mkCostAvailImpl :: Int -> ProofCost
mkCostAvailImpl c = Cost (-1) c

costALot, costBase :: Int
costALot  = 100
costBase  = 1

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
  =  ProvenOr       { prvnPred           :: Pred        , prvnEdges         :: [PredOccId]
                    , prvnCost           :: ProofCost
                    }
  |  ProvenAnd      { prvnPred           :: Pred        , prvnEdges         :: [PredOccId]
                    , prvnCost           :: ProofCost   , prvnEvid          :: CExpr
                    }
  |  ProvenShare    { prvnPred           :: Pred        , prvnEdge          :: PredOccId    }
  |  ProvenArg      { prvnPred           :: Pred        , prvnCost          :: ProofCost    }
  |  ProvenLcl      { prvnPred           :: Pred        , prvnEvid          :: CExpr        }

prvnIsAnd :: ProvenNode -> Bool
prvnIsAnd (ProvenAnd _ _ _ _) = True
prvnIsAnd _                   = False

instance Show ProvenNode where
  show _ = ""

instance PP ProvenNode where
  pp (ProvenOr  pr es c)     = "OR:"    >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es >#< "cost=" >|< pp c
  pp (ProvenAnd pr es c ev)  = "AND:"   >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es >#< "cost=" >|< pp c >#< "evid=" >|< pp ev
  pp (ProvenShare pr e    )  = "SHARE:" >#< "pr=" >|< pp pr >#< "edge="  >|< ppCommaList [e]
  pp (ProvenArg pr c)        = "ARG:"   >#< "pr=" >|< pp pr                                 >#< "cost=" >|< pp c
  pp (ProvenLcl pr ev)       = "LCL:"   >#< "pr=" >|< pp pr                                                      >#< "evid=" >|< pp ev
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
                             s' = s `cAppSubst` poiCExprLToCSubst asS'
                         in  (s',partition canAsSubst (s' `cAppSubst` asB))
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
            where  spl n@(_,ProvenAnd _ _ _ _) = ([n],[],[])
                   spl n@(_,ProvenLcl _ _    ) = ([n],[],[])
                   spl n@(_,ProvenArg _ _    ) = ([],[n],[])
                   spl n                       = ([],[],[n])
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
          allCSubst = provenAsCSubst `cAppSubst` provenRefToBoundCSubst `cAppSubst` aliasRefCSubst
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
          bindMp
            = let s p = let rAll = prvgReachableTo g [poId p]
                            r = rAll `delFromSet` poId p
                        in  (poId p
                            ,(rAll
                             ,[ CBind_Bind (poiHNm i) (allCSubst `cAppSubst` ev) | (i,ev) <- provenForBind, i `elementOf` r ]
                            ))
              in  map s . map (\(p,(i:_)) -> PredOcc p (maybe i head . lookupFM p2oi $ p)) $ p2iL

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code gen for a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgCxBindLMap :: ProvenGraph -> CSubst -> CxBindLMap
prvgCxBindLMap g@(ProvenGraph i2n _ _ p2fi) cSubst
  =  let  factPoiL = concat (eltsFM p2fi)
          leaveL = prvgAllLeaves g
          leaveS = mkSet leaveL
          binds  = [ (i,(CBind_Bind (poiHNm i) (cSubst `cAppSubst` prvnEvid n),r `Set.intersect` leaveS))
                   | (i,n) <- fmToList i2n
                   , prvnIsAnd n && not (i `elem` factPoiL)
                   , let (r,_) = prvg2ReachableFrom g [i]
                   ]
     in   CxBindLMap
          .  listToFM
          .  map  (\i -> let dpdS = prvgReachableTo g [i]
                         in  (i,[ b | (i,b) <- binds, i `elementOf` dpdS ])
                  )
          $  leaveL
%%]
prvgCxBindLMap :: ProvenGraph -> CxBindLMap
prvgCxBindLMap (ProvenGraph i2n _ _ p2fi)
  =  let  factPoiL = concat (eltsFM p2fi)
          m =  map  (\l@((poi,n):_)
                      ->  ((poiCxId poi,prvnPred n)
                          ,(mkSet (assocLKeys l)
                           ,[ CBind_Bind (poiHNm i) (prvnEvid p) | (i,p) <- l ]
                          ))
                    )
               . groupSortOn (poiCxId . fst)
               . filter (\(i,n) -> prvnIsAnd n && not (i `elem` factPoiL))
               . fmToList
               $ i2n
     in   listToFM m

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
                                Just (ProvenAnd _ es _ _ ) -> rr reachSet' es
                                Just (ProvenOr  _ es _   ) -> rr reachSet' es
                                _                          -> reachSet'
          rr = foldr r
     in   rr emptySet
%%]

%%[9
prvg2ReachableFrom :: ProvenGraph -> [PredOccId] -> (Set PredOccId,[PredOccId])
prvg2ReachableFrom (ProvenGraph i2n _ _ _) poiL
  =  let  nextOf poi
            =  case lookupFM i2n poi of
                  Just (ProvenAnd _ es _ _ ) -> es
                  Just (ProvenOr  _ es _   ) -> es
                  _                          -> []
          rr r@(reachSet,revTopSort) poiL
            =  let  newPoiL = filter (not . (`elementOf` reachSet)) . concat . map nextOf $ poiL
               in   if null newPoiL
                    then r
                    else rr (reachSet `Set.union` mkSet newPoiL,newPoiL ++ revTopSort) newPoiL
     in   rr (mkSet poiL,poiL) poiL
%%]

%%[9
prvgReachableTo :: ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableTo (ProvenGraph i2n _ _ _)
  =  let  allNd = eltsFM i2n
          r uid reachSet
            =  if uid `elementOf` reachSet
               then  reachSet
               else  let  reachSet' = addToSet reachSet uid
                          reachFrom fromNd toUid
                            =  case fromNd of
                                 ProvenAnd _ es _ _  | toUid `elem` (filter (\i -> case lookupFM i2n i of {Just (ProvenLcl _ _) -> False ; _ -> True}) es)
                                                                        -> True
                                 ProvenOr  _ es _    | toUid `elem` es  -> True
                                 _                                      -> False
                     in   rr reachSet' (keysFM . filterFM (\i n -> reachFrom n uid) $ i2n)
          rr = foldr r
     in   rr emptySet
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Original nodes, fact nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgOrigs :: ProvenGraph -> [PredOccId]
prvgOrigs (ProvenGraph _ _ p2oi _)
  =  concat (eltsFM p2oi)
%%]

%%[9
prvgFacts :: ProvenGraph -> [PredOccId]
prvgFacts (ProvenGraph _ _ _ p2fi)
  =  concat (eltsFM p2fi)

prvgIsFact :: ProvenGraph -> PredOccId -> Bool
prvgIsFact g = (`elem` prvgFacts g)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Nodes which do not depend on others
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgAllLeaves :: ProvenGraph -> [PredOccId]
prvgAllLeaves g@(ProvenGraph i2n _ p2oi _)
  = keysFM (filterFM (const isLf) i2n) ++ prvgFacts g
  where  isLf (ProvenArg _ _) = True
         isLf _               = False
         isFact = prvgIsFact g
%%]
prvgAllLeaves :: ProvenGraph -> [PredOccId]
prvgAllLeaves g@(ProvenGraph i2n _ p2oi _)
  = keysFM (filterFM (\i n -> not (isFact i) && isLf n) i2n)
  where  isLf (ProvenArg _ _     ) = True
         isLf (ProvenAnd _ es _ _) = null es || all isFact es
         isLf _               = False
         isFact = prvgIsFact g

%%[9
prvgArgLeaves :: ProvenGraph -> [PredOccId] -> [Pred]
prvgArgLeaves g@(ProvenGraph i2n _ p2oi _) topSort
  =  filter (any (not . tvCatIsFixed) . eltsFM . tyFtvMp . predTy) l
  where  provenAnds pr = [ n | n@(ProvenAnd ndPr _ _ _) <- eltsFM i2n, ndPr == pr ]
         isFact = prvgIsFact g
         (l,_)
           = foldr  (\poi sl@(l,d) ->
                        let  n = fromJust . lookupFM i2n $ poi
                             pr = prvnPred n
                             isOrig = pr `elemFM` p2oi
                             done = pr `elementOf` d
                             dpdPrsOf (ProvenAnd _ es _ _)
                               = catMaybes . map (fmap prvnPred . lookupFM i2n) . filter (not . isFact) $ es
                             needed n = any (`elementOf` d) (dpdPrsOf n)
                        in   case n of
                                 ProvenAnd _ es _ _ | not isOrig && not done
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
%%]

prvgArgLeaves :: ProvenGraph -> [(Pred,PrfCtxtId)]
prvgArgLeaves (ProvenGraph i2n _ p2oi _)
  =  [(pr,poiCxId poi) | (pr,poiL) <- fmToList p2oi, isNotProvenAnd pr, poi <- poiL ]
  where isNotProvenAnd pr = null [ ndPr | prvn <- eltsFM i2n, prvnIsAnd prvn, let ndPr = prvnPred prvn, ndPr == pr ]

prvgArgLeaves :: ProvenGraph -> [(Pred,PrfCtxtId)]
prvgArgLeaves (ProvenGraph i2n _ p2oi _)
  =  [(pr,ci) | (pr,poiL) <- fmToList p2oi, poi <- poiL, let ci = poiCxId poi, isNotProvenAnd ci pr ]
  where isNotProvenAnd ci pr = null [ ndPr | (poi,prvn) <- fmToList i2n, prvnIsAnd prvn, let ndPr = prvnPred prvn, ci == poiCxId poi, ndPr == pr ]

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
                             ProvenAnd pr es c ev    -> ProvenAnd pr (map backUid es) c (backCSubst `cAppSubst` ev)
                             ProvenOr pr es c        -> ProvenOr pr (map backUid es) c
                             ProvenShare pr e        -> ProvenShare pr (backUid e)
                             _                       -> n
     in   g  { prvgIdNdMp = listToFM . map (\(i,n) -> (backUid i,backN n)) . fmToList $ i2n
             , prvgPrIdMp = mapFM (\p l -> maybe l (\(i:_) -> [i]) (lookupFM p2fi p)) $ p2i
             }
%%]

%%[9
prvg2BackMp :: ProvenGraph -> FiniteMap PredOccId PredOccId
prvg2BackMp g@(ProvenGraph _ p2i p2oi p2fi)
  =  let  factPoiL       =  concat . eltsFM $ p2fi
          isAFact        =  (`elem` factPoiL)
          mkBack b       =  listToFM [ (bp,p) | (pr,(p:pL)) <- b, bp <- pL ++ maybe [] id (lookupFM p2i pr), not (isAFact bp) ]
          backL          =  fmToList p2oi
          omitIntermPrL  =  assocLKeys (backL ++ fmToList p2fi)
          backOrigMp     =  mkBack backL
          backIntermMp   =  mkBack (fmToList p2i)
     in   backIntermMp `plusFM` backOrigMp
%%]

%%[9
prvg2BackToOrig :: FiniteMap PredOccId PredOccId -> ProvenGraph -> ProvenGraph
prvg2BackToOrig poiBackMp g@(ProvenGraph i2n p2i p2oi p2fi)
  =  let  backPoi poi   =  maybe poi id (lookupFM poiBackMp poi)
          backN n       =  case n of
                             ProvenAnd pr es c ev    -> ProvenAnd pr (map backPoi es) c ev
                             ProvenOr pr es c        -> ProvenOr pr (map backPoi es) c
                             ProvenShare pr e        -> ProvenShare pr (backPoi e)
                             _                       -> n
     in   g  { prvgIdNdMp = listToFM . map (\(i,n) -> (backPoi i,backN n)) . fmToList $ i2n
             , prvgPrIdMp = mapFM (\_ (i:_) -> maybe [i] (:[]) (lookupFM poiBackMp i)) $ p2i
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
                     , rulCost      = mkCost (costBase + 2 * costBase * sz)
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
  =  let  (h,mnci,t) = tgamPop ci ci g
          h' = tgamUpd ci n (\_ p -> p {pegiRuleL = deleteBy (\r1 r2 -> rulId r1 == rulId r2) r . pegiRuleL $ p}) h
     in   maybe h' (\nci -> tgamPushGam nci ci ci t h') mnci

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


