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

%%[9 export(ProvenNode(..),ProvenGraph(..),prvgAddPrNd,prvgAddPrUids,prvgAddNd,prvgCode,prvgBackToOrig,prvgReachableFrom)
%%]

%%[9 export(Rule(..),emptyRule,mkInstElimRule)
%%]

%%[9 export(ProofCost(..),mkCost,mkCostAvailImpl,costAdd,costALot,costBase,costAddHi)
%%]

%%[9 export(ClsFuncDep)
%%]

%%[9 export(PrIntroGamInfo(..),PrIntroGam,emptyPIGI)
%%]

%%[9 export(PrElimGamInfo(..),PrElimGam,peGamAdd,peGamDel,peGamAddKnPr,peGamAddKnPrL)
%%]

%%[9 export(ProofCtxt(..),emptyProofCtxt)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cost
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofCost  = Cost {costHi, costLo :: Int}
                deriving (Eq,Show)

instance Ord ProofCost where
  (Cost h1 l1) `compare` (Cost h2 l2)
    | h1 == h2   = l1 `compare` l2
    | otherwise  = h1 `compare` h2

mkCost :: Int -> ProofCost
mkCost c = Cost 1 c

mkCostAvailImpl :: Int -> ProofCost
mkCostAvailImpl c = Cost 0 c

costALot, costBase :: Int
costALot  = 100
costBase  = 1

costAdd :: ProofCost -> ProofCost -> ProofCost
c1@(Cost h1 l1) `costAdd` c2@(Cost h2 l2) = Cost (h1 `max` h2) (l1 + l2)

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
       { prvgIdNdMp         :: FiniteMap PredOccId ProvenNode
       , prvgPrIdMp         :: FiniteMap Pred [PredOccId]
       , prvgPrOrigIdMp     :: FiniteMap Pred [PredOccId]
       }

prvgAddPrUids :: Pred -> [PredOccId] -> ProvenGraph -> ProvenGraph
prvgAddPrUids pr uidL g@(ProvenGraph _ p2i _)
  =  g {prvgPrIdMp = addToFM_C (++) p2i pr uidL}

prvgAddPrNd :: Pred -> [PredOccId] -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddPrNd pr uidL@(uid:_) nd g@(ProvenGraph i2n p2i _)
  =  (prvgAddPrUids pr uidL g) {prvgIdNdMp = addToFM i2n uid nd}

prvgAddNd :: PredOccId -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddNd uid nd g@(ProvenGraph i2n _ _)
  =  g {prvgIdNdMp = addToFM i2n uid nd}

instance Show ProvenGraph where
  show _ = ""

instance PP ProvenGraph where
  pp (ProvenGraph i2n p2i p2oi)
    =      "Pr->Ids    :" >#< (ppAssocL . assocLMapSnd ppCommaList . fmToList $ p2i)
       >-< "Id->Nd     :" >#< ppAssocL (fmToList i2n)
       >-< "Pr->OrigIds:" >#< ppAssocL (fmToList p2oi)
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
                       >-< ppCommaList (prfsPredsToProve s)
                       >-< ppCommaList (prfsPredsOrigToProve s))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code gen for a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgSplitIntoSubstBind :: ProvenGraph -> AssocL PredOccId ProvenNode -> (CSubst,AssocL PredOccId CExpr)
prvgSplitIntoSubstBind g@(ProvenGraph _ p2i _) provenNdL
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
                ,partition canAsSubst (assocLMapSnd prvnEvid provenNdL)
                )

prvgCode :: [PredOcc] -> ProvenGraph -> (CBindLMap,CSubst,Set PredOccId,PP_DocL)
prvgCode prL g@(ProvenGraph i2n p2i p2oi)
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
                        -> let allI = (iL `List.union` maybe [] id (lookupFM p2oi p)) \\ [i]
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
              in  map s . map (\(p,(i:_)) -> PredOcc p (maybe i head . lookupFM p2oi $ p)) $ p2iL
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
%%% Reachable nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgReachableFrom :: ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableFrom (ProvenGraph i2n _ _)
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
prvgReachableFrom :: ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableFrom (ProvenGraph i2n _ _)
  =  let  r uid rh@(reachSet,hideSet)
            =  if uid `elementOf` reachSet || uid `elementOf` hideSet
               then  rh
               else  let  reachSet' = addToSet reachSet uid
                     in   case lookupFM i2n uid of
                                Just (ProvenAnd _ es _ _ ) -> rr (reachSet',hideSet `Set.union` mkSet h) r
                                                              where (r,h) = partition (\i -> case lookupFM i2n i of {Just (ProvenLcl _ _) -> False ; _ -> True})
                                                                                      es
                                Just (ProvenOr  _ es _   ) -> rr (reachSet',hideSet) es
                                _                          -> (reachSet',hideSet)
          rr = foldr r
     in   fst . rr (emptySet,emptySet)

%%[9
prvgReachableTo :: ProvenGraph -> [PredOccId] -> Set PredOccId
prvgReachableTo (ProvenGraph i2n _ _)
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
%%% Back to orginal nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgBackToOrig :: ProvenGraph -> ProvenGraph
prvgBackToOrig g@(ProvenGraph i2n p2i p2oi)
  =  let  backL         =  [ (uid,o)  | (p,uidL@(i:is)) <- fmToList p2i
                                      , let {mo = lookupFM p2oi p; o = maybe i head mo}
                                      , uid <- maybe is (const uidL) mo
                           ]
          backFM        =  listToFM backL
          backUid uid   =  maybe uid id (lookupFM backFM uid)
          backCSubst    =  poiCExprLToCSubst . assocLMapSnd mkCExprPrHole $ backL
          backN n       =  case n of
                             ProvenAnd pr es c ev    -> ProvenAnd pr (map backUid es) c (backCSubst `cAppSubst` ev)
                             ProvenOr pr es c        -> ProvenOr pr (map backUid es) c
                             ProvenShare pr e        -> ProvenShare pr (backUid e)
                             _                       -> n
     in   g  { prvgIdNdMp = listToFM . map (\(i,n) -> (backUid i,backN n)) . fmToList $ i2n
             , prvgPrIdMp = mapFM (\p l -> maybe l (\(i:_) -> [i]) (lookupFM p2oi p)) $ p2i
             }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Functional dependency
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type ClsFuncDep = ([Int],[Int])

instance PP ClsFuncDep where
  pp (f,t) = ppCommaList f >|< "->" >|< ppCommaList t
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

emptyRule = Rule Ty_Any head hsnUnknown (mkPrId uidStart) (mkCost costALot) []

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
%%% Gamma for elim rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data PrElimGamInfo
  =  PrElimGamInfo
       { pegiRuleL          :: [Rule]
       } deriving Show

type PrElimGam      = Gam HsName PrElimGamInfo

instance Substitutable PrElimGamInfo where
  s |=>  pegi = pegi { pegiRuleL = s |=> pegiRuleL pegi }
  ftv    pegi = ftv (pegiRuleL pegi)

instance PP PrElimGamInfo where
  pp pegi = ppCommaList (pegiRuleL pegi)

peGamAdd :: HsName -> Rule -> PrElimGam -> PrElimGam
peGamAdd n r g
  =  let  (h,t) = gamPop g
          h' = gamUpdAdd n (PrElimGamInfo [r]) (\_ p -> p {pegiRuleL = r : pegiRuleL p}) h
     in   h' `gamPushGam` t

peGamDel :: HsName -> Rule -> PrElimGam -> PrElimGam
peGamDel n r g
  =  let  (h,t) = gamPop g
          h' = gamUpd n (\_ p -> p {pegiRuleL = deleteBy (\r1 r2 -> rulId r1 == rulId r2) r . pegiRuleL $ p}) h
     in   h' `gamPushGam` t

peGamAddKnPr :: HsName -> PredOccId -> Pred -> PrElimGam -> PrElimGam
peGamAddKnPr n i p
  = peGamAdd (predMatchNm p) (mkInstElimRule n i 0 (mkTyPr p))

peGamAddKnPrL :: UID -> [Pred] -> PrElimGam -> (PrElimGam,[HsName],[PredOccId])
peGamAddKnPrL i prL g
  =  foldr  (\(i,p) (g,nL,idL) -> let n = poiHNm i in (peGamAddKnPr n i p g,n:nL,i:idL))
            (g,[],[])
            (zip (map mkPrId . mkNewUIDL (length prL) $ i) prL)
%%]


