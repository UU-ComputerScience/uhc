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

%%[9 import(EHTy,EHTyPretty,EHTyQuantify,EHCore,EHCorePretty,EHCoreSubst,EHCommon,EHGam,EHCnstr,EHSubstitutable)
%%]

%%[9 import(EHDebug)
%%]

%%[9 import(EHError) export(ProofState(..))
%%]

%%[9 export(ProvenNode(..),ProvenGraph(..),prvgAddPrNd,prvgAddPrUids,prvgAddNd,ProofCost(..),prvgCode,prvgBackToOrig,prvgReachableFrom)
%%]

%%[9 export(Rule(..),emptyRule,mkInstElimRule,costAdd,costALot,costBase)
%%]

%%[9 export(ClsFuncDep)
%%]

%%[9 export(PrIntroGamInfo(..),PrIntroGam,emptyPIGI)
%%]

%%[9 export(PrElimGamInfo(..),PrElimGam,peGamAdd,peGamDel,peGamAddKnPr,peGamAddKnPrL)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver, proven nodes in proven graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProvenNode
  =  ProvenOr       { prvnPred           :: Pred        , prvnEdges         :: [UID]
                    , prvnCost           :: ProofCost
                    }
  |  ProvenAnd      { prvnPred           :: Pred        , prvnEdges         :: [UID]
                    , prvnCost           :: ProofCost   , prvnEvid          :: CExpr
                    }
  |  ProvenShare    { prvnPred           :: Pred        , prvnEdge          :: UID          }
  |  ProvenArg      { prvnPred           :: Pred        , prvnCost          :: ProofCost    }

instance Show ProvenNode where
  show _ = ""

instance PP ProvenNode where
  pp (ProvenOr  pr es c)     = "OR:"    >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es >#< "cost=" >|< pp c
  pp (ProvenAnd pr es c ev)  = "AND:"   >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es >#< "cost=" >|< pp c >#< "evid=" >|< pp ev
  pp (ProvenShare pr e    )  = "SHARE:" >#< "pr=" >|< pp pr >#< "edge="  >|< ppCommaList [e]
  pp (ProvenArg pr c)        = "ARG:"   >#< "pr=" >|< pp pr                                 >#< "cost=" >|< pp c
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver, proven graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProvenGraph
  =  ProvenGraph
       { prvgIdNdMp         :: FiniteMap UID ProvenNode
       , prvgPrIdMp         :: FiniteMap Pred [UID]
       , prvgPrOrigIdMp     :: FiniteMap Pred [UID]
       }

prvgAddPrUids :: Pred -> [UID] -> ProvenGraph -> ProvenGraph
prvgAddPrUids pr uidL g@(ProvenGraph _ p2i _)
  =  g {prvgPrIdMp = addToFM_C (++) p2i pr uidL}

prvgAddPrNd :: Pred -> [UID] -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddPrNd pr uidL@(uid:_) nd g@(ProvenGraph i2n p2i _)
  =  (prvgAddPrUids pr uidL g) {prvgIdNdMp = addToFM i2n uid nd}

prvgAddNd :: UID -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddNd uid nd g@(ProvenGraph i2n _ _)
  =  g {prvgIdNdMp = addToFM i2n uid nd}

instance Show ProvenGraph where
  show _ = ""

instance PP ProvenGraph where
  pp (ProvenGraph i2n p2i p2oi)
    =  (ppAssocL . assocLMapSnd ppCommaList . fmToList $ p2i)
       >-< ppAssocL (fmToList i2n)
       >-< ppAssocL (fmToList p2oi)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver, state of prover
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofState
  =  ProofState     { prfsProvenGraph        :: ProvenGraph     , prfsUniq               :: UID
                    , prfsPredsToProve       :: [PredOcc]       , prfsPredsOrigToProve   :: [Pred]
                    , prfsErrL               :: [Err]
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
prvgCode :: [PredOcc] -> ProvenGraph -> (CBindLMap,CSubst,Set PredOccId)
prvgCode prL g@(ProvenGraph i2n p2i p2oi)
  =  let  i2nL = fmToList i2n
          p2iL = fmToList p2i
          i2i  = listToFM [ (i,ii) | (_,ii) <- p2iL, i <- ii ]
          (provenPrL,argPrL,overlapPrL)
            = (concat p, concat a, concat o)
            where  spl n@(_,ProvenAnd _ _ _ _) = ([n],[],[])
                   spl n@(_,ProvenArg _ _    ) = ([],[n],[])
                   spl n                       = ([],[],[n])
                   (p,a,o) = unzip3 . map spl $ i2nL
          canAsSubst (_,CExpr_Var _)  = True
          canAsSubst (_,CExpr_Int _)  = True
          canAsSubst _                = False
          (provenAsCSubst,(_,provenForBind))
            = head . dropWhile (\(_,(l,_)) -> not (null l))
            . iterate
                (\(s,(asS,asB))
                    -> let asS' = concat . map (\(i,n) -> zip (maybe [] id . lookupFM i2i $ i) (repeat n)) $ asS
                           s' = s `cAppSubst` assocLCExprToCSubst asS'
                       in  (s',partition canAsSubst (s' `cAppSubst` asB))
                )
            $ (emptyCSubst,partition canAsSubst (map (\(i,ProvenAnd _ _ _ e) -> (i,e)) provenPrL))
          r = map fst argPrL
          m1 = provenAsCSubst `cAppSubst` assocLCExprToCSubst [ (i,CExpr_Var (uidHNm i)) | (i,_) <- provenForBind ]
          m2 = assocLCExprToCSubst . concat
             . map (\(p,(uid:uidL))
                        -> let allUidL = (uidL `List.union` maybe [] id (lookupFM p2oi p)) \\ [uid]
                           in  zip allUidL (repeat (CExpr_Hole uid))
                   )
             $ p2iL
          b = let s p = let rAll = prvgReachableTo g [poId p]
                            r = rAll `delFromSet` poId p
                        in  (poId p,(rAll,[ CBind_Bind (uidHNm i) ev | (i,ev) <- provenForBind, i `elementOf` r ]))
              in  map s . map (\(p,(i:_)) -> PredOcc p (maybe i head . lookupFM p2oi $ p)) $ p2iL
          rem = concat [ uidL | (_,uidL) <- p2iL, any (`elem` r) uidL ]
     in   (listToFM b,m1 `cAppSubst` m2,mkSet rem)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reachable nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgReachableFrom :: ProvenGraph -> [UID] -> Set UID
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

prvgReachableTo :: ProvenGraph -> [UID] -> Set UID
prvgReachableTo (ProvenGraph i2n _ _)
  =  let  allNd = eltsFM i2n
          r uid reachSet
            =  if uid `elementOf` reachSet
               then  reachSet
               else  let  reachSet' = addToSet reachSet uid
                          reachFrom fromNd toUid
                            =  case fromNd of
                                 ProvenAnd _ es _ _  | toUid `elem` es  -> True
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
          backCSubst    =  assocLCExprToCSubst . assocLMapSnd (CExpr_Hole) $ backL
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
%%% Cost
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofCost  = CostAvailImpl {costCost :: Int} | CostInt {costCost :: Int}
                deriving (Eq,Show,Ord)

costALot, costBase :: Int
costALot  = 100
costBase  = 1

costAdd :: ProofCost -> ProofCost -> ProofCost
costAdd (CostInt c1)            (CostInt c2)            = CostInt (c1 + c2)
costAdd (CostAvailImpl c1)      (CostAvailImpl c2)      = CostAvailImpl (c1 + c2)
costAdd (CostAvailImpl _ )      c2                      = c2
costAdd c1                      _                       = c1

instance PP ProofCost where
  pp (CostInt c)       = pp c
  pp (CostAvailImpl c) = pp "-Inf:" >|< pp c
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

emptyRule = Rule Ty_Any head hsnUnknown uidStart (CostInt costALot) []

mkInstElimRule :: HsName -> PredOccId -> Int -> Ty -> Rule
mkInstElimRule n i sz ctxtToInstTy
  =  let  r =  Rule  { rulRuleTy    = ctxtToInstTy
                     , rulMkEvid    = \ctxt -> CExpr_Var n `mkCExprApp` ctxt
                     , rulNmEvid    = n
                     , rulId        = i
                     , rulCost      = CostInt (costBase + 2 * costBase * sz)
                     , rulFuncDeps  = []
                     }
     in   r

instance Substitutable Rule where
  s |=>  r = r { rulRuleTy = s |=> rulRuleTy r }
  ftv    r = ftv (rulRuleTy r)

instance Show Rule where
  show r = show (rulNmEvid r) ++ "::" ++ show (rulRuleTy r)

instance PP Rule where
  pp r = ppTyPr (rulRuleTy r) >#< ":>" >#< pp (rulNmEvid r) >|< "/" >|< pp (rulId r) >#< "|" >|< ppCommaList (rulFuncDeps r)
%%]

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
peGamAddKnPr n i p = peGamAdd (predMatchNm p) (mkInstElimRule n i 0 (Ty_Pred p))

peGamAddKnPrL :: PredOccId -> [Pred] -> PrElimGam -> (PrElimGam,[HsName],[PredOccId])
peGamAddKnPrL i prL g
  =  foldr  (\(i,p) (g,nL,idL) -> let n = uidHNm i in (peGamAddKnPr n i p g,n:nL,i:idL))
            (g,[],[])
            (zip (mkNewUIDL (length prL) i) prL)
%%]


