% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 import(Maybe,List,FiniteMap,Set,UU.Pretty)
%%]

%%[9 import(EHTy,EHTyPretty,EHTyQuantify,EHCode,EHCodePretty,EHCodeSubst,EHCommon,EHGam,EHCnstr)
%%]

%%[9 import(EHDebug)
%%]

%%[9 export(ProofState(..))
%%]

%%[9 export(ProvenNode(..),ProvenGraph(..),prvgAddPrNd,prvgAddPrUids,prvgAddNd,ProofCost(..),prvgCode,prvgReachableFrom)
%%]

%%[9 export(Rule(..),emptyRule,mkInstElimRule,costAdd,costALot,costBase)
%%]

%%[9 export(PrIntroGamInfo(..),PrIntroGam,emptyPIGI)
%%]

%%[9 export(PrElimGamInfo(..),PrElimGam,peGamAdd,peGamAddKnPr)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofState
  =  ProofState
       { prfsProvenGraph        :: ProvenGraph
       , prfsUniq               :: UID
       , prfsPredsToProve       :: [PredOcc]
       , prfsPredsOrigToProve   :: [PredOcc]
       }

data ProvenNode
  =  ProvenOr
       { prvnPred           :: Pred
       , prvnOrEdges        :: [UID]
       , prvnCost           :: ProofCost
       }
  |  ProvenAnd
       { prvnPred           :: Pred
       , prvnAndEdges       :: [UID]
       , prvnCost           :: ProofCost
       , prvnEvidence       :: CExpr
       }
  |  ProvenShare
       { prvnPred           :: Pred
       , prvnShareEdge      :: UID
       }
  |  ProvenArg
       { prvnPred           :: Pred
       , prvnCost           :: ProofCost
       }
  
data ProvenGraph
  =  ProvenGraph
       { prvgIdNdMp         :: FiniteMap UID ProvenNode
       , prvgPrIdMp         :: FiniteMap Pred (Maybe UID,[UID])
       }

prvgAddPrUids :: Pred -> Maybe UID -> [UID] -> ProvenGraph -> ProvenGraph
prvgAddPrUids pr mbOrig uidL g@(ProvenGraph _ p2i)
  =  g {prvgPrIdMp = addToFM_C (\(mo,o) (mn,n) -> (maybe mo Just mn,o++n)) p2i pr (mbOrig,uidL)}

prvgAddPrNd :: Pred -> Maybe UID -> [UID] -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddPrNd pr mbOrig uidL@(uid:_) nd g@(ProvenGraph i2n p2i)
  =  (prvgAddPrUids pr mbOrig uidL g) {prvgIdNdMp = addToFM i2n uid nd}

prvgAddNd :: UID -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddNd uid nd g@(ProvenGraph i2n _)
  =  g {prvgIdNdMp = addToFM i2n uid nd}

instance Show ProvenNode where
  show _ = ""

instance Show ProvenGraph where
  show _ = ""

instance PP ProvenNode where
  pp (ProvenOr  pr es c)     = "OR:"    >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es >#< "cost=" >|< pp c
  pp (ProvenAnd pr es c ev)  = "AND:"   >#< "pr=" >|< pp pr >#< "edges=" >|< ppCommaList es >#< "cost=" >|< pp c >#< "evid=" >|< pp ev
  pp (ProvenShare pr e    )  = "SHARE:" >#< "pr=" >|< pp pr >#< "edge="  >|< ppCommaList [e]
  pp (ProvenArg pr c)        = "ARG:"   >#< "pr=" >|< pp pr                                 >#< "cost=" >|< pp c

instance PP ProvenGraph where
  pp (ProvenGraph i2n p2i)
    =  (ppAssocL
       . assocLMapSnd (\(mbOrig,uidL) -> maybe (pp "-") pp mbOrig >|< "/" >|< ppCommaList uidL)
       . fmToList $ p2i)
       >-< ppAssocL (fmToList i2n)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code for a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgCode :: [PredOcc] -> ProvenGraph -> (CBindLMap,CSubst,Set PredOccId)
prvgCode prL g@(ProvenGraph i2n p2i)
  =  let  i2nL = fmToList i2n
          p2iL = fmToList p2i
          i2i  = listToFM [ (i,ii) | (_,(_,ii)) <- p2iL, i <- ii ]
          canAsSubst (_,CExpr_Var _)  = True
          canAsSubst _                = False
          (provenL,argL,overlapL)
            = (concat p, concat a, concat o)
            where  spl n@(_,ProvenAnd _ _ _ _) = ([n],[],[])
                   spl n@(_,ProvenArg _ _    ) = ([],[n],[])
                   spl n                       = ([],[],[n])
                   (p,a,o) = unzip3 . map spl $ i2nL
          (provenAsCSubst,(_,provenForBind))
            = head
            . dropWhile (\(_,(l,_)) -> not (null l))
            . iterate
                (\(s,(asS,asB))
                    -> let asS' = concat . map (\(i,n) -> zip (maybe [] id . lookupFM i2i $ i) (repeat n)) $ asS
                           s' = s `cAppSubst` assocLCExprToCSubst asS'
                       in  (s',partition canAsSubst (s' `cAppSubst` asB))
                )
            $ (emptyCSubst,partition canAsSubst (map (\(i,ProvenAnd _ _ _ e) -> (i,e)) provenL))
          r = map fst argL
          m1 = provenAsCSubst `cAppSubst` assocLCExprToCSubst [ (i,CExpr_Var (uidHNm i)) | (i,_) <- provenForBind ]
          m2 = assocLCExprToCSubst . concat . map (\(_,(_,uidL@(uid:_))) -> zip uidL (repeat (m1 `cAppSubst` CExpr_Hole uid))) $ p2iL
          b = let s p = let r = prvgReachableTo g [poId p] `delFromSet` poId p
                        in  (poId p,[ CBind_Bind (uidHNm i) ev | (i,ev) <- provenForBind, i `elementOf` r ])
              in  map s prL
          rem = concat [ uidL | (_,(_,uidL)) <- p2iL, any (`elem` r) uidL ]
     in   (listToFM b,m2,mkSet rem)
%%]

          b = map (\(i,ev) -> CBind_Bind (uidHNm i) (m2 `cAppSubst` ev)) provenForBind

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reachable nodes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgReachableFrom :: ProvenGraph -> [UID] -> Set UID
prvgReachableFrom (ProvenGraph i2n _)
  =  let  r uid reachSet
            =  if uid `elementOf` reachSet
               then  reachSet
               else  let  reachSet' = addToSet reachSet uid
                     in   case fromJust (lookupFM i2n uid) of
                                ProvenAnd _ es _ _  -> rr reachSet' es
                                ProvenOr  _ es _    -> rr reachSet' es
                                _                   -> reachSet'
          rr = foldr r
     in   rr emptySet

prvgReachableTo :: ProvenGraph -> [UID] -> Set UID
prvgReachableTo (ProvenGraph i2n _)
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
%%% Cost
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data ProofCost  = CostLow | CostInt Int
                deriving (Eq,Show,Ord)

costALot, costBase :: Int
costALot  = 100
costBase  = 1

costAdd :: ProofCost -> ProofCost -> ProofCost
costAdd (CostInt c1) (CostInt c2)  = CostInt (c1 + c2)
costAdd CostLow      c2            = c2
costAdd c1           _             = c1

instance PP ProofCost where
  pp (CostInt c) = pp c
  pp CostLow     = pp "-Inf"
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
       , rulCost            :: ProofCost
       }

emptyRule = Rule Ty_Any head hsnUnknown (CostInt costALot)

mkInstElimRule :: HsName -> Int -> Ty -> Rule
mkInstElimRule n sz ctxtToInstTy
  =  let  r =  Rule  { rulRuleTy    = ctxtToInstTy
                     , rulMkEvid    = \ctxt -> CExpr_Var n `mkCExprApp` ctxt
                     , rulNmEvid    = n
                     , rulCost      = CostInt (costBase + 2 * costBase * sz)
                     }
     in   r

instance Substitutable Rule where
  s |=>  r = r { rulRuleTy = s |=> rulRuleTy r }
  ftv    r = ftv (rulRuleTy r)

instance Show Rule where
  show r = show (rulNmEvid r) ++ "::" ++ show (rulRuleTy r)

instance PP Rule where
  pp r = ppTy (rulRuleTy r) >#< "~>" >#< pp (rulNmEvid r)
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

peGamAddKnPr :: HsName -> Pred -> PrElimGam -> PrElimGam
peGamAddKnPr n p = peGamAdd (predNm p) (mkInstElimRule n 0 (Ty_Pred p))
%%]


