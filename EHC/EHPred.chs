% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 import(FiniteMap,Set,UU.Pretty)
%%]

%%[9 import(EHTy,EHTyPretty,EHCode,EHCodePretty,EHCodeSubst,EHCommon,EHGam,EHCnstr)
%%]

%%[9 export(PredOccId,PredOcc(..),ProofState(..))
%%]

%%[9 export(ProvenNode(..),ProvenGraph(..),prvgAddPrNd,prvgAddPrUids,prvgAddNd,ProofCost,prvgCode)
%%]

%%[9 export(Rule(..),emptyRule,costALot)
%%]

%%[9 export(PrIntroGamInfo(..),PrElimGamInfo(..),PrIntroGam,PrElimGam,emptyPIGI)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type PredOccId
  =  UID

data PredOcc
  =  PredOcc
       { poPr               :: Pred
       , poId               :: PredOccId
       }

data ProofState
  =  ProofState
       { prfsProvenGraph        :: ProvenGraph
       , prfsUniq               :: UID
       , prfsPredsToProve       :: [PredOcc]
       , prfsPredsOrigToProve   :: [Pred]
       }

type ProofCost
  =  Int

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
       , prvgPrIdMp         :: FiniteMap Pred [UID]
       }

prvgAddPrUids :: Pred -> [UID] -> ProvenGraph -> ProvenGraph
prvgAddPrUids pr uidL g@(ProvenGraph _ p2i)
  =  g {prvgPrIdMp = addToFM_C (++) p2i pr uidL}

prvgAddPrNd :: Pred -> [UID] -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddPrNd pr uidL@(uid:_) nd g@(ProvenGraph i2n p2i)
  =  (prvgAddPrUids pr uidL g) {prvgIdNdMp = addToFM i2n uid nd}

prvgAddNd :: UID -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddNd uid nd g@(ProvenGraph i2n _)
  =  g {prvgIdNdMp = addToFM i2n uid nd}

instance Substitutable PredOcc where
  s |=>  (PredOcc pr id)  = PredOcc (tyPred (s |=> Ty_Pred pr)) id
  ftv    (PredOcc pr _)   = ftv (Ty_Pred pr)

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
  pp (ProvenGraph i2n p2i) = (ppAssocL . assocLMapSnd ppCommaList . fmToList $ p2i) >-< ppAssocL (fmToList i2n)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Code for a ProvenGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
prvgCode :: ProvenGraph -> (CBindL,CSubst,Set PredOccId)
prvgCode (ProvenGraph i2n p2i)
  =  let  c i n
            =  case n of
                 ProvenAnd _ _ _ ev
                   -> [CBind_Bind (uidHNm i) (m `cAppSubst` ev)]
                 _ -> []
          i2nL = fmToList i2n
          p2iL = fmToList p2i
          r = [ uid | (uid,ProvenArg _ _) <- i2nL ]
          b = concat . map (uncurry c) $ i2nL
          m = assocLToCSubst . concat . map (\(_,uidL@(uid:_)) -> zip uidL (repeat (CExpr_Var (uidHNm uid)))) $ p2iL
          rem = concat [ uidL | (_,uidL) <- p2iL, any (`elem` r) uidL ]
     in   (b,m,mkSet rem)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
costALot :: Int
costALot = 100

data Rule
  =  Rule
       { rulRuleTy          :: Ty
       , rulMkEvid          :: [CExpr] -> CExpr
       , rulNmEvid          :: HsName
       , rulCost            :: ProofCost
       }

emptyRule = Rule Ty_Any head hsnUnknown costALot

instance Show Rule where
  show r = show (rulNmEvid r) ++ "::" ++ show (rulRuleTy r)

instance PP Rule where
  pp r = ppTy (rulRuleTy r) >#< "~>" >#< pp (rulNmEvid r)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for intro rules and elim rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data PrIntroGamInfo
  =  PrIntroGamInfo
       { pigiPrToEvidTy     :: Ty
       , pigiKi             :: Ty
       , pigiRule           :: Rule
       } deriving Show

emptyPIGI = PrIntroGamInfo Ty_Any Ty_Any emptyRule

data PrElimGamInfo
  =  PrElimGamInfo
       { pegiRuleL          :: [Rule]
       } deriving Show

type PrIntroGam     = Gam HsName PrIntroGamInfo
type PrElimGam      = Gam HsName PrElimGamInfo

instance PP PrIntroGamInfo where
  pp pigi = pp (pigiRule pigi) >#< "::" >#< ppTy (pigiPrToEvidTy pigi) >#< ":::" >#< ppTy (pigiKi pigi)

instance PP PrElimGamInfo where
  pp pegi = ppCommaList (pegiRuleL pegi)

instance Substitutable PrIntroGamInfo where
  s |=> pigi        =   pigi { pigiKi = s |=> pigiKi pigi }
  ftv   pigi        =   ftv (pigiKi pigi)
%%]


