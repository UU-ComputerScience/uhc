% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 import(FiniteMap,UU.Pretty)
%%]

%%[9 import(EHTy,EHTyPretty,EHCode,EHCodePretty,EHCommon,EHGam,EHCnstr)
%%]

%%[9 export(PredOcc(..),ProofState(..))
%%]

%%[9 export(ProvenNode(..),ProvenGraph(..),prvgAddPrNd,prvgAddNd,ProofCost)
%%]

%%[9 export(Rule(..),emptyRule)
%%]

%%[9 export(PrIntroGamInfo(..),PrElimGamInfo(..),PrIntroGam,PrElimGam,emptyPIGI)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data PredOcc
  =  PredOcc
       { poPr               :: Pred
       , poId               :: UID
       }

data ProofState
  =  ProofState
       { prfsProvenGraph    :: ProvenGraph
       , prfsUniq           :: UID
       , prfsPredsToProve   :: [PredOcc]
       }

type ProofCost
  =  Int

data ProvenNode
  =  ProvenOr
       { prvnPred           :: Pred
       , prvnOrEdges        :: [UID]
       }
  |  ProvenAnd
       { prvnPred           :: Pred
       , prvnAndEdges       :: [UID]
       , prvnEvidence       :: CExpr
       , prvnCost           :: ProofCost
       }
  |  ProvenFail
       { prvnPred           :: Pred
       }
  
data ProvenGraph
  =  ProvenGraph
       { prvgIdNdMp         :: FiniteMap UID ProvenNode
       , prvgPrIdMp         :: FiniteMap Pred UID
       }

prvgAddPrNd :: Pred -> UID -> ProvenNode -> ProvenGraph -> ProvenGraph
prvgAddPrNd pr uid nd g@(ProvenGraph i2n p2i)
  =  g {prvgIdNdMp = addToFM i2n uid nd, prvgPrIdMp = addToFM p2i pr uid}

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
  pp (ProvenOr pr es) 		 = "OR:" 	>#< "pr=" >|< pp pr >#< "edges=" >|< ppListSep "[" "]" "," es
  pp (ProvenAnd pr es ev c)  = "AND:" 	>#< "pr=" >|< pp pr >#< "edges=" >|< ppListSep "[" "]" "," es >#< "cost=" >|< pp c >#< "evid=" >|< pp ev
  pp (ProvenFail pr) 		 = "FAIL:" 	>#< "pr=" >|< pp pr

instance PP ProvenGraph where
  pp (ProvenGraph i2n p2i) = ppAssocL (fmToList p2i) >-< ppAssocL (fmToList i2n)
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

emptyRule = Rule Ty_Any head hsnUnknown 100

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
  pp pegi = ppListSep "[" "]" "," (pegiRuleL pegi)

instance Substitutable PrIntroGamInfo where
  s |=> pigi        =   pigi { pigiKi = s |=> pigiKi pigi }
  ftv   pigi        =   ftv (pigiKi pigi)
%%]


