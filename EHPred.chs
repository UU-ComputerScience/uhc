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

%%[9 import(EHTy,EHTyPretty,EHCode,EHCommon,EHGam,EHCnstr)
%%]

%%[9 export(PredL,PredOcc(..),ProvenPred(..),ProofState(..),OnePredProof(..))
%%]

%%[9 export(Rule(..),RuleL)
%%]

%%[9 export(PrIntroGamInfo(..),PrElimGamInfo(..),PrIntroGam,PrElimGam)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type PredL
  =  [Pred]

data PredOcc
  =  PredOcc
       { poPr           :: Pred
       , poId           :: UID
       }

data ProvenPred
  =  ProvenPred
       { prvPredOcc     :: PredOcc
       , prvEvidence    :: CExpr
       }

type ProvenPredL
  =  [ProvenPred]

data ProofState
  =  ProofState
       { prfsIntroPrL   :: ProvenPredL
       , prfsProvenPrL  :: ProvenPredL
       , prfsIntermPrL  :: ProvenPredL
       }

data OnePredProof pr
  =  OnePredProof
       { oneprfResolve  :: pr -> ProvenPredL -> ProofState
       }

data ProvenNode
  =  ProvenNode
       { prvnPred       :: Pred
       , prvnEdges      :: [UID]
       , prvnEvidence   :: CExpr
       }

data ProvenGraph
  =  ProvenGraph
       { prvgGraph      :: FiniteMap UID ProvenNode
       , prvgPreds      :: FiniteMap Pred UID
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data Rule
  =  Rule
       { rulRuleTy      :: Ty
       , rulMkEvid      :: [CExpr] -> CExpr
       , rulNmEvid      :: HsName
       }

instance Show Rule where
  show r = show (rulNmEvid r) ++ "::" ++ show (rulRuleTy r)

instance PP Rule where
  pp r = ppTy (rulRuleTy r) >#< "~>" >#< pp (rulNmEvid r)

type RuleL
  =  [Rule]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gamma for intro rules and elim rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
data PrIntroGamInfo
  =  PrIntroGamInfo
       { pigiEvidTy     :: Ty
       , pigiKi         :: Ty
       , pigiRule       :: Rule
       } deriving Show

data PrElimGamInfo
  =  PrElimGamInfo
       { pegiRuleL      :: RuleL
       } deriving Show

type PrIntroGam     = Gam HsName PrIntroGamInfo
type PrElimGam      = Gam HsName PrElimGamInfo

instance PP PrIntroGamInfo where
  pp pigi = pp (pigiRule pigi) >#< "::" >#< ppTy (pigiEvidTy pigi) >#< ":::" >#< ppTy (pigiKi pigi)

instance PP PrElimGamInfo where
  pp pegi = ppListSep "[" "]" "," (pegiRuleL pegi)

instance Substitutable PrIntroGamInfo where
  s |=> pigi        =   pigi { pigiKi = s |=> pigiKi pigi }
  ftv   pigi        =   ftv (pigiKi pigi)
%%]


