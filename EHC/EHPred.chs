% $Id: EHC.lag 199 2004-05-12 19:11:13Z andres $

%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 import(EHTy,EHCode,EHCommon)
%%]

%%[9 export(PredOcc(..),ProvenPred(..),ProofState(..),OnePredProof(..))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pred resolver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type PredL
  = [Pred]

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
  = [ProvenPred]

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

type ProvenPredEvidence
  = [CExpr] -> CExpr

data Rule
  =  Rule
       { rulConseq      :: Pred
       , rulPrereq      :: PredL
       , rulEvidence    :: ProvenPredEvidence
       }

type RuleL
  = [Rule]
%%]


