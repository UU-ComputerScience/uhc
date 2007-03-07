%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared between CHR+Pred and (in particular) Ty/FitsIn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

This file exists to avoid module circularities.

%%[9 module {%{EH}Pred.CommonCHR} import({%{EH}CHR},{%{EH}CHR.Constraint}) export(module {%{EH}CHR},module {%{EH}CHR.Constraint})
%%]

%%[9 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%[9 import({%{EH}Base.Common})
%%]

%%[9 import({%{EH}Ty})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(RedHowAnnotation(..))
data RedHowAnnotation
  =  RedHow_ByInstance    HsName  Pred  PredScope		-- inst name, for pred, in scope
  |  RedHow_BySuperClass  HsName  Int   CTag			-- field name, offset, tag info of dict
  |  RedHow_ProveObl      UID  PredScope
  |  RedHow_Assumption    UID  HsName  PredScope
  |  RedHow_ByScope
  deriving (Eq, Ord)
%%]

%%[9
instance Show RedHowAnnotation where
  show = showPP . pp
%%]

%%[9
instance PP RedHowAnnotation where
  pp (RedHow_ByInstance   s p sc)  = "inst" >#< s >|< sc >#< "::" >#< p
  pp (RedHow_BySuperClass s _ _ )  = "super" >#< s
  pp (RedHow_ProveObl     i   sc)  = "prove" >#< i >#< sc
  pp (RedHow_Assumption   _ n sc)  = "assume" >#< n >#< sc
  pp (RedHow_ByScope            )  = pp "scope"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(mkProveConstraint,mkAssumeConstraint,mkAssumeConstraint')
mkProveConstraint :: Pred -> UID -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkProveConstraint pr i sc =  (Prove (mkCHRPredOcc pr sc),RedHow_ProveObl i sc)

mkAssumeConstraint' :: Pred -> UID -> HsName -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint' pr i n sc =  (Assume (mkCHRPredOcc pr sc),RedHow_Assumption i n sc)

mkAssumeConstraint :: Pred -> UID -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint pr i sc =  mkAssumeConstraint' pr i (mkHNm i) sc
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint to info map for CHRPredOcc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRPredOccCnstrMp)
type CHRPredOccCnstrMp = ConstraintToInfoMap CHRPredOcc RedHowAnnotation
%%]

%%[9 export(gathPredLToProveCnstrMp,gathPredLToAssumeCnstrMp)
gathPredLToProveCnstrMp :: [PredOcc] -> CHRPredOccCnstrMp
gathPredLToProveCnstrMp l = cnstrMpFromList [ mkProveConstraint (poPr po) (poId po) (poScope po) | po <- l ]

gathPredLToAssumeCnstrMp :: [PredOcc] -> CHRPredOccCnstrMp
gathPredLToAssumeCnstrMp l = cnstrMpFromList [ mkAssumeConstraint (poPr po) (poId po) (poScope po) | po <- l ]
%%]

