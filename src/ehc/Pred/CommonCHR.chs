%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared between CHR+Pred and (in particular) Ty/FitsIn
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

This file exists to avoid module circularities.

%%[(9 hmtyinfer) module {%{EH}Pred.CommonCHR} import({%{EH}CHR},{%{EH}CHR.Constraint}) export(module {%{EH}CHR},module {%{EH}CHR.Constraint})
%%]

%%[(9 hmtyinfer) import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[(9 hmtyinfer) import(EH.Util.Pretty)
%%]

%%[(9 hmtyinfer) import({%{EH}Base.Common})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty})
%%]

%%[(9 hmtyinfer) import({%{EH}Base.CfgPP})
%%]

%%[(99 hmtyinfer) import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(RedHowAnnotation(..))
data RedHowAnnotation
  =  RedHow_ByInstance    !HsName  !Pred  !PredScope		-- inst name, for pred, in scope
  |  RedHow_BySuperClass  !HsName  !Int   !CTag				-- field name, offset, tag info of dict
  |  RedHow_ProveObl      !UID  !PredScope
  |  RedHow_Assumption    !VarUIDHsName  !PredScope
  |  RedHow_ByScope		  (AlwaysEq String)						-- variant, for distinguishing during debugging
%%[[10
  |  RedHow_ByLabel       !Label !LabelOffset !PredScope
%%]]
%%[[13
  |  RedHow_Lambda        !UID !PredScope
%%]]
%%[[16
  |  RedHow_ByEqSymmetry
  |  RedHow_ByEqTrans
  |  RedHow_ByEqCongr
  |  RedHow_ByEqTyReduction !Ty !Ty
  |  RedHow_ByPredSeqUnpack
  |  RedHow_ByEqFromAssume
  |  RedHow_ByEqIdentity
%%]]
  deriving (Eq, Ord)
%%]

%%[(99 hmtyinfer) export(rhaMbId)
rhaMbId :: RedHowAnnotation -> Maybe UID
rhaMbId (RedHow_ProveObl i _) = Just i
rhaMbId _                     = Nothing
%%]

%%[(9 hmtyinfer)
instance Show RedHowAnnotation where
  show = showPP . pp
%%]

%%[(9 hmtyinfer)
instance PP RedHowAnnotation where
  pp (RedHow_ByInstance   s p sc)  =    "inst"   >#< ppParensCommas [pp s, pp sc]
  pp (RedHow_BySuperClass s _ _ )  =    "super"  >#< s
  pp (RedHow_ProveObl     i   sc)  =    "prove"  >#< i >#< sc
  pp (RedHow_Assumption   vun sc)  =    "assume" >#< ppParensCommas [pp vun, pp sc]
  pp (RedHow_ByScope      v     )  =    "scope"  >|< ppParens v
%%[[10
  pp (RedHow_ByLabel      l o sc)  =    "label"  >#< l >|< "@" >|< o >|< sc
%%]]
%%[[13
  pp (RedHow_Lambda       i   sc)  =    "lambda" >#< i >#< sc
%%]]
%%[[16
  pp (RedHow_ByEqSymmetry) = pp "eqsym"
  pp (RedHow_ByEqTrans   ) = pp "eqtrans"
  pp (RedHow_ByEqCongr   ) = pp "eqcongr"
  pp (RedHow_ByEqTyReduction ty1 ty2) = "eqtyred" >#< ty1 >#< "~>" >#< ty2
  pp (RedHow_ByPredSeqUnpack) = pp "unpack"
  pp (RedHow_ByEqFromAssume)  = pp "eqassume"
  pp (RedHow_ByEqIdentity)    = pp "identity"
%%]]
%%]
  pp (RedHow_ByInstance   s p sc)  =    "inst"   >#< s >|< sc >#< "::" >#< p

%%[(20 hmtyinfer)
instance PPForHI RedHowAnnotation where
  ppForHI (RedHow_ByInstance   s p sc)  =    "redhowinst"   >#< ppCurlysCommasBlock [ppForHI s, ppForHI p, ppForHI sc]
  ppForHI (RedHow_BySuperClass s o tg)  =    "redhowsuper"  >#< ppCurlysCommasBlock [ppForHI s, pp o, ppForHI tg]
  ppForHI (RedHow_ProveObl     i   sc)  =    "redhowprove"  >#< ppCurlysCommasBlock [ppForHI i, ppForHI sc]
  ppForHI (RedHow_Assumption   vun sc)  =    "redhowassume" >#< ppCurlysCommasBlock [ppForHI vun, ppForHI sc]
  ppForHI (RedHow_ByScope      v     )  =    "redhowscope"  >#< ppCurlysCommasBlock [ppForHI v]
  ppForHI (RedHow_ByLabel      l o sc)  =    "redhowlabel"  >#< ppCurlysCommasBlock [ppForHI l, ppForHI o, ppForHI sc]
  ppForHI (RedHow_Lambda       i   sc)  =    "redhowlambda" >#< ppCurlysCommasBlock [ppForHI i, ppForHI sc]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint construction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer).mkConstraintFuncs export(mkProveConstraint,mkAssumeConstraint,mkAssumeConstraint')
mkProveConstraint :: Pred -> UID -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkProveConstraint pr i sc =  (Prove (mkCHRPredOcc pr sc),RedHow_ProveObl i sc)

mkAssumeConstraint'' :: Pred -> VarUIDHsName -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint'' pr vun sc =  (Assume (mkCHRPredOcc pr sc),RedHow_Assumption vun sc)

mkAssumeConstraint' :: Pred -> UID -> HsName -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint' pr i n sc =  mkAssumeConstraint'' pr (VarUIDHs_Name i n) sc

mkAssumeConstraint :: Pred -> UID -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint pr i sc =  mkAssumeConstraint'' pr (VarUIDHs_UID i) sc
%%]

%%[(99 hmtyinfer) -9.mkConstraintFuncs export(mkProveConstraint,mkAssumeConstraint,mkAssumeConstraint')
mkProveConstraint :: Range -> Pred -> UID -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkProveConstraint r pr i sc =  (Prove (mkCHRPredOccRng r pr sc),RedHow_ProveObl i sc)

mkAssumeConstraint'' :: Range -> Pred -> VarUIDHsName -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint'' r pr vun sc =  (Assume (mkCHRPredOccRng r pr sc),RedHow_Assumption vun sc)

mkAssumeConstraint' :: Range -> Pred -> UID -> HsName -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint' r pr i n sc =  mkAssumeConstraint'' r pr (VarUIDHs_Name i n) sc

mkAssumeConstraint :: Range -> Pred -> UID -> PredScope -> (Constraint CHRPredOcc RedHowAnnotation,RedHowAnnotation)
mkAssumeConstraint r pr i sc =  mkAssumeConstraint'' r pr (VarUIDHs_UID i) sc
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint to info map for CHRPredOcc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(CHRPredOccCnstrMp)
type CHRPredOccCnstrMp = ConstraintToInfoMap CHRPredOcc RedHowAnnotation
%%]

%%[(9 hmtyinfer) export(gathPredLToProveCnstrMp,gathPredLToAssumeCnstrMp)
gathPredLToProveCnstrMp :: [PredOcc] -> CHRPredOccCnstrMp
gathPredLToProveCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkProveConstraint (poPr po) (poId po) (poScope po) | po <- l ]

gathPredLToAssumeCnstrMp :: [PredOcc] -> CHRPredOccCnstrMp
gathPredLToAssumeCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkAssumeConstraint (poPr po) (poId po) (poScope po) | po <- l ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 hmtyinfer)
instance ForceEval VarUIDHsName where
  forceEval x@(VarUIDHs_Name i n) | forceEval i `seq` forceEval n `seq` True = x
  forceEval x@(VarUIDHs_UID  i  ) | forceEval i `seq` True = x
  forceEval x@(VarUIDHs_Var  i  ) | forceEval i `seq` True = x
%%[[102
  fevCount (VarUIDHs_Name i n) = cm1 "VarUIDHs_Name" `cmUnion` fevCount i `cmUnion` fevCount n
  fevCount (VarUIDHs_UID  i  ) = cm1 "VarUIDHs_UID"  `cmUnion` fevCount i
  fevCount (VarUIDHs_Var  i  ) = cm1 "VarUIDHs_Var"  `cmUnion` fevCount i
%%]]

instance ForceEval RedHowAnnotation where
  forceEval x@(RedHow_ByInstance   _ p sc)  | forceEval p `seq` forceEval sc `seq` True = x
  forceEval x@(RedHow_ProveObl     _   sc)  | forceEval sc `seq` True = x
  forceEval x@(RedHow_Assumption   _   sc)  | forceEval sc `seq` True = x
  forceEval x@(RedHow_ByLabel      l o sc)  | forceEval l `seq` forceEval o `seq` forceEval sc `seq` True = x
  forceEval x@(RedHow_Lambda       _   sc)  | forceEval sc `seq` True = x
  forceEval x                               = x
%%[[102
  fevCount (RedHow_ByInstance   s p sc)  = cm1 "RedHow_ByInstance"     `cmUnion` fevCount s `cmUnion` fevCount p `cmUnion` fevCount sc
  fevCount (RedHow_BySuperClass s o tg)  = cm1 "RedHow_BySuperClass"    `cmUnion` fevCount s `cmUnion` fevCount o `cmUnion` fevCount tg
  fevCount (RedHow_ProveObl     i   sc)  = cm1 "RedHow_ProveObl"        `cmUnion` fevCount i `cmUnion` fevCount sc
  fevCount (RedHow_Assumption   vun sc)  = cm1 "RedHow_Assumption"      `cmUnion` fevCount vun `cmUnion` fevCount sc
  fevCount (RedHow_ByScope      _     )  = cm1 "RedHow_ByScope"
%%[[16
  fevCount (RedHow_ByEqSymmetry       )  = cm1 "RedHow_ByEqSymmetry"
  fevCount (RedHow_ByEqTrans          )  = cm1 "RedHow_ByEqTrans"
  fevCount (RedHow_ByEqCongr          )  = cm1 "RedHow_ByEqCongr"
  fevCount (RedHow_ByEqTyReduction ty1 ty2)  = cm1 "RedHow_ByEqTyReduction" `cmUnion` fevCount ty1 `cmUnion` fevCount ty2
  fevCount (RedHow_ByPredSeqUnpack    )  = cm1 "RedHow_ByPredSeqUnpack"
  fevCount (RedHow_ByEqFromAssume     )  = cm1 "RedHow_ByEqFromAssume"
  fevCount (RedHow_ByEqIdentity       )  = cm1 "RedHow_ByEqIdentity"
%%][102
%%]]
  fevCount (RedHow_ByLabel      l o sc)  = cm1 "RedHow_ByLabel"         `cmUnion` fevCount l `cmUnion` fevCount o `cmUnion` fevCount sc
  fevCount (RedHow_Lambda       i   sc)  = cm1 "RedHow_Lambda"      	`cmUnion` fevCount i `cmUnion` fevCount sc
%%]]
%%]
