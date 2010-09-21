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
%%[(9 hmtyinfer) import({%{EH}VarMp})
%%]

%%[(20 hmtyinfer) import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[(9999 hmtyinfer) import({%{EH}Base.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction info: how reduction was done
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(RedHowAnnotation(..))
data RedHowAnnotation
  =  RedHow_ByInstance    !HsName  !Pred  !PredScope		-- inst name, for pred, in scope
  |  RedHow_BySuperClass  !HsName  !Int   !CTag						-- field name, offset, tag info of dict
  |  RedHow_ProveObl      !UID  !PredScope
  |  RedHow_Assumption    !VarUIDHsName  !PredScope
  |  RedHow_ByScope		  !ByScopeRedHow							-- variant, for distinguishing during debugging
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
  deriving
    ( Eq, Ord
%%[[20
    , Typeable, Data
%%]]
    )
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
  pp (RedHow_ByInstance   s p sc)       =    "inst"   >#< {- ppParens (vm >#< "|=>") >#< -} ppParensCommas [pp p, pp s, pp sc]
  pp (RedHow_BySuperClass s _ _ )       =    "super"  >#< s
  pp (RedHow_ProveObl     i   sc)       =    "prove"  >#< i >#< sc
  pp (RedHow_Assumption   vun sc)       =    "assume" >#< ppParensCommas [pp vun, pp sc]
  pp (RedHow_ByScope      v     )       =    "scope"  >|< ppParens v
%%[[10
  pp (RedHow_ByLabel      l o sc)       =    "label"  >#< l >|< "@" >|< o >|< sc
%%]]
%%[[13
  pp (RedHow_Lambda       i   sc)       =    "lambda" >#< i >#< sc
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction info: specifically, how scope reduction was done, (1) for comparison (2) for debugging
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(ByScopeRedHow(..))
data ByScopeRedHow
  = ByScopeRedHow_Prove							-- scope reduction based on Prove
  | ByScopeRedHow_Assume						-- scope reduction based on Assume
  | ByScopeRedHow_Other (AlwaysEq String)		-- other reason
  deriving
    ( Eq, Ord
%%[[20
    , Typeable, Data
%%]]
    )

-- equality plays no role ??
{-
instance Eq ByScopeRedHow where
  _ == _ = True

instance Ord ByScopeRedHow where
  _ `compare` _ = EQ
-}

instance Show ByScopeRedHow where
  show ByScopeRedHow_Prove     = "prv"
  show ByScopeRedHow_Assume    = "ass"
  show (ByScopeRedHow_Other s) = show s

instance PP ByScopeRedHow where
  pp = pp . show
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

%%[(9 hmtyinfer) export(predOccCnstrMpLiftScope)
-- | Lift predicate occurrences to new scope, used to lift unproven predicates to an outer scope.
predOccCnstrMpLiftScope :: PredScope -> CHRPredOccCnstrMp -> CHRPredOccCnstrMp
predOccCnstrMpLiftScope sc
  = Map.mapKeysWith (++) c . Map.map (map i)
  where c (Prove o@(CHRPredOcc {cpoCxt=cx}))
            = Prove (o {cpoCxt = cx {cpocxScope = sc}})
        c x = x
        i (RedHow_ProveObl id _)
            = RedHow_ProveObl id sc
        i x = x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, ForceEval, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 hmtyinfer)
instance Serialize ByScopeRedHow where
  sput (ByScopeRedHow_Prove          ) = sputWord8 0
  sput (ByScopeRedHow_Assume         ) = sputWord8 1
  sput (ByScopeRedHow_Other a        ) = sputWord8 2 >> sput a
  sget = do
    t <- sgetWord8
    case t of
      0 -> return ByScopeRedHow_Prove
      1 -> return ByScopeRedHow_Assume
      2 -> liftM  ByScopeRedHow_Other   sget

instance Serialize RedHowAnnotation where
  sput (RedHow_ByInstance       a b c  ) = sputWord8 0  >> sput a >> sput b >> sput c -- >> sput d
  sput (RedHow_BySuperClass     a b c  ) = sputWord8 1  >> sput a >> sput b >> sput c
  sput (RedHow_ProveObl         a b    ) = sputWord8 2  >> sput a >> sput b
  sput (RedHow_Assumption       a b    ) = sputWord8 3  >> sput a >> sput b
  sput (RedHow_ByScope          a      ) = sputWord8 4  >> sput a
  sput (RedHow_ByLabel          a b c  ) = sputWord8 5  >> sput a >> sput b >> sput c
  sput (RedHow_Lambda           a b    ) = sputWord8 6  >> sput a >> sput b
%%[[16
  sput (RedHow_ByEqSymmetry          ) = sputWord8 7
  sput (RedHow_ByEqTrans             ) = sputWord8 8
  sput (RedHow_ByEqCongr             ) = sputWord8 9
  sput (RedHow_ByEqTyReduction  a b  ) = sputWord8 10 >> sput a >> sput b
  sput (RedHow_ByPredSeqUnpack       ) = sputWord8 11
  sput (RedHow_ByEqFromAssume        ) = sputWord8 12
  sput (RedHow_ByEqIdentity          ) = sputWord8 13
%%]]
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 RedHow_ByInstance       sget sget sget -- sget
              1  -> liftM3 RedHow_BySuperClass     sget sget sget 
              2  -> liftM2 RedHow_ProveObl         sget sget 
              3  -> liftM2 RedHow_Assumption       sget sget 
              4  -> liftM  RedHow_ByScope          sget 
              5  -> liftM3 RedHow_ByLabel          sget sget sget
              6  -> liftM2 RedHow_Lambda           sget sget 
%%[[16
              7  -> return RedHow_ByEqSymmetry     
              8  -> return RedHow_ByEqTrans        
              9  -> return RedHow_ByEqCongr        
              10 -> liftM2 RedHow_ByEqTyReduction  sget sget 
              11 -> return RedHow_ByPredSeqUnpack  
              12 -> return RedHow_ByEqFromAssume   
              13 -> return RedHow_ByEqIdentity     
%%]]

%%]
