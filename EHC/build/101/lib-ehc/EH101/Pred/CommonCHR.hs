module EH101.Pred.CommonCHR
( module EH101.CHR, module EH101.CHR.Constraint
, RedHowAnnotation (..)
, CHRConstraint, CHRIntermediateUntilAssume
, ByScopeRedHow (..)
, patchToAssumeConstraint
, CHRPredOccCnstrTraceMp, CHRPredOccCnstrMp
, gathPredLToProveCnstrMp, gathPredLToAssumeCnstrMp
, predOccCnstrMpLiftScope
, rhaMbId
, mkProveConstraint, mkAssumeConstraint, mkAssumeConstraint' )
where
import EH101.CHR
import EH101.CHR.Constraint
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH.Util.Pretty
import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize



{-# LINE 37 "src/ehc/Pred/CommonCHR.chs" #-}
data RedHowAnnotation
  =  RedHow_ByInstance    !HsName  !Pred  !PredScope		-- inst name, for pred, in scope
  |  RedHow_BySuperClass  !HsName  !Int   !CTag						-- field name, offset, tag info of dict
  |  RedHow_ProveObl      !UID  !PredScope
  |  RedHow_Assumption    !VarUIDHsName  !PredScope
  |  RedHow_ByScope		  !ByScopeRedHow							-- variant, for distinguishing during debugging
  |  RedHow_ByLabel       !Label !LabelOffset !PredScope
  |  RedHow_Lambda        !UID !PredScope
  deriving
    ( Eq, Ord
    , Typeable, Data
    )

{-# LINE 67 "src/ehc/Pred/CommonCHR.chs" #-}
rhaMbId :: RedHowAnnotation -> Maybe UID
rhaMbId (RedHow_ProveObl i _) = Just i
rhaMbId _                     = Nothing

{-# LINE 73 "src/ehc/Pred/CommonCHR.chs" #-}
instance Show RedHowAnnotation where
  show = showPP . pp

{-# LINE 78 "src/ehc/Pred/CommonCHR.chs" #-}
instance PP RedHowAnnotation where
  pp (RedHow_ByInstance   s p sc)       =    "inst"   >#< {- ppParens (vm >#< "`varUpd`") >#< -} ppParensCommas [pp p, pp s, pp sc]
  pp (RedHow_BySuperClass s _ _ )       =    "super"  >#< s
  pp (RedHow_ProveObl     i   sc)       =    "prove"  >#< i >#< sc
  pp (RedHow_Assumption   vun sc)       =    "assume" >#< ppParensCommas [pp vun, pp sc]
  pp (RedHow_ByScope      v     )       =    "scope"  >|< ppParens v
  pp (RedHow_ByLabel      l o sc)       =    "label"  >#< l >|< "@" >|< o >|< sc
  pp (RedHow_Lambda       i   sc)       =    "lambda" >#< i >#< sc

{-# LINE 106 "src/ehc/Pred/CommonCHR.chs" #-}
-- | Constraint specialized to scoped predicates with reduction info
type CHRConstraint    = Constraint CHRPredOcc RedHowAnnotation

-- | intermediate structure for holding constraint and related info until it can safely be assumed
type CHRIntermediateUntilAssume = (CHRPredOcc,(PredScope,CHRPredOccCnstrTraceMp))

{-# LINE 118 "src/ehc/Pred/CommonCHR.chs" #-}
data ByScopeRedHow
  = ByScopeRedHow_Prove							-- scope reduction based on Prove
  | ByScopeRedHow_Assume						-- scope reduction based on Assume
  | ByScopeRedHow_Other (AlwaysEq String)		-- other reason
  deriving
    ( Eq, Ord
    , Typeable, Data
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

{-# LINE 166 "src/ehc/Pred/CommonCHR.chs" #-}
mkProveConstraint :: Range -> Pred -> UID -> PredScope -> (CHRConstraint,RedHowAnnotation)
mkProveConstraint r pr i sc =  (Prove (mkCHRPredOccRng r pr sc),RedHow_ProveObl i sc)

mkAssumeConstraint'' :: Range -> Pred -> VarUIDHsName -> PredScope -> (CHRConstraint,RedHowAnnotation)
mkAssumeConstraint'' r pr vun sc =  (Assume (mkCHRPredOccRng r pr sc),RedHow_Assumption vun sc)

mkAssumeConstraint' :: Range -> Pred -> UID -> HsName -> PredScope -> (CHRConstraint,RedHowAnnotation)
mkAssumeConstraint' r pr i n sc =  mkAssumeConstraint'' r pr (VarUIDHs_Name i n) sc

mkAssumeConstraint :: Range -> Pred -> UID -> PredScope -> (CHRConstraint,RedHowAnnotation)
mkAssumeConstraint r pr i sc =  mkAssumeConstraint'' r pr (VarUIDHs_UID i) sc

{-# LINE 180 "src/ehc/Pred/CommonCHR.chs" #-}
patchToAssumeConstraint :: UID -> PredScope -> (PredScope -> RedHowAnnotation -> x -> x) -> (CHRConstraint,x) -> (CHRConstraint,x)
patchToAssumeConstraint i sc set (c,x)
  = (Assume (pr {cpoCxt = cx {cpocxScope = sc}}), set sc (RedHow_Assumption (VarUIDHs_UID i) sc) x)
  where pr = cnstrPred c
        cx = cpoCxt pr

{-# LINE 192 "src/ehc/Pred/CommonCHR.chs" #-}
type CHRPredOccCnstrTraceMp = ConstraintToInfoTraceMp CHRPredOcc RedHowAnnotation
type CHRPredOccCnstrMp      = ConstraintToInfoMap     CHRPredOcc RedHowAnnotation

{-# LINE 197 "src/ehc/Pred/CommonCHR.chs" #-}
gathPredLToProveCnstrMp :: [PredOcc] -> CHRPredOccCnstrMp
gathPredLToProveCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkProveConstraint (poPr po) (poId po) (poScope po) | po <- l ]

gathPredLToAssumeCnstrMp :: [PredOcc] -> CHRPredOccCnstrMp
gathPredLToAssumeCnstrMp l = cnstrMpFromList [ rngLift (poRange po) mkAssumeConstraint (poPr po) (poId po) (poScope po) | po <- l ]

{-# LINE 205 "src/ehc/Pred/CommonCHR.chs" #-}
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

{-# LINE 222 "src/ehc/Pred/CommonCHR.chs" #-}
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
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 RedHow_ByInstance       sget sget sget -- sget
              1  -> liftM3 RedHow_BySuperClass     sget sget sget
              2  -> liftM2 RedHow_ProveObl         sget sget
              3  -> liftM2 RedHow_Assumption       sget sget
              4  -> liftM  RedHow_ByScope          sget
              5  -> liftM3 RedHow_ByLabel          sget sget sget
              6  -> liftM2 RedHow_Lambda           sget sget

