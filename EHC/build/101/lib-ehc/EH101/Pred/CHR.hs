module EH101.Pred.CHR
( module EH101.Pred.CommonCHR
, Guard (..)
, PartialOrdering (..), toOrdering, toPartialOrdering
, isLetProveCandidate, isLetProveFailure )
where
import EH101.CHR
import EH101.CHR.Constraint
import EH101.Pred.CommonCHR
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import EH.Util.Pretty
import EH.Util.AGraph
import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import EH101.Substitutable
import EH101.Ty.FitsInCommon2
import EH101.Ty.FitsIn
import EH101.Ty.TreeTrieKey
import EH101.Base.Builtin
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize




{-# LINE 42 "src/ehc/Pred/CHR.chs" #-}
data Guard
  = HasStrictCommonScope    PredScope PredScope PredScope                   -- have strict/proper common scope?
  | IsVisibleInScope        PredScope PredScope                             -- is visible in 2nd scope?
  | NotEqualScope           PredScope PredScope                             -- scopes are unequal
  | EqualScope              PredScope PredScope                             -- scopes are equal
  | IsStrictParentScope     PredScope PredScope PredScope                   -- parent scope of each other?
  | NonEmptyRowLacksLabel   Ty LabelOffset Ty Label                         -- non empty row does not have label?, yielding its position + rest
  deriving (Typeable, Data)

{-# LINE 63 "src/ehc/Pred/CHR.chs" #-}
ppGuard :: Guard -> PP_Doc
ppGuard (HasStrictCommonScope   sc1 sc2 sc3) = ppParensCommas' [sc1 >#< "<" >#< sc2,sc1 >#< "<=" >#< sc3]
ppGuard (IsStrictParentScope    sc1 sc2 sc3) = ppParens (sc1 >#< "==" >#< sc2 >#< "/\\" >#< sc2 >#< "/=" >#< sc3)
ppGuard (IsVisibleInScope       sc1 sc2    ) = sc1 >#< "`visibleIn`" >#< sc2
ppGuard (NotEqualScope          sc1 sc2    ) = sc1 >#< "/=" >#< sc2
ppGuard (EqualScope             sc1 sc2    ) = sc1 >#< "==" >#< sc2
ppGuard (NonEmptyRowLacksLabel  r o t l    ) = ppParens (t >#< "==" >#< ppParens (r >#< "| ...")) >#< "\\" >#< l >|< "@" >|< o

{-# LINE 82 "src/ehc/Pred/CHR.chs" #-}
instance Show Guard where
  show _ = "CHR Guard"

instance PP Guard where
  pp = ppGuard

{-# LINE 94 "src/ehc/Pred/CHR.chs" #-}
instance VarExtractable CHRPredOccCnstrMp TyVarId where
  varFreeSet        x = Set.unions [ varFreeSet k | k <- Map.keys x ]

instance VarUpdatable CHRPredOccCnstrMp VarMp where
  varUpd s x = Map.mapKeysWith (++) (varUpd s) x

instance VarExtractable Guard TyVarId where
  varFreeSet        (HasStrictCommonScope   p1 p2 p3) = Set.unions $ map varFreeSet [p1,p2,p3]
  varFreeSet        (IsStrictParentScope    p1 p2 p3) = Set.unions $ map varFreeSet [p1,p2,p3]
  varFreeSet        (IsVisibleInScope       p1 p2   ) = Set.unions $ map varFreeSet [p1,p2]
  varFreeSet        (NotEqualScope          p1 p2   ) = Set.unions $ map varFreeSet [p1,p2]
  varFreeSet        (EqualScope             p1 p2   ) = Set.unions $ map varFreeSet [p1,p2]
  varFreeSet        (NonEmptyRowLacksLabel  r o t l ) = Set.unions [varFreeSet r,varFreeSet o,varFreeSet t,varFreeSet l]

instance VarUpdatable Guard VarMp where
  varUpd s (HasStrictCommonScope   p1 p2 p3) = HasStrictCommonScope   (s `varUpd` p1) (s `varUpd` p2) (s `varUpd` p3)
  varUpd s (IsStrictParentScope    p1 p2 p3) = IsStrictParentScope    (s `varUpd` p1) (s `varUpd` p2) (s `varUpd` p3)
  varUpd s (IsVisibleInScope       p1 p2   ) = IsVisibleInScope       (s `varUpd` p1) (s `varUpd` p2)
  varUpd s (NotEqualScope          p1 p2   ) = NotEqualScope          (s `varUpd` p1) (s `varUpd` p2)
  varUpd s (EqualScope             p1 p2   ) = EqualScope             (s `varUpd` p1) (s `varUpd` p2)
  varUpd s (NonEmptyRowLacksLabel  r o t l ) = NonEmptyRowLacksLabel  (s `varUpd` r)  (s `varUpd` o)  (s `varUpd` t)  (s `varUpd` l)

{-# LINE 133 "src/ehc/Pred/CHR.chs" #-}
instance VarExtractable VarUIDHsName TyVarId where
  varFreeSet          (VarUIDHs_Var i)  = Set.singleton i
  varFreeSet          _                 = Set.empty

-- instance VarUpdatable VarUIDHsName VarMp where
instance VarLookup m ImplsVarId VarMpInfo => VarUpdatable VarUIDHsName m where
  varUpd s a                   = maybe a id $ varmpAssNmLookupAssNmCyc a s

{-# LINE 143 "src/ehc/Pred/CHR.chs" #-}
instance VarExtractable RedHowAnnotation TyVarId where
  varFreeSet        (RedHow_Assumption   vun sc)  = Set.unions [varFreeSet vun,varFreeSet sc]
  varFreeSet        (RedHow_ByLabel      l o sc)  = Set.unions [varFreeSet l,varFreeSet o,varFreeSet sc]
  varFreeSet        _                             = Set.empty

instance VarUpdatable RedHowAnnotation VarMp where
  varUpd s (RedHow_Assumption   vun sc)  = RedHow_Assumption (varUpd s vun) (varUpd s sc)
  varUpd s (RedHow_ByLabel      l o sc)  = RedHow_ByLabel (varUpd s l) (varUpd s o) (varUpd s sc)
  varUpd _ x                             = x

{-# LINE 166 "src/ehc/Pred/CHR.chs" #-}
instance CHREmptySubstitution VarMp where
  chrEmptySubst = emptyVarMp


{-# LINE 176 "src/ehc/Pred/CHR.chs" #-}
instance CHRCheckable FIIn Guard VarMp where
  chrCheck env subst x
    = chk x
    where subst' = subst |+> fiVarMp env
          chk (HasStrictCommonScope (PredScope_Var vDst) sc1 sc2)
            = do { let sc1' = varUpd subst' sc1
                       sc2' = varUpd subst' sc2
                 ; scDst <- pscpCommon sc1' sc2'
                 ; if scDst == sc1'
                   then Nothing
                   else return $ vDst `varmpScopeUnit` scDst
                 }
          chk (IsStrictParentScope (PredScope_Var vDst) sc1 sc2)
            = do { let sc1' = varUpd subst' sc1
                       sc2' = varUpd subst' sc2
                 ; scDst <- pscpCommon sc1' sc2'
                 ; if scDst == sc1' && sc1' /= sc2'
                   then return $ vDst `varmpScopeUnit` scDst
                   else Nothing
                 }
          chk (NotEqualScope sc1 sc2) | isJust c
            = if fromJust c /= EQ then return emptyVarMp else Nothing
            where c = pscpCmp (varUpd subst' sc1) (varUpd subst' sc2)
          chk (EqualScope sc1 sc2) | isJust c
            = if fromJust c == EQ then return emptyVarMp else Nothing
            where c = pscpCmp (varUpd subst' sc1) (varUpd subst' sc2)
          chk (IsVisibleInScope scDst@(PredScope_Var vDst) sc1) | isJust mbSc
            = chk (IsVisibleInScope (fromJust mbSc) sc1)
            where mbSc = varmpScopeLookupScopeCyc scDst subst'
          chk (IsVisibleInScope (PredScope_Var vDst) sc1)
            = return $ vDst `varmpScopeUnit` sc1
          chk (IsVisibleInScope scDst sc1) | pscpIsVisibleIn (varUpd subst' scDst) (varUpd subst' sc1)
            = return emptyVarMp
          chk (NonEmptyRowLacksLabel r1@(Ty_Var tv _) (LabelOffset_Var vDst) ty lab)
            |  fiAllowTyVarBind env r1
            && not (null exts) && presence == Absent -- tyIsEmptyRow row
            = return $ (vDst `varmpOffsetUnit` LabelOffset_Off offset)
                       `varUpd` (tv `varmpTyUnit` row)
            where (row,exts) = tyRowExtsWithLkup (varmpTyLookupCyc2 subst') ty
                  (offset,presence) = tyExtsOffset lab' $ tyRowCanonOrder exts
                  (Label_Lab lab') = varUpd subst' lab
          chk _
            = Nothing

{-# LINE 286 "src/ehc/Pred/CHR.chs" #-}
instance CHRMatchable FIIn Pred VarMp where
  chrMatchTo fi subst pr1 pr2
    = do { (_,subst') <- fitPredIntoPred (fi {fiVarMp = subst |+> fiVarMp fi}) pr1 pr2
         ; return subst'
         }

{-# LINE 294 "src/ehc/Pred/CHR.chs" #-}
instance CHRMatchable FIIn CHRPredOccCxt VarMp where
  chrMatchTo e subst (CHRPredOccCxt_Scope1 sc1) (CHRPredOccCxt_Scope1 sc2) = chrMatchTo e subst sc1 sc2

instance CHRMatchable FIIn PredScope VarMp where
  chrMatchTo _ subst (PredScope_Var v1) sc2@(PredScope_Var v2) | v1 == v2    = Just emptyVarMp
  chrMatchTo e subst (PredScope_Var v1) sc2                    | isJust mbSc = chrMatchTo e subst (fromJust mbSc) sc2
                                                                             where mbSc = varmpScopeLookup v1 subst
  chrMatchTo e subst sc1                    (PredScope_Var v2) | isJust mbSc = chrMatchTo e subst sc1 (fromJust mbSc)
                                                                             where mbSc = varmpScopeLookup v2 subst
  chrMatchTo _ subst _                      (PredScope_Var v2)               = Nothing
  chrMatchTo _ subst (PredScope_Var v1) sc2                                  = Just $ v1 `varmpScopeUnit` sc2
  chrMatchTo _ subst (PredScope_Lev l1)     (PredScope_Lev l2) | l1 == l2    = Just emptyVarMp
  chrMatchTo _ subst _                  _                                    = Nothing

{-# LINE 310 "src/ehc/Pred/CHR.chs" #-}
instance CHRMatchable FIIn CHRPredOcc VarMp where
  chrMatchTo fi subst po1 po2
    = do { subst1 <- chrMatchTo fi subst (cpoPr po1) (cpoPr po2)
         ; subst2 <- chrMatchTo fi subst (cpoCxt po1) (cpoCxt po2)
         ; return $ subst2 |+> subst1
         }

{-# LINE 319 "src/ehc/Pred/CHR.chs" #-}
instance CHRMatchable FIIn Label VarMp where
  chrMatchTo _ subst (Label_Var v1) lb2@(Label_Var v2) | v1 == v2    = Just emptyVarMp
  chrMatchTo e subst (Label_Var v1) lb2                | isJust mbLb = chrMatchTo e subst (fromJust mbLb) lb2
                                                                     where mbLb = varmpLabelLookup v1 subst
  chrMatchTo e subst lb1                (Label_Var v2) | isJust mbLb = chrMatchTo e subst lb1 (fromJust mbLb)
                                                                     where mbLb = varmpLabelLookup v2 subst
  chrMatchTo _ subst _                  (Label_Var v2)               = Nothing
  chrMatchTo _ subst (Label_Var v1) lb2                              = Just $ v1 `varmpLabelUnit` lb2
  chrMatchTo _ subst (Label_Lab l1)     (Label_Lab l2) | l1 == l2    = Just emptyVarMp
  chrMatchTo _ subst _              _                                = Nothing

{-# LINE 332 "src/ehc/Pred/CHR.chs" #-}
instance CHRMatchable FIIn LabelOffset VarMp where
  chrMatchTo _ subst (LabelOffset_Var v1) of2@(LabelOffset_Var v2) | v1 == v2    = Just emptyVarMp
  chrMatchTo s subst (LabelOffset_Var v1) of2                      | isJust mbOf = chrMatchTo s subst (fromJust mbOf) of2
                                                                                 where mbOf = varmpOffsetLookup v1 subst
  chrMatchTo s subst of1                      (LabelOffset_Var v2) | isJust mbOf = chrMatchTo s subst of1 (fromJust mbOf)
                                                                                 where mbOf = varmpOffsetLookup v2 subst
  chrMatchTo _ subst _                        (LabelOffset_Var v2)               = Nothing
  chrMatchTo _ subst (LabelOffset_Var v1) of2                                    = Just $ v1 `varmpOffsetUnit` of2
  chrMatchTo _ subst (LabelOffset_Off l1)     (LabelOffset_Off l2) | l1 == l2    = Just emptyVarMp
  chrMatchTo _ subst _                    _                                      = Nothing

{-# LINE 351 "src/ehc/Pred/CHR.chs" #-}
data PartialOrdering
  = P_LT | P_EQ | P_GT | P_NE
  deriving (Eq,Show)

toPartialOrdering :: Ordering -> PartialOrdering
toPartialOrdering o
  = case o of
      EQ -> P_EQ
      LT -> P_LT
      GT -> P_GT

toOrdering :: PartialOrdering -> Maybe Ordering
toOrdering o
  = case o of
      P_EQ -> Just EQ
      P_LT -> Just LT
      P_GT -> Just GT
      _    -> Nothing

{-# LINE 376 "src/ehc/Pred/CHR.chs" #-}
-- | Consider a pred for proving if: no free tvars, or its free tvars do not coincide with those globally used
isLetProveCandidate :: (VarExtractable x v) => Set.Set v -> x -> Bool
isLetProveCandidate glob x
  = Set.null fv || Set.null (fv `Set.intersection` glob)
  where fv = varFreeSet x

isLetProveFailure :: (VarExtractable x v) => Set.Set v -> x -> Bool
isLetProveFailure glob x
  = Set.null fv
  where fv = varFreeSet x

{-# LINE 393 "src/ehc/Pred/CHR.chs" #-}
instance Serialize Guard where
  sput (HasStrictCommonScope     a b c  ) = sputWord8 0  >> sput a >> sput b >> sput c
  sput (IsVisibleInScope         a b    ) = sputWord8 1  >> sput a >> sput b
  sput (NotEqualScope            a b    ) = sputWord8 2  >> sput a >> sput b
  sput (EqualScope               a b    ) = sputWord8 3  >> sput a >> sput b
  sput (IsStrictParentScope      a b c  ) = sputWord8 4  >> sput a >> sput b >> sput c
  sput (NonEmptyRowLacksLabel    a b c d) = sputWord8 5  >> sput a >> sput b >> sput c >> sput d
  sget = do t <- sgetWord8
            case t of
              0  -> liftM3 HasStrictCommonScope     sget sget sget
              1  -> liftM2 IsVisibleInScope         sget sget
              2  -> liftM2 NotEqualScope            sget sget
              3  -> liftM2 EqualScope               sget sget
              4  -> liftM3 IsStrictParentScope      sget sget sget
              5  -> liftM4 NonEmptyRowLacksLabel    sget sget sget sget

