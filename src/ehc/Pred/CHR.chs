%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional Pred admin for Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[9 module {%{EH}Pred.CHR} import({%{EH}CHR},{%{EH}CHR.Constraint})
%%]

%%[9 import(qualified Data.Set as Set,Data.Maybe)
%%]

%%[9 import(UU.Pretty,EH.Util.AGraph,EH.Util.PPUtils)
%%]

%%[9 import({%{EH}Base.Common})
%%]

%%[9 import({%{EH}Ty},{%{EH}Cnstr},{%{EH}Substitutable},{%{EH}Ty.FitsIn},{%{EH}Ty.TrieKey})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
instance CHRMatchable FIIn Pred Cnstr where
  chrMatchTo fi pr1 pr2
    = do { (_,subst) <- fitPredInPred fi pr1 pr2
         ; return subst
         }

instance CHRMatchable FIIn PredScope Cnstr where
  chrMatchTo _ (PredScope_Var v1) sc2@(PredScope_Var v2) | v1 /= v2 = Just $ v1 `cnstrScopeUnit` sc2
  chrMatchTo _ _                      (PredScope_Var v2)            = Nothing
  chrMatchTo _ (PredScope_Var v1) sc2                               = Just $ v1 `cnstrScopeUnit` sc2
  chrMatchTo _ (PredScope_Lev l1)     (PredScope_Lev l2) | l1 == l2 = Just emptyCnstr
  chrMatchTo _ _                  _                                 = Nothing

instance CHRMatchable FIIn PredOccId Cnstr where
  chrMatchTo _ (PredOccId_Var v1) sc2@(PredOccId_Var v2) | v1 /= v2 = Just $ v1 `cnstrPoiUnit` sc2
  chrMatchTo _ _                      (PredOccId_Var v2)            = Nothing
  chrMatchTo _ (PredOccId_Var v1) sc2                               = Just $ v1 `cnstrPoiUnit` sc2
  chrMatchTo _ (PredOccId   _ i1)     (PredOccId   _ i2) | i1 == i2 = Just emptyCnstr
  chrMatchTo _ _                  _                                 = Nothing

instance CHRMatchable FIIn PredOcc Cnstr where
  chrMatchTo fi po1 po2
    = do { subst1 <- chrMatchTo fi (poPr po1) (poPr po2)
         ; subst2 <- chrMatchTo fi (poPoi po1) (poPoi po2)
         ; subst3 <- chrMatchTo fi (poScope po1) (poScope po2)
         ; return $ subst3 |=> subst2 |=> subst1
         }

instance CHREmptySubstitution Cnstr where
  chrEmptySubst = emptyCnstr

instance CHRSubstitutable PredOcc TyVarId Cnstr where
  chrFtv        x = Set.fromList (ftv x)
  chrAppSubst s x = s |=> x

instance CHRSubstitutable Cnstr TyVarId Cnstr where
  chrFtv        x = Set.empty
  chrAppSubst s x = s |=> x

instance CHRSubstitutable Guard TyVarId Cnstr where
  chrFtv        (HasCommonScope   p1 p2 p3) = Set.unions $ map (Set.fromList . ftv) [p1,p2,p3]
  chrFtv        (IsParentScope    p1 p2   ) = Set.unions $ map (Set.fromList . ftv) [p1,p2]
  chrFtv        (IsVisibleInScope p1 p2   ) = Set.unions $ map (Set.fromList . ftv) [p1,p2]

  chrAppSubst s (HasCommonScope   p1 p2 p3) = HasCommonScope   (s |=> p1) (s |=> p2) (s |=> p3)
  chrAppSubst s (IsParentScope    p1 p2   ) = IsParentScope    (s |=> p1) (s |=> p2)
  chrAppSubst s (IsVisibleInScope p1 p2   ) = IsVisibleInScope (s |=> p1) (s |=> p2)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(RedHowAnnotation(..))
data RedHowAnnotation
  =  RedHow_ByInstance    HsName  -- Pred
  |  RedHow_BySuperClass  HsName  Int   CTag
  |  RedHow_ProveObl      Int
  |  RedHow_Assumption    Int
  |  RedHow_ByScope
  |  RedHow_ByScopeA
  deriving (Eq, Ord)
%%]

%%[9
instance Show RedHowAnnotation where
  show (RedHow_ByInstance s)        = show s
  show (RedHow_BySuperClass s _ _)  = show s
  show (RedHow_ProveObl i)          = "prove" ++ show i
  show (RedHow_Assumption i)        = "assume" ++ show i
  show (RedHow_ByScope)             = "scope"
  show (RedHow_ByScopeA)            = "scopeA"
%%]

%%[9 export(RedInfo(..),mkRedInfo)
data RedInfo
  = RedInfo
      { redinfoAnn      :: RedHowAnnotation
      }

mkRedInfo :: RedHowAnnotation -> RedInfo
mkRedInfo a = RedInfo a
%%]

%%[9
instance Show RedInfo where
  show i = show (redinfoAnn i)

instance PP RedInfo where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Guard, CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Guard(..))
data Guard
  = HasCommonScope      PredScope PredScope PredScope                   -- have common scope?
  | IsParentScope       PredScope PredScope                             -- is parent scope?
  | IsVisibleInScope    PredScope PredScope                             -- is visible in 2nd scope?
%%]

%%[9
instance Show Guard where
  show _ = "CHR Guard"

instance PP Guard where
  pp (HasCommonScope sc1 sc2 sc3) = sc1 >#< "<=" >#< ppParensCommas [sc2,sc3]
  pp (IsParentScope sc1 sc2) = sc1 >#< "+ 1 ==" >#< sc2
  pp (IsVisibleInScope sc1 sc2) = sc1 >#< ">=" >#< sc2
%%]

%%[9
instance CHRCheckable Guard Cnstr where
  chrCheck (HasCommonScope (PredScope_Var vDst) sc1 sc2)
    = do { scDst <- pscpCommon sc1 sc2
         ; return $ vDst `cnstrScopeUnit` scDst
         }
  chrCheck (IsParentScope (PredScope_Var vDst) sc1)
    = do { scDst <- pscpParent sc1
         ; return $ vDst `cnstrScopeUnit` scDst
         }
  chrCheck (IsVisibleInScope (PredScope_Var vDst) sc1)
    = return $ vDst `cnstrScopeUnit` sc1
  chrCheck (IsVisibleInScope scDst sc1) | pscpIsVisibleIn scDst sc1
    = return emptyCnstr
  chrCheck _
    = Nothing
%%]

