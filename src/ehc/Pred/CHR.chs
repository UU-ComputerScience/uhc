%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional Pred admin for Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[9 module {%{EH}Pred.CHR} import({%{EH}CHR},{%{EH}CHR.Constraint})
%%]

%%[9 import({%{EH}Pred.CommonCHR}) export(module {%{EH}Pred.CommonCHR})
%%]

%%[9 import(qualified Data.Map as Map,qualified Data.Set as Set,Data.Maybe)
%%]

%%[9 import(EH.Util.Pretty,EH.Util.AGraph)
%%]

%%[9 import({%{EH}Base.Common})
%%]

%%[9 import({%{EH}Ty},{%{EH}VarMp},{%{EH}Substitutable},{%{EH}Ty.FitsIn},{%{EH}Ty.TrieKey})
%%]

%%[10 import({%{EH}Base.Builtin})
%%]

%%[20 import({%{EH}Base.CfgPP})
%%]

%%[99 import({%{EH}Base.ForceEval},{%{EH}Ty.Trf.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
instance CHRMatchable FIIn Pred VarMp where
  chrMatchTo fi subst pr1 pr2
    = do { (_,subst') <- fitPredIntoPred (fi {fiVarMp = subst |=> fiVarMp fi}) pr1 pr2
         ; return subst'
         }
%%]

%%[9
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
%%]
instance CHRMatchable FIIn PredScope VarMp where
  chrMatchTo _ subst (PredScope_Var v1) sc2@(PredScope_Var v2) | v1 == v2  = Just emptyVarMp
                                                               | otherwise = Just $ v1 `varmpScopeUnit` sc2
  chrMatchTo _ subst _                      (PredScope_Var v2)             = Nothing
  chrMatchTo _ subst (PredScope_Var v1) sc2                                = Just $ v1 `varmpScopeUnit` sc2
  chrMatchTo _ subst (PredScope_Lev l1)     (PredScope_Lev l2) | l1 == l2  = Just emptyVarMp
  chrMatchTo _ subst _                  _                                  = Nothing

%%[9
instance CHRMatchable FIIn CHRPredOcc VarMp where
  chrMatchTo fi subst po1 po2
    = do { subst1 <- chrMatchTo fi subst (cpoPr po1) (cpoPr po2)
         ; subst2 <- chrMatchTo fi subst (cpoScope po1) (cpoScope po2)
         ; return $ subst2 |=> subst1
         }

instance CHREmptySubstitution VarMp where
  chrEmptySubst = emptyVarMp

instance CHRSubstitutable CHRPredOcc TyVarId VarMp where
  chrFtv        x = ftvSet x
  chrAppSubst s x = s |=> x

instance CHRSubstitutable PredScope TyVarId VarMp where
  chrFtv        x = ftvSet x
  chrAppSubst s x = s |=> x

instance CHRSubstitutable CHRPredOccCnstrMp TyVarId VarMp where
  chrFtv        x = Set.unions [ chrFtv k | k <- Map.keys x ]
  chrAppSubst s x = Map.mapKeysWith (++) (chrAppSubst s) x

instance CHRSubstitutable VarMp TyVarId VarMp where
  chrFtv        x = Set.empty
  chrAppSubst s x = s |=> x

instance CHRSubstitutable Guard TyVarId VarMp where
  chrFtv        (HasStrictCommonScope   p1 p2 p3) = Set.unions $ map ftvSet [p1,p2,p3]
  chrFtv        (IsStrictParentScope    p1 p2 p3) = Set.unions $ map ftvSet [p1,p2,p3]
  chrFtv        (IsVisibleInScope       p1 p2   ) = Set.unions $ map ftvSet [p1,p2]
  chrFtv        (NotEqualScope          p1 p2   ) = Set.unions $ map ftvSet [p1,p2]
  chrFtv        (EqualScope             p1 p2   ) = Set.unions $ map ftvSet [p1,p2]
%%[[10
  chrFtv        (NonEmptyRowLacksLabel  r o t l ) = Set.unions [ftvSet r,ftvSet o,ftvSet t,ftvSet l]
%%]]

  chrAppSubst s (HasStrictCommonScope   p1 p2 p3) = HasStrictCommonScope   (s |=> p1) (s |=> p2) (s |=> p3)
  chrAppSubst s (IsStrictParentScope    p1 p2 p3) = IsStrictParentScope    (s |=> p1) (s |=> p2) (s |=> p3)
  chrAppSubst s (IsVisibleInScope       p1 p2   ) = IsVisibleInScope       (s |=> p1) (s |=> p2)
  chrAppSubst s (NotEqualScope          p1 p2   ) = NotEqualScope          (s |=> p1) (s |=> p2)
  chrAppSubst s (EqualScope             p1 p2   ) = EqualScope             (s |=> p1) (s |=> p2)
%%[[10
  chrAppSubst s (NonEmptyRowLacksLabel  r o t l ) = NonEmptyRowLacksLabel  (s |=> r)  (s |=> o)  (s |=> t)  (s |=> l)
%%]]
%%]

%%[9
instance CHRSubstitutable VarUIDHsName TyVarId VarMp where
  chrFtv          (VarUIDHs_Var i)  = Set.singleton i
  chrFtv          _                 = Set.empty
  chrAppSubst s a                   = maybe a id $ varmpAssNmLookupAssNmCyc a s
%%]

%%[9
instance CHRSubstitutable RedHowAnnotation TyVarId VarMp where
  chrFtv        (RedHow_Assumption   vun sc)  = Set.unions [chrFtv vun,chrFtv sc]
%%[[10
  chrFtv        (RedHow_ByLabel      l o sc)  = Set.unions [chrFtv l,chrFtv o,chrFtv sc]
%%]]
  chrFtv        _                             = Set.empty

  chrAppSubst s (RedHow_Assumption   vun sc)  = RedHow_Assumption (chrAppSubst s vun) (chrAppSubst s sc)
%%[[10
  chrAppSubst s (RedHow_ByLabel      l o sc)  = RedHow_ByLabel (chrAppSubst s l) (chrAppSubst s o) (chrAppSubst s sc)
%%]]
  chrAppSubst _ x                             = x
%%]

%%[10
instance CHRSubstitutable Label TyVarId VarMp where
  chrFtv        x = ftvSet x
  chrAppSubst s x = s |=> x

instance CHRSubstitutable LabelOffset TyVarId VarMp where
  chrFtv        x = ftvSet x
  chrAppSubst s x = s |=> x
%%]

%%[10
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
%%]
  chrMatchTo _ subst (Label_Var v1) lb2@(Label_Var v2) | v1 == v2  = Just emptyVarMp
                                                       | otherwise = Just $ v1 `varmpLabelUnit` lb2
  chrMatchTo _ subst _                  (Label_Var v2)             = Nothing
  chrMatchTo _ subst (Label_Var v1) lb2                            = Just $ v1 `varmpLabelUnit` lb2
  chrMatchTo _ subst (Label_Lab l1)     (Label_Lab l2) | l1 == l2  = Just emptyVarMp
  chrMatchTo _ subst _              _                              = Nothing

%%[10
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
%%]
instance CHRMatchable FIIn LabelOffset VarMp where
  chrMatchTo _ subst (LabelOffset_Var v1) of2@(LabelOffset_Var v2) | v1 == v2  = Just emptyVarMp
                                                                   | otherwise = Just $ v1 `varmpOffsetUnit` of2
  chrMatchTo _ subst _                        (LabelOffset_Var v2)             = Nothing
  chrMatchTo _ subst (LabelOffset_Var v1) of2                                  = Just $ v1 `varmpOffsetUnit` of2
  chrMatchTo _ subst (LabelOffset_Off l1)     (LabelOffset_Off l2) | l1 == l2  = Just emptyVarMp
  chrMatchTo _ subst _                    _                                    = Nothing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Lattice ordering, for annotations which have no ordering
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

This should be put in some library

%%[9 export(PartialOrdering(..),toOrdering,toPartialOrdering)
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
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Guard, CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(Guard(..))
data Guard
  = HasStrictCommonScope    PredScope PredScope PredScope                   -- have strict/proper common scope?
  | IsVisibleInScope        PredScope PredScope                             -- is visible in 2nd scope?
  | NotEqualScope           PredScope PredScope                             -- scopes are unequal
  | EqualScope              PredScope PredScope                             -- scopes are equal
  | IsStrictParentScope     PredScope PredScope PredScope                   -- parent scope of each other?
%%[[10
  | NonEmptyRowLacksLabel	Ty LabelOffset Ty Label							-- non empty row does not have label?, yielding its position + rest
%%]]
%%]

%%[9
ppGuard :: Guard -> PP_Doc
ppGuard (HasStrictCommonScope   sc1 sc2 sc3) = ppParensCommas' [sc1 >#< "<" >#< sc2,sc1 >#< "<=" >#< sc3]
ppGuard (IsStrictParentScope    sc1 sc2 sc3) = ppParens (sc1 >#< "==" >#< sc2 >#< "/\\" >#< sc2 >#< "/=" >#< sc3)
ppGuard (IsVisibleInScope       sc1 sc2    ) = sc1 >#< "`visibleIn`" >#< sc2
ppGuard (NotEqualScope          sc1 sc2    ) = sc1 >#< "/=" >#< sc2
ppGuard (EqualScope             sc1 sc2    ) = sc1 >#< "==" >#< sc2
%%[[10
ppGuard (NonEmptyRowLacksLabel  r o t l    ) = ppParens (t >#< "==" >#< ppParens (r >#< "| ...")) >#< "\\" >#< l >|< "@" >|< o
%%]]
%%]
ppGuard (IsStrictParentScope    sc1 sc2 sc3) = ppParens (ppParens (sc1 >#< "==" >#< sc2 >#< "\\/" >#< sc1 >#< "==" >#< sc3 ) >#< "/\\" >#< sc2 >#< "/=" >#< sc3)

%%[9
instance Show Guard where
  show _ = "CHR Guard"

instance PP Guard where
  pp = ppGuard
%%]

%%[20
instance PPForHI Guard where
  ppForHI (HasStrictCommonScope   sc1 sc2 sc3) = "HasStrictCommonScope"  >#< (ppCurlysCommas $ map ppForHI [sc1,sc2,sc3])
  ppForHI (IsStrictParentScope    sc1 sc2 sc3) = "IsStrictParentScope"   >#< (ppCurlysCommas $ map ppForHI [sc1,sc2,sc3])
  ppForHI (IsVisibleInScope       sc1 sc2    ) = "IsVisibleInScope"      >#< (ppCurlysCommas $ map ppForHI [sc1,sc2])
  ppForHI (NotEqualScope          sc1 sc2    ) = "NotEqualScope"         >#< (ppCurlysCommas $ map ppForHI [sc1,sc2])
  ppForHI (EqualScope             sc1 sc2    ) = "EqualScope"            >#< (ppCurlysCommas $ map ppForHI [sc1,sc2])
  ppForHI (NonEmptyRowLacksLabel  r o t l    ) = "NonEmptyRowLacksLabel" >#<  ppCurlysCommas [ppForHI r, ppForHI o, pp t, ppForHI l]
%%]

%%[9
instance CHRCheckable FIIn Guard VarMp where
  chrCheck env subst x
    = chk x
    where subst' = subst |=> fiVarMp env
          chk (HasStrictCommonScope (PredScope_Var vDst) sc1 sc2)
            = do { let sc1' = chrAppSubst subst' sc1
                       sc2' = chrAppSubst subst' sc2
                 ; scDst <- pscpCommon sc1' sc2'
                 ; if scDst == sc1'
                   then Nothing
                   else return $ vDst `varmpScopeUnit` scDst
                 }
          chk (IsStrictParentScope (PredScope_Var vDst) sc1 sc2)
            = do { let sc1' = chrAppSubst subst' sc1
                       sc2' = chrAppSubst subst' sc2
                 ; scDst <- pscpCommon sc1' sc2'
                 ; if scDst == sc1' && sc1' /= sc2'
                   then return $ vDst `varmpScopeUnit` scDst
                   else Nothing
                 }
          chk (NotEqualScope sc1 sc2) | isJust c
            = if fromJust c /= EQ then return emptyVarMp else Nothing
            where c = pscpCmp (chrAppSubst subst' sc1) (chrAppSubst subst' sc2)
          chk (EqualScope sc1 sc2) | isJust c
            = if fromJust c == EQ then return emptyVarMp else Nothing
            where c = pscpCmp (chrAppSubst subst' sc1) (chrAppSubst subst' sc2)
          chk (IsVisibleInScope scDst@(PredScope_Var vDst) sc1) | isJust mbSc
            = chk (IsVisibleInScope (fromJust mbSc) sc1)
            where mbSc = varmpScopeLookupScopeCyc scDst subst'
          chk (IsVisibleInScope (PredScope_Var vDst) sc1)
            = return $ vDst `varmpScopeUnit` sc1
          chk (IsVisibleInScope scDst sc1) | pscpIsVisibleIn (chrAppSubst subst' scDst) (chrAppSubst subst' sc1)
            = return emptyVarMp
%%[[10
          chk (NonEmptyRowLacksLabel (Ty_Var tv TyVarCateg_Plain) (LabelOffset_Var vDst) ty lab) | not (null exts) && presence == Absent -- tyIsEmptyRow row
            = return $ (vDst `varmpOffsetUnit` LabelOffset_Off offset)
                       |=> (tv `varmpTyUnit` row)
            where (row,exts) = tyRowExtsWithLkup (varmpTyLookupCyc2 subst') ty
                  (offset,presence) = tyExtsOffset lab' $ tyRowCanonOrder exts
                  (Label_Lab lab') = chrAppSubst subst' lab
%%]]
          chk _
            = Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Criterium for proving in a let expression
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(isLetProveCandidate,isLetProveFailure)
isLetProveCandidate :: (Ord v, CHRSubstitutable x v s) => Set.Set v -> x -> Bool
isLetProveCandidate glob x
  = Set.null fv || Set.null (fv `Set.intersection` glob)
  where fv = chrFtv x

isLetProveFailure :: (Ord v, CHRSubstitutable x v s) => Set.Set v -> x -> Bool
isLetProveFailure glob x
  = Set.null fv
  where fv = chrFtv x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ForceEval
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
instance ForceEval Guard where
  forceEval x@(HasStrictCommonScope   sc1 sc2 sc3) | forceEval sc1 `seq` forceEval sc2 `seq` forceEval sc3 `seq` True = x
  forceEval x@(IsStrictParentScope    sc1 sc2 sc3) | forceEval sc1 `seq` forceEval sc2 `seq` forceEval sc3 `seq` True = x
  forceEval x@(IsVisibleInScope       sc1 sc2    ) | forceEval sc1 `seq` forceEval sc2 `seq` True = x
  forceEval x@(NotEqualScope          sc1 sc2    ) | forceEval sc1 `seq` forceEval sc2 `seq` True = x
  forceEval x@(EqualScope             sc1 sc2    ) | forceEval sc1 `seq` forceEval sc2 `seq` True = x
  forceEval x@(NonEmptyRowLacksLabel  r o t l    ) | forceEval r `seq` forceEval o `seq` forceEval t `seq` forceEval l `seq` True = x
%%[[101
  fevCount (HasStrictCommonScope   sc1 sc2 sc3) = cm1 "HasStrictCommonScope"  `cmUnion` fevCount sc1 `cmUnion` fevCount sc2 `cmUnion` fevCount sc3
  fevCount (IsStrictParentScope    sc1 sc2 sc3) = cm1 "IsStrictParentScope"   `cmUnion` fevCount sc1 `cmUnion` fevCount sc2 `cmUnion` fevCount sc3
  fevCount (IsVisibleInScope       sc1 sc2    ) = cm1 "IsVisibleInScope"      `cmUnion` fevCount sc1 `cmUnion` fevCount sc2
  fevCount (NotEqualScope          sc1 sc2    ) = cm1 "NotEqualScope"         `cmUnion` fevCount sc1 `cmUnion` fevCount sc2
  fevCount (EqualScope             sc1 sc2    ) = cm1 "EqualScope"            `cmUnion` fevCount sc1 `cmUnion` fevCount sc2
  fevCount (NonEmptyRowLacksLabel  r o t l    ) = cm1 "NonEmptyRowLacksLabel" `cmUnion` fevCount r   `cmUnion` fevCount o `cmUnion` fevCount t `cmUnion` fevCount l
%%]]
%%]

