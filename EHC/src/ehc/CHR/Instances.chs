%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional Pred admin for Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[(9 hmtyinfer) module {%{EH}CHR.Instances} import(UHC.Util.CHR,{%{EH}CHR.Key})
%%]

%%[(9 hmtyinfer) import({%{EH}CHR.Constraint}, {%{EH}CHR.Guard}) export(module {%{EH}CHR.Constraint}, module {%{EH}CHR.Guard})
%%]

%%[(9 hmtyinfer) import(qualified Data.Map as Map,qualified Data.Set as Set,Data.Maybe)
%%]

%%[(9 hmtyinfer) import(UHC.Util.Pretty,UHC.Util.AGraph)
%%]

%%[(9 hmtyinfer) import({%{EH}Base.Common},{%{EH}Base.TermLike})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty},{%{EH}VarMp},{%{EH}Substitutable},{%{EH}Ty.FitsInCommon2},{%{EH}Ty.FitsIn},{%{EH}Ty.TreeTrieKey})
%%]

%%[(10 hmtyinfer) import({%{EH}Base.HsName.Builtin})
%%]

%%[(41 hmtyinfer) import({%{EH}Ty.Trf.MergePreds}, {%{EH}Ty.FitsInCommon}, {%{EH}Opts}, Debug.Trace)
%%]

%%[(50 hmtyinfer) import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[(9999 hmtyinfer) import({%{EH}Base.ForceEval},{%{EH}Ty.Trf.ForceEval})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Substitutable instances: VarExtractable, VarUpdatable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
type instance ExtrValVarKey ConstraintToInfoMap = TyVarId

instance VarExtractable ConstraintToInfoMap where
  varFreeSet        x = Set.unions [ varFreeSet k | k <- Map.keys x ]

instance VarUpdatable ConstraintToInfoMap VarMp where
  varUpd s x = Map.mapKeysWith (++) (varUpd s) x

type instance ExtrValVarKey Guard = TyVarId

instance VarExtractable Guard where
  varFreeSet        (HasStrictCommonScope   p1 p2 p3) = Set.unions $ map varFreeSet [p1,p2,p3]
  varFreeSet        (IsStrictParentScope    p1 p2 p3) = Set.unions $ map varFreeSet [p1,p2,p3]
  varFreeSet        (IsVisibleInScope       p1 p2   ) = Set.unions $ map varFreeSet [p1,p2]
  varFreeSet        (NotEqualScope          p1 p2   ) = Set.unions $ map varFreeSet [p1,p2]
  varFreeSet        (EqualScope             p1 p2   ) = Set.unions $ map varFreeSet [p1,p2]
%%[[10
  varFreeSet        (NonEmptyRowLacksLabel  r o t l ) = Set.unions [varFreeSet r,varFreeSet o,varFreeSet t,varFreeSet l]
%%]]
%%[[41
  varFreeSet        (IsCtxNilReduction t1 t2)         = Set.unions [varFreeSet t1, varFreeSet t2]
  varFreeSet        (EqsByCongruence t1 t2 ps)        = Set.unions [varFreeSet t1, varFreeSet t2, varFreeSet ps]
  varFreeSet        (EqualModuloUnification t1 t2)    = Set.unions [varFreeSet t1, varFreeSet t2]
%%]]

instance VarUpdatable Guard VarMp where
  varUpd s (HasStrictCommonScope   p1 p2 p3) = HasStrictCommonScope   (s `varUpd` p1) (s `varUpd` p2) (s `varUpd` p3)
  varUpd s (IsStrictParentScope    p1 p2 p3) = IsStrictParentScope    (s `varUpd` p1) (s `varUpd` p2) (s `varUpd` p3)
  varUpd s (IsVisibleInScope       p1 p2   ) = IsVisibleInScope       (s `varUpd` p1) (s `varUpd` p2)
  varUpd s (NotEqualScope          p1 p2   ) = NotEqualScope          (s `varUpd` p1) (s `varUpd` p2)
  varUpd s (EqualScope             p1 p2   ) = EqualScope             (s `varUpd` p1) (s `varUpd` p2)
%%[[10
  varUpd s (NonEmptyRowLacksLabel  r o t l ) = NonEmptyRowLacksLabel  (s `varUpd` r)  (s `varUpd` o)  (s `varUpd` t)  (s `varUpd` l)
%%]]
%%[[41
  varUpd s (IsCtxNilReduction t1 t2)         = IsCtxNilReduction (s `varUpd` t1) (s `varUpd` t2)
  varUpd s (EqsByCongruence t1 t2 ps)        = EqsByCongruence (s `varUpd` t1) (s `varUpd` t2) (s `varUpd` ps)
  varUpd s (UnequalTy t1 t2)                 = UnequalTy (s `varUpd` t1) (s `varUpd` t2)
  varUpd s (EqualModuloUnification t1 t2)    = EqualModuloUnification (s `varUpd` t1) (s `varUpd` t2)
%%]]
%%]

%%[(9 hmtyinfer)
type instance ExtrValVarKey VarUIDHsName = TyVarId

instance VarExtractable VarUIDHsName where
  varFreeSet          (VarUIDHs_Var i)  = Set.singleton i
  varFreeSet          _                 = Set.empty

-- instance VarUpdatable VarUIDHsName VarMp where
instance (VarLookup m (SubstVarKey m) (SubstVarVal m), SubstVarKey m ~ ImplsVarId, SubstVarVal m ~ VarMpInfo) => VarUpdatable VarUIDHsName m where
  varUpd s a                   = maybe a id $ varmpAssNmLookupAssNmCyc a s
%%]

%%[(9 hmtyinfer)
type instance ExtrValVarKey RedHowAnnotation = TyVarId

instance VarExtractable RedHowAnnotation where
  varFreeSet        (RedHow_Assumption   vun sc)  = Set.unions [varFreeSet vun,varFreeSet sc]
%%[[10
  varFreeSet        (RedHow_ByLabel      l o sc)  = Set.unions [varFreeSet l,varFreeSet o,varFreeSet sc]
%%]]
  varFreeSet        _                             = Set.empty

instance VarUpdatable RedHowAnnotation VarMp where
  varUpd s (RedHow_Assumption   vun sc)  = RedHow_Assumption (varUpd s vun) (varUpd s sc)
%%[[10
  varUpd s (RedHow_ByLabel      l o sc)  = RedHow_ByLabel (varUpd s l) (varUpd s o) (varUpd s sc)
%%]]
%%[[41
  varUpd s (RedHow_ByEqTyReduction  ty1 ty2) = RedHow_ByEqTyReduction (s `varUpd` ty1) (s `varUpd` ty2)
%%]]
  varUpd _ x                             = x
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR instances: CHREmptySubstitution
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
instance CHREmptySubstitution VarMp where
  chrEmptySubst = emptyVarMp

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR instances: CHRMatchable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
instance CHRMatchable FIIn Pred VarMp where
  chrMatchTo fi subst pr1 pr2
    = do { (_,subst') <- fitPredIntoPred (fi {fiVarMp = subst |+> fiVarMp fi}) pr1 pr2
         ; return subst'
         }
%%]

%%[(9 hmtyinfer)
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
%%]

%%[(9 hmtyinfer)
instance CHRMatchable FIIn CHRPredOcc VarMp where
  chrMatchTo fi subst po1 po2
    = do { subst1 <- chrMatchTo fi subst (cpoPr po1) (cpoPr po2)
         ; subst2 <- chrMatchTo fi subst (cpoCxt po1) (cpoCxt po2)
         ; return $ subst2 |+> subst1
         }
%%]

%%[(10 hmtyinfer)
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

%%[(10 hmtyinfer)
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR instances: CHRCheckable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
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
%%[[10
          chk (NonEmptyRowLacksLabel r1@(Ty_Var tv _) (LabelOffset_Var vDst) ty lab)
            |  fiAllowTyVarBind env r1
            && not (null exts) && presence == Absent -- tyIsEmptyRow row
            = return $ (vDst `varmpOffsetUnit` LabelOffset_Off offset)
                       `varUpd` (tv `varmpTyUnit` row)
            where (row,exts) = tyRowExtsWithLkup (varmpTyLookupCyc2 subst') ty
                  ((offset,presence),_) = tyExtsOffset lab' $ rowCanonOrder exts
                  (Label_Lab lab') = varUpd subst' lab
%%]]
%%[[41
          chk (IsCtxNilReduction t1 t2)
            = if foHasErrs fo || tStart == tRes
              then Nothing
              else return varMp
            where
              tStart = subst' `varUpd` t1
              t1' = tmpoTy $ tyMergePreds [] tStart
              t2' = subst' `varUpd` t2
              uid = fiUniq env
              fiOpts = unifyFIOpts { fioUniq = uid, fioBindRVars = FIOBindNoBut (varFreeSet t2), fioPredAsTy = True, fioLeaveRInst = True }
              fo = fitsIn fiOpts (fiEnv env) uid subst' t1' t2'
              varMp = foVarMp fo
              tRes = varMp `varUpd` foTy fo -- hum, I would assume that this substitution is already applied by fitsIn...?!
          
          chk (EqsByCongruence t1 t2 obls)
            = if foHasErrs fo || null preds
              then Nothing
              else return varMp
            where
              t1'   = subst' `varUpd` t1   -- todo: check if these substitutions a really needed (maybe its already done by the solver before calling chk)
              t2'   = subst' `varUpd` t2
              obls' = subst' `varUpd` obls
              uid   = fiUniq env
              fiOpts = unifyFIOpts { fioUniq = uid, fioFitFailureToProveObl = True, fioBindRVars = FIOBindNoBut Set.empty, fioDontBind = fioDontBind (fiFIOpts env), fioPredAsTy = True, fioLeaveRInst = True }
              fo = fitsIn fiOpts (fiEnv env) uid subst' t1' t2'
              cnstrs = Map.keys (foGathCnstrMp fo)
              preds  = [ cpoPr p | (Prove p) <- cnstrs ]
              ps = foldr PredSeq_Cons PredSeq_Nil preds
              (PredSeq_Var v) = obls
              varMp = v `varmpPredSeqUnit` ps `varUpd` foVarMp fo
          
          chk (UnequalTy t1 t2)
            = if (subst' `varUpd` t1) == (subst' `varUpd` t2)
              then Nothing
              else return emptyVarMp
          
          chk (EqualModuloUnification t1 t2)
            | isTyVar t1 = return emptyVarMp
            | isTyVar t2 = return emptyVarMp
            where
              isTyVar t = case (subst' `varUpd` t) of
                            Ty_Var _ TyVarCateg_Plain -> True
                            _                         -> False
          {-
          chk (EqualModuloUnification t1 t2)
            = if foHasErrs fo
              then Nothing
              else return emptyVarMp
            where
              t1'    = subst' `varUpd` t1
              t2'    = subst' `varUpd` t2
              uid    = fiUniq env
              fiOpts = unifyFIOpts { fioUniq = uid, fioPredAsTy = True, fioLeaveRInst = True }
              fo     = fitsIn fiOpts (fiEnv env) uid subst' t1' t2'
          -}
%%]]
          chk _
            = Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR instances: IsCHRConstraint, IsCHRGuard, IsCHRSolvable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
instance IsCHRConstraint FIIn Constraint VarMp

instance IsCHRGuard FIIn Guard VarMp

-- instance IsCHRPrio FIIn Prio VarMp
%%]

