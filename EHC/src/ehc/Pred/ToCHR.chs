%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expansion to Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

Conversion from Pred to CHR.

%%[(9 hmtyinfer) module {%{EH}Pred.ToCHR} import({%{EH}Opts},{%{EH}Base.Common},{%{EH}Base.TermLike},{%{EH}Ty},{%{EH}Ty.Ftv},{%{EH}Error},{%{EH}VarMp},{%{EH}Substitutable})
%%]

%%[(9 hmtyinfer) import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[(9 hmtyinfer) import(UHC.Util.CHR,{%{EH}CHR.Constraint},{%{EH}CHR.Guard},{%{EH}CHR.Solve},{%{EH}CHR.Key})
%%]

%%[(9 hmtyinfer) import({%{EH}Pred.Heuristics},{%{EH}Pred.Evidence},{%{EH}Pred.RedGraph})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty.FitsInCommon2}, {%{EH}Ty.Trf.Canonic})
%%]

-- debug
%%[(9 hmtyinfer) import(UHC.Util.Pretty,UHC.Util.Utils)
%%]

%%[(13 hmtyinfer) import({%{EH}Base.HsName.Builtin})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intermediate structures for constructing CHR variants of class/instance decl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(CHRClassDecl, CHRScopedInstanceDecl)
type CHRClassDecl'          a info      = ([a], a, [info])
type CHRClassDecl                       = CHRClassDecl' Pred RedHowAnnotation
type CHRScopedInstanceDecl' a info sc   = ([a], a, info, sc)
type CHRScopedInstanceDecl              = CHRScopedInstanceDecl' Pred RedHowAnnotation PredScope
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion/expansion into CHR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
type MkRes1 = (CHRStore, ([CHRPredOcc],CHRPredOcc) )
type MkResN = (CHRStore,[([CHRPredOcc],CHRPredOcc)])
%%]

Variables used in CHR's are implicitly universally quantified for each constraint,
they will match against a concrete constraint and then bound the info found therein.
Hence we can safely use non-unique variables.

%%[(9 hmtyinfer)
([sc1,sc2,sc3]
 ,[pr1,pr2,pr3]
%%[[10
 ,[ty1,ty2,ty3,ty4]
 ,[lab1]
 ,[off1]
%%]]
%%[[13
 ,[pr1v]
 ,[pa1]
%%]]
 )
  = ( map PredScope_Var [u1,u2,u3]
    , map Pred_Var [u7,u8,u9]
%%[[10
    , map mkTyMetaVar {- mkTyVar -} [u10,u11,u14,u15]
    , map Label_Var [u12]
    , map LabelOffset_Var [u13]
%%]]
%%[[13
    , [u7]
    , map PredSeq_Var [u4]
%%]]
    )
%%[[9
  where [u1,u2,u3,u4,u5,u6,u7,u8,u9] = mkNewLevUIDL 9 uidStart
%%][10
  where [u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15] = mkNewLevUIDL 15 uidStart
%%]]
%%]

CHR rules for proving constraints. These rules have to obey the invariant that
the new proof obligations are individually weaker than the original proof
obligations, i.e. that there is no sequence of reductions that can lead from the
new proof obligations to the original ones.

In terms of entailment: for a CHR rule H => B, B may not entail H.

20100508 AD: this is no longer true, a cycle thus may arise, which is
reflected in a letrec definition on the evidence/witness level. However,
the CHR + solver uses set semantics, meaning that an individual
constraint can only be reduced once still. However, the reduction steps
will reflect the cycle still.

%%[(9 hmtyinfer) export(initCHRStore)
-- | The basic initial set of CHRs
initCHRStore :: CHRStore
initCHRStore
  = chrStoreFromElems $
      [ scopeProve, {- scopeAssum1, -} scopeAssum2 ]
%%[[10
      ++ [ labelProve1, labelProve2 ]
%%]]
%%[[13
      ++ [ instForall, predArrow, predSeq1, predSeq2 ]
%%]]
%%[[41
      -- comment out predSeq1 and predSeq2 above because they conflict with the unpack rules!
      ++ [ {-rlEqScope,-} rlEqTrans, rlEqSym, rlEqCongr, rlUnpackCons , {- rlCtxToNil, -} rlEqSymPrv, rlEqTransPrv, rlEqCongrPrv, rlUnpackConsPrv, rlUnpackNilPrv, rlPrvByAssume, rlPrvByIdentity ]
%%]]
  where p1s1         = mkCHRPredOcc pr1 sc1
        p1s2         = mkCHRPredOcc pr1 sc2
        p1s3         = mkCHRPredOcc pr1 sc3
        scopeProve   = [mkProve p1s1, mkProve p1s2] 
                         ==> [mkReduction p1s2 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "prv")) [p1s3]]
                          |> [IsStrictParentScope sc3 sc1 sc2]
{-
        scopeAssum1  = [mkProve p1s1, mkAssume p1s2] 
                         ==> [mkReduction p1s1 (RedHow_Assumption sc2) []]
                          |> [EqualScope sc1 sc2]
-}
        scopeAssum2  = [mkProve p1s1, mkAssume p1s2] 
                         ==> [mkReduction p1s1 (RedHow_ByScope ByScopeRedHow_Assume) [p1s2]]
                          |> [NotEqualScope sc1 sc2,IsVisibleInScope sc2 sc1]
%%[[10
        l1s1         = mkCHRPredOcc (Pred_Lacks ty1 lab1) sc1
        l2s1         = mkCHRPredOcc (Pred_Lacks ty2 lab1) sc1
        l3s1         = mkCHRPredOcc (Pred_Lacks recRowEmp lab1) sc1
        labelProve1  = [mkProve l1s1]
                         ==> [mkProve l2s1, mkReduction l1s1 (RedHow_ByLabel lab1 off1 sc1) [l2s1]]
                          |> [NonEmptyRowLacksLabel ty2 off1 ty1 lab1]
        labelProve2  = [mkProve l3s1]
                         ==> [mkReduction l3s1 (RedHow_ByLabel lab1 (LabelOffset_Off 0) sc1) []]
%%]]
%%[[13
        f1s1         = mkCHRPredOcc (tyPred $ mkTyQu tyQu_Forall [(pr1v,kiStar)] $ mkTyPr pr1) sc1	-- TBD
        f2s1         = mkCHRPredOcc pr1 sc1
        instForall   = [mkAssume f1s1]
                         ==> [mkAssume f2s1]
        a1s1         = mkCHRPredOcc (Pred_Arrow pa1 pr1) sc1
        a2s1         = mkCHRPredOcc pr1 sc1
        a3s1         = mkCHRPredOcc (Pred_Preds pa1) sc1
        predArrow    = [mkAssume a1s1, mkProve a2s1]
                         ==> [mkProve a3s1, mkReduction a2s1 (RedHow_ByInstance hsnUnknown pr1 sc1) [a1s1,a3s1]]
        s1s1         = mkCHRPredOcc (Pred_Preds (PredSeq_Cons pr1 pa1)) sc1
        s2s1         = mkCHRPredOcc pr1 sc1
        s3s1         = mkCHRPredOcc (Pred_Preds pa1) sc1
        predSeq1     = [mkProve s1s1]
                         <==> [mkProve s2s1, mkProve s3s1]
        predSeq2     = [mkProve $ mkCHRPredOcc (Pred_Preds PredSeq_Nil) sc1]
                         <==> ([] :: [Constraint])
%%]]
%%[[41
        eqT1T2s1 = mkCHRPredOcc (Pred_Eq ty1 ty2) sc1
        eqT1T2s2 = mkCHRPredOcc (Pred_Eq ty1 ty2) sc2
        eqT2T1s1 = mkCHRPredOcc (Pred_Eq ty2 ty1) sc1
        eqT2T3s1 = mkCHRPredOcc (Pred_Eq ty2 ty3) sc1
        eqT1T3s1 = mkCHRPredOcc (Pred_Eq ty1 ty3) sc1
        eqT2T3s2 = mkCHRPredOcc (Pred_Eq ty2 ty3) sc2
        eqT3T4s2 = mkCHRPredOcc (Pred_Eq ty3 ty4) sc2
        psPreds1 = mkCHRPredOcc (Pred_Preds pa1)  sc1
        psConss1 = mkCHRPredOcc (Pred_Preds (PredSeq_Cons pr1 pa1)) sc1
        psNils1  = mkCHRPredOcc (Pred_Preds PredSeq_Nil) sc1
        psHeads1 = mkCHRPredOcc pr1 sc1

        rlEqSym      = [mkAssume eqT1T2s1] ==> [mkAssume eqT2T1s1]                                                 -- symmetry
        rlEqTrans    = [mkAssume eqT1T2s1, mkAssume eqT2T3s2] ==> [mkAssume eqT1T3s1] |> [IsVisibleInScope sc2 sc1]  -- transitivity
        rlEqCongr    = [mkAssume eqT1T2s1] ==> [mkAssume psPreds1] |> [EqsByCongruence ty1 ty2 pa1]                -- congruence
        rlUnpackCons = [mkAssume psConss1] ==> [mkAssume psHeads1, mkAssume psPreds1]                                -- unpack a list of assumptions
        
        -- rlCtxToNil      = [mkProve eqT1T2s1] ==> [mkProve eqT1T3s1, mkReduction eqT1T2s1 (RedHow_ByEqTyReduction ty2 ty3) [eqT1T3s1]] |> [IsCtxNilReduction ty2 ty3]
        rlEqSymPrv      = [mkProve eqT1T2s1] ==> [mkProve eqT2T1s1, mkReduction eqT1T2s1 RedHow_ByEqSymmetry [eqT2T1s1]] |> [UnequalTy ty1 ty2]
        rlEqTransPrv    = [mkProve eqT1T2s1, mkAssume eqT2T3s2] ==> [mkProve eqT1T3s1, mkReduction eqT1T2s1 RedHow_ByEqTrans [eqT1T3s1]] |> [IsVisibleInScope sc2 sc1, UnequalTy ty2 ty3]
        rlEqCongrPrv    = [mkProve eqT1T2s1] ==> [mkProve psPreds1, mkReduction eqT1T2s1 RedHow_ByEqCongr [psPreds1]] |> [EqsByCongruence ty1 ty2 pa1]
        rlUnpackConsPrv = [mkProve psConss1] ==> [mkProve psHeads1, mkProve psPreds1, mkReduction psConss1 RedHow_ByPredSeqUnpack [psHeads1, psPreds1]]
        rlUnpackNilPrv  = [mkProve psNils1]  ==> [mkReduction psNils1 RedHow_ByPredSeqUnpack []]
        rlPrvByAssume   = [mkProve eqT1T2s1, mkAssume eqT1T2s2] ==> [mkReduction eqT1T2s1 RedHow_ByEqFromAssume []] |> [IsVisibleInScope sc2 sc1]  -- dirty hack: generated assumptions by chr are not added to the graph, so made a reduction instead
        rlPrvByIdentity = [mkProve eqT1T2s1] ==> [mkReduction eqT1T2s1 RedHow_ByEqIdentity []] |> [EqualModuloUnification ty1 ty2]
        
%%]]
        -- inclSc       = ehcCfgCHRInclScope $ feEHCOpts $ fiEnv env
%%]

%%[(9 hmtyinfer) export(mkScopedCHR2)
-- | Construct CHRs from class and instance decls
mkScopedCHR2
  :: FIIn -> [CHRClassDecl] -> [CHRScopedInstanceDecl]
       -> CHRStore -> (CHRStore,CHRStore)
mkScopedCHR2 env clsDecls insts prevStore
  = (chrStoreUnions [store2,instSimplStore], chrStoreUnions [assumeStore,instSimplStore])
  where  ucls        = mkNewLevUIDL (length clsDecls) $ fiUniq env
         ((assumeStore,assumePredOccs), (instStore,_))
                     = mkScopedChrs clsDecls canonInsts
         store2      = chrStoreUnions [assumeStore,prevStore]
         simplStores = zipWith (\u (cx,h,i) -> mkClassSimplChrs (env {fiUniq = u}) store2 (cx,h,i)) ucls clsDecls
         instSimplStore
         			 = chrStoreUnions $ instStore : simplStores
         canonInsts  = [ (map mkC cx, mkC hd, info, sc) | (cx,hd,info,sc) <- insts ]
                     where mkC = fst . predCanonic (emptyTyBetaRedEnv {tbredFI=env})
%%]

%%[(9 hmtyinfer)
-- | Construct simplification CHRs from class decls, building upon a given CHR store
mkClassSimplChrs :: FIIn -> CHRStore -> CHRClassDecl -> CHRStore
mkClassSimplChrs env rules (context, head, infos)
  = simps
  where simps        = chrStoreFromElems $ mapTrans (Set.fromList [head1]) [] head1 (zip infos (map (\p -> Red_Pred $ mkCHRPredOcc p sc1) context))
        (superClassesWork, superClassesDone, _ :: SolveTrace FIIn Constraint Guard VarMp)
                     = chrSolve' env rules (map (\p -> toSolverConstraint $ mkAssume $ mkCHRPredOcc p sc1) context)
        superClasses = superClassesWork ++ superClassesDone
        graph        = mkRedGraphFromReductions $ filterMb fromSolverConstraint superClasses
        head1        = mkCHRPredOcc head sc1
        head2        = mkCHRPredOcc head sc2
        head3        = mkCHRPredOcc head sc3
    
        mapTrans done reds subClass
          = concatMap (transClosure done reds subClass)
            . filter (\(_,x) -> not (rednodePred x `Set.member` done))
    
        transClosure done reds par (info, pr@(Red_Pred p@(CHRPredOcc {cpoPr = super})))
          = [superRule] ++ ({-if ehcCfgCHRScoped opts >= CHRScopedMutualSuper then -} [scopeRule1, scopeRule2] {- else [] -}) ++ rules
          where super1     = mkCHRPredOcc super sc1
                super2     = mkCHRPredOcc super sc2
                super3     = mkCHRPredOcc super sc3
                superRule  = [mkProve head1, mkProve p] ==> reds'
                scopeRule1 = [mkProve head1, mkProve super2] 
                               ==> [mkProve head3, mkReduction head1 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "sup1")) [head3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                scopeRule2 = [mkProve head2, mkProve super1] 
                               ==> [mkProve super3, mkReduction super1 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "sup2")) [super3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                reds'      = mkReduction p info [par] : reds
                rules      = mapTrans (Set.insert p done) reds' p (predecessors graph pr)

        opts          = feEHCOpts $ fiEnv env

mkScopedChrs :: [CHRClassDecl] -> [CHRScopedInstanceDecl] -> (MkResN,MkResN)
mkScopedChrs clsDecls insts
  = ((chrStoreUnions assumeStores,assumePredOccs), instChrs)
  where (assumeStores,assumePredOccs) = unzip $ mapMaybe mkAssumeChrs clsDecls 
        instChrs   = mkInstanceChrs insts

mkAssumeChrs :: CHRClassDecl -> Maybe MkRes1
mkAssumeChrs ([]     ,  _  , _    ) = Nothing
mkAssumeChrs (context, head, infos) =
  let prThis = mkCHRPredOcc head sc1
      super prSuper info = [mkAssume prSuper, mkReduction prSuper info [prThis]]
      prSuper = map (\c -> mkCHRPredOcc c sc1) context
  in  Just ( chrStoreSingletonElem $ [mkAssume prThis] ==> concat (zipWith super prSuper infos)
           , (prSuper,prThis)
           )

mkInstanceChrs :: [CHRScopedInstanceDecl] -> MkResN
mkInstanceChrs insts
  = (chrStoreUnions instStores,instChrs)
  where (instStores,instChrs) = unzip $ map mkInstanceChr insts

mkInstanceChr :: CHRScopedInstanceDecl -> MkRes1
mkInstanceChr (context, hd, i, s)
  = ( chrStoreSingletonElem
      $ [mkProve constraint]
          ==> mkReduction constraint i body : map mkProve body
            |> [s `IsVisibleInScope` sc1]
    , (body,constraint)
    )
  where constraint = mkCHRPredOcc hd sc1
        body = map (\p -> mkCHRPredOcc p sc1) context
%%]
  where superClasses = (\(w,d,t) -> trp "XX" (ppCHRStore' rules >-< context >#< ":" >#< (w++d) >-< ppSolveTrace t) (w++d)) $ chrSolve' env rules (map (\p -> mkAssume $ mkCHRPredOcc p sc1) context)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplification result which can be used as the starting point for further simplification
%%% with additional Constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(SimplifyResult, SimplifyResult''(..),emptySimplifyResult)
data SimplifyResult'' p i g s
  = SimplifyResult
      { simpresSolveState		:: SolveState FIIn (Constraint' p i) g s
      , simpresRedGraph			:: RedGraph' p i

      -- for debugging only:
      , simpresRedAlts			:: [HeurAlts' p i]
      , simpresRedTrees			:: [[(i, Evidence' p i)]]
      , simpresRedGraphs		:: [(String,RedGraph' p i)]
      , simpresRemPredL         :: [p]							-- remaining pred occurrences, which cannot be proven, as a list
      }

type SimplifyResult' g s = SimplifyResult'' CHRPredOcc RedHowAnnotation g s

type SimplifyResult = SimplifyResult' Guard VarMp

emptySimplifyResult :: SimplifyResult
emptySimplifyResult
  = SimplifyResult
      emptySolveState emptyRedGraph
      [] [] [] []
%%]

%%[(9 hmtyinfer) export(simplifyResultResetForAdditionalWork)
simplifyResultResetForAdditionalWork :: SimplifyResult -> SimplifyResult
simplifyResultResetForAdditionalWork r = r {simpresRedGraph = emptyRedGraph}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evidence construction from Constraint reduction graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
mkEvidence
  :: Heuristic
  -> ConstraintToInfoMap
  -> RedGraph
  -> ( -- ConstraintToInfoMap						-- remaining constraints
       ConstraintToInfoTraceMp						-- remaining constraints
     , InfoToEvidenceMap							-- mapping to evidence
     , [Err]											-- errors
%%[[9
     , [(HeurAlts, [(i, Evidence)])]        	-- debug info
%%][100
%%]]
     )
mkEvidence heur cnstrMp redGraph
  = ( {- (cnstrMp `Map.intersection` remCnstrMp) `Map.union` -}
      cnstrTraceMpFromList remCnstrMp
    , evidMp
    , err
%%[[9
    , dbg -- redAlts, redTrees                                  -- debug info
%%][100
%%]]
    )
  where (remCnstrMp,evidMp,err,dbg)
          = foldl (\(cm,em,err,dbg) c -> let (cm',em',err',dbg') = mk c in (cm' ++ cm, evidMpUnion em' em, err' ++ err, dbg' ++ dbg))
                  ([],Map.empty,[],[])
            $ Map.toList cnstrMp
        mk (Prove p, infos)
          = (remCnstrMp,evidMp,[],[(redAlts,redTrees)])
          where redAlts    = redAlternatives redGraph p
                redTrees   = heur infos redAlts
                evidMp     = foldr (uncurry evidMpInsert) Map.empty redTrees
                remCnstrMp = [ (Prove (utraceRedFrom u),(i,[u])) | (i,t) <- redTrees, u <- evidUnresolved t ]
        mk (c, infos)
          = ([ (c,(i,[])) | i <- infos],Map.empty,[],[])
%%]
  where (remCnstrMp,evidMp,err,dbg)
          = foldl (\(cm,em,err,dbg) c -> let (cm',em',err',dbg') = mk c in (cnstrMpUnion cm' cm, evidMpUnion em' em, err' ++ err, dbg' ++ dbg))
                  (Map.empty,Map.empty,[],[])
            $ Map.toList cnstrMp
        mk (Prove p, infos)
          = (remCnstrMp,evidMp,[],[(redAlts,redTrees)])
          where redAlts    = redAlternatives redGraph p
                redTrees   = heur infos redAlts
                evidMp     = foldr (uncurry evidMpInsert) Map.empty redTrees
                remCnstrMp = cnstrMpFromList
                             $ concatMap (\(i,t) -> zip (map mkProve (map utraceRedFrom $ evidUnresolved t)) (repeat i))
                             $ redTrees
        mk (c, infos)
          = (cnstrMpFromList $ zip (repeat c) infos,Map.empty,[],[])

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Split unresolved's into those which can be assumed via qualification, and the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Unresolved predicates can be resolved by assuming them.
This can be done for those predicates of which evidence can be passed as a function argument,
and for which no ambiguity exists.

%%[(9 hmtyinfer) export(partitionUnresolved2AssumableAndOthers)
partitionUnresolved2AssumableAndOthers :: ConstraintToInfoTraceMp -> ([CHRIntermediateUntilAssume],ConstraintToInfoTraceMp)
partitionUnresolved2AssumableAndOthers unresCnstrMp
  = (unres,cannotResCnstrMp)
  where (unresCnstrMp',cannotResCnstrMp)
                      = Map.partitionWithKey canAssume unresCnstrMp
                      where -- if p only ranges over non-fixed tvars, we potentially can assume them (if not found ambiguous later)
                            canAssume (Prove p) _ = Map.null $ Map.filter (tvCatIsFixed . tvinfoCateg) $ tyFtvMp $ predTy $ cpoPr p
                            canAssume _         _ = True
        unres         = [ (p,x) | (Prove p,x) <- shareUnresolvedAssumptionsByScope (unresCnstrMp') ]
%%]

%%[(9 hmtyinfer)
-- | Group unresolved constraints, reducing the various scopes to the outermost scope.
--   Find assume's wich have a common scope prefix, then share these.
--   Assumption: we will never share outer scopes because we only get passed inner scopes, because these will be abstracted over in bindings of a let expression.
shareUnresolvedAssumptionsByScope :: ConstraintToInfoTraceMp -> [(Constraint,(PredScope,ConstraintToInfoTraceMp))]
shareUnresolvedAssumptionsByScope unres
  = [ ( c
      , ( -- the common prefix off all scopes, i.e. the most global scope
          foldr1 (\s1 s2 -> panicJust "shareUnresolvedAssumptionsByScope" $ pscpCommon s1 s2)
                 [ cpoScope $ cnstrPred c | (c,_) <- cs ]
          -- all original info for this predicate
        , Map.fromList cs
        )
      )
    | cs@((c,_):_) <-
        -- sort, then group by predicate, so we get all scopes for each predicate
        groupSortOn (cpoPr . cnstrPred . fst) $ Map.toList unres
    ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make assumptions (Assume) from unresolved predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(patchUnresolvedWithAssumption)
-- | Transform unresolved Prove constraints to Assume variants, used either for quantification over, or for error messages about unresolved predicates
patchUnresolvedWithAssumption :: FIIn -> [CHRIntermediateUntilAssume] -> RedGraph -> InfoToEvidenceMap -> (ConstraintToInfoTraceMp,InfoToEvidenceMap)
patchUnresolvedWithAssumption env unres redGraph evidMp
  = ( assumeCnstrs
    , evidMpSubst (\p -> Map.lookup p assumeSubstMp) evidMp
    )
  where us = mkNewLevUIDL (length unres) $ fiUniq env
        assumeCnstrs
          = cnstrMpUnions
              [ cnstrTraceMpSingleton c i (concat [ us | es <- Map.elems trMap, (_,us) <- es ])
              | ((p,(sc,trMap)),u) <- zip unres us
              , let (c,i) = rngLift emptyRange mkAssumeConstraint (cpoPr p) u sc
              ]
        assumeSubstMp
          = Map.fromList [ (p,Evid_Proof p i []) | (Assume p,((i,_):_)) <- Map.toList assumeCnstrs ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplify: solving part, yielding a RedGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(chrSimplifySolveToRedGraph)
chrSimplifySolveToRedGraph
  ::   FIIn
    -> CHRStore
    -> ConstraintToInfoMap
    -> ConstraintToInfoMap
    -> SimplifyResult
    -> ( ConstraintToInfoMap
       , SimplifyResult
       )
chrSimplifySolveToRedGraph env chrStore cnstrInfoMpPrev cnstrInfoMp prevRes
  = ( cnstrInfoMpAll
    , emptySimplifyResult
        { simpresSolveState = solveState
        , simpresRedGraph   = redGraph
        , simpresRedGraphs  = ("chrSimplifySolveToRedGraph",redGraph) : simpresRedGraphs prevRes
        }
    )
  where (_,u1,u2) = mkNewLevUID2 $ fiUniq env
        solveState = chrSolve'' (env {fiUniq = u1}) chrStore (toSolverConstraint $ Map.keys $ cnstrInfoMp `Map.difference` cnstrInfoMpPrev) (simpresSolveState prevRes)
        cnstrInfoMpAll = cnstrMpUnion cnstrInfoMp cnstrInfoMpPrev
        redGraph
          = addToRedGraphFromReductions (filterMb fromSolverConstraint $ chrSolveStateDoneConstraints solveState)
            $ addToRedGraphFromAssumes cnstrInfoMpAll
            $ simpresRedGraph prevRes
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplify: evidence, yielding evidence mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(chrSimplifyRedGraphToEvidence)
chrSimplifyRedGraphToEvidence
  :: Heuristic
  -> ConstraintToInfoMap
  -> SimplifyResult
  -> ( ( ConstraintToInfoTraceMp, InfoToEvidenceMap, [Err] )
     , SimplifyResult
     )
chrSimplifyRedGraphToEvidence heur cnstrInfoMpAll simpRes
  = ( (chrSolveRemCnstrMp,chrSolveEvidMp,chrSolveErrs)
    , simpRes
%%[[9
        {simpresRedAlts = dbg1, simpresRedTrees = dbg2}
%%][100
%%]]
    )
  where ( chrSolveRemCnstrMp,chrSolveEvidMp,chrSolveErrs
%%[[9
         , dbg
%%][100
%%]]
         )
          = mkEvidence heur cnstrInfoMpAll (simpresRedGraph simpRes)
%%[[9
        (dbg1,dbg2) = unzip dbg
%%][100
%%]]
%%]

