%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expansion to Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

Conversion from Pred to CHR.

%%[(9 hmtyinfer) module {%{EH}Pred.ToCHR} import({%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Ty.Ftv},{%{EH}Error},{%{EH}VarMp})
%%]

%%[(9 hmtyinfer) import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[(9 hmtyinfer) import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Solve})
%%]

%%[(9 hmtyinfer) import({%{EH}Pred.CHR},{%{EH}Pred.Heuristics},{%{EH}Pred.Evidence},{%{EH}Pred.RedGraph})
%%]

%%[(9 hmtyinfer) import({%{EH}Ty.FitsInCommon2}, {%{EH}Ty.Trf.Canonic})
%%]

-- debug
%%[(9 hmtyinfer) import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[(13 hmtyinfer) import({%{EH}Base.Builtin})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(ScopedPredStore,ScopedPredCHR,ScopedPredStoreL)
type PredStore  p g s info = CHRStore p info g s
type PredStoreL p g s info = [CHR (Constraint p info) g s]
type ScopedPredStore  = PredStore  CHRPredOcc Guard VarMp RedHowAnnotation
type ScopedPredStoreL = PredStoreL CHRPredOcc Guard VarMp RedHowAnnotation
type ScopedPredCHR    = CHR (Constraint CHRPredOcc RedHowAnnotation) Guard VarMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RedGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(CHRRedGraph)
type CHRRedGraph = RedGraph CHRPredOcc RedHowAnnotation
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intermediate structures for constructing CHR variants of class/instance decl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(CHRClassDecl,CHRScopedInstanceDecl)
type CHRClassDecl           a info      = ([a], a, [info])
type CHRInstanceDecl        a info      = ([a], a, info)
type CHRScopedInstanceDecl  a info sc   = ([a], a, info, sc)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Info to Evidence map for CHRPredOcc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(CHRPredOccEvidMp)
type CHRPredOccEvidMp = InfoToEvidenceMap CHRPredOcc RedHowAnnotation
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion/expansion into CHR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
type MkRes1 = (ScopedPredStore, ([CHRPredOcc],CHRPredOcc) )
type MkResN = (ScopedPredStore,[([CHRPredOcc],CHRPredOcc)])
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
    , map mkTyVar [u10,u11,u14,u15]
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

%%[(9 hmtyinfer) export(initScopedPredStore)
initScopedPredStore :: ScopedPredStore
initScopedPredStore
  = chrStoreFromElems $
      [ scopeProve, {- scopeAssum1, -} scopeAssum2 ]
%%[[10
      ++ [ labelProve1, labelProve2 ]
%%]]
%%[[13
      ++ [ instForall, predArrow, predSeq1, predSeq2 ]
%%]]
%%[[16
      -- comment out predSeq1 and predSeq2 above because they conflict with the unpack rules!
      ++ [ {-rlEqScope,-} rlEqTrans, rlEqSym, rlEqCongr, rlUnpackCons , {- rlCtxToNil, -} rlEqSymPrv, rlEqTransPrv, rlEqCongrPrv, rlUnpackConsPrv, rlUnpackNilPrv, rlPrvByAssume, rlPrvByIdentity ]
%%]]
  where p1s1         = mkCHRPredOcc pr1 sc1
        p1s2         = mkCHRPredOcc pr1 sc2
        p1s3         = mkCHRPredOcc pr1 sc3
        scopeProve   = [Prove p1s1, Prove p1s2] 
                         ==> [Reduction p1s2 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "prv")) [p1s3]]
                          |> [IsStrictParentScope sc3 sc1 sc2]
{-
        scopeAssum1  = [Prove p1s1, Assume p1s2] 
                         ==> [Reduction p1s1 (RedHow_Assumption sc2) []]
                          |> [EqualScope sc1 sc2]
-}
        scopeAssum2  = [Prove p1s1, Assume p1s2] 
                         ==> [Reduction p1s1 (RedHow_ByScope ByScopeRedHow_Assume) [p1s2]]
                          |> [NotEqualScope sc1 sc2,IsVisibleInScope sc2 sc1]
%%[[10
        l1s1         = mkCHRPredOcc (Pred_Lacks ty1 lab1) sc1
        l2s1         = mkCHRPredOcc (Pred_Lacks ty2 lab1) sc1
        l3s1         = mkCHRPredOcc (Pred_Lacks tyRowEmpty lab1) sc1
        labelProve1  = [Prove l1s1]
                         ==> [Prove l2s1, Reduction l1s1 (RedHow_ByLabel lab1 off1 sc1) [l2s1]]
                          |> [NonEmptyRowLacksLabel ty2 off1 ty1 lab1]
        labelProve2  = [Prove l3s1]
                         ==> [Reduction l3s1 (RedHow_ByLabel lab1 (LabelOffset_Off 0) sc1) []]
%%]]
%%[[13
        f1s1         = mkCHRPredOcc (tyPred $ mkTyQu tyQu_Forall [(pr1v,kiStar)] $ mkTyPr pr1) sc1	-- TBD
        f2s1         = mkCHRPredOcc pr1 sc1
        instForall   = [Assume f1s1]
                         ==> [Assume f2s1]
        a1s1         = mkCHRPredOcc (Pred_Arrow pa1 pr1) sc1
        a2s1         = mkCHRPredOcc pr1 sc1
        a3s1         = mkCHRPredOcc (Pred_Preds pa1) sc1
        predArrow    = [Assume a1s1, Prove a2s1]
                         ==> [Prove a3s1, Reduction a2s1 (RedHow_ByInstance hsnUnknown pr1 sc1) [a1s1,a3s1]]
        s1s1         = mkCHRPredOcc (Pred_Preds (PredSeq_Cons pr1 pa1)) sc1
        s2s1         = mkCHRPredOcc pr1 sc1
        s3s1         = mkCHRPredOcc (Pred_Preds pa1) sc1
        predSeq1     = [Prove s1s1]
                         <==> [Prove s2s1, Prove s3s1]
        predSeq2     = [Prove $ mkCHRPredOcc (Pred_Preds PredSeq_Nil) sc1]
                         <==> []
%%]]
%%[[16
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

        rlEqSym      = [Assume eqT1T2s1] ==> [Assume eqT2T1s1]                                                 -- symmetry
        rlEqTrans    = [Assume eqT1T2s1, Assume eqT2T3s2] ==> [Assume eqT1T3s1] |> [IsVisibleInScope sc2 sc1]  -- transitivity
        rlEqCongr    = [Assume eqT1T2s1] ==> [Assume psPreds1] |> [EqsByCongruence ty1 ty2 pa1]                -- congruence
        rlUnpackCons = [Assume psConss1] ==> [Assume psHeads1, Assume psPreds1]                                -- unpack a list of assumptions
        
        -- rlCtxToNil      = [Prove eqT1T2s1] ==> [Prove eqT1T3s1, Reduction eqT1T2s1 (RedHow_ByEqTyReduction ty2 ty3) [eqT1T3s1]] |> [IsCtxNilReduction ty2 ty3]
        rlEqSymPrv      = [Prove eqT1T2s1] ==> [Prove eqT2T1s1, Reduction eqT1T2s1 RedHow_ByEqSymmetry [eqT2T1s1]] |> [UnequalTy ty1 ty2]
        rlEqTransPrv    = [Prove eqT1T2s1, Assume eqT2T3s2] ==> [Prove eqT1T3s1, Reduction eqT1T2s1 RedHow_ByEqTrans [eqT1T3s1]] |> [IsVisibleInScope sc2 sc1, UnequalTy ty2 ty3]
        rlEqCongrPrv    = [Prove eqT1T2s1] ==> [Prove psPreds1, Reduction eqT1T2s1 RedHow_ByEqCongr [psPreds1]] |> [EqsByCongruence ty1 ty2 pa1]
        rlUnpackConsPrv = [Prove psConss1] ==> [Prove psHeads1, Prove psPreds1, Reduction psConss1 RedHow_ByPredSeqUnpack [psHeads1, psPreds1]]
        rlUnpackNilPrv  = [Prove psNils1]  ==> [Reduction psNils1 RedHow_ByPredSeqUnpack []]
        rlPrvByAssume   = [Prove eqT1T2s1, Assume eqT1T2s2] ==> [Reduction eqT1T2s1 RedHow_ByEqFromAssume []] |> [IsVisibleInScope sc2 sc1]  -- dirty hack: generated assumptions by chr are not added to the graph, so made a reduction instead
        rlPrvByIdentity = [Prove eqT1T2s1] ==> [Reduction eqT1T2s1 RedHow_ByEqIdentity []] |> [EqualModuloUnification ty1 ty2]
        
%%]]
        -- inclSc       = ehcCfgCHRInclScope $ feEHCOpts $ fiEnv env
%%]

%%[(9 hmtyinfer) export(mkScopedCHR2)
mkScopedCHR2
  :: FIIn -> [CHRClassDecl Pred RedHowAnnotation] -> [CHRScopedInstanceDecl Pred RedHowAnnotation PredScope]
       -> ScopedPredStore -> (ScopedPredStore,ScopedPredStore)
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
                     where mkC = fst . predCanonic env
%%]

%%[(9 hmtyinfer)
mkClassSimplChrs :: FIIn -> ScopedPredStore -> CHRClassDecl Pred RedHowAnnotation -> ScopedPredStore
mkClassSimplChrs env rules (context, head, infos)
  = simps
  where simps        = chrStoreFromElems $ mapTrans (Set.fromList [head1]) [] head1 (zip infos (map (\p -> Red_Pred $ mkCHRPredOcc p sc1) context))
        superClasses = chrSolve env rules (map (\p -> Assume $ mkCHRPredOcc p sc1) context)
        graph        = mkRedGraphFromReductions superClasses
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
                superRule  = [Prove head1, Prove p] ==> reds'
                scopeRule1 = [Prove head1, Prove super2] 
                               ==> [Prove head3, Reduction head1 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "sup1")) [head3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                scopeRule2 = [Prove head2, Prove super1] 
                               ==> [Prove super3, Reduction super1 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "sup2")) [super3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                reds'      = Reduction p info [par] : reds
                rules      = mapTrans (Set.insert p done) reds' p (predecessors graph pr)

        opts          = feEHCOpts $ fiEnv env

mkScopedChrs :: [CHRClassDecl Pred RedHowAnnotation] -> [CHRScopedInstanceDecl Pred RedHowAnnotation PredScope] -> (MkResN,MkResN)
mkScopedChrs clsDecls insts
  = ((chrStoreUnions assumeStores,assumePredOccs), instChrs)
  where (assumeStores,assumePredOccs) = unzip $ mapMaybe mkAssumeChrs clsDecls 
        instChrs   = mkInstanceChrs insts

mkAssumeChrs :: CHRClassDecl Pred RedHowAnnotation -> Maybe MkRes1
mkAssumeChrs ([]     ,  _  , _    ) = Nothing
mkAssumeChrs (context, head, infos) =
  let prThis = mkCHRPredOcc head sc1
      super prSuper info = [Assume prSuper, Reduction prSuper info [prThis]]
      prSuper = map (\c -> mkCHRPredOcc c sc1) context
  in  Just ( chrStoreSingletonElem $ [Assume prThis] ==> concat (zipWith super prSuper infos)
           , (prSuper,prThis)
           )

mkInstanceChrs :: [CHRScopedInstanceDecl Pred RedHowAnnotation PredScope] -> MkResN
mkInstanceChrs insts
  = (chrStoreUnions instStores,instChrs)
  where (instStores,instChrs) = unzip $ map mkInstanceChr insts

mkInstanceChr :: CHRScopedInstanceDecl Pred RedHowAnnotation PredScope -> MkRes1
mkInstanceChr (context, hd, i, s)
  = ( chrStoreSingletonElem
      $ [Prove constraint]
          ==> Reduction constraint i body : map Prove body
            |> [s `IsVisibleInScope` sc1]
    , (body,constraint)
    )
  where constraint = mkCHRPredOcc hd sc1
        body = map (\p -> mkCHRPredOcc p sc1) context
%%]
  where superClasses = (\(w,d,t) -> trp "XX" (ppCHRStore' rules >-< context >#< ":" >#< (w++d) >-< ppSolveTrace t) (w++d)) $ chrSolve' env rules (map (\p -> Assume $ mkCHRPredOcc p sc1) context)

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplification result which can be used as the starting point for further simplification
%%% with additional Constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(SimplifyResult(..),emptySimplifyResult)
data SimplifyResult p i g s
  = SimplifyResult
      { simpresSolveState		:: SolveState p i g s
      , simpresRedGraph			:: RedGraph p i

      -- for debugging only:
      , simpresRedAlts			:: [HeurAlts p i]
      , simpresRedTrees			:: [[(i, Evidence p i)]]
      , simpresRedGraphs		:: [(String,RedGraph p i)]
      }

emptySimplifyResult :: Ord p => SimplifyResult p i g s
emptySimplifyResult
  = SimplifyResult
      emptySolveState emptyRedGraph
      [] [] []
%%]

%%[(9 hmtyinfer) export(simplifyResultResetForAdditionalWork)
simplifyResultResetForAdditionalWork :: Ord p => SimplifyResult p i g s -> SimplifyResult p i g s
simplifyResultResetForAdditionalWork r = r {simpresRedGraph = emptyRedGraph}
%%]
simplifyResultResetForAdditionalWork r = r {simpresSolveState = solveStateResetDone $ simpresSolveState r}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evidence construction from Constraint reduction graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer)
mkEvidence
  :: ( Ord p, Ord i
     , PP i, PP p -- for debugging
     ) => Heuristic p i -> ConstraintToInfoMap p i -> RedGraph p i
          -> ( ConstraintToInfoMap p i,InfoToEvidenceMap p i
             , [Err]
             , [(HeurAlts p i, [(i, Evidence p i)])]		-- debug info
             )
mkEvidence heur cnstrMp redGraph
  = ( {- (cnstrMp `Map.intersection` remCnstrMp) `Map.union` -} remCnstrMp, evidMp
    , err
    , dbg -- redAlts, redTrees									-- debug info
    )
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
                             $ concatMap (\(i,t) -> zip (map Prove (evidUnresolved t)) (repeat i))
                             $ redTrees
        mk (c, infos)
          = (cnstrMpFromList $ zip (repeat c) infos,Map.empty,[],[])
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Split unresolved's into those which can be assumed via qualification, and the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Unresolved predicates can be resolved by assuming them.
This can be done for those predicates of which evidence can be passed as a function argument,
and for which no ambiguity exists.

%%[(9 hmtyinfer) export(partitionUnresolved2AssumableAndOthers)
partitionUnresolved2AssumableAndOthers :: CHRPredOccCnstrMp -> ([(CHRPredOcc,PredScope)],CHRPredOccCnstrMp)
partitionUnresolved2AssumableAndOthers unresCnstrMp
  = (unres,cannotResCnstrMp)
  where (unresCnstrMp',cannotResCnstrMp)
                      = Map.partitionWithKey canAssume unresCnstrMp
                      where canAssume (Prove p) _ = Map.null $ Map.filter (tvCatIsFixed . tvinfoCateg) $ tyFtvMp $ predTy $ cpoPr p
                            canAssume _         _ = True
        unres         = [ (p,sc) | (Prove p,sc) <- shareUnresolvedAssumptionsByScope (Map.keys unresCnstrMp') ]
%%]

Find assume's wich have a common scope prefix, then share these.
Assumption: we will never share outer scopes because we only get passed inner scopes, because these will be abstracted over in bindings of a let expression.

%%[(9 hmtyinfer)
shareUnresolvedAssumptionsByScope :: [Constraint CHRPredOcc info] -> AssocL (Constraint CHRPredOcc info) PredScope
shareUnresolvedAssumptionsByScope unres
  = [ ( c
      , foldr1 (\s1 s2 -> panicJust "shareUnresolvedAssumptionsByScope" $ pscpCommon s1 s2)
               [ cpoScope $ cnstrPred c | c <- cs ]
      )
    | cs@(c:_) <- groupSortOn (cpoPr . cnstrPred) unres
    ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Make assumptions (Assume) from unresolved predicates
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(patchUnresolvedWithAssumption)
patchUnresolvedWithAssumption :: FIIn -> [(CHRPredOcc,PredScope)] -> CHRRedGraph -> CHRPredOccEvidMp -> (CHRPredOccCnstrMp,CHRPredOccEvidMp)
patchUnresolvedWithAssumption env unres redGraph evidMp
  = ( cnstrMpFromList assumeCnstrs
    , evidMpSubst (\p -> Map.lookup p assumeSubstMp) evidMp
    )
  where us = mkNewLevUIDL (length unres) $ fiUniq env
        assumeCnstrs  = [ rngLift emptyRange mkAssumeConstraint (cpoPr p) u sc | ((p,sc),u) <- zip unres us ]
        assumeSubstMp = Map.fromList [ (p,Evid_Proof p info []) | (Assume p,info) <- assumeCnstrs ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplify: solving part, yielding a RedGraph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(chrSimplifySolveToRedGraph)
chrSimplifySolveToRedGraph
  :: ( Ord p, Ord i
     , CHRMatchable FIIn p s, CHRCheckable FIIn g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable i tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , PP g, PP i, PP p -- for debugging
     ) => FIIn -> CHRStore p i g s -> ConstraintToInfoMap p i -> ConstraintToInfoMap p i
          -> SimplifyResult p i g s
          -> ( ConstraintToInfoMap p i
             , SimplifyResult p i g s
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
        solveState = chrSolve'' (env {fiUniq = u1}) chrStore (Map.keys $ cnstrInfoMp `Map.difference` cnstrInfoMpPrev) (simpresSolveState prevRes)
        cnstrInfoMpAll = cnstrMpUnion cnstrInfoMp cnstrInfoMpPrev
        redGraph
          = addToRedGraphFromReductions (chrSolveStateDoneConstraints solveState)
            $ addToRedGraphFromAssumes cnstrInfoMpAll
            $ simpresRedGraph prevRes
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplify: evidence, yielding evidence mappings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(9 hmtyinfer) export(chrSimplifyRedGraphToEvidence)
chrSimplifyRedGraphToEvidence
  :: ( Ord p, Ord i
     , PP g, PP i, PP p -- for debugging
     ) => Heuristic p i -> ConstraintToInfoMap p i
          -> SimplifyResult p i g s
          -> ( ( ConstraintToInfoMap p i, InfoToEvidenceMap p i, [Err] )
             , SimplifyResult p i g s
             )
chrSimplifyRedGraphToEvidence heur cnstrInfoMpAll simpRes
  = ( (chrSolveRemCnstrMp,chrSolveEvidMp,chrSolveErrs)
    , simpRes {simpresRedAlts = dbg1, simpresRedTrees = dbg2}
    )
  where (chrSolveRemCnstrMp,chrSolveEvidMp,chrSolveErrs,dbg)
          = mkEvidence heur cnstrInfoMpAll (simpresRedGraph simpRes)
        (dbg1,dbg2) = unzip dbg
%%]

