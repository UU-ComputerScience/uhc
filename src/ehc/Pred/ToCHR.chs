%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expansion to Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

Conversion from Pred to CHR.

%%[9 module {%{EH}Pred.ToCHR} import({%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Ty.Ftv},{%{EH}Error},{%{EH}VarMp})
%%]

%%[9 import(Data.Maybe,qualified Data.Set as Set,qualified Data.Map as Map)
%%]

%%[9 import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Solve})
%%]

%%[9 import({%{EH}Pred.CHR},{%{EH}Pred.Heuristics},{%{EH}Pred.Evidence},{%{EH}Pred.RedGraph})
%%]

%%[9 import({%{EH}Ty.FitsIn})
%%]

%%[9 import({%{EH}Ty.Trf.Canonic})
%%]

-- debug
%%[9 import(EH.Util.Pretty,EH.Util.Utils)
%%]

%%[13 import({%{EH}Base.Builtin})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ScopedPredStore,ScopedPredCHR)
type PredStore p g s info = CHRStore p info g s
type ScopedPredStore = PredStore CHRPredOcc Guard VarMp RedHowAnnotation
type ScopedPredCHR   = CHR (Constraint CHRPredOcc RedHowAnnotation) Guard VarMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intermediate structures for constructing CHR variants of class/instance decl
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRClassDecl,CHRScopedInstanceDecl)
type CHRClassDecl           a info      = ([a], a, [info])
type CHRInstanceDecl        a info      = ([a], a, info)
type CHRScopedInstanceDecl  a info sc   = ([a], a, info, sc)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Info to Evidence map for CHRPredOcc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(CHRPredOccEvidMp)
type CHRPredOccEvidMp = InfoToEvidenceMap CHRPredOcc RedHowAnnotation
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conversion/expansion into CHR
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
type MkRes1 =  (ScopedPredStore,([CHRPredOcc],CHRPredOcc))
type MkResN = (ScopedPredStore,[([CHRPredOcc],CHRPredOcc)])
%%]

Variables used in CHR's are implicitly universally quantified for each constraint,
they will match against a concrete constraint and then bound the info found therein.
Hence we can safely use non-unique variables.

%%[9
([sc1,sc2,sc3]
 ,[pr1,pr2,pr3]
%%[[10
 ,[ty1,ty2]
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
    , map mkTyVar [u10,u11]
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
  where [u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13] = mkNewLevUIDL 13 uidStart
%%]]
%%]

%%[9 export(initScopedPredStore)
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
  where p1s1         = mkCHRPredOcc pr1 sc1
        p1s2         = mkCHRPredOcc pr1 sc2
        p1s3         = mkCHRPredOcc pr1 sc3
        scopeProve   = [Prove p1s1, Prove p1s2] 
                         ==> [Reduction p1s2 RedHow_ByScope [p1s3]]
                          |> [IsStrictParentScope sc3 sc1 sc2]
{-
        scopeAssum1  = [Prove p1s1, Assume p1s2] 
                         ==> [Reduction p1s1 (RedHow_Assumption sc2) []]
                          |> [EqualScope sc1 sc2]
-}
        scopeAssum2  = [Prove p1s1, Assume p1s2] 
                         ==> [Reduction p1s1 RedHow_ByScope [p1s2]]
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
        f1s1         = mkCHRPredOcc (tyPred $ mkTyQu TyQu_Forall [pr1v] $ mkTyPr pr1) sc1
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
        -- inclSc       = ehcCfgCHRInclScope $ feEHCOpts $ fiEnv env
%%]

%%[9 export(mkScopedCHR2)
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
         canonInsts  = [ (map mkC cx,mkC hd,info,sc) | (cx,hd,info,sc) <- insts ]
                     where mkC = predCanonic env
%%]

%%[9
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
          = [superRule] ++ (if ehcCfgCHRScoped opts >= CHRScopedMutualSuper then [scopeRule1, scopeRule2] else []) ++ rules
          where super1     = mkCHRPredOcc super sc1
                super2     = mkCHRPredOcc super sc2
                super3     = mkCHRPredOcc super sc3
                superRule  = [Prove head1, Prove p] ==> reds'
                scopeRule1 = [Prove head1, Prove super2] 
                               ==> [Prove head3, Reduction head1 RedHow_ByScope [head3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                scopeRule2 = [Prove head2, Prove super1] 
                               ==> [Prove super3, Reduction super1 RedHow_ByScope [super3]]
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

%%[9 export(SimplifyResult(..),emptySimplifyResult)
data SimplifyResult p i g s
  = SimplifyResult
      { simpresSolveState		:: SolveState p i g s
      , simpresRedGraph			:: RedGraph p i
      }

emptySimplifyResult :: Ord p => SimplifyResult p i g s
emptySimplifyResult = SimplifyResult emptySolveState emptyRedGraph
%%]

%%[9 export(simplifyResultResetForAdditionalWork)
simplifyResultResetForAdditionalWork :: Ord p => SimplifyResult p i g s -> SimplifyResult p i g s
simplifyResultResetForAdditionalWork r = r {simpresRedGraph = emptyRedGraph}
%%]
simplifyResultResetForAdditionalWork r = r {simpresSolveState = solveStateResetDone $ simpresSolveState r}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evidence construction from Constraint reduction graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9
mkEvidence
  :: ( Ord p, Ord i
     , PP i, PP p -- for debugging
     ) => Heuristic p i -> ConstraintToInfoMap p i -> RedGraph p i
          -> (ConstraintToInfoMap p i,InfoToEvidenceMap p i,[Err])
mkEvidence heur cnstrMp redGraph
  = ( (cnstrMp `Map.intersection` remCnstrMp) `Map.union` remCnstrMp
    , evidMp
    , err
    )
  where (remCnstrMp,evidMp,err)
          = foldl (\(cm,em,err) c -> let (cm',em',err') = mk c in (cnstrMpUnion cm' cm, evidMpUnion em' em, err' ++ err))
                  (Map.empty,Map.empty,[])
            $ Map.toList cnstrMp
        mk (Prove p, infos)
          = (remCnstrMp,evidMp,[])
          where redTrees   = heur infos $ redAlternatives redGraph p
                evidMp     = foldr (uncurry evidMpInsert) Map.empty redTrees
                remCnstrMp = cnstrMpFromList
                             $ concatMap (\(i,t) -> zip (map Prove (evidUnresolved t)) (repeat i))
                             $ redTrees
        mk (c, infos)
          = (cnstrMpFromList $ zip (repeat c) infos,Map.empty,[])
%%]
          where (redTrees,err) = heur infos $ (\v -> trp "XX" (p >#< ":" >#< v) v) $ redAlternatives redGraph p

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Evidence construction from Constraint reduction graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(patchUnresolvedWithAssumption)
patchUnresolvedWithAssumption :: FIIn -> CHRPredOccCnstrMp -> CHRPredOccEvidMp -> (CHRPredOccCnstrMp,CHRPredOccEvidMp,CHRPredOccCnstrMp)
patchUnresolvedWithAssumption env unresCnstrMp evidMp
  = (cnstrMpFromList assumeCnstrs, evidMpSubst (\p -> Map.lookup p assumeSubstMp) evidMp, cannotResCnstrMp)
  where us = mkNewLevUIDL (Map.size unresCnstrMp) $ fiUniq env
        (unresCnstrMp',cannotResCnstrMp)
                      = Map.partitionWithKey canRes unresCnstrMp
                      where canRes (Prove p) _ = Map.null $ Map.filter tvCatIsFixed $ tyFtvMp $ predTy $ cpoPr p
                            canRes _         _ = True
        assumeCnstrs  = concat $ zipWith mk (Map.toList unresCnstrMp') us
                      where mk (Prove p,_) u = [mkAssumeConstraint (cpoPr p) u (cpoScope p)]
                            mk _           _ = []
        assumeSubstMp = Map.fromList [ (p,Evid_Proof p info []) | (Assume p,info) <- assumeCnstrs ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Simplify (including solving):
%%% construction of RedGraph, followed by evidence, using some (currently fixed) heuristic
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(chrSimplifyToEvidence)
chrSimplifyToEvidence
  :: ( Ord p, Ord i
     , CHRMatchable FIIn p s, CHRCheckable FIIn g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable i tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , PP g, PP i, PP p -- for debugging
     ) => FIIn -> CHRStore p i g s -> Heuristic p i -> ConstraintToInfoMap p i -> ConstraintToInfoMap p i
          -> SimplifyResult p i g s
          -> ((ConstraintToInfoMap p i,InfoToEvidenceMap p i,[Err]),SimplifyResult p i g s)
chrSimplifyToEvidence env chrStore heur cnstrInfoMpPrev cnstrInfoMp prevRes
  = (mkEvidence heur cnstrInfoMpAll redGraph,SimplifyResult solveState redGraph)
  where (_,u1,u2) = mkNewLevUID2 $ fiUniq env
        solveState = chrSolve'' (env {fiUniq = u1}) chrStore (Map.keys $ cnstrInfoMp `Map.difference` cnstrInfoMpPrev) (simpresSolveState prevRes)
        cnstrInfoMpAll = cnstrMpUnion cnstrInfoMp cnstrInfoMpPrev
        redGraph
          = addToRedGraphFromReductions (chrSolveStateDoneConstraints solveState)
            $ addToRedGraphFromAssumes cnstrInfoMpAll
            $ simpresRedGraph prevRes
%%]
  = (mkEvidence heur cnstrInfoMpAll redGraph,SimplifyResult solveState redGraph)
            $ addToRedGraphFromAssumes cnstrInfoMpAll
        solveState = chrSolve'' (env {fiUniq = u1}) chrStore (Map.keys $ cnstrInfoMp `Map.difference` cnstrInfoMpPrev) (simpresSolveState prevRes)
  = (mkEvidence heur cnstrInfoMp $ trp "ZZ" (ppRedGraph redGraph) $ redGraph,solveState)


chrSimplifyToEvidence env chrStore heur cnstrInfoMpPrev cnstrInfoMp prevRes
  = (mkEvidence heur cnstrInfoMp redGraph,SimplifyResult solveState redGraph)
  where (_,u1,u2) = mkNewLevUID2 $ fiUniq env
        solveState = chrSolve'' (env {fiUniq = u1}) chrStore (Map.keys $ cnstrInfoMp) (simpresSolveState prevRes)
        -- cnstrInfoMpAll = cnstrMpUnion cnstrInfoMp cnstrInfoMpPrev
        redGraph
          = addToRedGraphFromReductions (chrSolveStateDoneConstraints solveState)
            $ addToRedGraphFromAssumes cnstrInfoMp
            $ simpresRedGraph prevRes
