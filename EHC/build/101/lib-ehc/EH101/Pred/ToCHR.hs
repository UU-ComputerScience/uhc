module EH101.Pred.ToCHR
( ScopedPredStore, ScopedPredCHR, ScopedPredStoreL
, CHRRedGraph
, CHRClassDecl, CHRScopedInstanceDecl
, CHRPredOccEvidMp
, initScopedPredStore
, mkScopedCHR2
, SimplifyResult (..), emptySimplifyResult
, simplifyResultResetForAdditionalWork
, partitionUnresolved2AssumableAndOthers
, patchUnresolvedWithAssumption
, chrSimplifySolveToRedGraph
, chrSimplifyRedGraphToEvidence )
where
import EH101.Opts
import EH101.Base.Common
import EH101.Ty
import EH101.Ty.Ftv
import EH101.Error
import EH101.VarMp
import EH101.Substitutable
import Data.Maybe
import qualified Data.Set as Set
import qualified Data.Map as Map
import EH101.CHR
import EH101.CHR.Constraint
import EH101.CHR.Solve
import EH101.Pred.CHR
import EH101.Pred.Heuristics
import EH101.Pred.Evidence
import EH101.Pred.RedGraph
import EH101.Ty.FitsInCommon2
import EH101.Ty.Trf.Canonic
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Builtin


{-# LINE 36 "src/ehc/Pred/ToCHR.chs" #-}
type PredStore  p g s info = CHRStore p info g s
type PredStoreL p g s info = [CHR (Constraint p info) g s]
type ScopedPredStore  = PredStore  CHRPredOcc Guard VarMp RedHowAnnotation
type ScopedPredStoreL = PredStoreL CHRPredOcc Guard VarMp RedHowAnnotation
type ScopedPredCHR    = CHR CHRConstraint Guard VarMp

{-# LINE 48 "src/ehc/Pred/ToCHR.chs" #-}
type CHRRedGraph = RedGraph CHRPredOcc RedHowAnnotation

{-# LINE 56 "src/ehc/Pred/ToCHR.chs" #-}
type CHRClassDecl           a info      = ([a], a, [info])
type CHRInstanceDecl        a info      = ([a], a, info)
type CHRScopedInstanceDecl  a info sc   = ([a], a, info, sc)

{-# LINE 66 "src/ehc/Pred/ToCHR.chs" #-}
type CHRPredOccEvidMp = InfoToEvidenceMap CHRPredOcc RedHowAnnotation

{-# LINE 74 "src/ehc/Pred/ToCHR.chs" #-}
type MkRes1 = (ScopedPredStore, ([CHRPredOcc],CHRPredOcc) )
type MkResN = (ScopedPredStore,[([CHRPredOcc],CHRPredOcc)])

{-# LINE 83 "src/ehc/Pred/ToCHR.chs" #-}
([sc1,sc2,sc3]
 ,[pr1,pr2,pr3]
 ,[ty1,ty2,ty3,ty4]
 ,[lab1]
 ,[off1]
 ,[pr1v]
 ,[pa1]
 )
  = ( map PredScope_Var [u1,u2,u3]
    , map Pred_Var [u7,u8,u9]
    , map mkTyMetaVar {- mkTyVar -} [u10,u11,u14,u15]
    , map Label_Var [u12]
    , map LabelOffset_Var [u13]
    , [u7]
    , map PredSeq_Var [u4]
    )
  where [u1,u2,u3,u4,u5,u6,u7,u8,u9,u10,u11,u12,u13,u14,u15] = mkNewLevUIDL 15 uidStart

{-# LINE 128 "src/ehc/Pred/ToCHR.chs" #-}
-- | The basic initial set of CHRs
initScopedPredStore :: ScopedPredStore
initScopedPredStore
  = chrStoreFromElems $
      [ scopeProve, {- scopeAssum1, -} scopeAssum2 ]
      ++ [ labelProve1, labelProve2 ]
      ++ [ instForall, predArrow, predSeq1, predSeq2 ]
  where p1s1         = mkCHRPredOcc pr1 sc1
        p1s2         = mkCHRPredOcc pr1 sc2
        p1s3         = mkCHRPredOcc pr1 sc3
        scopeProve   = [Prove p1s1, Prove p1s2]
                         ==> [mkReduction p1s2 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "prv")) [p1s3]]
                          |> [IsStrictParentScope sc3 sc1 sc2]
{-
        scopeAssum1  = [Prove p1s1, Assume p1s2]
                         ==> [mkReduction p1s1 (RedHow_Assumption sc2) []]
                          |> [EqualScope sc1 sc2]
-}
        scopeAssum2  = [Prove p1s1, Assume p1s2]
                         ==> [mkReduction p1s1 (RedHow_ByScope ByScopeRedHow_Assume) [p1s2]]
                          |> [NotEqualScope sc1 sc2,IsVisibleInScope sc2 sc1]
        l1s1         = mkCHRPredOcc (Pred_Lacks ty1 lab1) sc1
        l2s1         = mkCHRPredOcc (Pred_Lacks ty2 lab1) sc1
        l3s1         = mkCHRPredOcc (Pred_Lacks tyRowEmpty lab1) sc1
        labelProve1  = [Prove l1s1]
                         ==> [Prove l2s1, mkReduction l1s1 (RedHow_ByLabel lab1 off1 sc1) [l2s1]]
                          |> [NonEmptyRowLacksLabel ty2 off1 ty1 lab1]
        labelProve2  = [Prove l3s1]
                         ==> [mkReduction l3s1 (RedHow_ByLabel lab1 (LabelOffset_Off 0) sc1) []]
        f1s1         = mkCHRPredOcc (tyPred $ mkTyQu tyQu_Forall [(pr1v,kiStar)] $ mkTyPr pr1) sc1	-- TBD
        f2s1         = mkCHRPredOcc pr1 sc1
        instForall   = [Assume f1s1]
                         ==> [Assume f2s1]
        a1s1         = mkCHRPredOcc (Pred_Arrow pa1 pr1) sc1
        a2s1         = mkCHRPredOcc pr1 sc1
        a3s1         = mkCHRPredOcc (Pred_Preds pa1) sc1
        predArrow    = [Assume a1s1, Prove a2s1]
                         ==> [Prove a3s1, mkReduction a2s1 (RedHow_ByInstance hsnUnknown pr1 sc1) [a1s1,a3s1]]
        s1s1         = mkCHRPredOcc (Pred_Preds (PredSeq_Cons pr1 pa1)) sc1
        s2s1         = mkCHRPredOcc pr1 sc1
        s3s1         = mkCHRPredOcc (Pred_Preds pa1) sc1
        predSeq1     = [Prove s1s1]
                         <==> [Prove s2s1, Prove s3s1]
        predSeq2     = [Prove $ mkCHRPredOcc (Pred_Preds PredSeq_Nil) sc1]
                         <==> []
        -- inclSc       = ehcCfgCHRInclScope $ feEHCOpts $ fiEnv env

{-# LINE 217 "src/ehc/Pred/ToCHR.chs" #-}
-- | Construct CHRs from class and instance decls
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
                     where mkC = fst . predCanonic (emptyTyBetaRedEnv {tbredFI=env})

{-# LINE 235 "src/ehc/Pred/ToCHR.chs" #-}
-- | Construct simplification CHRs from class decls, building upon a given CHR store
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
                               ==> [Prove head3, mkReduction head1 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "sup1")) [head3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                scopeRule2 = [Prove head2, Prove super1]
                               ==> [Prove super3, mkReduction super1 (RedHow_ByScope (ByScopeRedHow_Other $ AlwaysEq "sup2")) [super3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                reds'      = mkReduction p info [par] : reds
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
      super prSuper info = [Assume prSuper, mkReduction prSuper info [prThis]]
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
          ==> mkReduction constraint i body : map Prove body
            |> [s `IsVisibleInScope` sc1]
    , (body,constraint)
    )
  where constraint = mkCHRPredOcc hd sc1
        body = map (\p -> mkCHRPredOcc p sc1) context

{-# LINE 307 "src/ehc/Pred/ToCHR.chs" #-}
data SimplifyResult p i g s
  = SimplifyResult
      { simpresSolveState		:: SolveState p i g s
      , simpresRedGraph			:: RedGraph p i

      -- for debugging only:
      , simpresRedAlts			:: [HeurAlts p i]
      , simpresRedTrees			:: [[(i, Evidence p i)]]
      , simpresRedGraphs		:: [(String,RedGraph p i)]
      , simpresRemPredL         :: [p]							-- remaining pred occurrences, which cannot be proven, as a list
      }

emptySimplifyResult :: Ord p => SimplifyResult p i g s
emptySimplifyResult
  = SimplifyResult
      emptySolveState emptyRedGraph
      [] [] [] []

{-# LINE 327 "src/ehc/Pred/ToCHR.chs" #-}
simplifyResultResetForAdditionalWork :: Ord p => SimplifyResult p i g s -> SimplifyResult p i g s
simplifyResultResetForAdditionalWork r = r {simpresRedGraph = emptyRedGraph}

{-# LINE 337 "src/ehc/Pred/ToCHR.chs" #-}
mkEvidence
  :: ( Ord p, Ord i
     ) => Heuristic p i -> ConstraintToInfoMap p i -> RedGraph p i
          -> ( -- ConstraintToInfoMap p i						-- remaining constraints
               ConstraintToInfoTraceMp p i						-- remaining constraints
             , InfoToEvidenceMap p i							-- mapping to evidence
             , [Err]											-- errors
             )
mkEvidence heur cnstrMp redGraph
  = ( {- (cnstrMp `Map.intersection` remCnstrMp) `Map.union` -}
      cnstrTraceMpFromList remCnstrMp
    , evidMp
    , err
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

{-# LINE 400 "src/ehc/Pred/ToCHR.chs" #-}
partitionUnresolved2AssumableAndOthers :: CHRPredOccCnstrTraceMp -> ([CHRIntermediateUntilAssume],CHRPredOccCnstrTraceMp)
partitionUnresolved2AssumableAndOthers unresCnstrMp
  = (unres,cannotResCnstrMp)
  where (unresCnstrMp',cannotResCnstrMp)
                      = Map.partitionWithKey canAssume unresCnstrMp
                      where -- if p only ranges over non-fixed tvars, we potentially can assume them (if not found ambiguous later)
                            canAssume (Prove p) _ = Map.null $ Map.filter (tvCatIsFixed . tvinfoCateg) $ tyFtvMp $ predTy $ cpoPr p
                            canAssume _         _ = True
        unres         = [ (p,x) | (Prove p,x) <- shareUnresolvedAssumptionsByScope (unresCnstrMp') ]

{-# LINE 412 "src/ehc/Pred/ToCHR.chs" #-}
-- | Group unresolved constraints, reducing the various scopes to the outermost scope.
--   Find assume's wich have a common scope prefix, then share these.
--   Assumption: we will never share outer scopes because we only get passed inner scopes, because these will be abstracted over in bindings of a let expression.
shareUnresolvedAssumptionsByScope :: CHRPredOccCnstrTraceMp -> [(CHRConstraint,(PredScope,CHRPredOccCnstrTraceMp))]
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

{-# LINE 436 "src/ehc/Pred/ToCHR.chs" #-}
-- | Transform unresolved Prove constraints to Assume variants, used either for quantification over, or for error messages about unresolved predicates
patchUnresolvedWithAssumption :: FIIn -> [CHRIntermediateUntilAssume] -> CHRRedGraph -> CHRPredOccEvidMp -> (CHRPredOccCnstrTraceMp,CHRPredOccEvidMp)
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

{-# LINE 458 "src/ehc/Pred/ToCHR.chs" #-}
chrSimplifySolveToRedGraph
  :: ( Ord p, Ord i
     , CHRMatchable FIIn p s, CHRCheckable FIIn g s
     , VarLookupCmb s s
     , VarUpdatable s s, VarUpdatable g s, VarUpdatable i s, VarUpdatable p s
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

{-# LINE 492 "src/ehc/Pred/ToCHR.chs" #-}
chrSimplifyRedGraphToEvidence
  :: ( Ord p, Ord i
     ) => Heuristic p i -> ConstraintToInfoMap p i
          -> SimplifyResult p i g s
          -> ( ( ConstraintToInfoTraceMp p i, InfoToEvidenceMap p i, [Err] )
             , SimplifyResult p i g s
             )
chrSimplifyRedGraphToEvidence heur cnstrInfoMpAll simpRes
  = ( (chrSolveRemCnstrMp,chrSolveEvidMp,chrSolveErrs)
    , simpRes
    )
  where ( chrSolveRemCnstrMp,chrSolveEvidMp,chrSolveErrs
         )
          = mkEvidence heur cnstrInfoMpAll (simpresRedGraph simpRes)

