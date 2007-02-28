%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expansion to Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

Conversion from Pred to CHR.

%%[9 module {%{EH}Pred.ToCHR} import({%{EH}Base.Opts},{%{EH}Base.Common},{%{EH}Ty},{%{EH}Error},{%{EH}Cnstr})
%%]

%%[9 import(Data.Maybe,qualified Data.Map as Map)
%%]

%%[9 import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Solve})
%%]

%%[9 import({%{EH}Pred.CHR},{%{EH}Pred.Heuristics},{%{EH}Pred.Evidence},{%{EH}Pred.RedGraph})
%%]

%%[9 import({%{EH}Ty.FitsIn})
%%]

-- debug
%%[9 import(UU.Pretty,EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Rule store
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ScopedPredStore)
type PredStore p g s info = CHRStore p info g s
type ScopedPredStore = PredStore CHRPredOcc Guard Cnstr RedHowAnnotation
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
 ,[poi1,poi2,poi3]
 ,[pr1,pr2,pr3]
 )
  = ( map PredScope_Var [u1,u2,u3]
    , map PredOccId_Var [u4,u5,u6]
    , map Pred_Var [u7,u8,u9]
    )
  where [u1,u2,u3,u4,u5,u6,u7,u8,u9] = mkNewLevUIDL 9 uidStart
%%]

%%[9 export(initScopedPredStore)
initScopedPredStore :: ScopedPredStore
initScopedPredStore
  = chrStoreFromElems [scopeProve,scopeAssum]
  where  p1s1         = mkCHRPredOcc pr1 sc1
         p1s2         = mkCHRPredOcc pr1 sc2
         p1s3         = mkCHRPredOcc pr1 sc3
         scopeProve   = [Prove p1s1, Prove p1s2] 
                          ==> [Reduction p1s2 RedHow_ByScope [p1s3]]
                           |> [IsStrictParentScope sc3 sc1 sc2]
         scopeAssum   = [Prove p1s1, Assume p1s2] 
                          ==> [Reduction p1s1 RedHow_ByScope [p1s2]]
                            |> [NotEqualScope sc1 sc2,IsVisibleInScope sc2 sc1]
%%]
         scopeProve   = [Prove p1s1, Prove p1s2] 
                          ==> [Prove p1s3, Reduction p1s1 RedHow_ByScope [p1s3]]
                           |> [HasStrictCommonScope sc3 sc1 sc2]
         scopeAssum   = [Prove p1s1, Assume p1s2] 
                          ==> [Reduction p1s1 RedHow_ByScope [p1s3]]
                            |> [HasStrictCommonScope sc3 sc1 sc2]

%%[9 export(mkScopedCHR2)
mkScopedCHR2 :: FIIn -> [CHRClassDecl Pred RedHowAnnotation] -> [CHRScopedInstanceDecl Pred RedHowAnnotation PredScope] -> ScopedPredStore
mkScopedCHR2 env clsDecls insts
  = chrStoreUnions $ [assumeStore,instStore] ++ simplStores
  where  ucls = mkNewLevUIDL (length clsDecls) $ fiUniq env
         ((assumeStore,assumePredOccs), (instStore,_)) = mkScopedChrs clsDecls insts
         simplStores  = zipWith (\u (cx,h,i) -> mkClassSimplChrs (env {fiUniq = u}) assumeStore (cx,h,i)) ucls clsDecls

mkClassSimplChrs :: FIIn -> ScopedPredStore -> CHRClassDecl Pred RedHowAnnotation -> ScopedPredStore
mkClassSimplChrs env rules (context, head, infos)
  = chrStoreFromElems $ mapTrans [] head1 (zip infos (map (\p -> Red_Pred $ mkCHRPredOcc p sc1) context))
  where superClasses = chrSolve env rules (map (\p -> Assume $ mkCHRPredOcc p sc1) context)
        graph        = mkRedGraphFromReductions superClasses
        head1        = mkCHRPredOcc head sc1
        head2        = mkCHRPredOcc head sc2
        head3        = mkCHRPredOcc head sc3
        byScInfo     = RedHow_ByScope
    
        mapTrans reds subClass = concatMap (transClosure reds subClass)
    
        transClosure reds par (info, pr@(Red_Pred p@(CHRPredOcc {cpoPr = super})))
          = superRule : scopeRule1 : scopeRule2 : rules
          where super1     = mkCHRPredOcc super sc1
                super2     = mkCHRPredOcc super sc2
                super3     = mkCHRPredOcc super sc3
                superRule  = [Prove head1, Prove p] ==> reds'
                scopeRule1 = [Prove head1, Prove super2] 
                               ==> [Prove head3, Reduction head1 byScInfo [head3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                scopeRule2 = [Prove head2, Prove super1] 
                               ==> [Prove super3, Reduction super1 byScInfo [super3]]
                                 |> [HasStrictCommonScope sc3 sc1 sc2]
                reds'      = Reduction p info [par] : reds
                rules      = mapTrans reds' p (predecessors graph pr)             

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
patchUnresolvedWithAssumption :: FIIn -> CHRPredOccCnstrMp -> CHRPredOccEvidMp -> (CHRPredOccCnstrMp,CHRPredOccEvidMp)
patchUnresolvedWithAssumption env unresCnstrMp evidMp
  = (cnstrMpFromList assumeCnstrs, evidMpSubst (\p -> Map.lookup p assumeSubstMp) evidMp)
  where us = mkNewLevUIDL (Map.size unresCnstrMp) $ fiUniq env
        assumeCnstrs  = concat $ zipWith mk (Map.toList unresCnstrMp) us
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
     , CHRMatchable FIIn p s, CHRCheckable g s
     , CHRSubstitutable s tvar s, CHRSubstitutable g tvar s, CHRSubstitutable p tvar s
     , CHREmptySubstitution s
     , PP g, PP i, PP p -- for debugging
     ) => FIIn -> CHRStore p i g s -> Heuristic p i -> ConstraintToInfoMap p i
          -> ((ConstraintToInfoMap p i,InfoToEvidenceMap p i,[Err]),SolveState p i g s,RedGraph p i)
chrSimplifyToEvidence env chrStore heur cnstrInfoMp
  = (mkEvidence heur cnstrInfoMp redGraph,solveState,redGraph)
  where (_,u1,u2) = mkNewLevUID2 $ fiUniq env
        solveState = chrSolve'' (env {fiUniq = u1}) chrStore $ Map.keys cnstrInfoMp
        redGraph
          = addToRedGraphFromReductions (chrSolveStateDoneConstraints solveState)
            $ mkRedGraphFromAssumes cnstrInfoMp
%%]
  = (mkEvidence heur cnstrInfoMp $ trp "ZZ" (ppRedGraph redGraph) $ redGraph,solveState)

