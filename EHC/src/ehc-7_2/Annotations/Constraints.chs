%%[1 hs module {%{EH}Annotations.Constraints}
%%]

%%[7_2 import({%{EH}Base.Common}, {%{EH}Base.Builtin}, {%{EH}Ty}, {%{EH}Ty.AnnCommon}, qualified Data.Map as Map, Data.Map(Map), Data.Maybe, qualified Data.Set as Set, Data.Set(Set), Data.Tree(Tree(..)), EH.Util.Pretty, {%{EH}Ty.Pretty})
%%]

%%[7_2 import(Data.List(transpose, nub, nubBy, intersect, partition), Debug.Trace(trace), Data.Graph, qualified Data.Graph.Inductive as G, Control.Monad.State)
%%]

%%[7_2 export(AnnConstr(..), AnnComp(..), AnnConstrSet, BndgId, FlowSem(..), AnnConstrOps(..), AnnCompOps(..), Flattenable(..), FlattenOps(..), HasAnnotations(..), InstProjection(..), InstProjections, (..#), (#..))
%%]

%%[7_2 export(unwrapMap, wrappedFromList, wrappedSingleton, wrappedSetEmpty, wrappedSetUnion, unwrapSet, wrapSet, WrappedSet, WrappedAnnConstrSet)
%%]

%%[7_2 export(improveSubst, stagedPartialSolve, processConstraints, collect, distribute, hardFlowSem, softFlowSem, isUserFlow)
%%]

%%[7_2 export(composeRewriters, basicGraphRewriter, compToEqualityRewriter, rewriteSelfEdges, componentReduction, solveToBndgSubstitution)
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraints between annotations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Definition of constraints between annotations. There are three types of constraints:

  * Constraints that capture flow (Match)
  * Constraints that capture multiple occurrences (Comp)
  * Instantiation constraints to deal with polyvariance

Typical interpretations of flow constraints are equality or subtyping. Constraints that
capture occurences could be interpreted as a max or plus function. The occurrences are
represented as a tree, where the leaves are all the use-sites, the root is the
definition-site and the nodes between indicate how they occur (sequential, parallel).

The constraints can be between annotations and types. Constraints between types are
flattened to constraints between annotations before solving. For flattening to work,
the following invariant should hold:

  * Cocontravariance/belowness is the same for all annotations in a single constraint
    containing a type. Constraints between annotations do not have to satisfy this
    property.

The uniqueness inferencer generates constraints for which this invariant holds. Constraints
given by a programmer might not, but that is not a problem since a programmer is not allowed
to provide constraints involving types.

%%[7_2

type AnnConstrSet sem context = Set (AnnConstr sem context)
type BndgId = UID
data FlowSem s = HardFlow | SoftFlow | UserFlow !s deriving (Eq, Ord)

data AnnConstr sem context
  = Match    !UID !(FlowSem sem) !(Annotation Ty) !(Annotation Ty) !context
  | MatchTy  !UID !(FlowSem sem) !Ty !Ty !context
  | MatchTyAnn !UID !(FlowSem sem) !Ty !(Annotation Ty) !context
  | Comp     !UID !(AnnComp (Annotation Ty)) !(Annotation Ty) !context
  | CompTy   !UID !(AnnComp Ty) !Ty !context
  | Inst     !UID !BndgId !InstProjections !context

type InstProjections = [InstProjection]

data InstProjection
  = TyProjection !Ty !Ty
  | AnnProjection !(Annotation Ty) !(Annotation Ty)
  deriving (Eq, Ord, Show)

data AnnComp a
  = Plus !(AnnComp a) !(AnnComp a)
  | Max  !(AnnComp a) !(AnnComp a)
  | Embed !a
  deriving (Eq, Ord, Show)

instance Functor AnnComp where
  fmap f (x `Plus` y) = fmap f x `Plus` fmap f y
  fmap f (x `Max` y)  = fmap f x `Max`  fmap f y
  fmap f (Embed x)    = Embed (f x)

instance Eq (AnnConstr sem context) where
  a == b = getUID a == getUID b

instance Ord sem => Ord (AnnConstr sem context) where
  compare (Match _ semA annA1 annA2 _) (Match _ semB annB1 annB2 _)
    = compare semA semB `sequenceComp` compare annA1 annB1 `sequenceComp` compare annA2 annB2
  compare (MatchTy _ semA tyA1 tyA2 _) (MatchTy _ semB tyB1 tyB2 _)
    = compare semA semB `sequenceComp` compare tyA1 tyB1 `sequenceComp` compare tyA2 tyB2
  compare (MatchTyAnn _ semA tyA1 a2 _) (MatchTyAnn _ semB tyB1 b2 _)
    = compare semA semB `sequenceComp` compare tyA1 tyB1 `sequenceComp` compare a2 b2
  compare (Comp _ compA annA _) (Comp _ compB annB _)
    = compare compA compB `sequenceComp` compare annA annB
  compare (CompTy _ compA tyA _) (CompTy _ compB tyB _)
    = compare compA compB `sequenceComp` compare tyA tyB
  compare (Inst uA _ _ _) (Inst uB _ _ _)
    = compare uA uB
  compare (Match _ _ _ _ _) _                  = GT
  compare (MatchTy _ _ _ _ _) (MatchTyAnn _ _ _ _ _) = GT
  compare (MatchTy _ _ _ _ _) (Comp _ _ _ _)   = GT
  compare (MatchTy _ _ _ _ _) (CompTy _ _ _ _) = GT
  compare (MatchTyAnn _ _ _ _ _) (Comp _ _ _ _) = GT
  compare (MatchTyAnn _ _ _ _ _) (CompTy _ _ _ _) = GT
  compare (Comp _ _ _ _) (CompTy _ _ _ _)      = GT
  compare _ (Inst _ _ _ _)                     = GT
  compare _ _                                  = LT

sequenceComp :: Ordering -> Ordering -> Ordering
sequenceComp EQ c = c
sequenceComp c  _ = c

getUID :: AnnConstr sem context -> UID
getUID (Match uid _ _ _ _)   = uid
getUID (MatchTy uid _ _ _ _) = uid
getUID (MatchTyAnn uid _ _ _ _) = uid
getUID (Comp uid _ _ _)      = uid
getUID (CompTy uid _ _ _)    = uid
getUID (Inst uid _ _ _)      = uid

mapProjections :: (Ty -> Ty -> a) -> (Annotation Ty -> Annotation Ty -> a) -> InstProjections -> [a]
mapProjections tyF annF
  = map apply
  where
    apply (TyProjection tyFrom tyTo)    = tyF tyFrom tyTo
    apply (AnnProjection annFrom annTo) = annF annFrom annTo

partitionProjections :: InstProjections -> ([(Ty,Ty)], [(Annotation Ty, Annotation Ty)])
partitionProjections projs
  = ( [ (tyFrom, tyTo)   | p@(TyProjection tyFrom tyTo) <- projs, isTyProj p ]
    , [ (annFrom, annTo) | p@(AnnProjection annFrom annTo) <- projs, not (isTyProj p) ]
    ) 
  where
    isTyProj (TyProjection _ _) = True
    isTyProj _                  = False

%%]


Class to obtain annotations from different data types

%%[7_2

class HasAnnotations a where
  annotations :: a -> Annotations Ty
  annotations_ :: a -> [Annotation Ty]
  annotations_ = Set.toList . annotations

instance HasAnnotations Ty where
  annotations = tyAnnotations

instance HasAnnotations (Annotation Ty) where
  annotations = Set.singleton

instance HasAnnotations (AnnConstrSet sem context) where
  annotations = annotations . Set.toList

instance HasAnnotations (Map BndgId [Ty]) where
  annotations = annotations . Map.elems  

instance HasAnnotations a => HasAnnotations [a] where
  annotations = Set.unions . map annotations

instance HasAnnotations (AnnConstr sem context) where
  annotations (Match _ _ a b _)   = annotations a `Set.union` annotations b
  annotations (MatchTy _ _ a b _) = annotations a `Set.union` annotations b
  annotations (MatchTyAnn _ _ a b _) = annotations a `Set.union` annotations b
  annotations (Comp _ tree a _)   = annotations a `Set.union` annotations tree
  annotations (CompTy _ tree a _) = annotations a `Set.union` annotations tree
  annotations (Inst _ _ proj _)   = annotations proj

instance HasAnnotations InstProjection where
  annotations (TyProjection _ tyTo)   = annotations tyTo
  annotations (AnnProjection _ annTo) = Set.singleton annTo

instance HasAnnotations a => HasAnnotations (AnnComp a) where
  annotations (Embed a)  = annotations a
  annotations (Plus a b) = annotations a `Set.union` annotations b
  annotations (Max a b)  = annotations a `Set.union` annotations b

instance HasAnnotations (ConstrGraph sem) where
  annotations g
    = Set.fromList [ ann | (_, v@(Left ann)) <- G.labNodes g, either (const True) (const False) v]

%%]


Syntactic sugar to the constraint language defined above. You
can write:  a \+/ b <== c. The sugar is overloaded to work on
annotations and types.

%%[7_2

infixl 1 ..#
infixl 1 #..
infix 3 =>=
infix 3 =<=
infix 3 =>=!
infix 3 =<=!
infix 3 \-/
infix 3 \+/
infix 2 <==

(..#) :: (context -> AnnConstr sem context) -> context -> AnnConstr sem context
(#..) :: UID -> (UID -> context -> AnnConstr sem context) -> context -> AnnConstr sem context
(..#) = ($)
(#..) = flip ($)

class AnnConstrOps a where
  (=>=)  :: a -> a -> UID -> context -> AnnConstr sem context
  (=<=)  :: a -> a -> UID -> context -> AnnConstr sem context
  (<==)  :: AnnComp a -> a -> UID -> context -> AnnConstr sem context
  (=>=!) :: a -> a -> FlowSem sem -> UID -> context -> AnnConstr sem context
  (=<=!) :: a -> a -> FlowSem sem -> UID -> context -> AnnConstr sem context
  (=<=)  = flip (=>=)
  (=<=!) = flip (=>=!)

instance AnnConstrOps (Annotation Ty) where
  (=>=) a b u c = Match u hardFlowSem a b c
  (<==) t a u c = Comp u t a c
  (=>=!) a b s u c = Match u s a b c

instance AnnConstrOps Ty where
  (=>=) a b u c = MatchTy u hardFlowSem a b c
  (<==) t a u c = CompTy u t a c
  (=>=!) a b s u c = MatchTy u s a b c

class AnnCompOps a where
  (\-/) :: a -> a -> a
  (\+/) :: a -> a -> a

instance AnnCompOps (AnnComp a) where
  (\-/) = Max
  (\+/) = Plus

instance Ord a => AnnCompOps (Map a (AnnComp Ty)) where
  (\-/) = Map.unionWith (\-/)
  (\+/) = Map.unionWith (\+/)

hardFlowSem :: FlowSem a
hardFlowSem = HardFlow

softFlowSem :: FlowSem a
softFlowSem = SoftFlow

isUserFlow :: FlowSem sem -> Bool
isUserFlow (UserFlow _) = True
isUserFlow _            = False

%%]


Pretty printing.

%%[7_2.pretty

instance PP sem => PP (AnnConstr sem context) where
  pp (Match uid s lAnn rAnn _)
    = pp uid >|< ": " >|< pp lAnn >#< "=>=" >|< pp s >#< pp rAnn
  pp (MatchTy uid s lTy rTy _)
    = pp uid >|< ": " >|< ppTy lTy >#< "=>=" >|< pp s >#< ppTy rTy
  pp (Comp uid tree ann _)
    = pp uid >|< ": " >|< pp tree >#< "<==" >#< ann
  pp (CompTy uid tree ty _)
    = pp uid >|< ": " >|< pp tree >#< "<==" >#< ppTy ty
  pp (Inst uid bndgId projections _)
    = pp uid >|< ": Inst" >#< show bndgId >|< ":" >#< vlist projections

instance PP InstProjection where
  pp (TyProjection tyFrom tyTo) = ppTy tyFrom >#< "   ~>   " >#< ppTy tyTo
  pp (AnnProjection annFrom annTo) = pp annFrom >#< "   ~>   " >#< pp annTo

instance PP a => PP (AnnComp a) where
  pp (Plus lTree rTree)
    = "Plus" >-< "  " >|< ( pp lTree >-< pp rTree )
  pp (Max lTree rTree)
    = "Max"  >-< "  " >|< ( pp lTree >-< pp rTree )
  pp (Embed v) = pp v

instance PP sem => PP (AnnConstrSet sem context) where
  pp = Set.fold (>-<) empty

instance PP sem => PP (FlowSem sem) where
  pp HardFlow     = empty
  pp SoftFlow     = empty
  pp (UserFlow s) = pp s

instance Show sem => Show (FlowSem sem) where
  show HardFlow     = ""
  show SoftFlow     = ""
  show (UserFlow s) = show s

instance PP () where
  pp () = empty

instance PP sem => PP (Map BndgId (AnnConstrSet sem context)) where
  pp = Map.foldWithKey (\b s r -> show b >|< ":" >-< ("  " >|< s) >-< r) empty

instance Show (AnnConstr sem context) where
  show (Match uid _ annA annB _) = show uid ++ ": " ++ show annA ++ " =>= " ++ show annB
  show (MatchTy uid _ tyA tyB _) = show uid ++ ": " ++ show tyA ++ " =>= " ++ show tyB
  show (Comp uid tree ann _)     = show uid ++ ": " ++ show tree ++ " <= " ++ show ann
  show (CompTy uid tree ty _)    = show uid ++ ": " ++ show tree ++ " <= " ++ show ty
  show (Inst uid bndgId projs _) = show uid ++ ": Inst " ++ show bndgId ++ ": " ++ show projs

%%]


"Solve" the graph, by improving the substitution. Fixpoint computation on the
values currently mapped to the nodes of the graph.

%%[7_2

improveSubst :: (Eq a, Show a, Show sem, Ord sem) => a -> (FlowSem sem -> a -> a -> (a, a)) -> (AnnComp a -> a -> (AnnComp a, a)) -> (ConstrGraph sem) -> Map (Annotation Ty) a -> Map (Annotation Ty) a
improveSubst bot flowF compF g
  = worklistFix processNode (concatMap constrs (G.nodes g))
  where
    nodeMap = getNodeMap g
  
    constrs n
      = let compNeighbors = [ n' | n' <- G.neighbors g n, either (const False) (const True) (fromJust (G.lab g n')) ]
            flowNeighbors =  [ (s, nA, nB) | (nA, nB, e) <- G.out g n, isFlowEdge e, let FlowEdge s = e ]
                          ++ [ (s, nA, nB) | (nA, nB, e) <- G.inn g n, isFlowEdge e, let FlowEdge s = e ]
         in map Left flowNeighbors ++ map Right compNeighbors
  
    processNode c subst
      = case c of
          Left (sSem, nA, nB)
            -> let annA = either id (error "processNode: expecting a comp node") (fromJust (G.lab g nA))
                   annB = either id (error "processNode: expecting a comp node") (fromJust (G.lab g nB))
                   valA = Map.findWithDefault bot annA subst
                   valB = Map.findWithDefault bot annB subst
                   (valA', valB') = foldr (\s (vA, vB) -> flowF s vA vB) (valA, valB) (Set.toList sSem)
                   subst' = Map.insert annB valB' (Map.insert annA valA' subst)
                   aNode = addIfChanged valA valA' nA
                   bNode = addIfChanged valB valB' nB
                   newNodes = concatMap constrs (aNode ++ bNode)
                in (subst', newNodes)
          Right n
            -> let (Just (Right (toAnn, annComp))) = G.lab g n
                   anns = collect annComp
                   vals = map (\a -> Map.findWithDefault bot a subst) anns
                   valComp = distribute vals annComp
                   toVal = Map.findWithDefault bot toAnn subst
                   (valComp', toVal') = compF valComp toVal
                   vals' = collect valComp'
                   subst' = Map.insert toAnn toVal' (foldr (uncurry Map.insert) subst (zip anns vals'))
                   newNodes =  addIfChanged toVal toVal' (Map.findWithDefault (error "processNode: no node id for annotation") toAnn nodeMap)
                            ++ concat (zipWith3 (\a vBefore vAfter ->
                                let n' = Map.findWithDefault (error "processNode: no node id for annotation") a nodeMap
                                 in addIfChanged vBefore vAfter n') anns vals vals')
                   newNodes' = concatMap constrs newNodes
                in (subst', newNodes')

    addIfChanged a b n
      | a == b    = []
      | otherwise = [n]

worklistFix :: (n -> a -> (a, [n])) -> [n] -> a -> a
worklistFix f
  = wFix
  where
    wFix []     a = a
    wFix (n:ns) a = let (a', ns') = f n a
                     in wFix (ns' ++ ns) a'

%%]


Partial solving of constraint graphs: improves the substitution and applies a graph reduction.

%%[7_2

partialSolve :: Ord sem => (ConstrGraph sem -> Map (Annotation Ty) a -> Map (Annotation Ty) a) -> (Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem) -> Annotations Ty -> ConstrGraph sem -> Map (Annotation Ty) a -> (ConstrGraph sem, Map (Annotation Ty) a)
partialSolve substF
  = stagedPartialSolve [substF]

stagedPartialSolve :: Ord sem => [ConstrGraph sem -> Map (Annotation Ty) a -> Map (Annotation Ty) a] -> (Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem) -> Annotations Ty -> ConstrGraph sem -> Map (Annotation Ty) a -> (ConstrGraph sem, Map (Annotation Ty) a)
stagedPartialSolve substFs rewriteF scopeAnns g s
  = let s' = foldl (flip ($ g)) s substFs
        g' = rewriteF scopeAnns s' g
     in (g', s')

%%]


Conversion from constraints into constraint graphs.
Parameterized by a partial solver/graph rewriter.

%%[7_2

processConstraints :: (Show sem, PP sem, Ord sem) => (Annotations Ty -> ConstrGraph sem -> a -> (ConstrGraph sem, a)) -> a -> FlattenOps context -> Map BndgId (Annotations Ty) -> Map BndgId [Ty] -> Annotations Ty -> Map BndgId (AnnConstrSet sem context) -> Map BndgId (ConstrGraph sem) -> UID -> (a, Map BndgId (ConstrGraph sem), UID)
processConstraints f ini ops additionalAnns scope exclude newBndgs initialPrep uid
  = foldl (\initial bndgIds -> foldr (processBndg bndgIds) initial bndgIds) (ini, initialPrep, uid) (solveOrder newBndgs)
  where
    processBndg bndgIds bndgId (ini, bndgGraphs, uid)
      = let cset = Map.findWithDefault (error ("prepBndg:cset: No such binding-group: " ++ show bndgId ++ " | " ++ show newBndgs)) bndgId newBndgs
            sc   = Map.findWithDefault (error ("prepBndg:sc: No such binding-group: " ++ show bndgId ++ " | " ++ show scope)) bndgId scope
            aas  = Map.findWithDefault (error ("prepBndg:aas: No such binding-group: " ++ show bndgId ++ " | " ++ show additionalAnns)) bndgId additionalAnns
            cs   = flatten bndgIds ops cset
            as   = aas `Set.union` annotations sc `Set.union` annotations cset
            g    = csToGr as cs
            is   = filter (\c -> case c of Inst _ bId _ _ -> not (bId `elem` bndgIds) ;_ -> False) (Set.toList cset)
            (g', uid')  = foldr (uncurry . processInst bndgId bndgGraphs) (g, uid) is
            (g'', ini') = f (aas `Set.union` annotations sc `Set.union` exclude) g' ini
         in (ini', Map.insert bndgId g'' bndgGraphs, uid')

    processInst bndgIdTo bndgGraphs (Inst _ bndgIdFrom projs context) g uid
      = let tps = concat (mapProjections (\tA tB -> [tA, tB]) (\_ _ -> []) projs)
            infos = FlattenInfo { flatVarianceFun  = inferVariance ops context tps
                                , flatBelownessFun = inferBelowness ops context tps
                                }
            source = Map.findWithDefault (error ("processInst:source: No such binding group: " ++ show bndgIdFrom ++ " | " ++ show bndgGraphs ++ " | " ++ show (solveOrder newBndgs) ++ " | " ++ show (Map.keys newBndgs))) bndgIdFrom bndgGraphs
            sc = Map.findWithDefault (error ("processInst:sc: No such binding-group: " ++ show bndgIdTo ++ " | " ++ show scope)) bndgIdTo scope
            scfBelowness = inferBelowness ops context sc
            exclude' = exclude `Set.union` Set.filter (\a -> scfBelowness a /= Below) (annotations sc)
         in instantiate infos (flatExpFun ops) exclude' source projs uid g

%%]

Obtain a fixpoint substitution from the graphs of the binding-groups.

%%[7_2

solveToBndgSubstitution :: (Ord sem, Eq l) => (ConstrGraph sem -> Map (Annotation Ty) l -> Map (Annotation Ty) l) -> Map BndgId (ConstrGraph sem) -> Map (Annotation Ty) l -> Map (Annotation Ty) l
solveToBndgSubstitution solveF graphs
  = worklistFix solveBndg initialBndgs
 where
    bndgAnnMap   = Map.map annotations_ graphs
    flatBndgAnns = [(a, b) | (b, as) <- Map.assocs bndgAnnMap, a <- as ]
    annBndgMap   = Map.fromList [ (a, map snd as) | as <- groupAllBy fst flatBndgAnns, let ((a, _) : _) = as ]
    initialBndgs = Map.keys graphs

    solveBndg bndgId subst
      = let g = Map.findWithDefault (error "solveToBndgSubstitution:g:undefined") bndgId graphs
            subst' = solveF g subst
            bndgs = [ b | a <- diff subst subst', b <- Map.findWithDefault (error "solveToBndgSubstitution:bndgs:undefined") a annBndgMap, b /= bndgId ]
         in (subst', bndgs)

diff :: (Ord k, Eq l) => Map k l -> Map k l -> [k]
diff a b = [ k | (k, p) <- Map.assocs $ Map.intersectionWith (/=) a b, p ]

%%]

Determine binding-group solve order. The solve order is in such a way that a referenced binding-
group is solved before the using binding-group. Cycles are solved as a singular binding-group.

%%[7_2

solveOrder :: Show sem => Map BndgId (AnnConstrSet sem context) -> [[BndgId]]
solveOrder bndgs
  = let deps = Map.unionsWith Set.union ( [ Map.singleton bndgIdFrom (Set.singleton bndgIdTo)
                                          | (bndgIdFrom, cset) <- Map.assocs bndgs, c <- Set.elems cset
                                          , bndgIdTo <- isInst c ] ++
                                          [ Map.singleton bndgId (Set.singleton bndgId)
                                          | bndgId <- Map.keys bndgs ] )
        nodes = [ (n, n, Set.elems outset) | (n,outset) <- Map.assocs deps ]
        order = stronglyConnComp nodes
     in map sccToList order
  where
    isInst (Inst _ bndgId _ _) = [bndgId]
    isInst _  = []
    
    sccToList :: SCC BndgId -> [BndgId]
    sccToList (AcyclicSCC n) = exist [n]
    sccToList (CyclicSCC ns) = exist ns
    
    exist :: [BndgId] -> [BndgId]
    exist = filter (`Map.member` bndgs)

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Annotated type to AnnTree conversion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Representation of an annotated type as a tree of annotations. An annotation tree
is an abstraction with respect to the structure of types, made to make make dealing
with annotated types easier in functions defined on constraints containing
annotated types. We provide an unification function that, given a set of annotated
types containing a type that is at least one is the most general of them all, converts
each type into an annotation tree that is equal shaped to the annotation trees of the
others, possibly by dupplicating an annotation and only up to a NonUnifiable node.

These trees are heavily used for flattening. Before we flatten a constraint containing
types, we unify the types to an annotation tree. The unification function will ensure
that the trees are of equal shape, such that a flattening operation can process it
node by node, without having to handle mismatched amounts of annotations e.d. This is
an important abstraction, since the annotated types have quite some diversity:
truncated type constructors, expanded type constructors.

Another property is that a type variable is always expanded to the same annotation tree.
If a type variable v occurs twice, both it's occurrences are expanded to exactly the
same tree. It is achieved by generating and collecting "Delayed" nodes, which are
themselves unified in a separate pass. Delayed nodes never occur outside the unification
function.

%%[7_2

data AnnTree
  = AnnNode !(Annotation Ty) [AnnTree] ![Annotation Ty]
  | Delayed !TyVarId !TyVarId Ty
  | NotUnifiable AnnTree
  deriving (Eq, Ord, Show)

data AnnTreePath
  = PathBranch !Int !AnnTreePath
  | PathSet !Int
  | PathEnd
  deriving (Eq, Ord, Show)

data TyLeafInfo
  = InfoLeaf (Annotation Ty) Ty [Ty]
  | InfoRecord (Annotation Ty) Ty
  | InfoOtherwise
  deriving Show

leftmostTyLeafInfo :: Ty -> TyLeafInfo
leftmostTyLeafInfo ty = fst (find [] (error ("leftmostTyLeafInfo: no parent: " ++ show ty)) ty)
  where
    find args parent ty
      = case ty of
          Ty_App tl tr -> let res@(_, isRecord) = find (tr:args) ty tl
                              (info, _)         = find args ty tr
                           in if isRecord
                              then (info, False)
                              else res
          Ty_Ann ann ty'   -> ( InfoLeaf ann ty' args
                              , case ty' of
                                  Ty_Con nm                    -> hsnIsRec nm
                                  Ty_TruncTyCon _  (Ty_Con nm) -> hsnIsRec nm
                                  _                            -> False
                              )
          Ty_Quant _ _ ty' -> find args ty ty'
          Ty_Ext _ _ _     -> (InfoRecord (tyOutermostAnn parent) ty, True)
          _                -> (InfoOtherwise, False)

unifyToAnnTree :: ([Annotation Ty] -> [Ty] -> HsName -> HsName -> [Ty]) -> [Ty] -> [AnnTree]
unifyToAnnTree truncExpFun
  = toTree Map.empty
  where
    toTree :: Map TyVarId TyVarId -> [Ty] -> [AnnTree]
    toTree cnstr tys
      = let trees = toTreeWithDelayed tys
         in stripDelayed cnstr trees
    
    stripDelayed :: Map TyVarId TyVarId -> [AnnTree] -> [AnnTree]
    stripDelayed cnstr trees
      = let cnstr' = gatherCnstr trees cnstr
            blocked = gatherBlocked trees
            delayed = seqToList (concatSeqs (map (gatherDelayed cnstr' blocked) trees))
            subst = Map.fromAscList [ (fst (head tpairs), toTreeWithDelayed (reverse (map snd tpairs)))
                                    | tpairs <- groupAllBy fst delayed, not (null tpairs) ]
            (nondelayed, _) = threadMap (distributeDelayed cnstr') subst trees
         in if null delayed
            then nondelayed
            else stripDelayed cnstr' nondelayed
    
    toTreeWithDelayed :: [Ty] -> [AnnTree]
    toTreeWithDelayed []
      = []
    toTreeWithDelayed tps
      = let ty = outermostMostGeneralTy tps
            (fs, rs, us) = unzip3 (map (unify ty) tps)
            rs' = transposeWithEmpty (map toTreeWithDelayed (transposeWithEmpty rs))
         in if and us
            then zipWith ($) fs rs'
            else trace ("failed to unify (normal behavior, remove this trace): " ++ show tps) $ map (NotUnifiable . tyToAnnTree) tps
         
    unify :: Ty -> Ty -> ([AnnTree] -> AnnTree, [Ty], Bool)
    unify tySpec ty
      = case (leftmostTyLeafInfo tySpec, leftmostTyLeafInfo ty) of
          (InfoLeaf annA (Ty_Var tv _) argsA, InfoRecord annB tyB)
            -> cucceed (Delayed tv tv ty)
          (InfoRecord annA tyA, InfoRecord annB tyB) | tyA `sameRecord` tyB
            -> succeed (\args -> AnnNode annB args []) (map snd (tyRowCanonOrder (tyToplevelExts tyB)))
          (InfoRecord annA tyA, InfoLeaf annB Ty_Any argsB)
            -> succeed (\args -> AnnNode annB args []) [ ty | _ <- tyToplevelExts tyA ]
          (InfoLeaf annA tyA argsA, InfoLeaf annB tyB argsB)
            -> case tyA of
                 Ty_Var tv _
                   -> case tyB of
                        Ty_Var tv' _ -> cucceed (Delayed tv tv' (Ty_Ann annB Ty_Any))
                        _            -> cucceed (Delayed tv tv ty)
                 Ty_ExpTyCon constrsA argIdsA origTyA
                   -> case tyB of
                        Ty_ExpTyCon constrsB argIdsB origTyB | origTyA `sameTyCon` origTyB
                          -> succeed (\args -> AnnNode annB args [])
                                     (allFields constrsB)
                        Ty_TruncTyCon annsB origTyB | origTyA `sameTyCon` origTyB
                          -> let constrNames = map (\(TyExpConstr_Constr n _) -> n) constrsA
                                 dataName    = tyToplevelConNm origTyB
                              in succeed (\args -> AnnNode annB args [])
                                         (concatMap (truncExpFun (Set.toList annsB) argsB dataName) constrNames)
                        Ty_Any
                          -> succeed (\args -> AnnNode annB args [])
                                     (replicate (length (allFields constrsA)) ty)
                        _ -> uniFail
                 Ty_TruncTyCon annsA origTyA
                   -> case tyB of
                        Ty_TruncTyCon annsB origTyB | origTyA `sameTyCon` origTyB
                          -> succeed (\args -> AnnNode annB args (Set.toList annsB)) argsB
                        Ty_Any
                          -> succeed (\args -> AnnNode annB args (replicate (Set.size annsA) annB))
                                     (replicate (length argsA) ty)
                        _ -> uniFail
                 Ty_Con _ | tyA `sameTyCon` tyB
                   -> succeed (\args -> AnnNode annB args []) argsB
                 Ty_Con _ | tyB == Ty_Any
                   -> succeed (\args -> AnnNode annB args []) (replicate (length argsA) ty)
                 Ty_Any
                   -> cucceed (AnnNode annB [] [])
                 _ -> trace ("types not unifyable: normal supported behavior - doesn't happen a lot though. (this trace should be removed)")
                        $ uniFail
          _ -> uniFail

    succeed :: ([AnnTree] -> AnnTree) -> [Ty] -> ([AnnTree] -> AnnTree, [Ty], Bool)
    succeed f ts = (f, ts, True)
    
    cucceed :: AnnTree -> ([AnnTree] -> AnnTree, [Ty], Bool)
    cucceed t = (const t, [], True)
    
    uniFail :: ([AnnTree] -> AnnTree, [Ty], Bool)
    uniFail = ( error "unifyToAnnTree: unify: input types not unifyable"
              , error "unifyToAnnTree: unify: input types not unifyable"
              , False
              )

gatherBlocked :: [AnnTree] -> Set TyVarId
gatherBlocked
  = snd . threadMap gather Set.empty
  where
    gather tree set
      = case tree of
          AnnNode _ trees _  -> let (_, set') = threadMap gather set trees
                                 in ((), set')
          Delayed _ _ ty     -> ((), tyAllTyVars ty `Set.union` set)
          NotUnifiable tree' -> let (_, set') = gather tree' set
                                 in ((), set')

gatherCnstr :: [AnnTree] -> Map TyVarId TyVarId -> Map TyVarId TyVarId
gatherCnstr trees initial
  = snd (threadMap gather initial trees)
  where
    gather tree cnstr
      = case tree of
          AnnNode _ trees _   -> let (_, cnstr') = threadMap gather cnstr trees
                                  in ((), cnstr')
          Delayed var1 var2 _ -> ((), Map.insert (applySubst cnstr var2) (applySubst cnstr var1) cnstr)
          NotUnifiable tree'  -> let (_, cnstr') = gather tree' cnstr
                                  in ((), cnstr')

applySubst :: Map TyVarId TyVarId -> TyVarId -> TyVarId
applySubst m tv = Map.findWithDefault tv tv m

gatherDelayed :: Map TyVarId TyVarId -> Set TyVarId -> AnnTree -> Seq (TyVarId, Ty)
gatherDelayed cnstr blocked tree
  = case tree of
      AnnNode _ trees _     -> concatSeqs (map (gatherDelayed cnstr blocked) trees)
      Delayed var1 var2 ty  -> let var = applySubst cnstr var1
                                in if var `Set.member` blocked
                                   then emptySeq
                                   else unitSeq (var, ty)
      NotUnifiable tree'    -> gatherDelayed cnstr blocked tree'

distributeDelayed :: Map TyVarId TyVarId -> AnnTree -> Map TyVarId [AnnTree] -> (AnnTree, Map TyVarId [AnnTree])
distributeDelayed cnstr tree subst
  = case tree of
      AnnNode ann trees anns -> let (trees', subst') = threadMap (distributeDelayed cnstr) subst trees
                                 in (AnnNode ann trees' anns, subst')
      Delayed var1 var2 _    -> let var = applySubst cnstr var1
                                 in case Map.lookup var subst of
                                      Nothing     -> (tree, subst)
                                      Just (t:ts) -> (t, Map.insert var ts subst)
      NotUnifiable tree'     -> let (tree'', subst') = (distributeDelayed cnstr) tree' subst
                                 in (NotUnifiable tree'', subst')

tyToAnnTree :: Ty -> AnnTree
tyToAnnTree ty
  = case leftmostTyLeafInfo ty of
      InfoLeaf ann leaf args
        -> case leaf of
             Ty_ExpTyCon constrs ids _
               -> AnnNode ann (map tyToAnnTree (allFields constrs)) []
             Ty_TruncTyCon anns _
               -> AnnNode ann (map tyToAnnTree args) (Set.toList anns)
             _ -> AnnNode ann (map tyToAnnTree args) []
      InfoRecord ann rec
        -> AnnNode ann (map (tyToAnnTree . snd) (tyRowCanonOrder (tyToplevelExts rec))) []
      _ -> error ("tyToAnnTree: no annotation in ty: " ++ show ty)

allFields :: TyExpConstrs -> [Ty]
allFields = concatMap (\(TyExpConstr_Constr _ fields) -> map (\(TyExpField_Field ty) -> ty) fields)

sameTyCon :: Ty -> Ty -> Bool
sameTyCon (Ty_Con a) (Ty_Con b) = a == b
sameTyCon _ _ = False

sameRecord :: Ty -> Ty -> Bool
sameRecord a b = let fieldsA = map fst (tyRowCanonOrder (tyToplevelExts a))
                     fieldsB = map fst (tyRowCanonOrder (tyToplevelExts b))
                  in fieldsA == fieldsB

outermostMostGeneralTy :: [Ty] -> Ty
outermostMostGeneralTy = foldr1 pick
  where
    pick :: Ty -> Ty -> Ty
    pick tyA tyB
      = case (leftmostTyLeafInfo tyA, leftmostTyLeafInfo tyB) of
          (InfoLeaf _ tyL _, InfoLeaf _ tyR _)
            -> pick' tyL tyR tyA tyB
          (InfoRecord _ _, InfoLeaf _ (Ty_Any) _)
            -> tyA
          (InfoRecord _ _, InfoLeaf _ _ _)
            -> tyB
          _ -> tyA

    pick' :: Ty -> Ty -> Ty -> Ty -> Ty
    pick' tyA tyB realA realB
      = case (tyA, tyB) of
          (_, Ty_Any)     -> realA
          (Ty_Any, _)     -> realB
          (_, Ty_Var _ _) -> realB
          (Ty_Var _ _, _) -> realA
          (Ty_Con _, _)   -> realB
          (_, Ty_Con _)   -> realA
          (_, Ty_TruncTyCon _ _) -> realA
          (Ty_TruncTyCon _ _, _) -> realB
          _                      -> realA

transposeWithEmpty :: [[a]] -> [[a]]
transposeWithEmpty xs
  | all null xs = xs
  | otherwise   = transpose xs

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint flattening
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Flatten a constraint set. Inst-constraints are not flattened and are dealt with at
another place (ignored for flattening).

%%[7_2

data FlattenOps context
  = FlattenOps { flatExpFun       :: [Annotation Ty] -> [Ty] -> HsName -> HsName -> [Ty]
               , inferVariance    :: context -> [Ty] -> Annotation Ty -> CoContraVariance
               , inferBelowness   :: context -> [Ty] -> Annotation Ty -> Belowness
               }

data FlattenInfo
  = FlattenInfo { flatVarianceFun  :: Annotation Ty -> CoContraVariance
                , flatBelownessFun :: Annotation Ty -> Belowness
                }

class Flattenable a sem context | a -> sem, a -> context where
  flatten :: [BndgId] -> FlattenOps context -> a -> [AnnConstr sem context]
  flatten_ :: [BndgId] -> FlattenOps context -> a -> Seq (AnnConstr sem context)
  flatten bndgIds ops = seqToList . flatten_ bndgIds ops

instance Flattenable a sem context => Flattenable [a] sem context where
  flatten_ bndgIds ops = concatSeqs . fmap (flatten_ bndgIds ops)

instance Flattenable a sem context => Flattenable (Set a) sem context where
  flatten_ bndgIds ops = flatten_ bndgIds ops . Set.toList

instance Show sem => Flattenable (AnnConstr sem context) sem context where
  flatten_ bndgIds ops c
    = let (tps, context)
            = case c of
                MatchTy _ _ tyA tyB ctx -> ([tyA, tyB], ctx)
                MatchTyAnn _ _ ty _ ctx -> ([ty], ctx)
                CompTy _ tree ty ctx -> (ty : (seqToList (compTys tree)), ctx)
                Inst _ _ projs ctx -> (concat (mapProjections (\tA tB -> [tA, tB]) (\_ _ -> []) projs), ctx)
          infos = FlattenInfo { flatVarianceFun  = inferVariance ops context tps
                              , flatBelownessFun = inferBelowness ops context tps
                              }
       in case c of
            MatchTy uid sem tyA tyB context
              -> let [atA, atB] = unifyToAnnTree (flatExpFun ops) [tyA, tyB]
                  in flattenMatch uid context sem infos atA atB
            MatchTyAnn uid sem ty ann context
              -> let anns = filter ((/= Below) . flatBelownessFun infos) (annotations_ ty)
                     uids = mkInfNewLevUIDL uid
                  in mkSeq (zipWith (\a u -> u #.. (ann =>=! a) sem ..# context) anns uids)
            CompTy uid tree ty context
              -> let trees = unifyToAnnTree (flatExpFun ops) (ty : collect tree)
                     tree' = distribute trees tree
                  in flattenComp uid context infos tree' (head trees) (tail trees)
            Inst uid bndgId projs context | bndgId `elem` bndgIds
              -> let belowFilter
                       = filterSeq (\(Match _ _ a1 a2 _) -> not (flatBelownessFun infos a1 == NotBelow && flatBelownessFun infos a2 == NotBelow) )
                     flatMatchTy tyFrom tyTo uid
                       = let [atA, atB] = unifyToAnnTree (flatExpFun ops) [tyFrom, tyTo]
                             (uidL, uidR) = mkNewLevUID uid
                             cs = flattenMatch uidL context hardFlowSem infos atA atB <+> flattenMatch uidR context hardFlowSem infos atB atA
                          in belowFilter cs
                     matchAnn annA annB uid
                       = let (uidL, uidR) = mkNewLevUID uid
                          in belowFilter $ unitSeq (uidL #.. annA =>= annB ..# context) <+> unitSeq (uidR #.. annB =>= annA ..# context)
                     uids = mkInfNewLevUIDL uid
                  in concatSeqs (zipWith ($) (mapProjections flatMatchTy matchAnn projs) uids)
            _ -> unitSeq c
    where
      compTys c
        = case c of
            Plus a b -> compTys a <+> compTys b
            Max  a b -> compTys a <+> compTys b
            Embed t  -> unitSeq t

flattenMatch :: UID -> context -> FlowSem sem -> FlattenInfo -> AnnTree -> AnnTree -> Seq (AnnConstr sem context)
flattenMatch uid context sem infos (AnnNode annA treesA annsA) (AnnNode annB treesB annsB)
  = let (uidA, uidB) = mkNewLevUID uid
        infUidA = mkInfNewLevUIDL uidA
        infUidB = mkInfNewLevUIDL uidB
     in     concatSeqs (zipWith3 (\uid -> withCoCon (flatVarianceFun infos) (\a b -> uid #.. (a =>=! b) sem ..# context)) infUidA (annA : annsA) (annB : annsB))
        <+> concatSeqs (zipWith3 (\uid -> flattenMatch uid context sem infos) infUidB treesA treesB)
flattenMatch uid context sem infos (NotUnifiable atA@(AnnNode annA _ _)) (NotUnifiable atB@(AnnNode annB _ _))
  = let (uidA, uidB, uidC) = mkNewLevUID2 uid
     in     withCoCon (flatVarianceFun infos) (\a b -> uidA #.. (a =>=! b) sem ..# context) annA annB
        <+> flattenDefault uidB context sem infos True annA atA
        <+> flattenDefault uidC context sem infos True annB atB

flattenComp :: UID -> context -> FlattenInfo -> AnnComp AnnTree -> AnnTree -> [AnnTree] -> Seq (AnnConstr sem context)
flattenComp uid context infos compTree
  = flatComp uid
  where
    flatComp uid annTree annTrees
      = case annTree of
          AnnNode ann _ _ | flatBelownessFun infos ann >= NotBelow
            -> let (anns, subtrees) = unzip (map unAnnNode (annTree : annTrees))
                   annsT     = transpose anns
                   subtreesT = transpose subtrees
                   (uidA, uidB) = mkNewLevUID uid
                   infUidA = mkInfNewLevUIDL uidA
                   infUidB = mkInfNewLevUIDL uidB
                in mkSeq (zipWith (\uid (a:as) -> uid #.. distribute as compTree <== a ..# context) infUidA annsT)
                   <+> concatSeqs (zipWith (\uid (s:ss) -> flatComp uid s ss) infUidB subtreesT)
          NotUnifiable tA@(AnnNode annA _ _) | flatBelownessFun infos annA >= NotBelow
            -> error (  "flattenComp::not unifiable::not below arrow::situation:: uncomment the code in this module to allow this situation to be dealt with:\n"
                     ++ show tA
                     )
               {-
               let (uidA, uidB, uidC) = mkNewLevUID2 uid
                   infUidB = mkInfNewLevUIDL uidB
                   anns = map (\(NotUnifiable (AnnNode annB _ _)) -> annB) annTrees
                in unitSeq (uidA #.. distribute anns compTree <== annA ..# context)
                   <+> flattenDefault uidC context hardFlowSem infos False annA tA
                   <+> concatSeqs (zipWith (\t u -> flattenDefault u context hardFlowSem infos False annA t) annTrees infUidB)
               -}
          _ -> emptySeq

flattenDefault :: UID -> context -> FlowSem sem -> FlattenInfo -> Bool -> Annotation Ty -> AnnTree -> Seq (AnnConstr sem context)
flattenDefault uid context sem infos proceedBelow parentAnn tree
  = rec tree uid
  where
    rec tree uid
      = case tree of
          NotUnifiable t
            -> rec t uid
          AnnNode ann trees anns
            -> let (uidA, uidB, uidC, uidD, uidE) = mkNewLevUID4 uid
                   infUidC = mkInfNewLevUIDL uidC
                   infUidD = mkInfNewLevUIDL uidD
                   infUidE = mkInfNewLevUIDL uidE
                in eqMatch ann uidA uidB
                   <+> concatSeqs (zipWith3 eqMatch anns infUidC infUidD)
                   <+> concatSeqs (zipWith rec trees infUidE)

    eqMatch ann u1 u2
      | proceedBelow || flatBelownessFun infos ann >= NotBelow
          = mkSeq [ u1 #.. (ann =>=! parentAnn) sem ..# context
                  , u2 #.. (ann =<=! parentAnn) sem ..# context ]
      | otherwise 
          = emptySeq

unAnnNode :: AnnTree -> ([Annotation Ty], [AnnTree])
unAnnNode (AnnNode ann trees anns) = (ann : anns, trees)

withCoCon :: (Annotation Ty -> CoContraVariance) -> (Annotation Ty -> Annotation Ty -> AnnConstr sem context) -> Annotation Ty -> Annotation Ty -> Seq (AnnConstr sem context)
withCoCon varFun match annA annB
  = let varA = varFun annA
        varB = varFun annB
     in case (join varA varB) of
          CoVariant       -> unitSeq (annA `match` annB)
          ContraVariant   -> unitSeq (annB `match` annA)
          CoContraVariant -> unitSeq (annA `match` annB) <+> unitSeq (annB `match` annA)
  where
    join x y | x == y    = x
             | otherwise = CoContraVariant

collect :: AnnComp a -> [a]
collect = seqToList . collect_
collect_ (x `Plus` y) = collect_ x <+> collect_ y
collect_ (x `Max` y)  = collect_ x <+> collect_ y
collect_ (Embed x)    = unitSeq x

distribute :: [b] -> AnnComp a -> AnnComp b
distribute zs tree = fst (distribute_ tree zs)

distribute_ :: AnnComp a -> [b] -> (AnnComp b, [b])
distribute_ (x `Plus` y) zs
  = let (x', zs')  = distribute_ x zs
        (y', zs'') = distribute_ y zs'
     in (x' `Plus` y', zs'')
distribute_ (x `Max` y) zs
  = let (x', zs')  = distribute_ x zs
        (y', zs'') = distribute_ y zs'
     in (x' `Max` y', zs'')
distribute_ (Embed _) (z:zs)
  = (Embed z, zs)
distribute_ (Embed _) [] = error "distribute: Embed encountered, but no replacement available"

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Construction of the constraint graph from flattened constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Convert a constraint set to a constraint graph

%%[7_2

csToGr :: Ord sem => Annotations Ty -> [AnnConstr sem context] -> ConstrGraph sem
csToGr anns cl
  = unionFlowEdges
  $ G.run_ G.empty
  $ do mapM_ (G.insMapNodeM . Left) (Set.toList anns)
       mapM_ pushConstraint (nub' cl)
  where
    nub' = Set.toList . Set.fromList

pushConstraint :: AnnConstr sem context -> G.NodeMapM ConstrNode (ConstrEdge sem) G.Gr ()
pushConstraint (Match _ sem a b _)
  = do G.insMapEdgeM (Left a, Left b, FlowEdge (Set.singleton sem))
       return ()
pushConstraint c@(Comp _ tree a _)
  = do let node = Right (a, tree)
       G.insMapNodeM node
       G.insMapEdgeM (node, Left a, CompEdge)
       mapM_ (\a -> G.insMapEdgeM (Left a, node, CompEdge)) (annotations_ tree)
pushConstraint _
  = return ()

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dealing with instantiation constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Instantiate the constraint graph of the definition site into the constraint graph of the use site. The
procedure is conceptually not too difficult, but the implementation is somewhat involving:

1. First the constraint graph of the definition site is freshly annotated and inserted at the use-site,
   for the part of the type of the use-site that has the same structure as the type of the definition
   site.
   
2. The type of the use site could be more specific than the type of the definition site, and may have
   more structure at places where the definitino site has a quantified type variable. For annotations
   on this structure, we duplicate the constraints on paths between type variables, since we only need
   to record flow, the binding-group itself cannot touch deeper into the structure.

%%[7_2

instantiate :: (Show sem, Ord sem) => FlattenInfo -> ([Annotation Ty] -> [Ty] -> HsName -> HsName -> [Ty]) -> Annotations Ty -> ConstrGraph sem -> InstProjections -> UID -> ConstrGraph sem -> (ConstrGraph sem, UID)
instantiate infos truncExpFun exclude graph projs uid destGraph
  = let (tyProjs, annProjs) = partitionProjections projs
        toAnnTree (tyFrom, tyTo) = let [treeFrom, treeTo] = unifyToAnnTree truncExpFun [tyFrom, tyTo] in (treeFrom, treeTo)
        treeProjs = map toAnnTree tyProjs
        tyVarAnns = Set.unions (map tyAllTyVarAnns (map fst tyProjs))

        (graph', uid', annMap) = freshGraph exclude graph uid
        destGraph' = destGraph `unionGraph` graph'

        eqs  = annProjs ++ seqToList (concatSeqs (map (uncurry (equalities tyVarAnns)) treeProjs))
        eqs' = substituteFreshAnnotations annMap eqs
        nodeMap = getNodeMap destGraph'
        nodeMapOrig = getNodeMap graph
        destGraph'' = addEqualities nodeMap eqs' destGraph'

        newAnns = groupByPath (seqToList (concatSeqs (map (uncurry (getAnnotationsBelowTyVar tyVarAnns)) treeProjs)))

        components = gComponents graph
        (destGraph''', uid'') = foldr addComponent (destGraph'', uid') components

        addComponent component (dest, u)
          = let varNodes = tyVarNodes tyVarAnns component
                betweenNodes = nodesBetweenTyVars nodeMapOrig tyVarAnns varNodes component
                component' = subgraph betweenNodes component

                addFreshGraph (_,varMap) (dest, u)
                  = let (fromVar, toVar) = head (Map.elems varMap)
                        varianceFrom  = flatVarianceFun infos fromVar
                        varianceTo    = flatVarianceFun infos toVar
                        otherVariance = varianceFrom /= varianceTo
                        isCoCon       = varianceFrom == CoContraVariant || varianceTo == CoContraVariant
                        isBelow       = flatBelownessFun infos toVar == Below

                        (component'', u', freshMap) = freshGraph exclude component' u
                        component''' = if isBelow
                                       then subgraph (Set.fromList [n | (n,a) <- G.labNodes component''
                                                                      , case a of Left _ -> True; _ -> False ]) component''
                                       else component''

                        component'''' = if otherVariance
                                        then reverseFlow isCoCon component'''
                                        else component'''

                        dest' = dest `unionGraph` component'''
                        nodeMap' = getNodeMap dest'
                        eqs = [ (Map.findWithDefault (error ("instantiate:eqs:undefined - " ++ show origAnn ++ " - " ++ show freshMap)) origAnn freshMap, replAnn)
                              | (origAnn, (_, replAnn)) <- Map.assocs varMap, origAnn `elem` varNodes ]
                        dest'' = addEqualities nodeMap' eqs dest'
                     in (dest'', u')

                newAnns' = filter (\(_,mp) -> any (`Map.member` mp) varNodes) newAnns
             in if Set.size betweenNodes > length varNodes
                then foldr addFreshGraph (dest, u) newAnns'
                else (dest, u)
     in (destGraph''', uid'')
     
equalities :: Annotations Ty -> AnnTree -> AnnTree -> Seq (Annotation Ty, Annotation Ty)
equalities tyVarAnns
  = eqs
  where
    eqs (AnnNode a treesA annsA) (AnnNode b treesB annsB)
      = let recSeq = concatSeqs (zipWith eqs treesA treesB)
            annSeq = mkSeq (zip annsA annsB)
         in unitSeq (a, b)
            <+> annSeq <+> if a `Set.member` tyVarAnns
                           then emptySeq
                           else recSeq
    eqs (NotUnifiable treeA) (NotUnifiable treeB)
      = defaultAnn treeA <+> defaultAnn treeB
    eqs _ _
      = error "equalities: trees not unified"

substituteFreshAnnotations :: Map (Annotation Ty) (Annotation Ty) -> [(Annotation Ty, Annotation Ty)] -> [(Annotation Ty, Annotation Ty)]
substituteFreshAnnotations annMap eqs
  = map (\(a, b) -> let errMsg = error ("equalities: can't find fresh version of " ++ show a ++ " in " ++ show annMap ++ " ~ " ++ show a ++ " does not exist in non-fresh graph. When creating the following equalities: " ++ show eqs)
                        a'     = Map.findWithDefault errMsg a annMap
                     in (a', b)) eqs

addEqualities :: Ord sem => Map (Annotation Ty) G.Node -> [(Annotation Ty, Annotation Ty)] -> ConstrGraph sem -> ConstrGraph sem
addEqualities nodeMap eqs g
  = foldr addEq g eqs
  where
    addEq (annA, annB)
      = let nAnnA = Map.findWithDefault (error ("addEqualities:nAnnA:undefined - " ++ show annA ++ " - " ++ show nodeMap)) annA nodeMap
            nAnnB = Map.findWithDefault (error ("addEqualities:nAnnB:undefined - " ++ show annB ++ " - " ++ show nodeMap)) annB nodeMap
         in insEdge nAnnA nAnnB hardFlowSem . insEdge nAnnB nAnnA hardFlowSem

insEdge :: Ord sem => G.Node -> G.Node -> FlowSem sem -> ConstrGraph sem -> ConstrGraph sem
insEdge nA nB sem g
  = let (Just (pr, n', a, su), g') = G.match nA g
        (suOther, suSimilar)       = partition ((/= nB) . snd) su
        su' = suOther ++ [ (FlowEdge $ Set.insert sem $ Set.unions [ s | (e@(FlowEdge s), _) <- suSimilar, isFlowEdge e ], nB) ]
     in (pr, n', a, su') G.& g'

defaultAnn :: AnnTree -> Seq (Annotation Ty, Annotation Ty)
defaultAnn (AnnNode a trees anns)
  = mkSeq (zip (repeat a) anns ++ zip (repeat a) (concatMap allAnns trees))
  where
    allAnns (AnnNode a trees anns) = a : (anns ++ concatMap allAnns trees)
    allAnns (NotUnifiable tree) = allAnns tree
defaultAnn _ = emptySeq

getAnnotationsBelowTyVar :: Annotations Ty -> AnnTree -> AnnTree -> Seq (Annotation Ty, AnnTreePath, Annotation Ty, Annotation Ty)
getAnnotationsBelowTyVar annsOnTyVars
  = walk Nothing id
  where
    walk mParent pathf treeA treeB
      = case (treeA, treeB) of
          (AnnNode annA treesA annsA, AnnNode annB treesB annsB)
            -> case mParent of
                 Just parent
                   -> unitSeq (parent, pathf PathEnd, annA, annB)
                      <+> concatSeqs (zipWith3 (\n -> walk mParent (pathf . PathBranch n)) [0..] treesA treesB)
                      <+> mkSeq (zipWith3 (\n a b -> (parent, pathf (PathSet n), a, b)) [0..] annsA annsB)
                 Nothing
                   -> let parent = if annA `Set.member` annsOnTyVars
                                   then Just annA
                                   else Nothing
                       in concatSeqs (zipWith (walk parent pathf) treesA treesB)
          _ -> emptySeq

groupByPath :: [(Annotation Ty, AnnTreePath, Annotation Ty, Annotation Ty)] -> [(AnnTreePath, Map (Annotation Ty) (Annotation Ty, Annotation Ty))]
groupByPath = map (\l -> let l' = [(par,(a,b)) | (par,_,a,b) <- l ]
                             p  = head [ p | (_,p,_,_) <- l ]
                          in (p, Map.fromList l'))
            . groupAllBy (\(_, p, _, _) -> p)

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constraint graph and utility functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

A constraint graph has two types of nodes: annotations and compositions. Annotation nodes
represent an annotation, composition nodes are a placeholder for the encoding of 1-to-many
edges.

%%[7_2

type ConstrGraph sem = G.Gr ConstrNode (ConstrEdge sem)
type ConstrNode      = Either (Annotation Ty) (Annotation Ty, AnnComp (Annotation Ty))
data ConstrEdge sem  = FlowEdge (Set (FlowSem sem)) | CompEdge deriving (Eq, Ord, Show)

instance Show sem => PP (ConstrGraph sem) where
  pp = text . G.graphviz'

freshGraph :: Annotations Ty -> ConstrGraph sem -> UID -> (ConstrGraph sem, UID, Map (Annotation Ty) (Annotation Ty))
freshGraph exclude g uid
  = let nodes = G.labNodes g
        (pairs, uid') = threadMap (\(_, a) u -> ((a, u), uidNext u)) uid nodes

        annMap = Map.fromList $ do (Left ann, u) <- pairs
                                   return (ann, fresh ann (ann { annUID = u, annInstFrom = Just ann }))

        replace (Left a)          = Left (subst a)
        replace (Right (a, tree)) = Right (subst a, fmap subst tree)

        subst a = Map.findWithDefault (error "freshGraph:subst:undefined") a annMap

        fresh ann ann'
          | ann `Set.member` exclude = ann
          | otherwise                = ann'
     in (G.nmap replace g, uid', annMap)

tyVarNodes :: Annotations Ty -> ConstrGraph sem -> [Annotation Ty]
tyVarNodes annsOnTyVars g
  = [ x | (_, a@(Left x)) <- G.labNodes g, either (`Set.member` annsOnTyVars) (const False) a ]

nodesBetweenTyVars :: Map (Annotation Ty) G.Node -> Annotations Ty -> [Annotation Ty] -> ConstrGraph sem -> Set G.Node
nodesBetweenTyVars nodeMap annsOnTyVars tyVarNodes g
  = let g' = G.trc g
        tvNodes = [ Map.findWithDefault (error "nodesBetweenTyVars:undefined") a nodeMap | a <- tyVarNodes ]
        intersects a b = not (null (a `intersect` b))
     in Set.fromList (tvNodes ++ [ n | n <- G.nodes g', G.pre g' n `intersects` tvNodes, G.suc g' n `intersects` tvNodes ])

isFlowEdge :: ConstrEdge a -> Bool
isFlowEdge (FlowEdge _) = True
isFlowEdge _            = False

-- side-effect removes self edges
unionGraph :: Ord sem => ConstrGraph sem -> ConstrGraph sem -> ConstrGraph sem
unionGraph gA gB
  = let g = gA `unionDisjointGraph` gB
        collisions = [ (fst (head gr), map fst (tail gr))  | gr <- groupAllBy snd (G.labNodes g), length gr > 1 ]
     in foldr merge g collisions
  where
    merge (n,ns) g
      = let ns' = n:ns
            out = nubAdjList [ (e, n') | n <- ns', (n', e) <- G.lsuc g n ]
            inn = nubAdjList [ (e, n') | n <- ns', (n', e) <- G.lpre g n ]
            (Just a) = G.lab g n
         in (inn, n, a, out) G.& G.delNodes ns' g

unionDisjointGraph :: ConstrGraph sem -> ConstrGraph sem -> ConstrGraph sem
unionDisjointGraph a b
  = let nodesA   = G.labNodes a
        nodesB   = G.labNodes b
        edgesA   = G.labEdges a
        edgesB   = G.labEdges b
        maxNode  = 1 + maximum (0 : G.nodes b)
        sumNodes = [(n+maxNode,a) | (n,a) <- nodesA] ++ nodesB
        sumEdges = [(n1+maxNode,n2+maxNode,e) | (n1,n2,e) <- edgesA] ++ edgesB
     in G.mkGraph sumNodes sumEdges

getNodeMap :: ConstrGraph sem -> Map (Annotation Ty) G.Node
getNodeMap g = Map.fromList [ (a,n) | (n,v) <- G.labNodes g, either (const True) (const False) v, let (Left a) = v ]

gComponents :: ConstrGraph sem -> [ConstrGraph sem]
gComponents g
  = map (uncurry G.mkGraph) (zip ln le)
  where ln = map (\x -> [(u,l)   |(u,l)   <-vs, elem u x]) cc
        le = map (\x -> [(u,v,l) |(u,v,l) <-es, elem u x]) cc
        vs = G.labNodes g
        es = G.labEdges g
        cc = G.components g

subgraph :: Set G.Node -> ConstrGraph sem -> ConstrGraph sem
subgraph nodes g
  = let vs  = G.labNodes g
        es  = G.labEdges g
        vs' = [ m | m@(n,_) <- vs, n `Set.member` nodes ]
        es' = [ e | e@(n1,n2,_) <- es, n1 `Set.member` nodes, n2 `Set.member` nodes ]
     in G.mkGraph vs' es'

reverseFlow :: Ord sem => Bool -> ConstrGraph sem -> ConstrGraph sem
reverseFlow keepOld g
  = let vs  = G.labNodes g
        es  = G.labEdges g
        es' =  [(n1,n2,e) | (n1,n2,e) <- es, not (isFlowEdge e) || keepOld]
            ++ [(n2,n1,e) | (n1,n2,e) <- es, isFlowEdge e]
     in G.mkGraph vs es'

unionFlowEdges :: Ord sem => ConstrGraph sem -> ConstrGraph sem
unionFlowEdges g
  = let vs  = G.labNodes g
        es  = G.labEdges g
        es' = nubEdgeList es
     in G.mkGraph vs es'

nubEdgeList :: Ord sem => [(G.Node, G.Node, ConstrEdge sem)] -> [(G.Node, G.Node, ConstrEdge sem)]
nubEdgeList
  = concatMap merge . groupAllBy (\(a, b, _) -> (a, b))
  where
    merge xs
      = let ((a,b):_, edges) = unzip (map (\(a, b, c) -> ((a, b), c)) xs)
            (flowEdges, compEdges) = partition isFlowEdge edges
         in if length flowEdges > 0
            then [(a, b, unionSems flowEdges)]
            else []
            ++ [ (a, b, e) | e <- nub compEdges ]

nubAdjList :: Ord sem => [(ConstrEdge sem, G.Node)] -> [(ConstrEdge sem, G.Node)]
nubAdjList adjL
  = let (flowAdj, compAdj) = partition (isFlowEdge . fst) adjL
        flowAdj' = [ (unionSems fL, b) | xs <- groupAllBy snd flowAdj, let (fL,b:_) = unzip xs ]
        compAdj' = nubBy (\(_,a) (_, b) -> a == b) compAdj
     in flowAdj' ++ compAdj'

unionSems :: Ord sem => [ConstrEdge sem] -> ConstrEdge sem
unionSems
  = FlowEdge . Set.unions . map (\(FlowEdge sems) -> sems)

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Graph rewrite
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[7_2

componentReduction :: (Ord sem, Show sem) => Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem
componentReduction sacredAnns _
  = dropUnusedComponents sacredAnns

basicGraphRewriter :: (Ord sem, Show sem) => (FlowSem sem -> Bool) -> Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem
basicGraphRewriter edgeFilter sacredAnns _
  = mergeEquivalentComponents sacredAnns edgeFilter . dropUnusedComponents sacredAnns

compToEqualityRewriter :: (Ord sem, Show sem) => Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem
compToEqualityRewriter _ _
  = splitCompNodes hardFlowSem

rewriteSelfEdges :: Ord sem => (FlowSem sem -> Bool) -> Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem
rewriteSelfEdges edgeFilter _ _ g
  = let vs  = G.labNodes g
        es  = G.labEdges g
        es' = [ e | e@(nA,nB,v) <- es, nA /= nB || required v ]
     in G.mkGraph vs es'
  where
    required (FlowEdge s) = any (not . edgeFilter) (Set.toList s)
    required _            = True

composeRewriters :: (Ord sem, Show sem) => [Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem] -> Annotations Ty -> Map (Annotation Ty) a -> ConstrGraph sem -> ConstrGraph sem
composeRewriters rws anns subst g
  = foldl (\g f -> f anns subst g) g rws
  

mergeEquivalentComponents :: (Ord sem, Show sem) => Annotations Ty -> (FlowSem sem -> Bool) -> ConstrGraph sem -> ConstrGraph sem
mergeEquivalentComponents sacredAnns edgeFilter (g :: ConstrGraph s)
  = let vs  = G.labNodes g
        es  = G.labEdges g
        es' = [ a | a@(_, _, e@(FlowEdge sems)) <- es, isFlowEdge e, any edgeFilter (Set.toList sems) ]
        reduced :: ConstrGraph s
        reduced = G.mkGraph vs es'
        components = G.scc reduced
     in unionFlowEdges (foldr reduceComponent g components)
  where
    reduceComponent nodes g
      | length nodes <= 1 || any (either (const False) (const True)) labs
          = g
      | otherwise
          = let anns = zip (map (\(Left ann) -> ann) labs) nodes
                (sacred, nonSacred)   = partition ((`Set.member` sacredAnns) . fst) anns
                (sacred', nonSacred') = if null sacred
                                        then ([head nonSacred], tail nonSacred)
                                        else (sacred, nonSacred)
                allSacred = [ (FlowEdge (Set.singleton hardFlowSem), n) | (_, n) <- sacred' ]
                allOut    = allSacred ++ [ (l, n') | n <- nodes, (n', l) <- G.lsuc g n, not (n' `elem` nodes) ]
                allIn     = allSacred ++ [ (l, n') | n <- nodes, (n', l) <- G.lpre g n, not (n' `elem` nodes) ]
                g'        = G.insNodes (map (\(a,b) -> (b, Left a)) sacred') (G.delNodes nodes g)
                g''       = G.insEdges [ (n, n', l) | (a, n) <- sacred', (l, n') <- allOut ] g'
                g'''      = G.insEdges [ (n', n, l) | (a, n) <- sacred', (l, n') <- allIn ] g''
             in g'''
      where
        labs = map (fromJust . G.lab g) nodes

dropUnusedComponents :: Ord sem => Annotations Ty -> ConstrGraph sem -> ConstrGraph sem
dropUnusedComponents sacredAnns g
  = let annMap = Map.fromList [ (a, undefined) | a <- Set.toList sacredAnns ]
        isMember g' = Map.size (getNodeMap g' `Map.intersection` annMap) > 0
     in foldr unionDisjointGraph G.empty [ g' | g' <- gComponents g, isMember g' ]

splitCompNodes :: Ord sem => FlowSem sem -> ConstrGraph sem -> ConstrGraph sem
splitCompNodes sem g
  = let nmap = getNodeMap g
        vs = G.labNodes g
        es = G.labEdges g
        (ovs, cvs) = partition (\(_, e) -> either (const True) (const False) e) vs
        (oes, ces) = partition (\(_,_, e) -> isFlowEdge e) es
        es' = concat [ [createEdge nA nB, createEdge nB nA]
                     | (_, Right (a, comp)) <- cvs
                     , b <- collect comp
                     , let nA = Map.findWithDefault (error "splitCompNodes::undefined") a nmap
                     , let nB = Map.findWithDefault (error "splitCompNodes::undefined") b nmap
                     ]
        es'' = nubEdgeList (es' ++ oes)
     in G.mkGraph ovs es''
  where
    createEdge nA nB = (nA, nB, FlowEdge (Set.singleton sem))


newtype RewriteM sem lat a
  = RewriteM (State (RewriteState sem lat) a)

data RewriteState sem lat
  = RewriteState { rsGraph    :: ConstrGraph sem
                 , rsNodeMap  :: Map (Annotation Ty) G.Node
                 , rsSubst    :: Map (Annotation Ty) lat
                 , rsWorkList :: [G.Node]
                 }

instance Monad (RewriteM sem lat) where
  (RewriteM m) >>= f = RewriteM $ do a <- m
                                     let (RewriteM m') = f a
                                     m'
  return v = RewriteM (return v)

instance MonadState (RewriteState sem lat) (RewriteM sem lat) where
  get = RewriteM get
  put = RewriteM . put

fixRewrite :: Ord sem => ConstrGraph sem -> Map (Annotation Ty) lat -> (ConstrNode -> RewriteM sem lat ()) -> ConstrGraph sem
fixRewrite g subst f
  = rsGraph (execState (state fix) init)
  where
    fix = do s <- get
             let worklist = rsWorkList s
             unless (null worklist)
               $ do let (n : ns) = worklist
                    put (s { rsWorkList = ns })
                    let mNode = rsGraph s `G.lab` n
                    when (isJust mNode)
                      $ do let node = fromJust mNode
                           f node
                           fix

    init = RewriteState { rsGraph    = g
                        , rsNodeMap  = getNodeMap g
                        , rsSubst    = subst
                        , rsWorkList = G.nodes g
                        }
    
    state (RewriteM m) = m

composeRewrites :: [ConstrNode -> RewriteM sem lat ()] -> ConstrNode -> RewriteM sem lat ()
composeRewrites rws n
  = sequence_ (map (flip ($) n) rws)

graphGather :: Ord sem => (ConstrGraph sem -> Map (Annotation Ty) G.Node -> a) -> RewriteM sem lat a
graphGather f
  = do s <- get
       let nmap = rsNodeMap s
           g    = rsGraph s
       return (f g nmap)

graphTransform :: (ConstrGraph sem -> Map (Annotation Ty) G.Node -> (ConstrGraph sem, [G.Node])) -> RewriteM sem lat ()
graphTransform f
  = do s <- get
       let ns        = rsWorkList s
           nmap      = rsNodeMap s
           g         = rsGraph s
           (g', ns') = f g nmap
       put (s { rsGraph    = g'
              , rsWorkList = ns' ++ ns
              })

mergeNodes :: Ord sem => Annotation Ty -> Annotation Ty -> RewriteM sem lat ()
mergeNodes annA annB
  = graphTransform f
  where
    f g nmap
      | annA == annB
          = (g, [])
      | otherwise
          = let nA = Map.findWithDefault (error "mergeNodes::undefined") annA nmap
                nB = Map.findWithDefault (error "mergeNodes::undefined") annB nmap
                compAs = [ (n, tup) | n <- G.neighbors g nA, let e@(Right tup) = fromJust (G.lab g n), either (const False) (const True) e ]
                g1 = foldr (subst annA annB) g compAs
                (mA, g2) = G.match nA g1
                (mB, g3) = G.match nB g2
                (Just (toA, _, _, fromA)) = mA
                (Just (toB, _, b, fromB)) = mB
                g' = (nubAdjList (toA ++ toB), nB, b, nubAdjList (fromA ++ fromB)) G.& g2
             in (g', [nB])
           where
             subst annA annB (n, (to, comp)) g
               = let g' = n `G.delNode` g
                     to' = s to
                     comp' = fmap s comp
                     g'' = G.insNode (n, Right (to', comp')) g'
                     s a | a == annA = annB
                         | otherwise = a
                  in g''

isEquivalent :: Ord sem => (FlowSem sem -> Bool) -> Annotation Ty -> Annotation Ty -> RewriteM sem lat Bool
isEquivalent p annA annB
  = graphGather f
  where
    f g nmap
      | annA == annB
          = True
      | otherwise
          = let nA = Map.findWithDefault (error "isEquivalent::undefined") annA nmap
                nB = Map.findWithDefault (error "isEquivalent::undefined") annB nmap
             in isSuc nA nB && isSuc nB nA
           where
             isSuc nA nB
               = let sucA = G.lsuc g nA
                     sucA' = [ sem | (n, e@(FlowEdge sems)) <- sucA, isFlowEdge e, sem <- Set.toList sems ]
                  in any p sucA'

%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Wrapper for constraint sets
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Wrapper for quantified constraint sets between annotations. The wrapper is used when generating
constraint sets from the abstract syntax tree. These constraint do not have a special semantics
for the flow, and the wrapper quantifies over it. The solver framework is parameterized with
semantic functions and an initial constraint set, where a concrete type is chosen.

%%[7_2

type WrappedAnnConstrSet context = WrappedSet AnnConstr context

data WrappedSet f context = WrappedSet (forall sem . Ord (f sem context) => Set (f sem context))

instance Show (WrappedAnnConstrSet context) where
  show w = let s' :: AnnConstrSet String context
               s' = unwrapSet w
            in show s'

instance PP (WrappedAnnConstrSet context) where
  pp w = let s' :: AnnConstrSet String context
             s' = unwrapSet w
          in pp s'

instance PP (Map BndgId (WrappedAnnConstrSet context)) where
  pp = Map.foldWithKey (\b s r -> show b >|< ":" >-< ("  " >|< pp s) >-< r) empty

wrapSet :: (forall sem . Ord (f sem context) => Set (f sem context)) -> WrappedSet f context
wrapSet = WrappedSet

unwrapSet :: Ord (f sem context) => WrappedSet f context -> Set (f sem context)
unwrapSet (WrappedSet s) = s

wrappedSetUnion :: WrappedSet f context -> WrappedSet f context -> WrappedSet f context
wrappedSetUnion a b = wrapSet (unwrapSet a `Set.union` unwrapSet b)
 
wrappedSetEmpty :: WrappedSet f context
wrappedSetEmpty = wrapSet Set.empty

wrappedSingleton :: (forall sem . Ord (f sem context) => f sem context) -> WrappedSet f context
wrappedSingleton x = wrapSet (Set.singleton x)

wrappedFromList :: (forall sem . Ord (f sem context) => [f sem context]) -> WrappedSet f context
wrappedFromList xs = wrapSet (Set.fromList xs)

unwrapMap :: (Ord (f sem context), Ord k) => Map k (WrappedSet f context) -> Map k (Set (f sem context))
unwrapMap m = Map.fromAscList [ (k, unwrapSet s) | (k, s) <- Map.toAscList m ]

%%]
