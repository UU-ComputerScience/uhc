\section{Simplification graphs}
\label{s:simplification}
%{
%format intersect = "intersect"
%format `elem` = "`elem`"

% How can we represent alternatives?
% How can we generate code?
% How can we express sharing?
% Evidence/Sharing/Alternatives
%
% What is a simplification graph?       
% Why do we need simplification graphs? Sharing, Alternatives, Evidence 
% How are prove + assume constraints represented in the graph, or how does the framework interact with the graph? 
% How are reductions represented in the graph, or how does simplification interact with the graph?

In this section we explain how we represent the information needed to generated evidence in a graph.
The reductions and predicates represented in this graph are shared as much as possible.
Also different context-reduction alternatives can be expressed in the graph.
In this section we first explain the structure of a graph.
Then we show how graphs are used in the framework.
This section and the following section present a literate Haskell module for the second version of the framework:

\begin{hide} 

> module SolverV2 (solveConstraints, simplify, h98Heuristic, ghcHeuristic) where
 
\end{hide}
 
> import AGraph 
> import CHRSolver
>
> import Control.Monad.State
> import Data.List   (nub, maximumBy)
> import qualified Data.Map  as Map (Map, empty, insertWith, foldWithKey, keys, assocs) 

Two imported modules deserve special attention.
The module |AGraph| is a wrapper module for the functional graph library~\citep{erwig-inductive} and the module |CHRSolver| is the Haskell CHR solver introduced in Section~\ref{s::chrsolver}.

\subsection{Graph representation}
We represent reductions and information needed to generate evidence in a directed graph.
A directed graph |G| is an ordered pair |G = (V,A)| with
\begin{myitemize}
  \item A set of nodes |V|, and 
  \item a set of pairs of nodes |A|, representing directed edges.
\end{myitemize}
The idea behind simplification graphs is that nodes represent predicates and edges between nodes represent reductions.
Edges are annotated with meta-information, such as unique identifiers and error messages.
Again, consider the function |testInsert|, but now with a restricting type signature:
\begin{spec}
 testInsert :: (Sub ((Ord a)) a1) => (Int, a) -> [(Int, a)] -> Bool
 testInsert x xs =  let  ys = (Sub insert p1) x ((Sub sort p2) xs)
                    in   (Sub sort p3) ys (Sub (==) p4) ys
\end{spec}
The type signature of this function is skolemized with a fresh type constant, say |c1|.
After analyzing the function body we have to solve the constraints |Assume (Ord c1)| occurring at position |a1|,  |Prove (Ord (Int, c1))| at positions |p1, p2, p3|, and |Prove (Eq [(Int, c1)])| at position |p4|.
Solving these constraints results in the simplification graph presented in Figure~\ref{repgraphs}.
There is one special type of node besides the node for predicates:

\begin{Figure}{Simplification graph for the function |testInsert| with a restricting signature}{repgraphs}
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr1.pdf}
\end{center}
\end{Figure}

> data Node p  =  Pred p
>              |  And [p]
>              deriving (Eq, Ord)
>
> instance Show p => Show (Node p) where
>   show (Pred p)    = show p
>   show (And [])    = "True"
>   show (And _ )    = "And"

Multiple outgoing edges from the |Pred| node represent different reduction alternatives.
For example, in Figure~\ref{repgraphs} we see that there are two alternatives for reducing the predicate |Eq (Int, c1)|.
As opposed to the |Pred| node, multiple outgoing edges from an |And| node means that each edge is needed to resolve the predicate.
For example, the predicate |Ord(Int, c1)| is reduced to |Ord Int| and |Ord c1| in Figure~\ref{repgraphs}.
The list of predicates needed by the |And| constructor seems to be unnecessary, because this information is already available in the graph.
However, this list of predicates is needed to remember the order of the predicates we reduce to. 
For example, we must remember which predicate is needed for which component of a tuple. 
An |And| node without outgoing edges means that overloading is resolved at this point.
We abbreviate this special case of |And| with |true|.

> true  ::  Node p
> true  =   And []

We use the inductive graph library~\citep{erwig-inductive} to represent graphs in Haskell.
However, in this context it is not very interesting to explain the interface of this library.
Therefore, we present the wrapper module |(AGraph)| exporting only the following functions:
\begin{spec}
 emptyAGraph :: Ord a => AGraph a b

 insertEdge   :: Ord a =>    (a, a, b)    -> AGraph a b -> AGraph a b
 insertEdges  :: Ord a => [  (a, a, b)]   -> AGraph a b -> AGraph a b
 deleteEdge   :: Ord a =>    (a, a)       -> AGraph a b -> AGraph a b
 
 successors, predecessors :: Ord a => AGraph a b -> a -> [(b, a)]
\end{spec}
|AGraph| stands for A(nnotated) Graph. 
Such a graph consists of two types of annotations: annotations on nodes |(a)| and annotations on edges |(b)|.
The function |insertEdge| inserts an edge between nodes and inserts the nodes if they are not already present.
The function |deleteEdge| deletes all directed edges from the first node to the second node.
With the function |successors| and |predecessors| it is possible to inspect the graph.
In this section we use the following type synonym for |AGraph|'s of |Node|'s:

> type Graph p info = AGraph (Node p) info

\subsection{Representation of constraints}
In the previous section we have presented a third constraint beside the |Prove| and |Assume| constraint:

> data Constraint p info  =  Prove      p
>                         |  Assume     p
>                         |  Reduction  p info [p]
>                         deriving (Eq, Ord)

The |Reduction| constraint is a special constraint because it is only used internally; a user of the framework is not allowed to use it.
|Reduction| constraints result from solving |Prove| and |Assume| constraints with CHRs and are used to construct simplification graphs.
Furthermore, the |Reduction| constraint is the only constraint with an |info| component.
This |info| component can be used to store meta-information, such as unique identifiers and error messages.
We also make the constraint language an instance of the type class |Matchable|.

> instance (Matchable p s, Ord info) => Matchable (Constraint p info) s where
>   match (Prove  p) (Prove  q)      = match p q
>   match (Assume p) (Assume q)      = match p q
>   match _           _              = Nothing
> 
>   subst s      (Prove  p)          = Prove   (subst s p)
>   subst s      (Assume p)          = Assume  (subst s p)
>   subst s      (Reduction p i ps)  = Reduction (subst s p) i (map (subst s) ps)

To generate evidence and to represent information needed to generate evidence we have to extend the state of the solver:

> data SolveState p s info = 
>   SolveSt  { rules        :: [CHR (Constraint p info) s]
>            , heuristic    :: Heuristic    p info
>            , constraints  :: Constraints  p info
>            , evidence     :: EvidenceMap  p info
>            }
>
> type Constraints  p info = Map.Map (Constraint p info) [info]
> type EvidenceMap  p info = Map.Map info (Evidence p info)
>
> modifyConstraints  f = modify (\s -> s { constraints  = f (constraints  s)})
> modifyEvidence     f = modify (\s -> s { evidence     = f (evidence     s)})


The state is extended with two components: a heuristic and an evidence map.
In the next section we explain how the heuristic and the evidence map are used.
Furthermore, the constraint set in the state is replaced by a map from constraints to occurrences of constraints.
Evidence has to be generated for each occurrence of a constraint.
Each occurrence of a constraint is tupled with information uniquely identifying the constraint occurrence.
For example, a unique identifier may serve this purpose.
Additionally, location information for type error messages could be included as well.
Solving a list of constraints just consists of mapping the function |solveConstraint| in the monad:

> solveConstraints ::  (MonadState (SolveState p s info) m, Matchable p s, Ord info)
>                      => [(Constraint p info, info)] -> m ()
> solveConstraints        = mapM_ solveConstraint
>
> solveConstraint (c, i)  = modifyConstraints (Map.insertWith (++) c [i])

The function |solveConstraint| expects a constraint tupled with information.
A constraint is inserted into the constraint map together with the information packed into a singleton list.
Information lists are concatenated when a constraint is already in the map.
This means that there can be multiple occurrences of the same constraint.
There are a number of advantages of representing constraints annotated with information in a map.
For instance, constraints that occur multiple times have to be solved only once.
Furthermore, using the constraint map and the simplification graph we may share evidence.

\subsection{Simplification of constraints}
\label{ss:simplification}
Everything comes together when simplifying constraints.
Simplification can be applied at any stage during the type inference process, just as with the previous version of the framework.
The difference is that we now only use CHRs to generate |Reduction| constraints.
These |Reduction| constraints are represented in a simplification graph and a heuristic solves constraints by choosing a solution.
To illustrate simplification, consider the function |testInsert| again:
\begin{spec}
 testInsert :: (Sub ((Ord a)) a1) => (Int, a) -> [(Int, a)] -> Bool
 testInsert x xs =  let  ys = (Sub insert p1) x ((Sub sort p2) xs)
                    in   (Sub sort p3) ys (Sub (==) p4) ys
\end{spec}
The following list of constraints is generated for the function |testInsert|: 
\begin{spec}
 [ (Assume  (Ord c1)         , a1),  (Prove (Ord (Int, c1))   , p1), (Prove (Ord (Int, c1)), p2)
 , (Prove   (Ord (Int, c1))  , p3),  (Prove (Eq [(Int, c1)])  , p4)]
\end{spec}
Solving the above list of constraints result in the following constraint map:
\begin{spec}
 {Assume (Ord c1) mapsto [a1], Prove(Ord (Int, c1)) mapsto [p1, p2, p3], Prove (Eq [(Int, c1)]) mapsto [p4]}
\end{spec}
Simplification of these constraints is performed with the function |simplify|:

> simplify ::  (MonadState (SolveState p s info) m, Matchable p s, Ord info) => m ()
> simplify =
>   do  chrs      <- gets rules
>       cnstrs    <- gets constraints  
>       let  initGraph   = Map.foldWithKey addAssumption emptyAGraph cnstrs
>            reductions  = chrSolveList chrs (Map.keys cnstrs)
>            graph       = foldr addReduction initGraph reductions
>       modifyConstraints (const Map.empty)                             
>       mapM_ (constructEvidence graph) (Map.assocs  cnstrs)

First, |Assume| constraints are added to the empty graph by folding the following function over the constraint map:

> addAssumption :: Ord p => Constraint p info -> [info] -> Graph p info -> Graph p info
> addAssumption (Assume  p)  is  = insertEdges (zip3 (repeat (Pred p)) (repeat true) is) 
> addAssumption _            _   = id

An |Assume| constraint is represented in the graph as an edge from the assumed predicate to the |true| node. 
An edge is inserted for each occurrence of an assumption.
For example, the assumption for |Ord c1| annotated with |a1| is present in Figure~\ref{repgraphs}.
The next step is to generate |Reduction| constraints by solving the constraints using CHRs.
The resulting reductions are inserted into the graph by folding the following function over the set of constraints:

> addReduction :: Ord p => Constraint p info -> Graph p info -> Graph p info
> addReduction (Reduction p i [q])  =  insertEdge (Pred p, Pred q  , i)
> addReduction (Reduction p i ps)   =  let  andNd  = And ps
>                                           edges  = map (\q -> (andNd, Pred q, i)) ps
>                                      in   insertEdges ((Pred p, andNd, i) : edges)
> addReduction _                    =  id

The function |addReduction| consist of three cases:
The first case is not required, but improves the readability of the graphs by not inserting intermediate |And| nodes.
In the second case we add a list of edges: one edge from the predicate to an |And| node and |n| edges from the |And| node to the predicates we reduce to.
Nothing is done for other constraints in the third case.
|Prove| and |Assume| constraints propagated by CHRs are discarded.
The following reductions are generated when solving the constraints resulting from the function |testInsert|:
\begin{spec}
 {  Reduction (Ord  (Int, c1))    "ordTuple"  [Ord Int, Ord c1],  Reduction (Eq   Int)  "eqInt"   []
 ,  Reduction (Eq   [(Int, c1)])  "eqList"    [Eq (Int, c1)],     Reduction (Ord  Int)  "ordInt"  [] 
 ,  Reduction (Eq   (Int, c1))    "eqTuple"   [Eq Int, Eq c1],    Reduction (Eq   c1)   "eqOrd"   [Ord c1]
 ,  Reduction (Eq   (Int, c1))    "eqOrd"     [Ord (Int, c1)],    Reduction (Eq   Int)  "eqOrd"   [Ord Int]  
 }
\end{spec}
Inserting these reductions result in the graph presented in Figure~\ref{repgraphs}.
The last step of the simplification function is to generated evidence for each entry in the constraint map using the function |constructEvidence|.
The function |constructEvidence| determines which constraints are solved and which constraints remain unresolved using the generated simplification graph and heuristics.
Therefore we initialize the constraint map to the empty map so that the function |constructEvidence| can add the remaining constraints.
In the following section we explain how heuristics are formulated and how the function |constructEvidence| is implemented.


\section{Heuristics}
\label{s:heuristics}
% What are heuristics? why do we need heuristics?
% What can we do with heuristics?
%
% alternatives -> heuristic -> evidence
% 
% haskell98 heuristic
% ghc heuristic
The simplification graphs presented in the previous chapter contain the information needed to construct evidence.
However, in order to generate evidence we have to make choices between different context reduction alternatives.
We have chosen to isolate such choices in heuristics.
In this section we first explain how we have formulated heuristics.
Then we show the implementation of heuristics in the solver.
Finally, we present some example heuristics to show how different context-reduction strategies can be emulated and how overlapping instances are resolved.

\subsection{Formulation of heuristics}
The task of a heuristic is to choose between context reduction alternatives. 
For example, a choice between overlapping instances, scoped instances, or superclass versus instance. 
However, in some cases a heuristic must also be able to stop context reduction.
In such a situation no alternative is chosen, even if there exist alternatives.
For example, consider the function |testInsert|, now without a type signature:

\begin{spec}
 testInsert x xs =  let  ys = (Sub insert p1) x ((Sub sort p2) xs)
                    in   (Sub sort p3) ys (Sub (==) p4) ys
\end{spec}

\begin{Figure}{Simplification graphs for the function |testInsert|}{graphsheur}
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr2.pdf}
\end{center}
\end{Figure}

The constraints |Prove (Ord v1)| at positions |p1, p2, p3| and |Prove (Eq [v1])| at position |p4| are generated for this function.
Simplifying those constraints result in the leftmost graph of Figure~\ref{graphsheur}.
GHC~\citep{Ghc} infers the type |(Ord a, Eq [a]) => a -> [a] -> Bool| for the above function whereas Haskell~98 compilers would infer the type |Ord a => a -> [a] -> Bool|.
GHC stops context reduction at a certain point to maximize the possibilities for using overlapping instances. 
Furthermore, it is not always the case that the same choices are made for all the occurrences of a predicate.
For example, the programmer could annotate an occurrence of a predicate to choose a different context reduction strategy.

Complicated heuristics could be applied to the simplification graph to maximize sharing.
However, we choose to generate evidence for each |Prove| constraint separately and thereby not sharing intermediate computations.
A heuristic has the following type and is part of the state of the solver:

> type Heuristic p info = [info] -> Alts p info -> [(info, Evidence p info)]

A heuristic is a function that gets a list of the occurrences of the predicate as first parameter.
The second argument of this function is a datatype representing the reduction alternatives for the predicate we must prove:

> data Alts  p  info = Alts  { predicate  :: p,       alts     :: [Red p info]    }
> data Red   p  info = Red   { info       :: info,    context  :: [Alts p info]   }

The datatype |Alts| consists of a predicate that must be proven and a list of reduction alternatives (|alts|).
A heuristic can decide to stop context reduction or to choose one of these reduction alternatives.
The datatype |Red| consists of the information annotated on the edges of the graph and the list of predicates we reduced to.
A value of this datatype is extracted from a simplification graph in the following way:

> alternatives :: Ord p => Graph p info -> p -> Alts p info
> alternatives gr = recOr
>   where  recOr   p       = Alts  p  (map recAnd  (successors gr (Pred p))) 
>          recAnd  (i, n)  = Red   i  (map recOr   (preds n))
>          preds  n  =   case n of
>                          Pred  q   -> [q]
>                          And   qs  -> qs

The result of a heuristic is an association list mapping occurrences of predicates to evidence.
Evidence has the shape of a tree:

> data Evidence  p info  =   Proof p  info [Evidence p info] 
>                        |   Unresolved p
>
> unresolved :: Eq p => Evidence p info -> [p]
> unresolved (Unresolved p)  = [p]
> unresolved (Proof _ _ ps)  = nub (concatMap unresolved ps)

A |Proof| means that a predicate |p| can be constructed from a list of evidence.
Furthermore, a |Proof| is annotated with information.
However, it is not always possible to construct evidence at once.
Sometimes there are still proof obligations left. 
The |Unresolved| constructor is used when we are not (yet) able to prove a predicate |p|.
The function |unresolved| returns the predicates that are unresolved in an evidence tree.
The following function updates unresolved predicates if later in the type-inference process more information about these predicates is acquired:

> updateUnresolved :: Eq p => Evidence p info -> Evidence p info -> Evidence p info
> updateUnresolved e                 (Unresolved _)  = e
> updateUnresolved (Proof p i qs)    e               = Proof p i [updateUnresolved q e | q <- qs] 
> updateUnresolved u@(Unresolved q)  e@(Proof p _ _)
>            | q == p     = e
>            | otherwise  = u

The first parameter of this function is evidence that is updated and the second parameter is evidence that is inserted.

\subsection{Application of heuristics}
In the section~\ref{s:simplification} we have presented the simplification function. 
The last step of this function is to map the function |constructEvidence| over the constraint map.
The function |constructEvidence| generates evidence for the different occurrences of the constraint by using a heuristic to choose a solution from the simplification graph:

> constructEvidence ::  (MonadState (SolveState p s info) m, Matchable p s, Ord info)
>                       => Graph p info -> (Constraint p info, [info]) -> m ()
> constructEvidence graph  (Prove p, infos)  =
>   do  hrstc <- gets heuristic
>       let  trees  = hrstc infos (alternatives graph p)
>       modifyEvidence    (\em -> foldr insertEvidence em trees)      
>       solveConstraints  (concatMap remaining trees)
>
> constructEvidence _      (c, infos)        =
>       solveConstraints  (zip (repeat c) infos)

The above function generates evidence for |Proof| constraints. 
Other constraints are immediately inserted into the constraint map with the function |solveConstraints|.
To construct evidence, the heuristic is fetched from the state and bound to an identifier.
In the next step, the heuristic is applied to the occurrences and the alternatives for reducing predicate |p|.
The result of the heuristic is an association list from occurrences to evidence. 
The evidence is inserted in the map by folding the following function over the result of the heuristic:

> insertEvidence ::  (Eq p, Ord info) 
>                    => (info, Evidence p info) -> EvidenceMap p info -> EvidenceMap p info
> insertEvidence = uncurry (Map.insertWith updateUnresolved)
>
>
> remaining :: Eq p => (info, Evidence p info) -> [(Constraint p info, info)]
> remaining (i, tree) = zip (map Prove (unresolved tree)) (repeat i)

Evidence is updated with the function |updateUnresolved| if there already is an entry for |info| in the map.
Finally, |Prove| constraints are generated for the unresolved predicates in the evidence trees with the function |remaining|.
Those constraints are solved with the function |solveConstraints| we have discussed earlier in this chapter.

\subsection{Haskell~98 heuristic}
\label{ss::haskell98heur}
In Haskell~98~\citep{jones03haskell} the context of a type must be in head normal form (HNF).
This means that the type mentioned in a predicate may only consist of a type variable or the application of a type variable to one or more types.
For instance, the predicates in the context of the following type are in HNF:
\begin{spec}
 test :: (Monad m, Eq (m Char)) => m Char -> Bool
\end{spec}
However, the predicate in the following type is not in HNF:
\begin{spec}
 test :: Eq (Maybe a) => Maybe a -> Bool 
\end{spec}
Furthermore, different occurrences of the same predicate always have the same solution in Haskell~98.
Therefore, we introduce a special type synonym for such heuristics together with a conversion function:

> type SimpleHeuristic p info = Alts p info -> Evidence p info
>
> toHeuristic :: SimpleHeuristic p info -> Heuristic p info
> toHeuristic h infos alts = zip infos (repeat (h alts))

A simple heuristic does not depend on information attached to the occurrences of a predicate.
Furthermore, a simple heuristic can be converted to a heuristic by just repeating the solution for each occurrence of the predicate.
A heuristic must choose at each node between |i| different choices for each of the |j| following nodes.
In the worst case the heuristic has to choose between $i^j$ choices.
We call |j| the {\it branching factor}.
In general want to keep the branching factor as small as possible.
Simple heuristics with the smallest branching factor |(1)| can be defined with the following function:

> localChoice :: Eq info => (p -> [info] -> [info]) -> SimpleHeuristic p info  
> localChoice choose (Alts p reds) = 
>   let  redinfos  = choose p (map info reds)
>   in   case filter ((`elem` redinfos) . info) reds of
>          []            -> Unresolved p
>          [(Red i rs)]  -> Proof p i (map (localChoice choose) rs)
>          _             -> error "Alternatives left"

Besides the fact that the heuristic for Haskell~98 can be expressed as a local heuristic, it also has the property that the different reduction alternatives form a total order.
This means that each pair of alternatives can be compared for ordering.
Therefore we introduce a function that creates a simple heuristic from an ordering function:

> binChoice :: Eq info => (info -> info -> Ordering) -> SimpleHeuristic p info
> binChoice order = localChoice (const local)
>   where  local []  = []
>          local is  = [maximumBy order is]

We annotate the different constraints with values of the following datatype:

> data Annotation  =  ByInstance    String
>                  |  BySuperClass  String
>                  |  ProveObl      Int
>                  |  Assumption    Int
>                  deriving (Eq, Ord)

The constructors |ByInstance| and |BySuperclass| are used to annotate the CHRs generated for instance and class declarations respectively.
The constructor arguments of type String identify the corresponding dictionaries or dictionary transformers.
|Prove| constraints are annotated with |ProveObl| and |Assume| constraints are annotated with |Assumption|.
The integer arguments uniquely identify occurrences of |Prove| and |Assume| constraints. 

Using these annotations we can define the following heuristic for Haskell~98:

> haskell98 :: Annotation -> Annotation -> Ordering
> haskell98 (ByInstance    _)  _                  =  GT
> haskell98 _                  (ByInstance    _)  =  LT
> haskell98 (BySuperClass  _)  _                  =  GT
> haskell98 _                  (BySuperClass  _)  =  LT
> haskell98 (Assumption    _)  _                  =  GT
> haskell98 _                  (Assumption    _)  =  LT
> haskell98 (ProveObl      _)  _                  =  GT
> haskell98 _                  (ProveObl      _)  =  LT
>
> h98Heuristic :: Heuristic p Annotation
> h98Heuristic = toHeuristic (binChoice haskell98)

Predicates in Haskell~98 compilers are first simplified using instances.
This will ensure that the predicates are solved or that they are simplified to predicates in HNF.
If possible, the predicates are simplified using the class hierarchy and then using an assumption.
Note that Haskell~98 dictates that assumptions must always be in HNF.

Recall the graph generated for the function |testInsert| presented in Figure~\ref{graphsheur}.
The predicate |Ord v1| in this graph cannot be simplified further and remains.
However, |Eq [v1]| can be simplified to |Ord v1| and the Haskell~98 heuristic chooses for this simplification.
This will result in the following state of the solver:
\begin{spec}
 constraints  =  {  Prove (Ord v1) mapsto [p1,p2,p3,p4]  }
 evidence     =  {  p1,p2,p3  mapsto Unresolved (Ord v1)
                 ,  p4 mapsto Proof (Eq [v1]) "eqList" [Proof (Eq v1) "eqOrd" [Unresolved (Ord v1)]]} 
\end{spec}
In a compiler this is usually the point where generalization is performed.
The result is that the remaining proof obligations concerning the generalized type are assumed to hold.
In this example, the constraint |(Assume (Ord v1), a1)| is added.
Simplifying the new set of constraints result in the graph in the middle of Figure~\ref{graphsheur}.
After constructing more evidence the state of the solver will be: 
\begin{spec}
 constraints  =  {  Assume (Ord v1) mapsto [a1]  }
 evidence     =  {  p1,p2,p3  mapsto Proof (Ord v1)   "a1"      []
                 ,  p4 mapsto Proof (Eq [v1])  "eqList"  [Proof (Eq v1) "eqOrd" [Proof (Ord v1) "a1" []]]    } 
\end{spec}
It is easy to see that this result can be used to generate the following translated version of |testInsert|:
\begin{spec}
 testInsert a1 x xs =  let  ys = insert a1 x (sort a1 xs)
                       in   ((==) (eqList (eqOrd a1))) (sort a1 ys) ys
\end{spec}

\subsection{GHC heuristic}
As we have mentioned earlier, GHC~\citep{Ghc} utilizes another context reduction strategy than the one Haskell~98 dictates.
This deviation from Haskell~98 is needed to support various extensions in GHC such as overlapping instances and arbitrary contexts in types and type signatures~\citep{jones97type}.
Haskell~98 compilers reduce the context as far as possible before generalization. 
In contrast, GHC delays context reduction using instance declarations as long as possible.
A predicate is only reduced when it can be resolved locally (tautological predicate) or when forced by a type signature.
GHC first tries to reduce predicates using the following local heuristic:

> ghcBinSolve :: Annotation -> Annotation -> Ordering
> ghcBinSolve (Assumption    _)  _                  = GT
> ghcBinSolve _                  (Assumption    _)  = LT
> ghcBinSolve (BySuperClass  _)  _                  = GT
> ghcBinSolve _                  (BySuperClass  _)  = LT
> ghcBinSolve (ByInstance    _)  _                  = GT
> ghcBinSolve _                  (ByInstance    _)  = LT
> ghcBinSolve (ProveObl      _)  _                  = GT
> ghcBinSolve _                  (ProveObl      _)  = LT
>
> ghcSolve :: Eq p => SimpleHeuristic p Annotation
> ghcSolve = binChoice ghcBinSolve

The most notable difference between the heuristics of Haskell~98 and GHC is the way they reduce predicates using instance declarations.
Haskell~98 reduces predicates first with instance declarations, but GHC reduces predicates only with instance declarations if there are no other alternatives left.
The reason that this is possible is that GHC allows arbitrary contexts in type signatures. 
If GHC would first reduce using instances, it could possibly not find assumptions introduced by type signatures.
GHC uses another heuristic when there are still |Unresolved| nodes in the solution found by the |ghcSolve| heuristic:

> ghcLocalReduce :: a -> [Annotation] -> [Annotation]
> ghcLocalReduce _  reds =  let  p (BySuperClass _)  = True
>                                p _                 = False
>                           in   filter p reds
>
> ghcReduce :: Eq p => SimpleHeuristic p Annotation
> ghcReduce = localChoice ghcLocalReduce

This heuristic only reduces a predicate using the class hierarchy.
Context reduction is stopped if context reduction using the class hierarchy is not possible even when there are other alternatives.
We introduce the |try| combinator to combine both heuristics:

> try :: Eq p => SimpleHeuristic p info -> SimpleHeuristic p info -> SimpleHeuristic p info
> try f g a  | null (unresolved e)    = e
>            | otherwise              = g a
>            where e = f a

The |try| combinator applies the first heuristic and only when there are unresolved nodes in the result, the second heuristic is applied.
Now we are able to construct a heuristic that emulates the context-reduction behavior of GHC:

> ghcHeuristic :: Eq p => Heuristic p Annotation
> ghcHeuristic =  toHeuristic $
>                   try  ghcSolve
>                        ghcReduce

The |ghcHeuristic| still has a branching factor of one because the two sub heuristics both have a branching factor of one.
Two passes are needed for this heuristic to find a solution, but the pass of the second heuristic will never evaluate more reduction alternatives than the first pass.
Using this heuristic for resolving overloading in the function |testInsert| results in the rightmost graph of Figure~\ref{graphsheur} after generalization.
Using this heuristic results in the following translated version of |testInsert|: 
\begin{spec}
 testInsert (a2, a3) x xs =  let  ys = insert a2 x (sort a2 xs)
                             in   ((==) a3) (sort a2 ys) ys
\end{spec}

\subsection{Overlapping instances heuristic}
Until now we did not explain how to resolve overlapping instances using this framework.
We have chosen to encode overlapping instances in heuristics instead of using guards in CHRs.
Here we explain how to extend a local heuristic to support overlapping instances.
First, we need to encode more information into the annotation for instances:
\begin{spec}
 data Annotation p  =  ByInstance    String   p
                    |  ...
\end{spec}
We add the head of an instance declaration to the annotation.
The most specific instance is chosen when multiple instances are applicable for simplifying a predicate.
Note that the context of an instance declaration is not used in this process.
The following function determines what the most specific predicate is:

> specificness :: Matchable c s => c -> c -> Ordering
> specificness p q = 
>   case  match p q of 
>         Nothing  -> LT  
>         Just _   -> case  match q p of
>                           Nothing  -> GT
>                           Just _   -> error "no most specific instance"

This function is defined in terms of the one way unification function |match|.
More about this function can be found in Section~\ref{s::chrsolver}.
The predicate |q| is more specific than |p| if |p| does not match with |q|.
For example, |Eq [[a]]| is more specific than |Eq [a]| because there is no substitution to get |Eq [a]| from |Eq [[a]]|.
The most specific instance cannot be determined if the predicate |p| matches |q| and the other way around.
This is for example the case with the predicates |Eq (a, Int), Eq (Int, a)|.

Finally, a heuristic can easily be extended to support overlapping instances.
For example, by adding the following case to the Haskell~98 heuristic:
\begin{spec} haskell98 :: Annotation p -> Annotation p -> Ordering
 haskell98 (ByInstance _  p)  (ByInstance _  q)  =  specificness p q
 haskell98 (ByInstance _  _)  _                  =  GT
 haskell98 _                  (ByInstance _  _)  =  LT
 haskell98 ...                                   =  ...
\end{spec}

\section{Conclusion}
In this chapter we have shown how our framework supports the translation of a program with overloading into a program without overloading.
Furthermore, we have presented different heuristics to show how easy it is to experiment with different design decisions.
To achieve this we trace CHR derivations using reduction constraints.
However, CHR derivations can sometimes be non-deterministic or even non-confluent.
We avoid these problems by generating every type-correct reduction alternative for solving a constraint.
From these reduction constraints we construct simplification graphs.
These graphs enable us to share predicates as much as possible and to represent different reduction alternatives.
Another advantage of these graphs is that they nicely visualize the problem. 
We use heuristics to choose a solution from a simplification graph for each |Prove| constraint.
With these heuristic we are able to emulate Haskell~98 and GHC context reduction.
We also show that heuristics can easily be extended to support type class extensions such as overlapping instances.

%}
