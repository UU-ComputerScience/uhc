%{
%format not = "not"
%format intersect = "intersect"
\section{Implementation}
We have described a constraint language for specifying the overloading problem.
Furthermore, we have shown how CHRs can be used to check entailment.
This section describes the implementation of those ideas in our first version of the framework.
We explain how we represent the constraint language in Haskell.
Then we present a systematic translation from class and instance declarations into CHRs.
Finally, we show how constraints are solved using CHRs.

This section is also a literate Haskell module:
\begin{code}
 import CHRSolver
 import Control.Monad.State
 import qualified Data.Set  as Set
 import Data.List (intersect)
\end{code}
Besides some standard Haskell libraries we import the CHR solver presented in the previous section.

\subsection{Constraint language}
A constraint is either a proof obligation or an assumption:
\begin{code}
 data Constraint p   =   Prove    p   |   Assume   p
                         deriving (Eq, Ord)

 instance Functor Constraint where
   fmap f (Prove   p) = Prove   (f p)
   fmap f (Assume  p) = Assume  (f p)
\end{code}
The type variable |p| represents the type of the predicate.
We also make the type |Constraint| an instance of |Functor| so we can easily map functions over a |Constraint|.
Furthermore, we make |Constraint| an instance of |Matchable|:
\begin{code}
 instance Matchable p s => Matchable (Constraint p) s where
   match (Prove  p)  (Prove  q)     = match p q
   match (Assume p)  (Assume q)     = match p q
   match _           _              = Nothing

   subst s      (Prove  p)          = Prove   (subst s p)
   subst s      (Assume p)          = Assume  (subst s p)
\end{code}
|Prove| and |Assume| constraints only match when the predicates in these constraints match.
So we require that matching is defined on the type of the predicate language. 

\subsection{Translation to CHRs}
For the resolution of overloading we have to translate class and instance declarations into CHRs.
Class and instance declaration are translated into:
\begin{enumerate}
\item CHRs for propagating the class hierarchy and removing entailed constraints.
\item CHRs for simplifying constraints using the class hierarchy.
\item CHRs for simplifying constraints using instance declarations.
\end{enumerate}

We introduce some type synonyms before presenting the systematic translation:
\begin{code}
 type Rule p s = CHR (Constraint p) s
\end{code}
The type synonym |Rule| is a CHR on the earlier defined constraint language.
We also introduce type synonyms for instance and class declarations:
\begin{code}
 type ClassDecl     p = ([p], p)
 type InstanceDecl  p = ([p], p)
\end{code}
Both are tuples where the first component is the context and the second component is the head of the declaration.

\subsubsection*{Step 1: CHRs for propagating the class hierarchy and removing entailed constraints.}
We start with the translation from a class declaration into CHRs:
\begin{code}
 genAssumeChrs :: Matchable a s => ClassDecl a -> [Rule a s]
 genAssumeChrs (context, head) =
   let  solve      = [Assume head, Prove head] <=> [Assume head]
        propSuper  = [Assume head] ==> map Assume context
   in   [solve, propSuper]
\end{code}
For each class declaration we generate a simplification CHR.
This simplification removes a |Prove| constraint if there is an assumption in the set for the same predicate.
We do not remove the assumption, because it can be used to solve another |Prove| constraint.
Furthermore, a propagation CHR is generated that propagates assumptions for the context of a class declaration if there is an assumption for the head.
The simplification rule is generated for each class declaration, but in fact we could use the general rule: |Prove p, Assume p <=> Assume p|.
Adding such a rule would introduce an additional substitution from variables to predicates and because we do not need this rule in the remainder of this thesis we omit it for simplicity.
The function application |genAssumeChrs ([Eq a, Show a], Num a)| results from translating the class declaration |class (Eq a, Show a) => Num a|. 
This function application evaluates to the following list of CHRs:
\begin{spec}
 [  Assume (Num a), Prove (Num a) <=> Assume (Num a)
 ,  Assume (Num a) ==> Assume (Eq a), Assume (Show a) ]
\end{spec}

\subsubsection*{Step 2: CHRs for simplifying constraints using the class hierarchy.}
The propagation rules generated for the class declarations encode the class hierarchy.
In this step we use the rules of step 1 to generate CHRs for the simplification of |Prove| constraints using the class hierarchy.
\begin{code}
 genClassChrs :: Matchable a s => [ClassDecl a] -> [Rule a s]
 genClassChrs clsDecls =
   let  assumeChrs  = concatMap genAssumeChrs clsDecls
        simplChrs   = concatMap (genClassSimplChrs assumeChrs) clsDecls
   in   assumeChrs ++ simplChrs

 genClassSimplChrs :: Matchable a s => [Rule a s] -> ClassDecl a -> [Rule a s]
 genClassSimplChrs rules (context, head) =
   let  superClasses = chrSolveList rules (map Assume context)
        simpl (Assume sClass) = [Prove head, Prove sClass] <=> [Prove head]
   in   if elem (Assume head) superClasses
        then  error ("Cyclic class hierarchy")
        else  map simpl superClasses
\end{code}
We perform the following steps for each class declaration:
First we translate the predicates in the context into |Assume| constraints.
Then the assume constraints are solved using the rules for the propagation of the class hierarchy.
Solving those constraints result in a constraint set consisting of all superclasses of the predicate |head|.
The class hierarchy is cyclic if the head of the class declaration is an element of the superclass set.
If this is not the case, a simplification CHR is generated for each class in this set.
In this way we obtain a confluent rule set while checking if the class hierarchy is acyclic.
For example, consider the following application of the function |genClassSimplChrs| for the class: |class (Ord a, Num a) => Real a|.
\begin{spec}
 genClassSimplChrs
    [  Assume (Real  a) ==> Assume (Num  a), Assume (Ord   a)
    ,  Assume (Num   a) ==> Assume (Eq   a), Assume (Show  a)
    ,  Assume (Ord   a) ==> Assume (Eq   a) ]
    ([Ord a, Num a], Real a)
\end{spec}
Evaluating this expression results in: 
\begin{spec}
  [  Prove (Real a), Prove (Ord   a) <=> Prove (Real a)
  ,  Prove (Real a), Prove (Num   a) <=> Prove (Real a)
  ,  Prove (Real a), Prove (Eq    a) <=> Prove (Real a)
  ,  Prove (Real a), Prove (Show  a) <=> Prove (Real a)  ]
\end{spec}

\subsubsection*{Step 3: CHRs for simplifying constraints using instance declarations.}
To simplify |Prove| constraints using instances we generate the following CHR for each instance declaration:
\begin{code}
 genInstanceChrs :: Matchable a s => [InstanceDecl a] -> [Rule a s]
 genInstanceChrs =
   let  genSimpl (context, head) = [Prove head] <=> map Prove context
   in   map genSimpl
\end{code}
For each instance we generate a rule where the head of the instance is simplified into the context of the instance.
Finally, we concatenate the rules for class and instance declarations:
\begin{code}
 genChrs :: Matchable a s => [ClassDecl a] -> [InstanceDecl a] -> [Rule a s]
 genChrs classes insts =
   let  classChrs  = genClassChrs classes
        instChrs   = genInstanceChrs insts
   in   classChrs ++ instChrs
\end{code}

\subsection{Constraint solving}
The solver maintains a state when solving constraints.
This state consists of two components:
\begin{code}
 data SolveState p s = 
   SolveSt  {  constraints  :: ConstrSet p
            ,  rules        :: [Rule p s ]
            }

 type ConstrSet p = Set.Set (Constraint p)
\end{code}
The first component of the state is a set of both |Prove| and |Assume| constraints.
The second component is a list of CHRs for the constraint language.
The initial state of the solver consists of an empty constraint set and a list of CHRs generated for the class and instance declarations in the program.

Solving a constraint is straightforward: it consists of inserting the constraint into the constraint set:
\begin{code}
 solveConstraint :: (MonadState (SolveState p s) m, Matchable p s) => Constraint p -> m ()
 solveConstraint c =
   modifyConstrSet      (Set.insert c)

 modifyConstrSet f =
   modify (\s -> s { constraints = f (constraints s)})
\end{code}
Both |Assume| and |Prove| constraints are inserted into the constraint set.
Recall the definition of constraint satisfaction:
\begin{spec}
 Theta solves Prove   pi  ^^   isdef     ^^    PiTheta entails ThetaQ
 Theta solves Assume  pi  ^^   isdef     ^^    ThetaQ insign PiTheta
\end{spec}
As we have seen in this chapter |Prove| constraints result from instantiating type schemes and |Assume| constraints result from skolemizing type schemes of explicitly typed functions.
We immediately solve an |Assume| constraint by inserting it into the constraint set.
But a |Prove| constraint is only solved if it is entailed by the |Assume| constraints.

The following function solves |Prove| constraints by removing them from the constraint set if they are entailed by the |Assume| constraints:
\begin{code}
 simplify :: (MonadState (SolveState p s) m, Matchable p s) => m ()
 simplify =
   do  rls     <- gets rules
       modifyConstrSet (chrSolve rls)
\end{code}
This function can be applied at any stage in the type-inference process. 
Overloading is resolved if there are only |Assume| constraints left in the constraint set after the last simplification.
|Prove| constraints that eventually remain are reported to the user as an error.

The description of the framework would be complete if every function in a Haskell program would be explicitly typed because then all |Assume| constraints also are explicitly given.
However, for functions without a type signature we have to infer a minimal set of |Assume| constraints that entail the |Prove| constraints of the function body.
Predicates should be generalized if the predicate and the type that must be generalized have free type variables in common.
Therefore, we introduce a type class for substitutables:
\begin{code}
 class Eq v => Substitutable a v s | a -> v, a -> s where
   ftv         :: a -> [v]
   substitute  :: s -> a -> a

 instance Substitutable p v s => Substitutable (Constraint p) v s where
   ftv (Prove  p)         = ftv p
   ftv (Assume p)         = ftv p

   substitute s (Prove  p)          = Prove   (substitute s p)
   substitute s (Assume p)          = Assume  (substitute s p)
\end{code}
The overloaded function |ftv| computes a list of type variables of type |v| given  a value of type |a| and the function |substitute| applies a substitution |s|.
The following function returns the predicates that should be generalized given a list of type variables that are going to be generalized.
\begin{code}
 getProveObligations :: (MonadState (SolveState p s) m, Substitutable p v s) => [v] -> m [p]
 getProveObligations tps =
  do  prvs <- gets constraints
      let  g = not . null . intersect tps . ftv

           isProve (Prove _)  = True
           isProve _          = False

           unProve (Prove p)  = p
      return [ unProve p | p <- (Set.toList prvs), isProve p, g p ]
\end{code}
The predicates are selected with the function |g|.
This function returns true if the intersection of the free type variables and |tps| is not empty.
% TODO:: wat uitleg over multi-param type classes + generalization

We present the steps needed to generalize a type to illustrate the interaction between a compiler and the framework during this step.
We assume the compiler using this framework has a substitution |S| and an environment |Sigma|.
Generalizing a type consists of the following steps given type |tau| and a set of type variables that should remain monomorphic |CalM|.
\begin{myitemize}
\item The substitution |S| is applied to |tau|, |CalM|, and the constraint set. This ensures that the type information acquired during the analysis of the
binding group is applied.
\item The constraints are simplified by invoking the function |simplify|. Simplification ensures that the constraints are reduced to head normal form so that the minimal set of |Prove| constraints needed to resolve overloading is constructed.
\item The type variables that are going to be generalized are computed by removing the variables in |CalM| from the free type variables in |tau|.
\item The function |getProveObligations| is used to select the predicates that should be part of the generalized type. This function gets the set of type variables computed in the previous step as argument.
\item The type |tau| is generalized together with the selected predicates and stored into the environment |Sigma|.
\item Finally, the generalized predicates are added as assumptions to the constraint set. This makes the inferred context available as assumptions, so the prove constraints can be solved.
\end{myitemize}
%}
