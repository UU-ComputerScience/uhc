%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Chapter TYPE CLASS EXTENSIONS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Local instances}
\label{ch:localinstances}
This chapter describes how the framework can be used to resolve overloading in the context of local instances~\citep{dijkstra05phd}.
Local instances can be used, for example, to encode dynamically scoped variables~\citep{lewis00implicit}.
This extension also allows the programmer to shadow instances that are imported from other modules.

\section{Introduction}
Consider the following example program to illustrate the problem:

>  class Eq a where
>    (==) :: a -> a -> Bool
>
>  instance Eq Int where
>    (==) = primEqInt
>
>  test1 =  90 == 450
>
>  test2 =  let  instance Eq Int where
>                  x == y = primEqInt (mod x 360) (mod y 360)
>           in   90 == 450

With local instances, two instances for |Eq Int| are available: one in the global scope and one in the local scope.
The global instance must be used to resolve overloading in the function |test1| because that is the only instance in scope.
A local instance declaration shadows an instance declaration introduced at an outer level, and thus the local instance for |Eq Int| is used to resolve overloading in the function |test2|.

We could choose to not solve local instances using our framework and leave this to the compiler that uses this framework.
This can be achieved by simplifying predicates at each scope using our framework together with an environment containing the instances in scope.
However, we choose to formulate local instances using constraints.  
\begin{itemize}
\item 
Therefore, we annotate each predicate with a scope identifier and encode the lexical scoping rules into CHRs. 
The number of design decisions increases when allowing local instances.
Again, we do not encode design decisions into the constraints and CHRs.
\item 
Instead, we generate every correct context reduction alternative and let a heuristic choose the preferred solution. 
{\it Correct} in this sense means that the rules must respect lexical scoping, but not shadowing.
For instance, it is correct to use the global instance for |Eq Int| in the function |test2|, however, using the local instance in the function |test1| is not correct.
\end{itemize}

\section{Lexical scoping}
Lexical or static scoping is a static property of a program.
This makes it easier to reason about code, because it allows the programmer to reason as if variable bindings are carried out by substitution.

The structure of lexical scopes in a program can be represented with a tree corresponding to the nesting structure of the source text.
A scope is then defined as the path of the root to a node.
We represent a tree path with a list of integers.
The length of this list is equal to the depth of the scope and each child is identified by an integer which is unique within the directly enclosing parent scope.
The empty list identifies the global scope.

> type TreePath  =  [Int]
>
> isPrefixOf :: Eq a => [a] -> [a] -> Bool
> (x:xs)  `isPrefixOf`  (y:ys)  = x == y && xs `isPrefixOf` ys
> []      `isPrefixOf`  _       = True
> _       `isPrefixOf`  []      = False
>
> visibleIn :: TreePath -> TreePath -> Bool
> visibleIn = isPrefixOf

We can use the standard function |isPrefixOf| to check if something from scope |s| is visible in scope |t|.
For convenience, we abbreviate |isPrefixOf| with |visibleIn|.
For example, |[]| is visible in every scope and |[1,2]| is visible in |[1,2,2]|.
In the following sections, we use tree paths to annotate predicates with their scope.

\section{Entailment}

\begin{Figure}{Entailment rule for scoping and the adapted rule for instances}{scoperules}
\begin{center}
~\\
\rulerRule{|(Scope)|}{}
          {|P entails {(Sub pi t)} ^^ ^^ ^^ t `visibleIn` s|}
          {|P entails {(Sub pi s)}|}
\rulerRule{|(Inst)|}{}
          {|P entails Q  ^^  ^^  ^^  ^^  ^^  (Inst Q (Sub (=>) t) pi) insign Gamma ^^  ^^  ^^  ^^  ^^  t `visibleIn` s|}
          {|P entails {(Sub pi s)}|}
~\\
~\\
\end{center}
\end{Figure}

We first adapt the entailment relation before giving a translation into CHRs.
Each predicate |pi| is annotated with a scope identifier |s| |((Sub pi s))|.
For a |Prove| constraint this means that |pi| must be proven using the CHRs visible in scope |s|.
An assumption of |pi| in scope |s| means that |pi| is available in scope |s|.
In Figure~\ref{scoperules} we present new entailment rules to replace the rules presented in Chapter~\ref{ch:preliminaries} (figures~\ref{basicent} and \ref{classent}).

The |Scope| rule states that |P| entails |pi| in scope |s| if |P| entails |pi| in a parent scope |t|.
For example, consider the constraint set |{Assume((Sub (Ord a) [1]))|, |Prove((Sub (Ord a) [1,1]))}|.
There is an assumption available for |Ord a| and in an inner scope |Ord a| must be proven.
We have to use the |Scope| rule to proof that |{(Sub (Ord a) [1])} entails {(Sub (Ord a) [1,1])}|.
Another situation where we need this rule is with the constraint set |{Prove((Sub (Ord a) [1,2]))|, |Prove((Sub (Ord a) [1,1]))}|.
Two proof obligations concerning the same predicate occur in two sibling scopes.
The duplicate predicates in this set can be removed, because |{(Sub (Ord a) [1])} entails {(Sub (Ord a) [1,1]) (Sub (Ord a) [1,2])}|.

The |Inst| rule in Figure~\ref{scoperules} is the adapted version of Figure~\ref{classent}.
This rule states that only instances in scope can be used to resolve overloading.
Note that the predicates |Q| in the |Inst| rule are instantiated to scope |s|.
For example, the entailment relation |{(Sub (Eq a) [1,1])} entails {(Sub (Eq [a]) [1,1])}| holds when |(Inst {Eq a} (Sub (=>) []) Eq [a]) insign Gamma|.   

\section{Translation to CHRs}
We now present a translation from class and instance declaration into CHRs.
First, we explain which CHRs must be generated for instance declarations.
Then we present two alternative encodings of the |Scope| rule.

\subsection{Instance declarations}
To implement the |Inst| rule of Figure~\ref{scoperules}, we have to adapt the translation from instance declarations into CHRs.
We generate the following CHR for each instance declaration: |instance (pi1, .. , pin) => pi|, in scope |varsigma|.

>  Prove (pi, s)  ==>  varsigma `visibleIn` s 
>                 |    Reduction (pi, s) (ByInstance pi varsigma) [(pi1, s), ... , (pin, s)]
>                 ,    Prove (pi1, s), ... , Prove (pin, s)

The reduction constraint generated by this rule is annotated with the head of the instance declaration and a scope identifier.
This annotation is used in the next section to define a heuristic for local instances.
The difference with the translation presented in the previous chapter is that the predicates are annotated with a scope and that a guard checks whether the rule is in scope.
For example, the following rule is generated for the global instance |instance Eq a => Eq [a]|:
 
>  Prove (Eq [a], s)  ==>  [] `visibleIn` s 
>                     |    Reduction (Eq [a], s) (ByInstance "eqList" []) [(Eq a, s)]
>                     ,    Prove (Eq a, s)

\begin{Figure}{Simplification graph generated using the first encoding of the |Scope| rule}{scope1}
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr3.pdf}
\end{center}
\end{Figure}

\subsection{First encoding of scoping}
We only need the following CHR to implement the |Scope| rule of Figure~\ref{scoperules}:

> Prove (p, s) ==> not (null s) | Prove (p, init s), Reduction (p, s) (ByScope (init s)) [(p, init s)]

This CHR means that a predicate can be reduced to a predicate in the parent scope.
The guard prevents the rule to be applied in the global scope, because the global scope does not have a parent scope.
With this rule we encode the |Scope| rule in a very concise way corresponding directly to the |Scope| entailment rule.
Note that the generated reduction constraint is annotated with the scope we reduce to.
This annotation is used in the next section to define a heuristic for local instances.
In Figure~\ref{scope1} we present the graph generated when using this rule for solving the constraints |{Assume(Ord c1, [1]), Prove(Ord [c1], [1,2]), Prove(Eq [c1], [1,1])}|.
The drawback of this CHR is that it is applicable to every proof obligation that is not in the global scope.
This means that many reduction alternatives are generated which are not always very useful.
For that reason, we propose a second encoding of scoping.

\begin{Figure}{Simplification graph generated using the second encoding of the |Scope| rule}{scope2}
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr4.pdf}
\end{center}
\end{Figure}

\subsection{Second encoding of scoping}
We propose an alternative encoding of the |Scope| rule to reduce the number of alternatives generated.
The idea behind the new rules is that a reduction to a parent scope is only inserted when this results in further constraint simplification.
The following situations are reasons to insert a scope reduction:
\begin{myitemize}
\item When two |Prove| constraints concerning the same predicate appear in different scopes.        
\item When two |Prove| constraints concerning the same type appear in different scopes, where one predicate is a subclass of the other.            
\end{myitemize} 
In both cases constraints can be simplified by removing duplicates or by simplification using the class hierarchy. 
Later in this section we will discover a third reason to insert scope reductions.
We introduce the following CHR to detect and handle the first situation:

>  Prove (p, s), Prove (p, t)  ==>   not (s `visibleIn` t)
>                              |     Prove (p, commonPrefix s t)
>                              ,     Reduction  (p, s) 
>                                               (ByScope (commonPrefix s t))
>                                               [(p, commonPrefix s t)]


The head of this CHR matches if there are two prove constraints in different scopes concerning the same predicate |p|.
A reduction for the first constraint is only generated when the scope of the first constraint is not visible in the scope of the second constraint.
This guard is needed to prevent cycles in the simplification graphs resulting from reductions from the first constraint to itself.
Note that a single constraint never matches both the first and the second pattern of the head at the same time. 
However, when a constraint matches multiple patterns of a head, the constraint will be matched against each pattern in sequence when possible.
The greatest common prefix of two scopes is computed with the following function:

> commonPrefix :: Eq a => [a] -> [a] -> [a]
> commonPrefix (x:xs)  (y:ys)  | x == y     = x : commonPrefix xs ys
>                              | otherwise  = []
> commonPrefix _       _                    = []

The second situation where scope reductions are inserted is when two |Prove| constraints exist in different scopes which can be reduced using the class hierarchy.
To detect this situation, the following rules are generated for each predicate pair |(pi1, pi)| where |pi1| is a superclass of |pi|.
 
>  Prove (pi, s), Prove (pi1, t)  ==>   not (s `visibleIn` t)
>                                 |     Prove (pi, commonPrefix s t)
>                                 ,     Reduction  (pi, s) 
>                                                  (ByScope (commonPrefix s t))
>                                                  [(pi, commonPrefix s t)]
>
>  Prove (pi, s), Prove (pi1, t)  ==>   not (t `visibleIn` s)
>                                 |     Prove (pi1, commonPrefix s t)
>                                 ,     Reduction  (pi1, t) 
>                                                  (ByScope (commonPrefix s t))
>                                                  [(pi1, commonPrefix s t)]

These rules are very similar to the one for |Prove| constraints concerning the same predicates. 
Again, we annotate the generated reduction constraint with the scope we reduce to.
The difference is that we have to generate the above rules for each superclass of a class. 
For example, the following rules are generated for the declaration |class Eq a => Ord a|:

>  Prove (Ord a, s), Prove (Eq a, t)  ==>   not (s `visibleIn` t)
>                                     |     Prove (Ord a, commonPrefix s t)
>                                     ,     Reduction  (Ord a, s) 
>                                                      (ByScope (commonPrefix s t))
>                                                      [(Ord a, commonPrefix s t)]
>
>  Prove (Ord a, s), Prove (Eq a, t)  ==>   not (t `visibleIn` s)
>                                     |     Prove (Eq a, commonPrefix s t)
>                                     ,     Reduction  (Eq a, t) 
>                                                      (ByScope (commonPrefix s t))
>                                                      [(Eq a, commonPrefix s t)]

One rule is generated to add a scope reduction for the class and one to add a scope reduction for the superclass.

There is a third situation in which we insert a scope reduction: when there is an assumptions and a prove obligation for the same predicate. 
The following rule is added to detect this situation.

>  Prove (p, s), Assume (p, t)  ==>  s /= t
>                               ,    t `visibleIn` s 
>                               |    Reduction (p, s) (ByScope t) [(p, t)]

To sum up, we have to modify the systematic translation with respect to the following points:  
\begin{myitemize}
\item A guard must be added in the translation of instance declaration to check if the instance is in scope.
\item A CHR must be added to simplify |Prove| constraints concerning the same predicate in different scopes.
\item A CHR must be added to simplify a |Prove| constraint and an |Assume| constraint concerning the same predicate in different scopes.
\item CHRs must be generated for the transitive closure of the superclass relation to simplify |Prove| constraints in different scopes where one predicate is a subclass of the other. 
\end{myitemize}
In Figure~\ref{scope2} we present the graph generated when using the new translation for solving the constraints |{Assume(Ord c1, [1]), Prove(Ord [c1], [1,2]), Prove(Eq [c1], [1,1])}|.
Compared to Figure~\ref{scope1}, the number of edges is decreased considerably because no superfluous reductions are generated anymore.

\section{Heuristic}
We also have to define a new heuristic to choose the preferred solution.
For example, we must encode that a local instance shadows a global instance.
The different constraints are annotated with the values from the following datatype:

> data Annotation  =  ByInstance    String  TreePath
>                  |  BySuperClass  String
>                  |  Assumption    Int
>                  |  ByScope       TreePath
>                  deriving (Eq, Ord)

The difference with the annotations presented in Subsection~\ref{ss::haskell98heur} is that we have added a constructor for scoping and that the constructor |ByInstance| has an additional field of type |TreePath|.
The field |TreePath| is added to identify the scope of the used instance declaration.
Reductions generated by the CHRs for scoping are annotated with |ByScope|.
The field |TreePath| of the constructor |ByScope| is the scope where the predicate is reduced to.
Using these annotations we define the following heuristic:

> scoped :: Annotation -> Annotation -> Ordering
> scoped (ByInstance _  s)  (ByInstance _  t)  =  (length s) `compare` (length t)
> scoped (BySuperClass  _)  _                  =  GT
> scoped _                  (BySuperClass  _)  =  LT
> scoped (Assumption    _)  _                  =  GT
> scoped _                  (Assumption    _)  =  LT
> scoped (ByScope       s)  (ByScope       t)  =  (length s) `compare` (length t)
> 
> scopedHeuristic = toHeuristic $ binChoice scoped

The rule that a local instance shadows a global instance is encoded in the first case of the function |scoped|.
Instances are ordered by comparing the depth of the scope.
Predicates are first reduced using instances, the class hierarchy, and assumptions. 
A predicate is only reduced to a parent scope when there are no other options left.

In the last case of the heuristic we also compare the depth of the scopes.
The reason for this is that there are sometimes multiple alternatives for reducing a predicate to a parent scope. 
For example, the graph in Figure~\ref{scope3} results from the following constraint set:
> {Prove(Eq c1, [1,1,1]), Prove(Eq c1, [1,1]), Assume(Eq c1, [1])}
Two alternatives for reducing the predicate |(Eq c1, [1,1,1])| are generated: one resulting from the |Prove| constraint in the parent scope and the other resulting from the |Assume| constraint.
We annotate both alternatives with different values to be able to make a deterministic choice.

\begin{Figure}{Multiple alternatives for reducing to a parent scope.}{scope3}
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr5.pdf}
\end{center}
\end{Figure}

\section{Conclusion}
In this chapter we have given entailment rules for scoped instances.
We use the entailment rules as the starting point for a first translation.
Although the first translation is correct, many redundant reductions are generated.
As an alternative, we have presented a second translation to minimize the number of such reductions.

We emphasize that every possible reduction is generated and represented in a graph.
This maximizes the opportunities to experiment with scoped instances by testing different heuristics.
For example, a programmer could indicate that a constraint must be resolved with instances from the global scope.
The heuristic presented in this chapter nicely illustrates how the rule for preferring local instances can be encoded.

This chapter also shows the flexibility of the framework. 
We have used predicates annotated with scope identifiers and also showed how additional features can be implemented with small modifications in the translation.
