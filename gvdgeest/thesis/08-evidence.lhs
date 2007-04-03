%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Chapter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% How can we generate evidence for the resolution of overloading? 
% 1 To generate evidence we not only need to know that overloading can be resolved, but also how overloading can be resolved.  
% 2 To know how overloading is resolved we need to track the derivation of context-reduction.
% 3 In Haskell 98 there are already multiple derivations possible, but these derivation are confluent. But when using extensions such as overlapping instances and local instances some problems occur. We do not want to have non-confluence, so we only use propagation rules. 
% 4 The result of solving constraints with CHRs is a number of reductions and we use these reductions to construct a graph.
% 5 A heuristic is used to choose a solution from a graph.

% Atze::
% Algemeen punt: geef duidelijk aan wanneer je het over predicaten en de bijbehorende evidence hebt, nu loopt dat door elkaar heen. Zie ook enkele overige opm.
% zeg hier ook iets over hoe je dubbel werk voorkomt (omdat je niets verwijdert)
%
% hoe ga je dan om met dubbele info's voor een constraint? wat is daarvan de betekenis?
%
% op welk nivo wordt sharing uitgedrukt, in de AGraph of met eigen identifiers?
\chapter{Evidence translation}
\label{ch:evidence}
In this chapter we extend the framework with evidence translation.
This means that we also construct evidence associated with the solution of proof obligations.

\section{Introduction}
The translation of a language with overloading into a language without is called {\it evidence translation}.
There are two well-known translation schemes described in the literature.
The first one is the so-called dictionary translation scheme~\citep{wadler89how} where overloaded functions are translated into functions with additional dictionary parameters.
The second translation scheme is based on partial evaluation~\citep{jones95dictionary} where overloaded functions are translated into several specialized versions.
This framework aims at supporting the first translation scheme.

The CHRs presented in the previous chapter only check whether |Prove| constraints are entailed by |Assume| constraints.
However, to generate evidence we need to keep track of {\it how} constraints are solved.
Furthermore, we explain how non-confluency is avoided by generating all correct reduction alternatives for solving a constraint.
In the next section we give motivation for these changes and modify the translation from class and instance declarations into CHRs. 
Solving constraints with these new CHRs results in a set of all possible reductions. 
In the section~\ref{s:heuristics} we explain how these reductions and other information needed to generate evidence are represented in a graph.
Finally, we show how heuristics can be used to choose between different reduction alternatives in the graph.
As an illustration, we show how overlapping instances can be supported using the framework.

\section{Translation to CHRs}
In this section we adapt the translation from class and instance declarations into CHRs. 
We first show how the derivation steps performed by the CHR solver can be traced.
Then we explain why every correct alternative for simplifying a constraint must be generated and how this is achieved.
We do not give a new systematic translation because the main idea of the translation to CHRs stays the same.

\subsection{Tracing CHR derivations} 
In order to generate evidence, we have to consider the derivation steps needed to arrive at a solution.
For example, consider the following function for testing whether a list is still ordered after an insertion:
> testInsert :: Ord a => a -> [a] -> Bool
> testInsert x xs =  let  ys = insert x (sort xs)
>                    in   sort ys == ys
When type checking this function, the explicit type signature is skolemized with some type constant |(c1)|.
After analyzing the body of the function we discover that we have to solve the following constraint set:
> {Assume(Ord c1), Prove(Ord c1), Prove(Eq [c1])}
The following derivation is possible if we assume that the standard class and instance declarations for |Eq| and |Ord| are available:
>             {Assume(Ord c1),  Prove(Ord c1),  Prove(Eq [c1])}
> deriv() ^^  {Assume(Ord c1),  Assume(Eq c1),  Prove(Ord c1), Prove(Eq [c1])}
> deriv() ^^  {Assume(Ord c1),  Assume(Eq c1),  Prove(Eq [c1])}
> deriv() ^^  {Assume(Ord c1),  Assume(Eq c1),  Prove(Eq c1)}
> deriv() ^^  {Assume(Ord c1),  Assume(Eq c1)}
Overloading is resolved, because there are no |Prove| constraints left in the set of constraints. 
However, to generate code we need to know how we arrived at the final set of constraints.
For example, consider the translated version of the function |testInsert|:
> testInsert :: DictOrd a -> a -> [a] -> Bool
> testInsert d x xs =  let  ys = insert d x (sort d xs)
>                      in   ((==) (eqList (eqOrd d))) (sort d ys) ys
We need to know which intermediate steps are needed to deduce |Eq [c1]| from |Ord c1| to generate the code in the above function for using the equality |(==)| operator.

To achieve this, we let each CHR generate an additional |Reduction| constraint.
This constraint is used to record a derivation step.
Let us first extend the constraint language with a |Reduction| constraint:
> CalC   ::=   Prove      pi
>        |     Assume     pi
>        |     Reduction  pi info [pi1, ... , pin]
A |Reduction| constraint consists of three components: a predicate that is reduced, an annotation, and the predicates that are the result of the reduction.
A |Reduction| constraint means that it is possible to reduce the first component to the predicates that form the third component.
At the same time it means that the first component can be constructed using the evidence associated with the predicates that form the third component.
The third component of a reduction constraint is a list, because a predicate can be reduced to zero or more predicates and we want to remember the predicate order.
For example, the predicate |Eq Bool| is reduced to the empty list, but the predicate |Eq (v1, v2)| reduced to |[Eq v1, Eq v2]|.
The reduction constraints can be annotated with information. 
This information consists, for example of identifiers for generating code, information for generating type-error messages, or information which is used by heuristics to make a choice.

Consider the class and instance declarations below:
> class Eq a => Ord a 
> instance Eq a => Eq [a]
For these declarations we generate the following CHRs:
> Assume  (Ord a)                ==>  Assume  (Eq a),   Reduction (Eq a)    "eqOrd"   [Ord  a]
> Prove   (Ord a), Prove (Eq a)  <=>  Prove   (Ord a),  Reduction (Eq a)    "eqOrd"   [Ord  a]
> Prove   (Eq [a])               <=>  Prove   (Eq a),   Reduction (Eq [a])  "eqList"  [Eq   a]
The reduction constraints in the rules are annotated with the identifier of the dictionary transformer that performs the reverse step required for evidence construction.
Applying these rules on the set of constraint we considered earlier will result in the following constraint set:
>  {  Assume (Ord c1),  Assume(Eq c1)
>  ,  Reduction (Eq c1)    "eqOrd"   [Ord  c1]
>  ,  Reduction (Eq [c1])  "eqList"  [Eq   c1] ^^ }
This constraint set contains two reduction constraints which give information how |Eq [c1]| can be deduced from |Ord c1|. 
This information can be used to generate evidence.
We do not have enough information yet to perform the complete translation: however, we can already generate code for constructing dictionaries.

\subsection{Derivation alternatives and confluence}
A problem occurs when we simplify constraints with CHRs: constraint sets exist where different derivations lead to the same answer.
For example, consider the constraint set |{Prove(Eq [v1]), Prove(Ord [v1])}|.
Two derivations are possible: the first derivation simplifies the constraints by first using instances and then using the class hierarchy:
>             {Prove(Eq [v1]),  Prove(Ord [v1])}
> deriv() ^^  {Prove(Eq v1),    Prove(Ord [v1]),  Reduction (Eq [v1]) "eqList" [Eq v1]}  
> deriv() ^^  {Prove(Eq v1),    Prove(Ord v1),    Reduction (Eq [v1]) "eqList" [Eq v1]
>               ,Reduction (Ord [v1]) "ordList" [Ord v1]} 
> deriv() ^^  {Prove(Ord v1),  Reduction (Eq [v1]) "eqList" [Eq v1]
>               ,Reduction (Ord [v1]) "ordList" [Ord v1], Reduction (Eq v1) "eqOrd" [Ord v1]} 
The second derivation simplifies the constraints by reversing the preference for instances and classes; first the class hierarchy is used, then the available instances:
>             {Prove(Eq [v1]),   Prove(Ord [v1])}
> deriv() ^^  {Prove(Ord [v1]),  Reduction (Eq [v1]) "eqOrd" [Ord [v1]]}  
> deriv() ^^  {Prove(Ord v1),    Reduction (Eq [v1]) "eqOrd" [Ord [v1]]
>               ,Reduction (Ord [v1]) "ordList" [Ord v1]}  
Both derivations are correct and lead to the same answer.
However, it is not desirable that different derivations are chosen for the same constraint set in a non-deterministic way. 
The problem becomes even more evident with extensions such as overlapping instances.
Consider the following overlapping instances example:
> data Unit = Unit
>
> instance Eq [Unit] where
>   xs == ys = length xs == length ys
>
> instance Eq a => Eq [a] where ...
The CHRs generated for these instance declarations are:
> Prove(Eq [a])     <=> Prove(Eq a)
> Prove(Eq [Unit])  <=> true
Two non-confluent derivations are possible when we try to solve the constraint |Prove(Eq [Unit])|.
One derivation results in the empty constraint set and the other derivation results in |{Prove(Eq Unit)}|.
The most specific instance is preferred if there are multiple possibilities when using overlapping instances, but a CHR solver chooses one of the two derivations in a non-deterministic way. 
A solution proposed for this problem is to add guards to the simplification rules~\citep{stuckeysulz02overloading}:
> Prove(Eq [a])     <=> a /= Unit | Prove(Eq a)
> Prove(Eq [Unit])  <=> true
This results in a confluent rule set.
However, the design decision that the most specific instance is preferred is not really clear from the rules because the guard is added to the CHR generated for the `normal' instance and not for the overlapping instance.
Furthermore, guards can become very complex especially when using overlapping instances in combination with other type-class extensions, such as multi-parameter type classes or local instances.

To solve this problem we propose another solution:
We use CHRs to generate every possible type-correct derivation.
This means that we do not make a choice, but just generate all the possibilities.
With the reductions generated from this process we construct a graph and a heuristic chooses a solution from this graph.
With this approach we separate the process of finding possible solutions from the process of selecting the preferred solution.

The systematic translation from class and instance declarations into CHRs has to be adapted to generate every correct alternative. 
We never let CHRs remove constraints from the constraint set to achieve confluent derivations and to avoid encoding extensions in guards of CHRs.
Therefore, every applicable CHR will be applied to a constraint.
This means that we cannot use simplification CHRs anymore because they replace constraints by other constraints and thereby remove constraints from the constraint set.
We have to modify the systematic translation with respect to the following points:
\begin{myitemize}
\item The rule |Assume p, Prove p <=> Assume p| is not used anymore, because it removes constraints from the constraint set.
\item Other simplification CHRs are replaced by propagation CHRs. 
\item We let each CHR generate reduction constraints to trace the reduction step performed.
\end{myitemize}
Because these changes are relatively small we do not present the systematic translation again. % TODO::(appendix).
Instead, we give some examples of the adapted translation.
Consider the class declaration for numbers:
> class (Eq a, Show a) => Num a
The following CHRs are generated for this class declaration:
> Assume(Num a)  ==>  Assume(Eq    a), Reduction (Eq    a) "eqNum"     [Num a]
>                ,    Assume(Show  a), Reduction (Show  a) "showNum"   [Num a]
> Prove(Eq    a), Prove(Num a) ==> Reduction (Eq    a) "eqNum"     [Num a]
> Prove(Show  a), Prove(Num a) ==> Reduction (Show  a) "showNum"   [Num a]
The first rule adds the superclasses of |Num| to the constraint set and adds the corresponding reduction constraints.
The second and the third rule add a reduction from superclass to subclass if there are proof obligations for both classes.
Again consider the overlapping instance declarations:
> data Unit = Unit
>
> instance Eq [Unit] where
>   xs == ys = length xs == length ys
>
> instance Eq a => Eq [a] where ...
For these instance declarations we generate the following rules:
> Prove(Eq[a])     ==> Prove(Eq a), Reduction (Eq[a]) "eqList" [Eq a]
> Prove(Eq[Unit])  ==> Reduction (Eq [Unit]) "eqListUnit" []
The new rules for the overlapping instances are confluent, because the rules do not remove constraints from the set anymore.
We obtain the following result if we apply these rules on the constraint set |{Prove(Eq[Unit])}|.
>  {  Prove (Eq[Unit])
>  ,  Reduction (Eq[Unit]) "eqList" [Eq Unit]
>  ,  Prove(Eq Unit)
>  ,  Reduction (Eq[Unit]) "eqListUnit" []   }
The result of solving the constraint does not immediately tell us if overloading is resolved.
Moreover, there are two possible and correct solutions present in the constraint set.
That is the reason why we represent the reductions in a graph and select one of the solutions using a heuristics in the next section.

%include 08-implementation.lhs
