%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Chapter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main question in this chapter is:
% How is overloading implemented in the compiler Helium?
% What is Helium?
% How is overloading implemented in Helium?
% What are the weak/strong points of this implementation?
% What can/must be improved?
% 
% Keywords:
% Helium
% Top
% Haskell 98
% i.h.a. schuif je volgorde van solving constraints onder 't tapijt. Maak ergens expliciet hoe het een rol speelt en hoe Top daar mee omgaat.

\chapter{Overloading in Helium}
\label{ch:helium}
Helium~\citep{Helium} is a compiler designed with the emphasis on high quality type error messages. 
Helium is suitable for teaching students functional programming because the type errors are clear and precise.
It supports most of Haskell~98: the most notable omission is that the user cannot specify class and instance declarations.
However, Helium supports overloading with a finite set of built-in classes and instances.
Helium uses the constraint solver Top~\citep{heeren05} for type inferencing and checking.
In this chapter we will focus on how overloading is resolved in Helium and Top.


\section{The type system}
The type system of Helium is rather standard and has the following structure:

\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lllr}
\multicolumn{4}{l}{Type:} \\
|tau| & |::=| & |a| & (type variable)\\
      & | || |& |T| & (type constant)\\
      & | || |& |tau1 tau2| & (type application)\\
\multicolumn{4}{l}{Context:} \\
|P| & |::=| & |(pi1, .... , pin)| & (|n >= 0|)  \\
\multicolumn{4}{l}{Type class qualifier:} \\
|pi|& |::=| & |C tau| &  \\
\multicolumn{4}{l}{Type scheme:} \\
|sigma| & |::=| & |forall abar. P => tau| &  \\
\end{tabular}
\)\par\noindent\endgroup
\noindent

The types in this type system are partitioned in polymorphic types (|sigma|) and monomorphic types (|tau|).
A type variable is an identifier starting with a lowercase letter.
In this chapter we use the infinite list of identifiers |(v1, ..., vn)| as type variables.
Both a type constant (|T|) and a class name (|C|) are identifiers starting with an uppercase letter.
Type application allows us to write useless types like |(Int Bool)|.
Actually, we need a kind system to check well-formedness of types, but we assume that we only deal with well-formed types.
Type schemes with an empty context: |forall abar . () => tau| are written as: |forall abar . tau|.


\section{Constraint based type inferencing}
Helium collects type constraints by analyzing the abstract-syntax tree of a Haskell program.
This results in a tree of constraints which has the same shape as the abstract-syntax tree of the program.
This tree is flattened into a list using a flattening strategy.
The resulting constraints are solved by Top.

In this section we explain how these constraints are solved.
First, we consider equality constraints, and then we show how constraints for polymorphism are solved.

\subsection{The equality constraint}
An equality constraint specifies that two monomorphic types must be equal:

\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lllr}
\multicolumn{4}{l}{Constraints:} \\
|c| & |::=| & |tau1 == tau2| & (equality)\\
\end{tabular}
\)\par\noindent\endgroup
\noindent

For instance, consider the following function:
> f x y z =  if x
>            then  y
>            else  z
Analyzing the if-then-else part of this function yields the following result:
> CalA  =  [x mapsto v1, y mapsto v2, z mapsto v3]
> CalC  =  {  v1  == Bool
>          ,  v2  == v4
>          ,  v3  == v4      }
|CalA| is an assumption set relating monomorphic types to identifiers and |CalC| is the constraint set.
These constraints specify that the guard must have type |Bool| and that the then and else branch must have the same type.
The solver maintains a state while solving these constraints.
This state consists of a substitution which maps type variables to types:

\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lllr}
|state| & |::=| & |S| & \\
|S|     & |::=| & |[v1 mapsto tau1, ... , vn mapsto taun]| & \\
\end{tabular}
\)\par\noindent\endgroup             
\noindent

Helium solves equality constraints by generating substitutions under which the two types are syntactically equivalent.
The mapping |v1 mapsto Bool| is added to the substitution to solve the first constraint of the example.
The second constraint has two solutions: either |v2| is mapped to |v4|, or |v4| is mapped to |v2|.
The same holds for the last constraint.
Which of the two solutions is chosen does not matter.
Helium chooses to map the left hand side to the right hand side.
This results in the following solution for the list of example constraints:
> S =   [   v1 mapsto Bool,   v2 mapsto v4,   v3 mapsto v4    ]
It is not allowed for a type variable of one component of an equality constraint to occur in the other component.
For example consider the following constraint:
> v5 == Maybe v5
One could come up with the substitution |[v5 mapsto Maybe v5]| as solution, but this will result in an infinite type.
In other words, the substitution must be idempotent.
This restriction is called `the occurs check'.

\subsection{Constraints for polymorphism}
To support polymorphism we need to add another layer to the type system.
Beside monomorphic types (|tau|) and polymorphic types (|sigma|) we add |rho| types which are either type schemes (|sigma|) or type scheme variables (|sigmav|).
Furthermore, we extend the state with an environment which maps type scheme variables to type schemes:
> state  ::=  (S, Sigma)
> Sigma  ::=  [s1 mapsto sigma1 , ... , sn mapsto sigman]
We use the infinite list of identifiers |(s1, ..., sn)| as type scheme variables.
Also, three constraints are added to the constraint language to support polymorphism:
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lllr}
\multicolumn{4}{l}{Constraints:} \\
|c| & |::=| &  |tau1 == tau2| & (equality)\\
    & | || | & |sigmav := Gen(CalM, tau)| & (generalization)\\
    & | || | & |tau    := Inst(rho)| & (instantiation)\\
    & | || | & |tau    := Skol(CalM, rho)| & (skolemization)\\
\end{tabular}
\)\par\noindent\endgroup
\noindent

In the remainder of this subsection we will explain in more detail how polymorphism constraints are used and solved.

\subsubsection*{The generalization constraint}
A generalization constraint generalizes a type with respect to a set of type variables that should remain monomorphic (|CalM|). 
For instance, Helium generates the following constraints for the function: |id x = x|.
> CalA  =  [id mapsto v0, x mapsto v3]
> CalC  =  {   v0 == v2 -> v1
>          ,   v3 == v2
>          ,   v3 == v1
>          ,   id := Gen([], v0)   }
The first constraint states that the binding |id| is a function.
The second constraint states that the variable |x| should have the same type as the argument of the function.
The third constraint states that |x| should have the same type as the result of the function.
The last constraint generalizes the binding and maps the type-scheme variable |id| to the resulting type scheme.
Consider the state of the solver just before considering the last constraint:
> S       = [v0 mapsto v1 -> v1,   v3 mapsto v1,   v2 mapsto v1    ]
> Sigma   = []
Solving a generalization constraint |sigmav := Gen(CalM, tau)| consists of four steps:
\begin{itemize}
\item First, the substitution is applied to the set of type variables that should remain monomorphic (|CalM|) and the type |tau|. As a result the constraint becomes: |id := Gen([], v1 -> v1)|.
\item Second, the type variables that are going to be generalized are computed: |abar = ftv(tau) - ftv(CalM)|, in this case |v1|.
\item The third step is to generalize the type |tau| over the set of type variables |abar|.
\item And finally the generalized type is stored in the environment under the type-scheme variable |sigmav|, in this case |id|.
\end{itemize}
For our current example this will result in the following state:
> S       = [v0 mapsto v1 -> v1,   v3 mapsto v1,   v2 mapsto v1    ]
> Sigma   = [id mapsto forall a . a -> a]
Under this solution the equality constraints hold and the type for the function |id| is inferred to be |forall a . a -> a|.

\subsubsection*{The instantiation constraint}
An instantiation constraint states that type |tau| should be an instance of a type scheme.
For example, the following constraints are generated for the expression |(id 1, id 'c')|:
> CalA  =  []
> CalC  =  {   v4 := Inst(forall a . a -> a)
>          ,   v4 == Int -> v3
>          ,   v7 := Inst(forall a . a -> a)
>          ,   v7 == Char -> v6
>          ,   (v3, v6) == v2                  }
The polymorphic function |id| is instantiated twice: once for an argument of type |Int| and once for an argument of type |Char|.

Solving an instantiation constraint |tau := Inst(rho)| consists of three steps:
\begin{itemize}
\item First, if |rho| is a type scheme variable, the type scheme is looked up in the environment. In this example the type schemes are already known.
\item Second, the type scheme is instantiated by replacing the universally |(forall)| quantified variables with fresh type variables.
\item Finally, an equality constraint is generated between |tau| and the instantiated type.
\end{itemize}

Consider the first instantiation constraint of the above example.
The type scheme of this constraint is instantiated with the fresh type variable |v10|.
This results in the equality constraint |v4 == v10 -> v10| which means that |v4| is a function with one argument where the type of the argument is equal to the type of the result.
In the same way, the type scheme of the second instantiation constraint is instantiated with the fresh type variable |v11| resulting in the constraint |v7 == v11 -> v11|.
Solving the equality constraints results in the following solution:
>S        =   [v2 mapsto (Int, Char), v3 mapsto Int,v4 mapsto Int -> Int,v6 mapsto Char,v7 mapsto Char -> Char,v10 mapsto Int
>             ,v11 mapsto Char]
>Sigma    =   []
Under this solution the equality constraints holds.

\subsubsection*{The skolemization constraint}
A skolemization constraint states that type |tau| should be equal to a skolemized type scheme.
A type scheme can be skolemized by instantiating it with fresh type constants.
We use |v| for type variables and |c| for type constants.
The difference between the two is that a type constant cannot be unified with another type constant whereas a type variable can be unified with a type constant.
Let us define the identity function again, but now with an explicit type signature.
> id :: forall a . a -> a
> id x = x
Helium generates the following constraints for this function:
> CalA  =  [id mapsto v0, x mapsto v3]
> CalC  =  {   v0 == v2 -> v1
>          ,   v0 := Skol([], forall a . a -> a)
>          ,   v3 == v2
>          ,   v3 == v1                            }
If we compare these constraint to the constraints generated when not giving a type signature, a skolemization constraint is generated instead of a generalization constraint.
Also the order of the constraints differ: the generalization constraint did appear after the constraints for analyzing the body of the function |id|, but the skolemization constraint appears before.
In fact the generalization constraint \emph{must} appear after the constraints for analyzing the body, but the position of the skolemization constraint does not matter.

The following three steps are needed to solve a skolemization constraint |tau := Skol(CalM,rho)|:
\begin{itemize}
\item First, if |rho| is a type-scheme variable, the type scheme is looked up in the environment.
\item Second, the universally |(forall)| quantified variables are replaced with fresh type constants.
\item Third, the fresh type constants and |CalM| are stored together so that we can check afterwards that the type constants do not escape via a type variable in |CalM|.
\item Finally, an equality constraint between |tau| and the skolemized type is generated.
\end{itemize}

The type scheme of the constraint |v0 := Skol([], forall a . a -> a)| is skolemized with the fresh type constant |c5|.
The last step in solving the skolemization constraint is that we generate the constraint: |v0 == c5 -> c5|.
Solving the list of equality constraints will result in the following substitution:
> S = [v0 mapsto c5 -> c5,v1 mapsto c5,v2 mapsto c5,v3 mapsto c5]


\section{Constraints for overloading}
In this section we explain how overloading is resolved in Top.
To support overloading we extend the constraint language with two additional constraints:

\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lllr}
\multicolumn{4}{l}{Constraints:} \\
|c| & |::=| & |tau1 == tau2| & (equality)\\
    & | || | & |sigmav := Gen(CalM, tau)| & (generalization)\\
    & | || | & |tau    := Inst(rho)| & (instantiation)\\
    & | || | & |tau    := Skol(CalM, rho)| & (skolemization)\\
    & | || | & |Prove(pi)     |         & \emph{(prove qualifier)}\\
    & | || | & |Assume(pi)    |         & \emph{(assume qualifier)}\\
\end{tabular}
\)\par\noindent\endgroup
\noindent

The |Prove| constraint means that we have the obligation to proof the qualifier |pi|.
This means that evidence should be constructed of type |pi| at the location in the abstract-syntax tree where the |Prove| constraint occurs.
An |Assume| constraint means that we can assume the qualifier |pi|.
This means that evidence is available of type |pi|.

To solve these constraint we extend the state with the following components:
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lllr}
|state| & |::=| & |(S, Sigma, PiProve, PiAssume, PiGen)| & \\
|Pi   | & |::=| & | {pi1, ..., pin} | & \\
\end{tabular}
\)\par\noindent\endgroup
\noindent

|PiProve| is a set of qualifiers that must be proven and |PiAssume| is a set of qualifiers that can be assumed.
In the remainder of this chapter we explain what the meaning of the list of qualifiers |PiGen| is.

\subsection{Generating overloading constraints}
|Assume| and |Prove| constraints are not generated by Helium, but result from solving instantiation and skolemization constraints.
Consider the following example to illustrate this:
> elem :: Eq a => a -> [a] -> Bool
> elem x []       = False
> elem x (y:ys)   = x == y || elem x ys
Notice that this function has an explicit type signature and that the overloaded functions |(==)| and |elem| are used in the body of |elem|.
In this example we do not consider every generated constraint, but only the following skolemization and instantiation constraints:
> CalC  =  {    v0    := Skol([], forall a . Eq a => a -> [a] -> Bool)
>          ,    v1    := Inst(forall a . Eq a => a -> a -> Bool)
>          ,    v2    := Inst(forall a . Eq a => a -> [a] -> Bool)       }

% skolemization
First, the type signature is skolemized: |v0 := Skol([], forall a . Eq a => a -> [a] -> Bool)|.
The quantified type variable is replaced with a fresh type constant: |Eq c3 => c3 -> [c3] -> Bool|.
Finally, an |Assume| constraint is generated for every qualifier in the skolemized type.
So for the above type signature the constraint |Assume (Eq c3)| is generated.

% instantiation
Second, the function |(==)| is instantiated: |v1 := Inst(forall a . Eq a => a -> a -> Bool)|.
The quantified type is instantiated with a fresh type variable: |Eq v4 => v4 -> v4 -> Bool|.
Finally, a |Prove| constraint is generated for every qualifier in the instantiated type.
So for the first instantiation constraint |Prove (Eq v4)| is generated.
Likewise, the quantified type of the third constraint is instantiated using the fresh type variable |v5| and the constraint |Prove (Eq v5)| is generated.
The type variables |v4| and |v5| will be substituted to |c3| later in the type inference process. 

To sum it up, instantiation generates a |Prove| constraint for every qualifier of the instantiated type scheme.
Skolemization generates an |Assume| constraint for every qualifier of the skolemized type scheme.


\subsection{Solving overloading constraints}
Solving a |Prove| or an |Assume| constraint is rather straightforward. 
For a |Prove| constraint the qualifier |pi| is inserted into the set of qualifiers to prove (|PiProve|).
In the same way the qualifier of an |Assume| constraint is inserted into the set of qualifiers that can be assumed (|PiAssume|). 

\subsubsection{Simplification}
The question that arises now is what happens with the set of assumed qualifiers and the set of qualifier to proof . 
The answer is that the constraints are solved during the simplification step.
In principle this step can be applied at any point in the solving process.
Top executes this step just before generalizing and just after solving the last constraint.
For example, consider the function |elem| again:
> elem :: Eq a => a -> [a] -> Bool
> elem x []       = False
> elem x (y:ys)   = x == y || elem x ys
When the constraints for the above function are solved the state is:
> S          = [v1 mapsto c1, v2 mapsto c1, v0 mapsto c1 -> [c1] -> Bool, v18 mapsto c1 -> c1 -> Bool, ...]
> Sigma      = []
> PiProve    = {Eq v1, Eq v2}
> PiAssume   = {Eq c1}
> PiGen      = {}
The last step of the constraint solver is to simplify the set of qualifiers to prove (|PiProve|).
The set of predicates to prove must be empty when the solver is finished, otherwise overloading is not resolved.
Simplification consists of three steps.
First, the substitution is applied to the qualifiers.
In our example the type variables are substituted to the type variable constant |c1|:
> PiProve    = {Eq c1}
> PiAssume   = {Eq c1}
Second, the prove qualifiers are simplified using instance declarations and the class hierarchy.
The last step is that the prove qualifiers entailed by the assume qualifiers are removed. 
In this example the assume qualifier |Eq c1| entails the prove qualifier |Eq c1|, because they are equal.
This means that the prove qualifier |Eq c1| can be removed. 
There are no prove qualifiers left so overloading is resolved in the function |elem| and the simplification step is finished.

\subsubsection{Generalization}
Simplification is part of the steps needed to solve the generalization constraint |sigmav := Gen(CalM, tau)|.
Consider the following example:
> f x y =   if x > y
>           then   show x
>           else   g x y
>
> g x y =   f (succ x) y 
For the above program the following constraints are generated; we only consider the generalize and instantiation constraints.
> CalA  =  [x mapsto v1, y mapsto v1]
> CalC  =  {   v1 -> v1 -> Bool   := Inst(forall a . Ord a => a -> a -> Bool)   -- (|>|)
>          ,   v1 -> String       := Inst(forall a . Show a => a -> String)     -- (show)
>          ,   v1 -> v1           := Inst(forall a . Enum a => a -> a)          -- (succ)
>          ,   f                  := Gen([], v1 -> v1 -> String)  
>          ,   g                  := Gen([], v1 -> v1 -> String)               }
Because the functions |f| and |g| are mutually recursive, the constraints of the body's of the functions are analyzed before both functions are generalized.
The first constraint is generated for the instantiation of the operator |(>)|, the second for the function |show|, and the third for the function |succ|.
The last two constraints generalize |f| and |g|, respectively.

There are eight steps needed to solve a generalization constraint |sigmav := Gen(CalM, tau)|:
> 1. ^^  simplification
> 2. ^^  substitution
> 3. ^^  abar      = ftv(tau) - ftv(CalM)
> 4. ^^  Pi1       = { pi bar pi <- PiProve   , (ftv pi intersect abar) /= []}
> 5. ^^  Pi2       = { pi bar pi <- PiGen     , (ftv pi intersect abar) /= []}
> 6. ^^  PiProve   = PiProve - Pi1
> 7. ^^  PiGen     = PiGen union Pi1
> 8. ^^  sigmav mapsto forall abar . (Pi1 union Pi2) => tau 
We first solve the first generalization constraint: |f  := Gen([], v1 -> v1 -> String)|.
The overloading state before solving this constraint is:
> PiProve    =  {Ord v1, Show v1, Enum v1}
> PiAssume   =  {}
> PiGen      =  {}
First, we simplify the predicates with the earlier described simplification step and then we apply the substitution.
The type variable over which we can generalize is |v1|.
This results in the following sets of qualifiers:
> Pi1   = {Ord v1, Show v1, Enum v1}
> Pi2   = {}
The set |Pi1| is subtracted from the set of proof predicates and inserted into the set of predicates that have been generalized.
Finally, the generalized type is: |forall a . (Ord a, Show a, Enum a) => a -> a -> String|.
Before we consider the last constraint: |g  := Gen([], v1 -> v1 -> String)|, the overloading state is:
> PiProve    =  {}
> PiAssume   =  {}
> PiGen      =  {Ord v1, Show v1, Enum v1}
Again the type variable over which we can generalize is |v1|.
The difference now is that filtering the set of prove qualifiers yields the empty set.
However, filtering the set of generalized qualifiers yields the predicates we expect:
> Pi1   = {}
> Pi2   = {Ord v1, Show v1, Enum v1}
Now again the result of the generalization is the type scheme: |forall a . (Ord a, Show a, Enum a) => a -> a -> String|.
As we have seen the generalized qualifiers are stored into the set of generalized qualifiers |(PiGen)| because they can show up in multiple type schemes.


\section{Conclusion}
If we only look at the implementation, the most beautiful aspect of Helium and Top is the elegant way of using constraints for type inference and overloading.
The use of constraints enables a nice separation of concerns: Helium generates the constraints, Top solves the constraints, and Helium uses the result of the solver.
It is also possible to use the constraint solver stand-alone to experiment with it.
Also, Helium can be asked to print the constraints of a program.

Not only the types should be inferred and checked for resolving overloading, but also code or evidence should be generated.
Evidence is not part of the solve result of Top, while Top has all the information to generate evidence.
Only the types are in the result and Helium must reconstruct context reduction to generate evidence.
This is the first point that could be improved.

A second point for improvement is that inside Top and Helium an environment with instance and class declarations is used to perform context reduction. 
Helium and Top could be further decoupled by using Constraint Handling Rules (CHRs) to represent this information.
CHRs are a much clearer specification of the meaning of class and instance declarations.
Furthermore, CHRs can be easily extended to support various type class extensions. 

A third remark is about the generalization constraint. 
A generalization constraint in Top now only covers one type |tau| to generalize. 
But a binding group may consist of more than one type that should be generalized.
Helium solves this by generating multiple generalization constraints in sequence.
However, for solving such a sequence Top must store a set of generalized qualifiers because they could be needed by subsequent generalizations.
We could get rid of the set of generalized qualifiers |(PiGen)| by adapting the generalization constraint to generalize a set of types.
