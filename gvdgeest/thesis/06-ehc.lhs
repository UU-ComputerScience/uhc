%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Chapter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% The main question in this chapter is:
% How is overloading implemented in the compiler EHC?
% What is EHC?
% How is overloading implemented in EHC?
% What are the weak/strong points of this implementation?
% What can/must be improved?
% 
% Keywords:
% EHC

\chapter{Overloading in EHC}
\label{ch:ehc}
In this chapter we explain how overloading is implemented in the Essential Haskell Compiler~(EHC)~\citep{dijkstra05phd, dijkstra05making}. 
First, we introduce the language EH and its type system.
Second, we explain how the resolution of overloading is implemented.
Finally, we conclude with a discussion of the strong and weak points of EHC.

\section{The language EH}
%format ehcConstrAlts = "\overline{I \overline{\sigma}}"
\begin{Figure}{Syntax of EH version 9}{syntax9}
\begin{center}
~\\
\begin{tabular}{lllrllll}
\multicolumn{4}{l}{Value expressions:} \\
|e| & |::=|  & |int || char |              & (literals) & \\
    & | || | & |i|                         & (variable) \\
    & | || | & |let dbar in e|             & (local definitions) \\
    & | || | & |e e|                       & (application) \\
    & | || | & |\i -> e|                   & (abstraction) \\
    & | || | & |e ^^ {! e <: pi !}|        & \emph{(explicit implicit application)} \\
    & | || | & |\ ^^ {! i <: pi !} -> e|   & \emph{(explicit implicit abstraction)} \\
\multicolumn{4}{l}{} \\
\multicolumn{4}{l}{Declarations of bindings:} \\
|d| & |::=| & |i :: sigma|                            & (value type signature) \\
    & | || | & |i = e|                                & (value binding) \\
    & | || | & |data sigmabar = ehcConstrAlts|        & (datatype) \\
    & | || | & |class pibar => pi where dbar|         & (class) \\
    & | || | & |instance pibar => pi where dbar|      & (introduced instance) \\
    & | || | & |instance i :: pibar => pi where dbar| & \emph{(named instance)} \\
    & | || | & |instance e <: pi|                     & \emph{(value introduced instance)} \\
    & | || | & |instance i <: pibar => pi where dbar| & \emph{(named introduced instance)} \\
    &  &  & \\
\end{tabular}
\end{center}
\end{Figure}

EHC is designed to experiment with advanced features such as higher ranked types, existential types, type classes, partial type signatures, and records.
On the other hand, the compiler is also an educational platform for teaching students how a compiler is implemented.
Therefore, the compiler consists of ten ordered versions, each adding new features on top of its preceding version.
Also, syntactic sugar is kept to a minimum.
In Figure~\ref{syntax9} we present the concrete syntax of EH associated with version 9, which resembles desugared Haskell~98~\citep{jones03haskell}.
In the next section we present the type system and the syntax of |sigma| and |pi| types.
In this section we explain three major differences with respect to Haskell~98.

\subsection{Instance declarations}
EH offers two additional forms of instance declarations besides the one already known from Haskell~98:
\begin{itemize}
\item A \emph{named instance} is not used by the compiler for the automatic resolution of overloading, but the dictionary is bound to the identifier |i|. This identifier can then be used for the \emph{value introduced instance}, but also as an explicitly passed dictionary to a function, of which we show examples hereafter.
\item A \emph{value introduced instance} adds a dictionary to the collection of instances that are used by the compiler for the automatic resolution of overloading.
\end{itemize}

There is a third form |(instance i <: pibar => pi where dbar)| which is syntactic sugar for the named instance combined with the value introduced instance:
> instance i :: pibar => pi where dbar
> instance i <: (pibar => pi)
First the dictionary is bound to |i|.
Then the dictionary is added to the instances that are used for the automatic resolution of overloading.
In other words, the named introduced instance means the same as the instance declaration we know from Haskell~98, but the resulting dictionary is also bound to an identifier.

\subsection{Explicit implicit application and abstraction}
A Haskell~98 compiler automatically inserts additional dictionary parameters as a result of overloading resolution and the programmer cannot influence this.
However, in EHC the programmer can overrule the automatic resolution by passing a dictionary explicitly.
In the following example we introduce a type class for equality and an two instances for |Eq Int|:

> let   class Eq a where
>         eq :: a -> a -> Bool
>
>       instance Eq Int where
>         eq = primEqInt
>
>       instance eqDeg :: Eq Int where
>         eq = \x y -> eq (mod x 360) (mod y 360)
>
> in    (eq                         90 450
>       ,eq ^^ {!eqDeg<:Eq Int!} ^^ 90 450)

The first instance for Eq Int is standard, the second is not.
Checking two angles for equality would not work with the default instance so we declare a named instance for equality modulo 360.
The instance for equality on degrees does not participate in the automatic resolution of overloading because it is only a named instance.
However, the programmer can explicitly pass the |Eq Int| dictionary for degrees to the function |eq|.
Evaluating the above expression yields |(False, True)|.

\subsection{Local instances}
In EH it is also possible to declare instances locally.
Named instances can be used as normal values, and participate in the usual scoping rules.
However, for instance declarations participating in the automatic resolution a different strategy must be used.
For example, when instances overlap, the innermost instance, as defined by lexical scoping, takes precedence over the outermost.
Two instances overlap when they are for the same class and if there is a substitution |(S)| which unifies the heads of the instances.
> (C sigmabar) overlaps (D sigmabar') if
> C == D && S sigmabar == S sigmabar'
>
> where
> S = bindbar
> bind ::= v mapsto sigma

The following EHC program illustrates local instances:
> let  class Eq a where
>        eq :: a -> a -> Bool
>
>      instance eqD <: Eq Int where
>        eq = primEqInt
>
>      test1 =  eq 90 450
>
>      test2 =  let  instance eqDeg <: Eq Int where
>                      eq = \x y -> primEqInt (mod x 360) (mod y 360)
>               in   eq 90 450
>
> in   (test1, test2)

The two instances in the above program overlap because they unify under the empty substitution.
In the body of the function |test1| the instance named |eqD| will be used because it is the only valid instance in scope.
However, the instance named |eqDeg| will be used in the body of the function |test2| because it is the innermost valid instance.
Note that class declarations are global in EHC just like in Haskell.

Finally, note that all bindings in a let expression are analyzed together, so a let expression in EHC is equal to a binding group in Haskell.


\section{The type system}

\begin{Figure}{Type language of EH version 9}{type9}
\begin{center}
~\\
\begin{tabular}{llllllll}
\multicolumn{4}{l}{Types:} \\
|sigma| & |::=|  & |Int || Char |     & (literals)  \\
        & | || | & |v|                & (type variable) \\
        & | || | & |T|                & (type constant) \\
        & | || | & |sigma sigma|      & (type application) \\
        & | || | & |forall vbar . sigma| & (universally quantified type) \\
        & | || | & |exists vbar . sigma| & (existentially quantified type) \\
        & | || | & |pibar => sigma|      & (implicit abstraction)\\
\multicolumn{4}{l}{} \\
\multicolumn{4}{l}{Predicates:} \\
|pi| & |::=|  & |C sigmabar          | & (predicate)  \\
     & | |||  & |pi => pi            | & (predicate transformer/abstraction)  \\
     & | |||  & |varpi               | & (predicate wildcard variable)\\
    &  &  & \\
\end{tabular}
\end{center}
\end{Figure}

The type system of EH implements features like higher ranked types, existential types, type classes, partial type signatures, and records.
The type language is presented in Figure~\ref{type9}.
Three features of this type system deserve special attention. 

\subsection{Higher ranked types}
The type system allows higher ranked types.
The rank of a type is the depth at which universal quantifiers appear on argument position (contra-variantly).
For instance, a rank-0 type is a monomorphic type without quantifiers.
A rank-1 type is only quantified on the outermost level, for instance, |forall a . a -> a|.
A rank-2 type is a function with a polymorphic argument, for instance:
> hr :: (forall a . a -> a) -> (Char, Int)
> hr = \id -> (id 'c', id 1)
EHC allows also ranks higher than 2.
In other word, EHC supports higher ranked types.
Note that EHC does not infer higher ranked types, but checks these types by propagating type annotations.
Type inference for ranks higher than 2 is even undecidable.

\subsection{Quantifiers and predicates everywhere}
Quantifiers and predicates may occur at the right-hand side of the function type constructor |(->)|.
EHC places the quantifiers and predicates as close as possible to the place where the quantified type variables occur.  
This differs from Haskell 98 which allows quantifiers only at the leftmost position in a type (implicitly).
To illustrate this, consider the following function:
> index = \xs       n   ->   if n == 0  
>                            then  head xs
>                            else  index (tail xs) (n-1)
A Haskell~98 compiler infers the following type for this function (ignoring the monomorphism restriction):
> forall a b . Num a => [b] -> a -> b
On the other hand EHC infers:
> forall b . [b] -> forall a . Num a => a -> b

\subsection{Higher order predicates}
Packaged as an instance declaration, higher order predicates are already available; for example the instance declaration for lists actually takes an instance for its arguments:
> instance Eq a => Eq [a] where ...
This instance declaration is translated into a function that creates a dictionary for |Eq [a]| from a dictionary for |Eq a|. 
Such a function is called a dictionary transformer. 
EHC allows not only predicates in the context, but also predicate transformers:
> g :: (forall a . Eq a => Eq [a]) => Int -> [Int] -> Bool
> g = \p q -> [p] == q
% TODO:: Beter example: instance Eq for data Fix f = In (f (Fix f))
The predicate transformer argument is used to resolve overloading in the above function.

\section{Implementation of overloading}
The richer type language of EH unfortunately implies a more complex implementation. 
In particular, allowing predicates to occur anywhere in a type means that EH has to anticipate for dictionaries to pass and be passed at arbitrary argument positions.
This is illustrated by the following example:
> index   :: forall b . [b] -> forall a . Num a => a -> b
> index = \xs       n   ->   if n == 0  
>                            then  head xs
>                            else  index (tail xs) (n-1)
> 
> main = index (Sup(varpi1)) "class" (Sup(varpi2)) 4  (Sup(varpi3))
There are three possible positions where dictionaries could be inserted at the place where the function |index| is used.
Compare this with Haskell~98 which only allows dictionaries before the first parameter of an identifier (position |varpi1|).
In EH predicate positions correspond directly to argument passing of dictionaries, a type therefore describes the parameter passing order.
EHC instantiates the type of the function |index| to |[Char] -> forall a . Num a => a -> Char|. 
No dictionaries have to be inserted at position 1 because there is no predicate before |[Char]| in the type. 
Furthermore, EHC instantiates |forall a . Num a => a -> Char| when the second parameter is applied to the function.
The type variable |a| is instantiated to |Int| and the predicate |Num Int| has to be proven.
For that reason, the dictionary corresponding to the predicate |Num Int| has to be inserted at position |varpi2| in the abstract-syntax tree.
No dictionary has to be inserted at position |varpi3|.

\subsection{Predicate substitution}
The above features of EH prevent us from using the standard solution for resolving overloading, because predicates may occur at any position in a type.
Instead, resolving of overloading is implemented using a substitution which on top of mapping type variables to types, also describes where which predicates have to be inserted:
> bind  ::=    v       mapsto sigma  
>       |      varpi   mapsto (pi, varpi)
>       |      varpi   mapsto emptyset  
This substitution not only maps type variables |(v)| to types |(sigma)|, but also maps predicate variables |(varpi)| to predicates |(pi)|.
For example, consider the use of |index| again:
> main = index (Sup(varpi1)) "class" (Sup(varpi2)) 4  (Sup(varpi3))
Predicate variables are distributed over the abstract-syntax tree during the type inference process.
The information that becomes available during type inferencing is represented in the substitution.
For instance, the solution for the above program is the following substitution:
> S =    { varpi1 mapsto emptyset, varpi2 mapsto (Num Int, varpi4), varpi3 mapsto emptyset, varpi4 mapsto emptyset }
Positions in the abstract-syntax tree where no predicates have to be proven are mapped to the empty set.

\subsection{Type matching}
The type of an EH expression is inferred and checked by performing a subsumption check at each node of the abstract-syntax tree. 
The subsumption check matches the expected type |(sigmak)| against the actual type |(sigma)| of an expression:
> sigma <= sigmak
The substitution |(S)| is one of the results of this function. 

Consider again the following fragment:
> main = index "class" 4
We present the steps that are performed by the subsumption check to type the use of the function |index| below.
At the left hand side of the subsumption check (|<=|) we see the type of the function |index|.
The right hand side shows the type constructed from analyzing the abstract-syntax tree and the fact that a predicate may occur anywhere. 
In the rightmost column we show which constraints are added to the substitution |S|.
> forall b .   [b]    ->    forall a .   Num a    =>   a ->    b      <= varpi1 => [Char] -> varpi2 => Int -> varpi3 => v6    
>              [v7]   ->    forall a .   Num a    =>   a ->    v7     <= varpi1 => [Char] -> varpi2 => Int -> varpi3 => v6   ^^   ^^   ^^  varpi1  mapsto emptyset 
>              [v7]   ->    forall a .   Num a    =>   a ->    v7     <= [Char] -> varpi2 => Int -> varpi3 => v6                           v7      mapsto Char 
>                           forall a .   Num a    =>   a ->    Char   <= varpi2 => Int -> varpi3 => v6                    
>                                        Num v8   =>   v8 ->   Char   <= varpi2 => Int -> varpi3 => v6                                     varpi2  mapsto Num v8
>                                                      v8 ->   Char   <= Int -> varpi3 => v6                                               v8      mapsto Int
>                                                              Char   <= varpi3 => v6                                                      varpi3  mapsto emptyset
>                                                              Char   <= v6                                                                v6      mapsto Char
The following steps are taken to check the use of the function |index|:
\begin{myitemize} 
\item The function |index| is instantiated with a fresh type variable |(v7)|.
\item No predicate is present in the left hand side which could bind to |varpi1|, so |varpi1| binds to the empty set of predicates. 
\item The type variable |v7| is bound to Char.
\item The type of function |index| is again instantiated with a fresh type variable |(v8)|.
\item The predicate variable |varpi2| is bound to the predicate |Num v8|.
\end{myitemize}
The subsumption check proceeds until the type is checked or an error is found.
The subsumption check also result in a list of predicates that must be proven in order to resolve overloading.
The context must provide a proof for the predicate |Num Int| because this predicate occurs at the left hand side of the above example.
The proof can not yet be given, so it is delayed until later, in particular when dealing with the enclosing let.

\subsection{Context-reduction rules}
EHC generates a rule for each class and instance declaration.
These rules are used to perform context reduction and to construct the code that has to be inserted.
A rule consists of the following components:
> Rule (sigma, vartheta, name, i, c)
A rule has a type (|sigma|), code for constructing evidence (|vartheta|), a name, a unique identifier, and a number indicating the cost of applying the rule.
The unique identifier |(i)| is a tuple consisting of a number identifying the scope in which the class or instance declaration occurs and a number identifying the rule itself. 
The cost |(c)| consist of the level of the rule and the cost of constructing a dictionary with this rule.
For a class declaration of the form: |class (pi1, ... , pin) => pi| the following rules are generated given a context identifier (|contextId|):
> i insign (1 ... n) :
>    u fresh
>    Rule (pi -> pii, vartheta, _, (contextId, u), 1)
The first component of the rule |pi -> pii| is the type of the coercion function that selects a superclass from the subclass dictionary.
The second component is the coercion function itself.
The scope where this class declaration is introduced is identified with |contextId|.
The rule is further uniquely identified with the identifier |u|.
The last component indicates that the cost of applying this rule is 1.
 
For an instance declaration of the form: |instance (pi1, ... , pin) => pi| the following rule is generated given a dictionary name (|nm|) and a context identifier (|contextId|):
> u fresh
> Rule (pi1 -> ... -> pin -> pi, vartheta, nm, (contextId, u), 2 * n)
For an instance declaration only one rule is generated. 
The first component of the rule is the type of the  function for constructing the dictionary.
The second component is the code for applying the construction function itself.
The third component is the name of the construction function.
The instance rule is identified with |contextId| and a unique identifier.
Finally, the cost of applying the rules is the number of predicates in the context of the instance declaration times 2.

\section{Conclusion}
A nice aspect of EHC is that it implements an advanced type system in combination with very experimental type-class extensions.
Also the encoding of class and instance declarations in rules is an elegant abstraction.

The resolution of overloading with such an advanced type system is complex and not yet sufficiently documented in the literature~\citep{dijkstra05phd, dijkstra05making}.
It is a challenge to precisely describe how overloading can be resolved with the type system of EHC.

EHC allows qualifiers and predicates at the right hand side of the function type |(->)|.
Consequently, dictionaries have to be inserted not only after an instantiated variable, but at almost every position in the abstract-syntax tree. 
The question is if this feature justifies the complexities it introduces.
There are two important reasons to support this feature. 
First, this feature allows us to write shorter programs:
> f :: (forall a . Eq a => a -> a) ->  ...
> g ::  forall b . b -> forall a . Eq a => a -> a
>
> x = f (g 2)
The following program expresses the same fragment in a language that does not allow qualifiers and predicates at arbitrary positions in a type:
> g :: forall a b . Eq a => b -> a -> a
>
> x =  let  g' = g 2       in   f g' 
In this fragment we must first bind the application |(g 2)| in a let to trigger generalization.
Second, the combination of this feature with existential types has not yet been fully explored, but it is expected that it offers a flexible mechanism for constructing records with higher ranked functions taking predicates, an essential ingredient for module systems.

The encoding of class and instance declarations in rules is elegant. 
However, the code where predicates are simplified and resolved using these rules is very complex and proved to be difficult to understand.
Deep knowledge about the resolving of overloading is encoded in this algorithm and the challenge is to use a solver without specific knowledge of type classes.

Finally, the syntax for explicit implicit application and explicit explicit abstraction is a bit verbose~\citep{dijkstra05making}.
Besides specifying the value of the predicate also the type has to be given:| {! dictEqInt <: Eq Int !}|.
It would be nice if we can do the same without this additional predicate annotation.
