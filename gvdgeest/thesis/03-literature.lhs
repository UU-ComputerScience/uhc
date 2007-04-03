%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Chapter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\chapter{Literature on type classes}
\label{ch:literature}
This chapter summarizes a selection of literature concerning type classes.
First, to give some background information for this thesis, and furthermore, to make a clear separation between our work and work of others.
We give a literature overview focusing on two aspects of type classes:
\begin{myitemize}
  \item Design; the design of type classes and various extensions to type classes.
  \item Constraints; the resolving of overloading using a constraint solver.
\end{myitemize}


\section{Design of type classes}
\label{sec:designoftypeclasses}
Type classes were introduced by Wadler and Blot~\citep{wadler89how} and Kaes~\citep{kaes88par}.
Their type class design was adopted by Haskell~\citep{jones03haskell}.
In this chapter we give a chronological overview of the various papers that have appeared about the design of type classes.

\subsection{A system of constructor classes}
The first extension, called constructor classes, was proposed by Jones~\citep{jones93system}.
This extension also allows type constructors to be in a class.
For example, the type class |Monad| is defined for types of kind |* -> *|, with instances for |[]| and |Maybe|:
>class Monad m where
>   return   :: a -> m a
>   (>>=)    :: m a -> (a -> m b) -> m b
>
>instance Monad Maybe where
>   return x   = Just x
>   n >>= f    = case  n of
>                      Nothing   -> Nothing
>                      Just x    -> f x
Here |m| in the definition of the class |Monad| is a type of kind |* -> *|, a type-level function from proper types to proper types.
The type expression |m a| means |m| applied to |a|.
|Maybe| and |[]| are both of kind |* -> *| so we can make them instances of |Monad|.
This extension is part of Haskell~98.

\subsection{A second look at overloading}
Odersky, Wadler, and Wehr~\citep{odersky95second} describe a simple restriction on type classes which ensures that no ambiguities can arise.
The restriction is that each function that is overloaded over type variable |a| must have a type of the form |a -> t|, where |t| may itself involve |a|.
Their redesign of type classes is called {\it System O}.
Below we present a System O example which we cannot express with Haskell 98 type classes:
>over   first
>
>inst   first :: (a, b) -> a
>       first (x, y) = x
>
>inst   first :: (a, b, c) -> a
>       first (x, y, z) = x
>
>ff :: (first :: a -> b, first :: b -> c) => a -> c
>ff r = first (first r)
Note that we can express this example using multi-parameter type classes and functional dependencies which we will explain later.
System O has a number of advantages: one does not have to decide in advance which operations belong together in a class and the definition of overloaded operators is less verbose.
On the other hand, the inferred types become more verbose because each overloaded function is mentioned separately in the context.
% niet beslissen over class hierarchy is ook nadeel, bijv omdat het ad-hoc definities makkelijk maakt zonder goed na te denken over de samenhang van overloaded functies

\subsection{Type classes: an exploration of the design space}
Peyton Jones, Jones, and Meijer~\citep{jones97type} argue that the original design of type classes is fairly conservative.
They discuss a number of design choices for each of the following aspects: multi-parameter type classes, context reduction, overlapping instances, instance types, instance contexts, superclasses, improving substitution, and class declarations.
In the conclusion the authors take the following design decisions:
\begin{myitemize}
  \item Allow multi-parameter type classes.
  \item No limitations on superclass contexts. 
  \item Allow the class variables to be constrained in the class-member type signatures.
  \item Forbid overlapping instance declarations.
  \item Allow arbitrary instance types in the head of an instance declaration, except that at least one must not be a type variable.
  \item Allow repeated type variables in the head of an instance declaration.
  \item Restrict the context of an instance declaration to mention type variables only.
  \item Allow arbitrary contexts in types and type signatures.
  \item Use the instance context reduction rule only when forced by a type signature, or when the overloading can be resolved at compile time.
\end{myitemize}
The Haskell compiler GHC~\citep{Ghc} implements these design choices when enabling extensions.
Even overlapping instances are allowed with an additional flag.
Furthermore, the design choice that the context of an instance declaration may only mention type variables is relaxed.
However, the predicates in the context must have fewer constructors and variables (taken together and counting repetitions) than the head.

\subsection{Type classes with functional dependencies}
Jones~\citep{jones00type} shows that the use of multiple-parameter type classes often causes ambiguities and inaccuracies.
To tackle this problem the author introduces {\it functional dependencies} between parameters of type classes.
In the example below: |a -> b| (|a| uniquely determines |b|), means that the relation between |a| and |b| is a function.
This means, for example, that it is forbidden to define both the instance |D Int Bool| and |D Int String|.
>class C a b 
>class D a b | a -> b 
>class E a b | a -> b, b -> a 
In the last class declaration the relation between |a| and |b| is a one-to-one mapping. 
So if there is an instance |E Int Bool| there cannot be another instance where either |a| is |Int|, or |b| is |Bool|.

The example we gave for System O can also be expressed with multi-parameter type classes and functional dependencies:
> class First a b | a -> b where
>   first :: a -> b
>
> instance First (a, b) a where
>   first (x, y) = x 
>
> instance First (a, b, c) a where
>   first (x, y, z) = x
>
> ff :: (First a b, First b c) => a -> c
> ff x = first (first x)

\subsection{Making implicit parameters explicit}
Dijkstra and Swierstra~\citep{dijkstra05making} describe how dictionaries can be passed explicitly to overloaded functions.
Consider the function below that checks if a value is an element of a list.
The predicate |Eq a| in the type of the function |elem| corresponds to an implicit argument, the dictionary for the predicate |Eq a|.
If we use this function we do not have to supply the dictionary for |Eq a|, it is automatically inserted by the compiler.
> elem :: Eq a => a -> [a] -> Bool
> elem x []       = False
> elem x (y:ys)   = x == y || elem x ys
>
> test xs = elem 4 xs
However, if we want to use this function with another equality, for example modulo 2, we cannot specify this, because it is not possible to influence the machinery that automatically inserts the dictionary.
With the extension described by Dijkstra and Swierstra it is possible to insert the dictionary implicitly and explicitly.
We can for example define a named instance of |Eq Int| where equality is defined modulo 2.
> instance dEqIntMod :: Eq Int where
>   x == y = primEqInt (x `mod` 2) (y `mod` 2)
>
> test xs = elem ^^ {!dEqIntMod <: Eq Int!} ^^ 4 xs
Now we can explicitly insert the named dictionary between the |{! ... !}| signs, where otherwise the normal dictionary for equality would be inserted.
The authors present three different instance declaration:
> instance                Eq Int where ...
> instance dEqIntMod <:   Eq Int where ...
> instance dEqIntMod ::   Eq Int where ...
The first two instance declarations participate in the machinery for automatic dictionary translation, and the last one does not.
Additionally the last two instance declarations can be used to explicitly insert dictionaries by referring to the introduced identifier to which the dictionary is bound.

\section{Constraints and type classes}
\label{sec:constraintsandtypeclasses}

\subsection{Type classes and constraint handling rules}
Glynn, Stuckey, and Sulzmann~\citep{glynn00type, sulz06extracting} describe how class and instance declarations can be translated into Constraint Handling Rules~(CHRs).
Furthermore, the authors show how instance declarations can be checked with CHRs and how predicates can be simplified using CHRs.
CHRs are a high level declarative language extension especially designed for writing constraint solvers~\citep{fruhwirth95chrs}.
CHRs are often embedded in a host language: constraints are defined in CHRs, but auxiliary computations are executed in the host language.
CHRs operate on a set of constraints and rewrite constraints into simpler ones until they are solved.

The following propagation CHR is generated for a class declaration of the form: |class Q => pi|.
> pi ==> Q
This propagation constraint means that the constraints |Q| can be inserted if |pi| is an element of the constraint set.
A propagation CHR is applied only once to each constraint to prevent non-termination. 
The following simplification CHR is generated for an instance declaration of the form: |instance Q => pi|.
> pi <=> Q
This simplification CHR means that |pi| can be {\it replaced} with |Q| when |pi| is an element of the constraint set. 
If the set |Q| is empty it is abbreviated with |true|.
CHRs are generated for all instance and class declarations in a program.
Instance declarations are correct when the generated CHRs form a confluent set.
A set of rules is {\it confluent} if from any given constraint set every possible order of rule applications ends in the same final constraint set.
For example, this is not the case with overlapping instances:
> data Foo = Foo
>
> instance Eq [Foo] where
>   xs == ys = length xs == length ys
The following CHRs are generated for the above instance declaration and the standard instance declaration for lists:
> Eq [a]    <=>  Eq a
> Eq [Foo]  <=>  true 
These rules are non-confluent because there are two possible derivations for solving the constraint |Eq [Foo]|.  
However, if we would allow overlapping instances then we typically choose the most specific instance.
The most specific instance is |instance Eq [Foo]| in this case.

The CHRs generated for class and instance declarations can be used to perform context reduction. 
However, an additional step is needed to simplify constraints using the class hierarchy.
The following CHRs are generated for a class declaration of the form |class (p1, ... , pin) => pi| to simplify constraints using the class hierarchy:
> pi, pii <=> pi
The CHRs resulting from the first translation simplify constraints using instance declarations and propagate the class hierarchy to check instance declarations.
The CHRs resulting from the second translation simplifies constraints using the class hierarchy.
To perform context reduction, constraints are first solved using the CHRs resulting from the first translation and the result is solved with the CHRs resulting from the second translation.
The authors do not explain how explicit type signatures are checked with CHRs.

\subsection{Implementing overloading in Chameleon}
Stuckey and Sulzmann present a minimal extension of the Hindley/Milner system to support overloading of identifiers~\citep{stuckeysulz02overloading}. 
This approach relies on a combination of the HM(X) type system framework with CHRs.
This proposal provided the basis for the Chameleon language described by Sulzmann and Wazny~\citep{sulz01chameleon}.
Chameleon supports only single overloaded definitions, so it is not possible to group overloaded identifiers in classes and arrange those classes in a hierarchy.
However, the programmer can specify arbitrary additional conditions by providing CHR propagation rules in the program text.
With these user provided CHRs it is possible to mimic Haskell class hierarchies. 
Here we will focus on the translation scheme described to resolve overloading in Haskell.

The authors use the same translation scheme as described in the previous subsection.
Consider the following example presented in the Chameleon paper:
> class Eq a where 
>   (==) :: a -> a -> Bool
> class Eq b => Foo a b where
>   foo :: b -> a
> instance Eq a => Foo [a] a
The following CHRs are generated for the above class and instance declarations:
> Foo a    b ==>  (Sub ((Eq b)) 1)   ^^   ^^     --   (Foo)
> Foo [a]  a <=>  (Sub ((Eq a)) 2)   ^^   ^^     --   (FooInst)
A propagation CHR is generated for the declaration of class |Foo| and a simplification CHR is generated for the instance declaration.
The overloaded functions |(==)| and |foo| are used in the following definition:
> f :: (Sub ((Foo a b)) 6) => b -> (Bool, a)
> f x = ((Sub foo 4) [x] (Sub (==) 3) x, (Sub foo 5) x) 
Analyzing the function |f| results in the following constraints:
> { (Sub ((Eq b)) 3), (Sub ((Foo [b] b)) 4), (Sub ((Foo a b)) 5), (Sub ((Foo a b)) 6) }
Each constraint is annotated with a location identifier.
Applying the generated CHRs to the constraints results in the following constraint set:
> {  (Sub ((Eq b)) 3), (Sub2 ((Eq b)) (4,2)), (Sub ((Foo a b)) 5), (Sub2 ((Eq b)) (5,1))
> ,  (Sub ((Foo a b)) 6), (Sub2 ((Eq b)) (6,1))                                            }
The constraints are annotated with justifications to keep track of the history of CHR applications.
The first four constraints are called inferred constraints whereas the last two constraints result from the type signature.
Propagated inferred constraints are discarded. 
An example of such a discarded constraint is |(Sub2 ((Eq b)) (5,1))| which results from applying the propagation rule |1| to the constraint at location |5|. 
The last step is to match the remaining inferred constraints against annotated constraints: |(Sub ((Foo a b)) 5)| is matched against |(Sub ((Foo a b)) 6)| and |(Sub ((Eq b)) 3), (Sub2 ((Eq b)) (4,2))| against |(Sub2 ((Eq b)) (6,1))|.
The justifications are used to generate evidence for the remaining inferred constraints. 

The authors admit that there is some non-determinism when matching inferred constraints against annotated constraints and solving constraints with CHRs.
However, they do not remove this non-determinism because the different solutions denote the same result.

\subsection{CHRs and functional dependencies}
Duck, Peyton Jones, Stuckey, and Sulzmann give a reformulation of functional dependencies in terms of CHRs~\citep{fds-chrs, sulz06chrfd}.
For example, consider the following class and instance declarations:
> class Coll c e | c -> e where
>   empty   :: c
>   insert  :: e -> c -> c
>   member  :: e -> c -> Bool
>
> instance Ord a => Coll [a] a where 
>   empty   = []
>   insert  = (:)
>   member  = elem
The improvement rules for the functional dependency |c -> e| are expressed with the following CHRs:
> Coll c e, Coll c d    ==> e == d
> Coll [a] b            ==> a == b
The first rule is generated for the functional dependency |c -> e| and expresses that if there are two constraints over the same type |c|, then it must be that |e| and |d| are the same type.
The second rule is generated for the instance declaration.
The authors have verified that the termination and consistency conditions~\citep{jones00type} are sufficient to guarantee sound and decidable type inference.
They even show how those conditions can be safely relaxed.

\subsection{Type class directives}
Heeren and Hage~\citep{heerenhage05directives} propose a number of directives to improve type error messages concerning Haskell type classes.
The authors introduce directives besides class and instance declarations to describe type classes.
These directives are used to improve error messages concerning type classes.
With these directives one is able to exclude a type from a type class, allow only a finite number of types in a type class, and express that the types in two type classes are disjoint.
Below are some example directives:
> never       Eq (a -> b)          : "functions cannot be tested for equality"
> never       Num Bool             : "arithmetic on booleans isn't supported"
> close       Integral             : "the only Integral instances are Int and Integer"
> disjoint    Integral Fractional  : "something which is fractional can never be integral"
Directives are checked during context reduction and the associated error message is presented if a directive does not hold.
