\chapter{Preliminaries}
\label{ch:preliminaries}
In this chapter we give some preliminary information concerning type classes and constraints.
We first introduce various aspects of type classes: class and instance declarations, context reduction, dictionary translation, and ambiguities.
In the last part of this chapter we introduce the entailment relation and qualified types.

\section{Type classes}
\label{sec:typeclasses}
Type classes are proposed as a solution for overloading identifiers in Haskell.
In this subsection we introduce Haskell~98 type classes and describe how a language with overloading can be translated into a language without overloading.

\subsection{Class declarations}
In Haskell~98, overloaded functions are declared as part of a {\it class declaration}:
> class Eq a where
>   (==)   ::   a -> a -> Bool
>   (/=)   ::   a -> a -> Bool
>
>   x /= y = not (x==y)
>   x == y = not (x/=y)
This introduces a new {\it type class predicate} named |Eq|.
Furthermore, two overloaded operators for equality (|==|) and inequality (|/=|) are introduced.
These operators are overloaded in the type variable |a|.
The type variable |a| is scoped over the class signature and the operator signatures.
Also, {\it default definitions} are given for both operators.
Type classes can also be arranged in a hierarchy; we can declare for instance:
> class Eq a => Ord a where
>   (<=) :: a -> a -> Bool
A class declaration consists of two parts; the part before the |=>| is called the {\it context} and the part after the |=>| is called the {\it head}.
Here the context specifies that |Eq| is a {\it superclass} of |Ord|.
The superclass relation must form a directed acyclic graph.
In Figure~\ref{haskellclasses} we present an overview of the standard Haskell classes.

\begin{Figure}{Standard Haskell classes}{haskellclasses}
\begin{center}
\includegraphics[scale=0.80]{graphs/Classes2.png}
\end{center}
\end{Figure}


\subsection{Instance declarations}
We can make a type an instance of a type class with the {\it instance declaration}.
For example, the following instance declarations show how equality and ordering on booleans is defined and how equality on lists is defined.
> instance Eq Bool where
>   True    ==   True    =   True
>   False   ==   False   =   True
>   _       ==   _       =   False
>
> instance Ord Bool where
>   _       <=   True    =   True
>   False   <=   _       =   True
>   _       <=   _       =   False
>
> instance Eq a => Eq [a] where
>    []       == []       =    True
>    (x:xs)   == (y:ys)   =    x==y && xs==ys
>    _        == _        =    False
In the first instance declaration we define equality on booleans.
We only have to specify a definition for equality, because we already have specified a default definition for inequality in the class declaration.
The third instance means that once we have equality on elements |(Eq a)|, we know how to provide equality on lists (|Eq [a]|).
This can also be seen in the definition of |(==)|, first equality on the first elements of the lists is determined (|x == y|), then the tails of the lists are compared for equality by a recursive call (| xs == ys |).
Note that |[[a]]| and |[[[a]]]| are now also instances of |Eq|, so this instance declaration makes an infinite number of types instances of |Eq|.

\subsection{Functions and context reduction}
The introduced type classes are used in the following functions for testing if a list is still ordered after an insertion:
> insert  :: Ord a => a -> [a] -> [a]
> sort    :: Ord a => [a] -> [a]
>
> testInsert :: Ord a => a -> [a] -> Bool
> testInsert x xs =  let  ys = insert x (sort xs)
>                    in   sort ys == ys
The signature contexts of the above functions mention the earlier introduce type class |Ord|. 
|Ord a| in the above signatures is a {\it type class predicate} or {\it type class qualifier}.
This is an additional requirement on the type variable |a|, restricting |a| to types that are instances of |Ord|.
When inferring the type of the function |testInsert|, we must prove the predicate |Ord a| because the functions |insert| and |sort| are used; luckily we may assume that there is an instance for |Ord a| because this predicate is also mentioned in the signature of |testInsert|.
However, the overloaded operator |(==)| is also used |(sort ys == ys)|, so we also have to prove the predicate |Eq [a]|.
The reason why we do not find this predicate in the signature is that |Eq [a]| can be derived from |Eq a| via the instance declaration for lists and |Eq a| can be derived from |Ord a| because |Eq| is a superclass of |Ord|.
This process is called context reduction, which is an optimization that minimizes the number of predicates required in a type signature.

\subsection{Dictionary translation}
A Haskell program with overloading can be translated into a Haskell program without, using the dictionary translation.
In Figure~\ref{translation} we show the translation of the example code snippets introduced in this chapter.
The class declarations are translated into datatype declarations and functions adding the default declarations to a dictionary.
Note how the superclass/subclass relation between |Ord| and |Eq| is visible in the field |deq| of datatype |DictOrd|.
The instance declarations are translated into functions constructing the dictionaries.

\begin{Figure}{Haskell program resulting from the dictionary translation}{translation}
%{
%format eq = (==)
%format ne = (/=)
%format le = (<=)
%include 02-translation.lhs
%}
\end{Figure}

The predicate in the function |testInsert| is translated into an additional parameter.
The caller of the function |testInsert| has to provide this parameter and it can be used in the body.
Finally, the overloaded functions are selected from the dictionary parameter with exactly the same derivation as we saw when we performed context reduction.

\section{Caveats}
\subsection{Ambiguous types and defaulting}
Using Haskell overloading can cause ambiguous types.
An {\it ambiguous type} is a type of the form |forall a . ctx => tau|, where |a| is occurring in |ctx|, but not in |tau|~\citep{jones03haskell}.
For example, the following function has an ambiguous type:
> am :: (Show a, Read a) => String
> am =  let  x = read "1.5" 
>       in   show x
>
> read   :: Read  a => String -> a
> show   :: Show  a => a -> String
The ambiguity arises because the use of |am| does not specify to which type |a| should be instantiated.
The reason for this is that |a| does not occur outside the context of the type scheme of |am|.
This is a consequence of the implicit nature of Haskell's type inference system.
Haskell provides several ways around this, which all make explicit to what type |a| should instantiate.
For instance, a programmer can annotate a value with an explicit type, such as |(x::Double)|.
Because such ambiguities often occur in the context of class |Num|, Haskell provides a way to resolve them without explicit type annotations, the {\it default declaration}: 
> default (t1, ... , tn)
where |n >= 0| and |tn| must be an instance of the |Num| class. An ambiguous type variable |v| can be defaulted if:
\begin{itemize}
\item
|v| appears in a predicate of the form |C a|, where |C| is a class, and
\item
at least one of these classes is |Num|, or a subclass of |Num|,
\item
all of the these classes are defined in the Haskell 98 Prelude.
\end{itemize}
The ambiguous type variable |v| is defaulted to the first type |ti| that is an instance of all the involved type classes.


\subsection{The monomorphism restriction}
In addition to the usual Hindley-Milner restriction that a type variable can only be generalized if it does not occur free in the environment, Haskell adds another restriction called the {\it monomorphism restriction}~\citep{jones03haskell}.
A constrained type variable is not generalized when a binding group is restricted.
Type variables are constrained if there is a class predicate concerning that type variable.
A binding group is unrestricted if:
\begin{itemize}
\item
every variable in the group is bound by a function binding or a simple pattern binding, and
\item
an explicit type signature is given for every variable in the group that is bound by a simple pattern binding.
\end{itemize}
Any monomorphic type variables that remain when type inference for an entire module is complete, are considered ambiguous, and are resolved to particular types using the defaulting rules.

As a consequence, the inferred type for the function |f x y = x + y| is what we expect: |Num a => a -> a -> a|.
However, the type remains monomorphic if we define the same function with a simple pattern binding instead of a function binding: |f = \x y -> x + y|.
A type is defaulted using the default declaration if the type remains monomorphic after type inferencing the entire module.
With the following default declaration:
> default (Int, Double)
the type of the function |f| is defaulted to |Int -> Int -> Int|, because |Int| is the first type in the default declaration that is an instance of |Num|.

%geef vb waarom, bijv:
%x = 3
%dat zonder monores het volgende type zou hebben:
%x :: Num a => a
%ergo: verlies van sharing, onverwachte (vanuit perspectief onwetende gebruiker) herberekening bij elke ref naar x
%overigens: menigeen gruwt van deze restrictie...

\section{Entailment and qualified types} 
Type class predicates are an example of qualified types~\citep{jones92theory}. 
A qualifier places an extra restriction on type variables.
For example, the qualifier in the signature of |testInsert|: |Ord a => a -> [a] -> Bool|, restricts |a| to the members of type class |Ord|.
Other qualifiers are used to type extensible records~\citep{jones99lightweight}:
\begin{myitemize}
\item |(r  has l :: a)| means that record |r| has a field |l| of type |a|.
\item |(r  lacks l)| means that the record |r| does not has a field |l|.
\end{myitemize}
The {\it entailment} relation |(entails)| describes the relation between two finite sets of qualifiers.
The meaning of this relation depends on the qualifiers used.
However, in Figure~\ref{basicent} we list three properties that hold for all qualifiers.
In these rules |P, Q,| and |R| are sets of qualifiers and |S| is a substitution mapping type variables to types.

The specific properties for type class qualifiers are listed in Figure~\ref{classent}.
For each class declaration: |class Q => pi|, we add |(Class Q => pi)| to the environment |(Gamma)| and for each instance: |instance Q => pi|, we add |(Inst Q => pi)|.
Let us first consider the |Inst| rule:
A set of predicates |P| entails |pi| if there is an instance |Inst Q => pi| and |P entails Q|.
Consider the following example derivation to illustrate the |Inst| rule.
We assume that the instance |(Inst {Eq a} => Eq [a])| is in the environment.
\begin{center}
\rulerRule{|(Closed)|}{}
          {\rulerRule{|(Inst)|}{}
           {|(Inst {Eq a} => Eq [a]) insign Gamma ^^ ^^ ^^| \rulerRule{|(Mono)|}{} {}{| {Eq a} entails {Eq a} |}}
           {|{Eq a} entails {Eq [a]}|} }
           {|{Eq v1} entails {Eq [v1]}|}
~\\
~\\
~\\
\end{center}

We search for a predicate |pi| that matches the {\it head} of an instance when using the |Inst| rule.
On the other hand, when using the |Super| rule, we search for a predicate |pi2| that is part of the {\it context}.
This is an important difference between the |Inst| and the |Super| rule, because the arrow in a class declaration could easily be misinterpreted as implication. 

\begin{Figure}{Basic rules for qualifier entailment}{basicent}
\begin{center}
~\\
\rulerRule{|(Mono)|}{}
          {|P supseteq Q|}
          {|P entails Q|}
\rulerRule{|(Trans)|}{}
          {|P entails Q ^^ ^^ ^^ Q entails R|}
          {|P entails R|}
\rulerRule{|(Closed)|}{}
          {|P entails Q|}
          {|SP entails SQ|}
~\\
~\\
\end{center}
\end{Figure}



\begin{Figure}{Additional rules for entailment relation of type class qualifiers}{classent}
\begin{center}
~\\
\rulerRule{|(Super)|}{}
          {|P entails pi1 ^^ ^^ ^^ pi2 insign Q| \\
           |(Class Q => pi1)  insign Gamma|}
          {|P entails {pi2}|}
\rulerRule{|(Inst)|}{}
          {|P entails Q  ^^ ^^ ^^ (Inst Q => pi) insign Gamma|}
          {|P entails {pi}|}
~\\
~\\
\end{center}
\end{Figure}

