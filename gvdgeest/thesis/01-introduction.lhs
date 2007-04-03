%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% Chapter Introduction
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\chapter{Introduction}
\label{ch:introduction}
Take a look at the following sentence: ``Shall we take a break?".
At first sight this is a very simple sentence, however `break' already has at least 28 different meanings.
Luckily, we are able infer the meaning of break from the context.
However, this is not always the case. 
Consider the following example attributed to the linguist Noam Chomsky: ``Flying aircraft may be hazardous."
Does this sentence mean that it is hazardous to be a pilot, or that airplanes themselves may be hazardous when flying?
We cannot infer the meaning of this sentence from the context and thus we need some additional context information. 
Inferring the meaning of a word from the context is reminiscent of overloading in programming languages.

In a programming language we typically want to use the same function identifier for different operations; we call such a function {\it an overloaded function}.
An example of an overloaded function is the operator |(==)|: we want to use this operator to compare two integers, but also to compare two floating point values, and perhaps even to compare two strings for equality.
Because the implementation of comparing two integers is different from that of comparing two strings, the compiler must infer the meaning of the operator |(==)| from the context.

There is an important difference between overloading and polymorphism.
Overloaded functions execute different code for different types of argument.
Comparing two integers requires other code to be executed than when comparing two strings.
On the other hand, {\it polymorphic} functions execute the same code for different types of arguments.
For example, the function |length :: forall a => [a] -> Int| can be executed for all types of lists.
Defining equality as a polymorphic function is even impossible, because this would imply we would be able to define equality between functions.
Alternatively, an overloaded function can be implemented by testing the type of the operands and then execute different code for integers and strings.
This is not a very elegant and extensible solution for overloading.

Type classes were introduced by Kaes~\citep{kaes88par} and Wadler and Blot~\citep{wadler89how} as a solution for overloading identifiers. 
Their type class design was adopted by Haskell~\citep{jones03haskell}.
The original design of type classes was conservative and for that reason a large number of extensions are proposed~\citep{jones97type}.
Implementing those extensions in a compiler and experimenting with those extensions is not very easy.
In this thesis we propose a framework for resolving overloading in which various type class extensions can be formulated.
A requirement for this framework is that it must be relatively easy to experiment with type class extensions and various design decisions.

\section{Research problem}
\label{sec:researchproblem}
Much effort had gone into specifications~\citep{halletall96typeclasses, faxen-02} and implementations~\citep{augustsson93implementing, jones99typing} of Haskell type classes.
However, no clear specifications and implementations of the various extensions exist, let alone a framework in which we can easily formulate type classes and extensions to type classes.
We list some problems with type classes:
\begin{myitemize}
\item
There is no general framework which can be used by compilers to resolve overloading in Haskell.
Instead each compiler uses its own solution.
This makes it difficult to experiment with type classes.
\item
Type classes, like many other language features, come with a price tag in terms of runtime efficiency.
In general, we want to optimize away such a cost when the corresponding feature is used.
\item
Error messages concerning type classes are often difficult to understand~\citep{heerenhage05directives}.
\end{myitemize}

\section{Objectives}
\label{sec:objectives}
The proposed research is to create a constraint-based framework in which we can formulate Haskell type classes and its various extensions.
As a proof of concept we use this framework in the compilers EHC~\citep{dijkstra05phd} and Helium~\citep{Helium}.
We first list the requirements of this framework:
\begin{myitemize}
\item The framework must support Haskell~98 type
classes.
\item The framework must support multi-parameter type classes.
\item The framework must support local instances and explicit passing of type class dictionaries, such as formulated by Dijkstra and Swierstra~\citep{dijkstra05making}.
\item The framework must be general enough for use in the compilers EHC and Helium to resolve overloading and generate code for type classes.
\item It must be easy to formulate type class extensions in the framework.
\item The framework must be well documented and structured, so that people can easily use, adapt, and experiment with the framework.
\end{myitemize}

Second, we list additional requirements which are nice to implement, but the absence of which will not cause this project to fail.
\begin{myitemize}
\item It should be easy to incorporate optimizations~\citep{augustsson93implementing} applied to the generated code into the framework.
\item Also the framework should be designed so that existing and future work on type class directives~\citep{heerenhage05directives} can be integrated.
\item It would be very nice to implement functional dependencies~\citep{jones00type} in the framework or the alternative solution in terms of associated types~\citep{chakravarty05associated, chakravarty05associatedsyn}.
\end{myitemize}

\section{Contribution}
\label{sec:contribution}
Are we the first developing such a framework?
Yes and no, for instance, Chameleon~\citep{sulz01chameleon} is a Haskell-style language, which implements the ideas described by Stuckey and Sulzmann~\citep{stuckeysulz02overloading}.
In Chameleon it is possible to define overloaded functions.
However, Chameleon does not support Haskell~98 type classes nor any type class extensions.
Very interesting is the formulation of overloading in rules and the way users can supply rules to experiment with the type system.
In Chapter~\ref{ch:literature} we review the work of Sulzmann et all.
The second framework is Top~\citep{heeren05}.
This is a constraint-based type inferencer for Haskell designed to produce high quality type error messages.
Top supports Haskell~98 type classes, but no type class extensions.
The interesting part of Top is the way users can script the type inference process~\citep{Helscripting} to give better type error messages and the formulation of overloading resolution using constraints.

\section{Structure of this thesis}
In Chapter~\ref{ch:preliminaries} we introduce type classes and qualifier entailment.
We review in Chapter~\ref{ch:literature} a selection of the literature concerning type classes.
The following two chapters describe the current situation.
This framework must become part of the constraint-based type inferencer Top. 
Currently, Top is used by the Helium compiler. 
In Chapter~\ref{ch:helium} we explain how overloading is resolved in Top.
The framework we describe in this thesis is also going to be used in EHC.
Therefore, we explain in Chapter~\ref{ch:ehc} how overloading is implemented in EHC.
In Chapter~\ref{ch:constraints} we describe the first version of the framework.
We explain how overloading resolution can be formulated with constraints and how those constraints can be solved with Constraint Handling Rules~\citep{fruhwirth95chrs}.
In Chapter~\ref{ch:evidence} we extend the framework to translate a language with overloading into a language without. 
Furthermore, we show how design decisions can be specified in the form of heuristics.
We explain in Chapter~\ref{ch:localinstances} how local instances can be formulated in our framework.
In Chapter~\ref{ch:improving} we extend the framework with improving substitution and show how the existing translation from functional dependencies into CHRs can be used.
Chapter~\ref{ch:conclusion} concludes.
