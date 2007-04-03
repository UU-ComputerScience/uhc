\section{Introduction}
\label{introduction}
We present techniques for embedding languages in Haskell~\cite{jones03haskell}.
These techniques are based on work of Hinze~\cite{hinzequote} and Okasaki~\cite{okasaki02postfix}.
As a first example we introduce a language for natural numbers.
> quote tick tick tick end
>  + 
> quote tick tick tick tick end
Between the |quote| and |end| keywords we can write concrete syntax, in this case just a sequence of |n| ticks representing the number |n|.
So the embedding represents the expression |3 + 4|.
The language above look like a sequence of terminals, but in fact the terminals are just functions and the spaces just function applications.
In this paper we show how these functions can be implemented, and also how we can embed less trivial languages.


This technique can be used to implement Domain Specific Languages (DSLs).
A DSL is a language tailored for a specific application domain.
We all use Domain Specific Languages, such as SQL for querying databases and HTML for specifying the mark-up of a web-page.  
Writing specifications or programs in a DSL has many advantages: they are easier to write, reason about, and modify compared to programs written in general purpose languages.
But the cost of designing and implementing a DSL from scratch is so high that we probably never break even~\cite{hudak98modular}.

\includegraphics[width=10cm]{payoff.jpg}

There are existing efforts and approaches to lower the start-up costs of a DSL. 
Examples are Lisp macros, which have been used for years to develop embedded languages, or the software-generator approach, where a specification written in a DSL is used to generate a program.
We try to embed a DSL in Haskell without preprocessing or macro-expanding, but by just using the Haskell syntax for functions and function application.
The following features are crucial for the embedding of a DSL in Haskell: currying, higher-order functions, polymorphism, and type classes.
% DSEL ??
