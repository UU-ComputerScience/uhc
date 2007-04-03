\begin{abstract}
We present work of Hinze~\cite{hinzequote} and Okasaki~\cite{okasaki02postfix} about a systematic approach to
embed languages in Haskell~\cite{jones03haskell} with a quote/antiquote mechanism. 
A quote/antiquote mechanism makes it possible to use the concrete syntax of a specific language inside a host language by enclosing it in quotes.
Inside the quotation one can return to the host language by using an antiquotation.

This technique can be used to embed for instance Domain Specific Languages (DSLs) in Haskell.
Domain Specific Languages have many benefits: programs are easier to understand and reason about.
The additional start-up costs of implementing a lexer, parser, compiler, and pretty-printer for a DSL however, are often too high.
We can lower those costs by embedding a DSL inside another language.
Such a language is called a Domain Specific Embedded Language (DSEL).

In this paper we introduce an increasingly complex syntax for a combinator library expressing Financial Contracts~\cite{jones01contracts}.
We start with postfix notation, prefix notation, and a grammar in Greibach Normal Form (GNF) for this DSEL.
Then we proceed by implementing a syntax given by an LL(1) grammar.
We show how to translate those grammars to an implementation in Haskell.
To accomplish this, we use: overloading, polymorphism, and higher order functions.
Also techniques reminiscent of the continuation monad are used.
\end{abstract}
