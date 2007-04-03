\section{Greibach Normal Form}
\label{gnf}
\subsection{Introduction}
In this section we extend the techniques used for embedding prefix notation to embed languages described by a grammar in Greibach Normal Form.
A grammar is in Greibach Normal Form (GNF) if all the productions are of the form: $N \to \alpha B_1 \dots B_n$, where $N$ is a nonterminal and $\alpha$ is a terminal followed by a possibly empty sequence of nonterminals.
A grammar in GNF cannot generate the empty word ($\epsilon$).
Every context-free grammar, which does not generate the null string, can be transformed to GNF.


A grammar in GNF is syntactically unambiguous if every pair of productions $N_1 \to \alpha B_1$ and $N_2 \to \beta B_2$ satisfies ($N_1 = N_2 \land \alpha = \beta) \Rightarrow B_1 = B_2$.
So for each production $N \to \alpha B_1 \dots B_n$, $N$ together with $\alpha$ must uniquely determine the following sequence of non-terminals ($B_1 \dots B_n$).
If you compare a nonterminal with a data type and a terminal with a constructor, an unambiguous grammar in GNF has the same expressive power as Haskell data type declarations.
Unfortunately this is not completely true, because the same terminal may appear in different productions, and in Haskell the same constructor cannot appear in different data type declarations. 
It appears that we can nicely encode a terminal with multiple occurrences using type classes.
We can enforce unambiguity with functional dependencies.


In this section we first show a systematic translation of grammars in GNF to an embedding in Haskell,
then we show how an extended syntax for financial contracts is embedded. 
> quote   give one dollar
>    or   give one euro
>    or   give zero
> end
In the extended syntax, a currency has to be given for the |one| contract.
Also the operator for combining contracts with |or| is now a left associative infix operator in the embedded language. 
Note that in the implementation this operator is an ordinary Haskell function.

\subsection{Systematic translation}
The embedding of a grammar in GNF is very similar to the embedding of prefix notation. 
The state is a stack of requests for nonterminals (the nonterminals we expect to parse).
The stack is represented by nested data types, for each nonterminal: |N|, a data type: |newtype N alpha = N (S -> alpha)|, is introduced.
The type-variable |alpha| is the type of the rest of the stack, and |S| is the type of the requested argument, or in other words the type of the semantic value that parsing the nonterminal |N| yields.


Because a terminal can occur in multiple productions, a type class must be introduced for each terminal $a$.
> class   A old new | old -> new where
>         a :: old -> CPS new
The functional dependency |old -> new| is pronounced as: |old| uniquely determines |new|. 
This enforces that the embedded grammar must be unambiguous.


For each production $N \to aB_1 \dots B_n$, an instance of the earlier introduced class must be defined:
> instance   A   (N alpha)          (B1 (         ...      (Bn alpha) ...))   where
>            a   (N ctx) = lift     (B1 (\v1 ->   ... ->   Bn (\vn -> ctx (f v1 ... vn))))
The meaning of this function is that we can parse the terminal $a$ if non-terminal $N$ is on top of the stack.
In order to parse nonterminal $N$ we expect to parse the nonterminals |B1 ... Bn| and combine the semantic values of parsing |B1 ... Bn| with the semantic function |f|.
The semantic function |f| yields the semantic value expected by the context |ctx|.


If a terminal $a$ occurs only once in the grammar, for each production $N \to aB_1 \dots B_n$ a function $a$ has to be introduced.
>   a ::   (N alpha)   ->   CPS    (B1 (         ...      (Bn alpha) ...))
>   a      (N ctx)     =    lift   (B1 (\v1 ->   ... ->   Bn (\vn -> ctx (f v1 ... vn))))
The implementation of the function is the same as the overloaded one.
The function implicitly satisfies the functional dependency because we can only introduce one function with the name $a$.


\subsection{Embedding financial contracts}
The translation scheme for embedding languages described by a grammar in GNF can now be applied to the extended syntax for financial contracts.
We first give the grammar of the extended syntax.
Because this grammar is not in GNF, we also show the grammar after left-factoring the productions of nonterminal |CS|.
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lrlllrl}
S    &$\to$&quote CS end &             &S  &$\to$&quote C CS\\
CS   &$\to$&C or CS      &             &CS &$\to$&or C CS\\
     &$||$ &C            &             &   &$||$ &end\\
C    &$\to$&give C       &$\Rightarrow$&C  &$\to$&give C\\
     &$||$ &one CU       &             &   &$||$ &one CU\\
     &$||$ &zero         &             &   &$||$ &zero\\
CU   &$\to$&euro         &             &CU &$\to$&euro\\
     &$||$ &dollar       &             &   &$||$ &dollar\\
\end{tabular}
\)\par\noindent\endgroup
\noindent


If we now add the production: C $\to$ give CU to the grammar in GNF, the grammar becomes ambiguous.
The nonterminal C and the terminal give no longer uniquely determine the following nonterminal, it could either be C or CU.
The functional dependency will statically prevent that we embed an ambiguous grammar.

The abstract syntax of the financial combinators is defined by the following data type declarations:
>type Contracts   =   [Contract]
>data Contract    =   Give    Contract
>                 |   One     Currency
>                 |   Zero
>data Currency    =   Euro | Dollar

The constructors will build the above abstract syntax, but of course other semantic functions can be used. 
To define more complex semantic functions, an Attributed Grammar (AG) system could be used.
The first step we should do now is introduce data types for each nonterminal, and determine the type of the semantic functions:
> newtype S    alpha = S    (Contracts -> alpha)
> newtype CS   alpha = CS   (Contracts -> alpha)
> newtype C    alpha = C    (Contract  -> alpha)
> newtype CU   alpha = CU   (Currency  -> alpha)
These types can, for example, be read as: in order to parse the nonterminal |CU|, a value of type |Currency| is needed.

The second step is to introduce an active terminal function for every production.

>quote                  =    lift   (C (\c -> CS (\cs -> c:cs)))
>or          (CS ctx)   =    lift   (C (\c -> CS (\cs -> ctx (c:cs))))
>end         (CS ctx)   =    ctx []
>zero        (C  ctx)   =    lift   (ctx Zero)
>one         (C  ctx)   =    lift   (CU (\cur -> ctx (One cur)))
>give        (C  ctx)   =    lift   (C  (\c   -> ctx (Give c )))
>dollar      (CU ctx)   =    lift   (ctx Dollar)
>euro        (CU ctx)   =    lift   (ctx Euro  )

The |quote| function is the only function without the context parameter, because it both starts the quotation and parses the left-hand side of the $S$ non-terminal.
Note that |quote| can be defined in terms of |or|.
The |end| function is the only function which doesn't lift the computation into the |CPS| monad, because it serves as an end mark of the quotation.
