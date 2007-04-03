\section{LL(1) parsing}
\label{ll1}
\subsection{Introduction}
The next step is to parse a language described by an LL(1) grammar.
LL(1) grammars are the class of grammars that can be parsed by an LL(1) parser.
An LL(1) parser is a top-down parser that parses the input from left to right and constructs the leftmost derivation. 
LL grammars are popular, because it is simple to create parser for them by hand.
The 1 in LL(1) means that the parser uses one token of look-ahead when parsing a sentence. 
In this section we explain the architecture of an LL(1) parser, then we give a systematic translation for embedding languages described by an LL(1) grammar.
Finally, we embed an even more advanced syntax for financial contracts.
> quote
>   give (one dollar and one euro)
>   or
>   give zero
> end
With this syntax we can combine contracts with left-associative infix |and| and |or| operators where |and| binds stronger then |or|.
Also parentheses can be used to show which contracts should be combined first, or to make the existing priorities explicit.

\subsection{Architecture of an LL(1) parser}
Using the grammar describing the new syntax for financial contracts, we explain the architecture of an LL(1) parser.
The following grammar describes the new syntax, and because this grammar is not LL(1) due to the productions of |S| and |AC|, we left factor |S| and |AC|.
After this operation we get an equivalent grammar which is LL(1).
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{lrlllrlrl}
S  &$\to$& AC or S  &             & S   &$\to$& AC S'    &    & \\
   &$||$ & AC       &             & S'  &$\to$& or AC S' &$||$& $\epsilon$\\
AC &$\to$& C and AC &             & AC  &$\to$& C AC'     &    & \\
   &$||$ & C        &$\Rightarrow$& AC' &$\to$& and C AC' &$||$& $\epsilon$\\
C  &$\to$& ( S )    &             & C   &$\to$& ( S )     &    & \\
   &$||$ & give C   &             &     &     & give C    &    & \\
   &$||$ & one  CU  &             &     &$\to$& one CU    &    & \\
   &$||$ & zero     &             &     &$||$ & zero      &    & \\
CU &$\to$& euro     &             & CU  &$\to$& euro      &    & \\
   &$||$ & dollar   &             &     &$||$ & dollar    &    & \\
\end{tabular}
\)\par\noindent\endgroup
An LL(1) parser needs a parsing table to decide which production it should expand depending on the non-terminal on top of the stack and the next input symbol.
To construct a parsing table, we have to know the set of first terminals for each non-terminal, including the empty derivation. 
If the empty derivation is in the first set of a non-terminal, we also need the follow set of the non-terminal to construct the parsing the table.
The follow set is the set of terminals that can follow a non-terminal in any derivation from the start symbol.

The following first- and follow sets are calculated for the grammar of our running example:
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{llll}
first(S )  & = \{(, give, one, zero\}   & follow(S )   & = \{)\} \\ 
first(S')  & = \{or, $\epsilon$\}       & follow(S')   & = \{)\} \\ 
first(AC)  & = \{(, give, one, zero\}   & follow(AC)   & = \{or, )\} \\ 
first(AC') & = \{and, $\epsilon$\}      & follow(AC')  & = \{or, )\} \\
first(C)   & = \{(, give, one, zero\}   & follow(C)    & = \{and, or, )\} \\ 
first(CU)  & = \{euro, dollar\}         & follow(CU)   & = \{and, or, )\} \\ 
\end{tabular}
\)\par\noindent\endgroup
To construct a parsing table for every terminal $x$ in the first set of non-terminal $A$, a production of the form $A \to w$ must be filled in at table position $T[A, x]$.
If the empty derivation is in the first set of $A$, also a production for each terminal in the follow-set must be filled in.
Applying these rules on the first- and follow sets results in the following parsing table.
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{l||llllll}
      &S            &S'              & AC           & AC'              & C            & CU \\
\hline
or    &             &S'$\to$or AC S'  &              & AC'$\to\epsilon$ &              &  \\
and   &             &                 &              & AC'$\to$and C AC'&              &  \\
(     &S $\to$AC S' &                 & AC$\to$C AC' &                  & C$\to$( S )  &  \\
)     &             &S'$\to\epsilon$  &              & AC'$\to\epsilon$ &              &  \\
give  &S $\to$AC S' &                 & AC$\to$C AC' &                  & C$\to$give C &  \\
one   &S $\to$AC S' &                 & AC$\to$C AC' &                  & C$\to$one CU &  \\
zero  &S $\to$AC S' &                 & AC$\to$C AC' &                  & C$\to$zero   &  \\
euro  &             &                 &              &                  &              &CU$\to$euro  \\
dollar&             &                 &              &                  &              &CU$\to$dollar  \\
end   &             &S'$\to\epsilon$  &              & AC'$\to\epsilon$ &              &  \\
\end{tabular}
\)\par\noindent\endgroup
Before embedding the syntax in Haskell, we first show how an LL(1) parser uses the parsing table to parse a list of input symbols.
An LL(1) parser consists of an input buffer, a stack on which it keeps symbols from the grammar, and a parsing table.

The parser starts with pushing the special terminal 'end' on the stack, which indicates the end of the input stream. Next it pushes the start symbol of the grammar on the stack:
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{l||l||l}
Step &Stack                                &Input buffer     \\
\hline
1    & S, end                              & give one euro end \\
2    & AC, S', end                         & give one euro end \\
3    & C, AC', S', end                     & give one euro end \\
4    & give, C, AC', S', end               & give one euro end \\
5    & C, AC', S', end                     & one euro end \\
n    & end                                 & end \\
\end{tabular}
\)\par\noindent\endgroup
Then only two operations take place. 
If the symbol on top of the stack is a nonterminal, an expansion takes place.
If it is a terminal, a parse takes place.
In the first step, the nonterminal S is on top of the stack, and the symbol 'give' is the next input symbol.
The parser looks up the expansion rule in table position T[S, give], and expands S to AC S'. 
In the second step, AC expands in the same way to C AC', and in the third step C expands to give C.
Now the terminal 'give' is popped from the top of the stack and is matched with the next input symbol, if they are the same, the input symbol is parsed.
Finally, the string is successfully parsed if the parser pops 'end' from the stack and reads 'end' from the input buffer.
If the terminal on top of the stack does not matches the terminal on the input buffer, the parser will report an error and stops.
Also if there is no rule in the parsing table for a given symbol non-terminal pair, the parser will stop and report an error.

\subsection{Systematic translation}
This section describes the systematic translation of an LL(1) grammar to an embedding in Haskell.
The state is a stack of pending symbols, both terminal and nonterminal symbols. 
The stack is again represented by nested data types.
For this reason, a data type has to be introduced for each terminal and nonterminal. 
For each symbol |A| we introduce a data type:  |newtype A alpha = A (S -> alpha)|, where |S| is the type of the semantic value associated with the symbol.
A symbol could have no semantic value if it just needs to be recognized, then the type becomes: |newtype A alpha = A alpha|.

The stack is initialized by the |quote| function.
This function pushes the start symbol on the stack and expects a semantic value, which is left untouched.
> quote = lift (S  (\x -> x))

The terminals must both encode the expansion rules of the parsing table, and the parsing of a terminal.
Which action has to take place depends on the symbol on top of the stack.
If the symbol on top of the stack is a nonterminal, an expansion has to be performed, otherwise a parsing step has to be taken place.
To support different operations depending on the top of the stack we use type classes.
For each terminal we introduce a type-class, and for each operation the terminal must support an instance is introduced.
For a terminal $t$ we introduce the following type-class.
> class   CT old new | old -> new where
>         t :: old -> CPS new
The functional dependency |old -> new| ensures that we cannot embed an ambiguous grammar.
The functional dependency means that the symbol on top of the stack uniquely determines the expansion that has to be taken place.


First we have to encode the parsing of a terminal: we introduce an instance so for each terminal $t$.
> instance CT (T alpha) alpha where
>   t (T ctx ) = lift (ctx s)
|CT| is the class we have introduced for the terminal $t$, $T$ is the data type introduced for terminal $t$, and |s| is the semantic value associated with parsing the terminal $t$.
If |s| is omitted, the parser is just a recognizer.


The second action that must be encoded is the expansion rules of the parsing table.
For each rule in the parsing table of the form $N \to B_1 \dots B_n$, at table position $T[N, t]$, the following instance is introduced.
> instance   CT   (N alpha)          ...   where
>   t    (N ctx) = t        (B1 (\v1 ->   ... ->   Bn (\vn -> ctx (f v1 ... vn))))
Note that $B_1 \dots B_n$ can be both terminals and nonterminals. 
The meaning of this function is that if the non-terminal $N$ is on top of the stack, the stack is extended with the symbols $B_1 \dots B_n$ we expect to parse, and combine the values of parsing $B_1 \dots B_n$ using the semantic function |f|.
The most notable difference with the instance for parsing a symbol is that instead of |lift|, |t| is applied to the modified stack.
This means that we do not parse the next symbol, but keep expanding till the correct terminal is on top of the stack.
The type that must be filled in on the place of the dots is not trivial. 
It is the expansion of |N| on the stack, after parsing terminal |t|.
This will become more clear if we show a concrete example.


For each rule in the parsing table of the form $N \to \epsilon$ at table position $T[N, t]$, the following instance is introduced.
> instance (CT  alpha beta) => CT  (N alpha) beta where
>   t (N ctx ) = t (ctx s)
The meaning of this function is that if nonterminal |N| is on top of the stack, we apply |ctx| on the semantic value |s|.
Again, we don't parse the next symbol but keep expanding. 
Because in this case there are no additional terminals or nonterminals pushed on the stack, the stack layout is not known when applying the overloaded function |t|.
This is the reason why we need to specify the context information |(CT alpha beta)|.



\subsection{Embedding financial contracts}
To make the translation more concrete we show some snippets of the code to implement the language for financial contracts.
The complete implementation can be found on \url{http://www.cs.uu.nl/wiki/Stc/TypedQuoteAntiquote}.
The language will be parsed to the abstract syntax, defined by the following data types: 
> data Contract   =   Or     Contract   Contract
>                 |   And    Contract   Contract
>                 |   Give   Contract
>                 |   One    Currency
>                 |   Zero 
> data Currency   =   Euro
>                 |   Dollar

The first step is to introduce a data type for each terminal and nonterminal.
For the terminal |give| we introduce the data type: |newtype G alpha = G alpha|.
Parsing terminal |give| has no semantic value.
For the non-terminals |S|, |AC|, and |C| we introduce the following data types:
> newtype S     alpha =   S    (Contract               -> alpha)
> newtype AC    alpha =   AC   (Contract               -> alpha)
> newtype C     alpha =   C    (Contract               -> alpha)
The parsing of each of the nonterminals yields a value of type |Contract|.

To support the different operations we introduce a type class for each terminal. 
For example, we introduce for the terminal |give| the following type class:
> class Give  old new | old -> new where
>     give  :: old -> CPS new

To implement the parse operation we introduce the following instance of the class:
> instance Give (G alpha) alpha where
>     give (G ctx) = lift ctx
The instance is used if data type |G|, the representation of terminal |give|, is on top of the stack, |G| is popped from the stack, and we proceed by parsing the next terminal. 

To explain how the expand action is implemented, recall the state of an LL(1) parser.
\begingroup\par\noindent\advance\leftskip\mathindent\(
\begin{tabular}{l||l||l}
Step &Stack                                &Input buffer     \\
\hline
1    & S, end                              & give one euro end \\
2    & AC, S', end                         & give one euro end \\
3    & C, AC', S', end                     & give one euro end \\
4    & give, C, AC', S', end               & give one euro end \\
5    & C, AC', S', end                     & one euro end \\
\end{tabular}
\)\par\noindent\endgroup
To implement the expand action, we introduce the following instance for the production S |->| AC S'.
>instance Give (S  alpha)   (C (AC' (S'  alpha))) where
>    give   (S  ctx)   = give (AC (\t -> S'  (\e' -> ctx (e' t))))
The function performs the expand action, and constructs the semantic value by applying the function that results from parsing S' to the result of parsing AC.
The most interesting part of this instance is the type |(C (AC' (S'  alpha)))|: this type can be explained if we look at the stack layout of step 5 in the table above.
The type is in fact the remaining expansion of S after parsing the terminal give.

The same holds for the implementation of the productions: |AC -> C AC'| and |C -> give C|.
>instance Give (AC alpha)   (C (AC' alpha)) where
>    give   (AC ctx)   = give (C (\f -> AC' (\t' -> ctx (t' f ))))
>
>instance Give (C alpha)    (C alpha) where
>    give   (C ctx)    = give (G (C (\c -> ctx (Give c))))
