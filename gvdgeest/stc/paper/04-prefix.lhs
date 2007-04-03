%format p1    = "\textcolor{red}{(\lambda c\ \to\ c)}"
%format p2    = "\textcolor{red}{(\lambda c\ \to\ Give\ c) }"
%format p3    = "\textcolor{red}{(\lambda c1\ c2\ \to\ Give\ (Or\ c1\ c2))}"
%format p4    = "\textcolor{red}{(\lambda c2\ \to\ Give\ (Or\ Zero\ c2))}"
%format p5    = "\textcolor{red}{(Give\ (Or\ Zero\ One))}"
%format p6    = "\textcolor{red}{Give\ (Or\ Zero\ One)}"

\section{Prefix notation}
\label{prefix}
\subsection{Introduction}
Embedding postfix notation in Haskell is possible, but can we do the same for prefix notation?
Prefix notation was invented by Jan Lukasiewics, a Polish mathematician. 
Therefore, it is also known as Polish notation.
In this notation a function precedes its arguments.
For example, |3 5 + 2 *| is prefix notation for the expression |(3 + 5) * 2|. 
Similar to postfix notation, it is not needed to write parenthesis in prefix notation, because the arity of functions is statically known.

Have a look at the contract from the last chapter in prefix notation:
> contract   =    quote give or zero one end
This contract should evaluate to |Give (Or Zero One)|.
In this section we first show a systematic translation of Haskell data constructors to a postfix notation embedding, then we show how the above example is implemented.

\subsection{Systematic translation of data types}
In postfix notation, a function follows its arguments, so a stack of arguments is a natural choice for the state.
But in prefix notation a function precedes its arguments so the state must be a stack of argument requests.
We represent a stack of argument requests by a function.
To enable prefix notation we first have to redefine the |quote| and |end| functions.
> quote    ::   CPS    (alpha   -> alpha)
> quote    =    lift   (\a      -> a)
> end      ::   alpha   ->    alpha
> end           a       =     a
The |quote| function pushes a request for one argument on the stack by lifting the identity function. 
The |end| function only stops the parsing process and returns the current state.

The |quote| and |end| function are independent from the data type to embed.
For writing a data type in prefix notation we only have to introduce a function |c| for each data constructor |C :: tau1 -> ... -> taun -> tau|.
> c ::   (tau -> alpha)   ->   CPS    (tau1 ->   ... ->   taun   ->    alpha)
> c      ctx              =    lift   (\t1       ...      tn     ->    ctx (C t1 ... tn))
The first argument of this prefix constructor is a function that requests something of type |tau|, also known as a context or expression with a hole. 
In order to deliver something of type |tau|, this function pushes argument request on the stack for |t1| till |tn|, by extending the function.
The type variable |alpha| represents the rest of the argument requests.

\subsection{Embedding financial contracts}
With the redefined quotation functions and the translation scheme for constructors we can embed prefix notation for the data type representing contracts.
> zero   ::   (Contract -> alpha)   ->   CPS     alpha
> zero        ctx                   =    lift    (ctx Zero)
> one    ::   (Contract -> alpha)   ->   CPS     alpha
> one         ctx                   =    lift    (ctx One)
> give   ::   (Contract -> alpha)   ->   CPS     (Contract -> alpha)
> give        ctx                   =    lift    (\c -> ctx (Give c))
> or     ::   (Contract -> alpha)   ->   CPS     (Contract -> Contract -> alpha)
> or          ctx                   =    lift    (\c1 c2 -> ctx (Or c1 c2))
As we can see  from the types, |zero| and |one| both reduce the number of pending arguments by one, |give| leaves the number of pending arguments the same, and |or| increases the number of arguments by one.

The types of the different subexpressions of our example also show the stack layout.
This statically guarantees that the syntax of our embedding is correct.
> quote                                  :: CPS (alpha -> alpha)
> quote   give                           :: CPS (Contract -> Contract)
> quote   give   or                      :: CPS (Contract -> Contract -> Contract)
> quote   give   or   zero               :: CPS (Contract -> Contract)
> quote   give   or   zero   one         :: CPS (Contract)
> quote   give   or   zero   one   end   :: Contract

> rquote       rgive   or     zero   one    end
> rgive   p1   ror     zero   one    end
> ror     p2   rzero   one    end
> rzero   p3   rone    end
> rone    p4   rend
> rend    p5
> p6
Again the expression is evaluated in the same order as an expression in postfix notation.
Only the state, and the functions manipulating the state have changed.
