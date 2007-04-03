%format rone  = "\textcolor{red}{one}"
%format rzero = "\textcolor{red}{zero}"
%format ror   = "\textcolor{red}{or}"
%format rgive = "\textcolor{red}{give}"
%format s1    = "\textcolor{red}{()}"
%format s2    = "\textcolor{red}{((), One)}"
%format s3    = "\textcolor{red}{(((), One), Zero)}"
%format s4    = "\textcolor{red}{((), Or\ One\ Zero)}"
%format s5    = "\textcolor{red}{((), Give\ (Or\ One\ Zero))}"
%format s6    = "\textcolor{red}{Give\ (Or\ One\ Zero)}"

\section{Postfix notation}
\label{postfix}
\subsection{Introduction}
In this section we show how to embed postfix notation in Haskell. 
Postfix notation, also known as Reverse Polish Notation (RPN), was invented by Australian philosopher and computer scientist Charles Hamblin.
It is derived from the Polish notation, which was introduced by the Polish mathematician Jan Lukasiewicz.
We will treat Polish or prefix notation in the next section.

In postfix notation, functions follow their arguments.
So |3 5 + 2 *| is postfix notation for the expression |(3 + 5) * 2|.
In postfix notation there is no need for writing parentheses, because the arity of functions is statically known. 
Haskell data constructors can be written in postfix notation form, but Haskell functions not, because the arity of Haskell functions is not statically known.

Postfix notation is evaluated using a stack.
If we evaluate the expression |3 5 + 2 *|, |3| and |5| are pushed on the stack.
Then the operator |+| pops two values from the stack and pushes the result back.
Next 3 is pushed on the stack, and, finally, the operator |*| pops two values from the stack and pushes the result back.


In this section we first show a systematic translation of Haskell data constructors to a postfix notation embedding, then we show how to embed postfix notation for financial contracts~\cite{jones01contracts}.

\subsection{Systematic translation}
The state threaded through the terminals of concrete syntax for natural numbers was just a single integer. 
Because evaluation of postfix notation is stack-based, the state is now a stack of arguments.
We represent the stack by nested tuples growing from left to right.
The stack could also be represented by a list, but that is not the right choice.
First, because nested tuples can contain elements of different types and a list not. 
Second, because the types of nested tuples correspond very closely to the actual values on the stack. 
This can be illustrated by the redefined |quote| and |end| functions:
> quote ::   CPS       ()
> quote =    lift      ()
> end ::   ((), alpha)   ->    alpha
> end      ((), a)       =     a
The |quote| function initializes the state to the empty stack, and the |end| function expects a singleton stack and pops the value from the stack.
The types correspond to the actual stack layout. 


The |quote| and |end| functions are independent from the actual data type we embed. 
To embed a data type in postfix notation we only have to introduce a function |c| for each data constructor |C :: tau1 -> ... -> taun -> tau |.
> c ::   (((alpha   , tau1   ), ...), taun)    ->    CPS    (alpha, tau)
> c      (((st      , t1     ), ...), tn)      =     lift   (st, C t1 ... tn)
The modification of the stack is visible in the type: the stack must contain at least |n| arguments of the correct type before we can apply |c|.
The values are popped from the stack and combined with the constructor |C|, and the result of this is pushed back on the stack.
The type variable |alpha| represents the type of the rest of the stack and the variable |st| represents the value of the rest of the stack.


\subsection{Embedding financial contracts}
As an example, we show an embedding for a combinator library expressing financial contracts \cite{jones01contracts}.
Simple financial contracts can be represented by the following data type:
> data Contract   =   Zero 
>                 |   One
>                 |   Give   Contract
>                 |   Or     Contract Contract
|Zero| is a contract without rights and obligations.
|One| is a contract that immediately pays the holder one unit.
|Give c| is a contract to acquire all the rights of |c| as obligations, and vice versa.
If you acquire |Or c1 c2| then you acquire |c1| or |c2|.


Postfix notation for financial contracts is not the most intuitive DSL for a domain expert, but for us it is a nice starting point.
> contract   =    quote one zero or give end
The above embedding should evaluate to: |Give (Or One Zero)|.
The holder of this contract has the obligation to give one unit or has no obligations.
We can now introduce a postfix constructor for every data constructor:
>zero    ::  alpha                                    ->    CPS    (alpha, Contract)
>zero        st                                       =     lift   (st, Zero)
>one     ::  alpha                                    ->    CPS    (alpha, Contract)
>one         st                                       =     lift   (st, One)
>give    ::  (alpha, Contract   )                     ->    CPS    (alpha, Contract)
>give        (st, c             )                     =     lift   (st, Give c)
>or      ::  ((alpha, Contract   ),   Contract   )    ->    CPS    (alpha, Contract)
>or          ((st, c1            ),   c2         )    =     lift   (st, Or c1 c2)
The postfix constructors show very nicely, both in the implementation and the type, how the stack is manipulated.
The functions |zero| and |one| push a value on the stack, |give| first pops a value and pushes back the result, and |or| first pops two values from the stack and pushes back the result.

Also the types of the subexpressions correspond very nicely to the stack layout.
This ensures that we get a type error if we make an error in postfix notation for financial contracts.
>quote                          :: CPS ()
>quote one                      :: CPS ((), Contract)
>quote one zero                 :: CPS (((), Contract), Contract)
>quote one zero or              :: CPS ((), Contract)
>quote one zero or give         :: CPS ((), Contract)
>quote one zero or give end     :: Contract

>rquote          rone    zero    or      give    end
>rone      s1    rzero   or      give    end
>rzero     s2    ror     give    end
>ror       s3    rgive   end
>rgive     s4    rend
>rend      s5
>s6
In the evaluation steps of this example, the expressions colored red are evaluated in the next step. 
The state is visualized in the second column and each terminal is applied exactly once to the state, in order of appearance.
