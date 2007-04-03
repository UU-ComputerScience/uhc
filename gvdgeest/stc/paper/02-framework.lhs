%format <| = "\triangleleft"
%format rquote = "\textcolor{red}{quote}"
%format rend   = "\textcolor{red}{end}"
%format rtick  = "\textcolor{red}{tick}"
%format rlift  = "\textcolor{red}{lift}"
%format r0     = "\textcolor{red}{0}"
%format r01    = "\textcolor{red}{(0 + 1)}"
%format r1     = "\textcolor{red}{1}"
%format r11    = "\textcolor{red}{(1 + 1)}"
%format r2     = "\textcolor{red}{2}"
%format r21    = "\textcolor{red}{(2 + 1)}"
%format r3     = "\textcolor{red}{3}"
%format term1 = "term_1"
%format termn = "term_n"


\section{A Framework}
\label{framework}
\subsection{Introduction}
Let's take a closer look at the example shown in the introduction.
This example looks like a sequence of instructions, but in fact the instructions are just functions and the spaces just function applications.
Function application is left-associative in Haskel, so one should read the example as:
> three = (((quote tick) tick) tick) end
Function application in Haskell is prefix, that is, a function precedes its arguments. 
If function application was both left-associative and postfix, that is, a function follows its arguments, we could simply implement the running example with the following functions:
> quote   ::              Int
> quote              =    0
>
> tick ::      Int   ->   Int
> tick         i     =    i + 1
>
> end ::       Int   ->   Int
> end          = id
There is nothing special about these functions, except that they are applied in postfix style.
We can see this very nicely if we show the evaluation steps.
The part of the expression colored red is evaluated in the next step.
>          rquote   tick   tick   tick   end
> r0       rtick    tick   tick   end
> r1       rtick    tick   end
> r2       rtick    end
> r3       rend
> r3
But the above functions will not work because Haskell doesn't have postfix function application. 
Ideally, we want to have the example work in Haskell and keep the simplicity of the postfix functions.

\subsection{Developing a Framework}
One idea is to introduce a postfix operator, which emulates postfix function application by first expecting an argument, and then a function.
> infixl 0 <|
>
> (<|) :: alpha -> (alpha -> r) -> r
>
> x <| f = f x
>
> three  = quote <| tick <| tick <| tick <| end
Note that this operator can also be defined as |(<||) = flip ($)|.
If we now write this operator between the terminals of the concrete syntax for naturals, we almost have a concrete syntax embedding, and simple function definitions.
After every terminal there is an operator, except for |end|.
This is because we want to stop with parsing after the terminal |end|. 
Our last step is try to get rid of the operators between the terminals of the concrete syntax. 
This can be arranged by building the operator inside each terminal function, except for |end|.
To reach this, we introduce the function |lift|, which has the same implementation as the postfix operator |(<||)|.
One can read this function as: I expect another function that will be applied on the result of this function.
We also introduce the type synonym CPS for the type of the lifted value. 
CPS is a monad, we will explore the relation between CPS and monads in the next subsection.
> type CPS alpha = forall r . (alpha -> r) -> r
>
> lift ::   alpha   ->    CPS alpha
> lift      a       =     \f -> f a
>
> quote   ::              CPS    Int
> quote              =    lift   0
>
> tick ::      Int   ->   CPS    Int
> tick         i     =    lift   (i + 1)
>
> end ::       Int   ->   Int
> end          =  id
>
> three = quote tick tick tick end
Now we can redefine the terminals of the concrete syntax for naturals, so we get a language embedding with simple function definitions.
If we take a closer look at the evaluation of the quotation we can see very clearly that the lift function models prefix function application.
> rquote                   tick    tick    tick    end
> rlift     r0    rtick    tick    tick    end
> rtick     r0             tick    tick    end
> rlift     r01   rtick    tick    end
> rtick     r1             tick    end
> rlift     r11   rtick    end
> rtick     r2             end
> rlift     r21   rend
> rend      r3
> r3
The expressions colored red are evaluated in the next step.
The second column is the state threaded through the terminals. 
Each terminal is applied to the state, just like postfix function application.
The lift function is responsible for this kind of behavior.

\subsection{Continuation Monads}
The technique to more or less emulate left-associative postfix function application is reminiscent of Continuation Passing Style (CPS).
CPS is a style of programming where one passes control to a function argument.
We use the |lift| function to lift a value into a CPS type.
As mentioned earlier, this type is also a monad.
The following implementation is the default for |Monad CPS|.
> instance Monad CPS where
>   return a   = \k -> k a
>   c >>= f    = \k -> c (\a -> f a k)
Note that a type synonym cannot be an instance of a type class in Haskell, but this presentation makes the examples more readable and easier to explain.
The |return| function of the CPS monad has the same implementation as the |lift| function of the framework earlier defined.
But the |>>=| operator seems unrelated, because in the |CPS| monad the continuation stands for the rest of the computation whereas in the running example it stands for the next parsing step.
Fortunately, there is another implementation for the instance |Monad CPS| and that is the monad for partial continuations.
> instance Monad CPS where
>   return a   = \k -> k a
>   c >>= f    = c f
The monad for partial continuations exactly captures our earlier defined framework. 
The implementation of |return| is unchanged, but the implementation of the |>>=| operator is now just function application.
If we now define a function to run a CPS computation, we can implement the running example in monadic style.
> run :: CPS alpha -> alpha
> run m = m id
>
> three = run (return 0 >>= tick >>= tick >>= tick)
The monadic style implementation of the running example is exactly the same as the implementation of the last subsection.
We prove this for the general case:
> quote term1 ... termn end = run (return 0 >>= term1 >>= ... >>= termn)
We use the earlier defined implementations of |quote|, |end| and |run|, and |return| and |>>=| are the monad for partial continuations.
> quote term1 ... termn end
= \{definition of |end|\}
> quote term1 ... termn id
= \{definition of |run|\}
> run (quote term1 ... termn)
= \{definition of |>>=|\}
> run (quote  >>= term1 >>= ... >>= termn)
= \{definition of |quote|\}
> run (lift 0 >>= term1 >>= ... >>= termn)
= \{definition of |lift|\}
> run ((\f -> f 0) >>= term1 >>= ... >>= termn)
= \{definition of |return|\}
> run (return 0 >>= term1 >>= ... >>= termn)
We have shown that the technique used to parse terminals is an instance of partial continuations.
In the running example the state threaded to the terminals was just a single integer.
In the next sections we only change the state to parse more advanced languages: we leave the parsing technique introduced in this section untouched.
