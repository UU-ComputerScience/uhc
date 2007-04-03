\documentclass{beamer}
\usepackage{calc}
\usepackage{color} 

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

\beamerboxesdeclarecolorscheme{goodpopup}{red}{yellow!60!black}
\beamerboxesdeclarecolorscheme{uu}{white}{white}
% If you want to change block colors
%\setbeamercolor{block title}{fg=white,bg=uuyellow}
\setbeamercolor{block body}{fg=black,bg=block title.bg!50!bg}

\newcommand{\bbropen}[1]{\begin{beamerboxesrounded}[shadow=true,scheme=uu]{#1} }
\newcommand{\bbrclose}{\end{beamerboxesrounded}}
\newcommand{\ignore}[1]{}
\setlength{\abovedisplayskip}{0cm}
\setlength{\belowdisplayskip}{0cm}
\newenvironment{slide}[1]{\begin{frame}\frametitle{#1}}{\end{frame}}


%\usetheme[]{uu}
%\usetheme[style=fancy]{uu}
%\usetheme{Berlin}
\usecolortheme[style=fancy]{uubeamer}

%format . = " . "
%format forall = "\forall"
%format tau1   = "\tau_1"
%format taun    = "\tau_n"
%format t1   = "t_1"
%format tn    = "t_n"
%format alpha   = "\alpha"
%format beta    = "\beta"
%format gamma   = "\gamma"
%format tau     = "\tau"
%format Bn      = "B_n"
%format B1      = "B_1"


\begin{document}


\title[Typed Quote/Antiquote]{Typed Quote/Antiquote}
\subtitle{Embedding languages in Haskell}
\author[Gerrit van den Geest]{Gerrit van den Geest}

\institute[Center for Software Technology]{
Center for Software Technology, Universiteit Utrecht\\
http://www.cs.uu.nl/groups/ST/ \\
$\ $\\
$\ $\\
Based on work of Ralf Hinze and Chris Okasaki}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Titlepage
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\begin{frame}
\titlepage
\end{frame}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Motivation
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Motivation}

\begin{slide}{Domain Specific Languages}
\begin{block}{A Domain Specific Language (DSL) is:}
\begin{itemize}
   \item A language for a particular domain
   \item Counterpart of a general-purpose language
   \item Not always a programming language
   \item For example: SQL, shell scripting, HTML, Make
\end{itemize}
\end{block}
\pause
\begin{block}{Programs written in a DSL are:}
\begin{itemize}
   \item Easier to write
   \item Easier to reason about
   \item Easier to learn for a domain expert
   \item Easier to modify
\end{itemize}
\end{block}
\end{slide}



\begin{slide}{The payoff of DSL technology}
    \includegraphics[width=10cm]{payoff.jpg}
\end{slide}


\begin{slide}{Domain Specific Embedded Languages (DSELs)}
\begin{block}{Inherit the infrastructure of another language}
\begin{itemize}
   \item Macro expansion of DSL to another language
   \item Generating software from a specification in a DSL
   \item Embed a DSL in another language
\end{itemize}
\end{block}
\pause
\begin{block}{Embed a DSL in Haskell}
\begin{itemize}
   \item Currying
   \item Higher-order functions
   \item Polymorphism
   \item Type classes (overloading)
\end{itemize}
\end{block}
\end{slide}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Framework
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Framework}


\begin{slide}{Embedding concrete syntax in Haskell}
\begin{block}{Concrete syntax for natural number}
> test =   quote
>             tick
>             tick
>             tick
>          end
>          +
>          quote
>             tick
>          end
>
How can we implement this?
\end{block}
\end{slide}


\begin{slide}{How we going to implement this?}
\begin{block}{Function application is left-assiociative}
> (((quote tick) tick) tick) end
\end{block}
\pause
\begin{block}{If Haskell had postfix function application...}
> quote   ::              Int
> quote              =    0  
>
> tick ::      Int   ->   Int   
> tick         i     =    i + 1   
>
> end ::       Int   ->   Int   
> end          i     =    i  
\end{block}
\end{slide}

%format <| = "\triangleleft"
\begin{slide}{Postfix function application}
\begin{block}{Introduce an postfix operator}
> (<|) :: alpha -> (alpha -> r) -> r
>
> x <| f = f x
>
> test  = quote <| tick <| tick <| tick <| end
\end{block}
\pause
\begin{block}{Pushing the operator into the terminals}
> quote ::                (Int -> r) -> r
> quote              =    (<|) 0
>
> tick ::      Int   ->   (Int -> r) -> r
> tick         i     =    (<|) (i + 1)
>
> end ::       Int   ->   Int   
> end          i     =    i  
\end{block}
\end{slide}

\begin{slide}{Finishing touch}
\begin{block}{The CPS monad}
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
> end          i     =    i  
>
> test = quote tick tick tick end  
\end{block}
\end{slide}

%format rlift  = "\textcolor{red}{lift}"
%format rquote = "\textcolor{red}{quote}"
%format rend   = "\textcolor{red}{end}"
%format rtick  = "\textcolor{red}{tick}"
%format r0     = "\textcolor{red}{0}"
%format r01    = "\textcolor{red}{(0 + 1)}"
%format r1     = "\textcolor{red}{1}"
%format r11    = "\textcolor{red}{(1 + 1)}"
%format r2     = "\textcolor{red}{2}"
%format r21    = "\textcolor{red}{(2 + 1)}"
%format r3     = "\textcolor{red}{3}"
\begin{slide}{Evaluation steps}
\begin{block}{The quotation is evaluated like this}
> quote      =   lift   0  
> tick   i   =   lift   (i + 1)
> end    i   =   i 
>
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
\end{block}
\end{slide}

\begin{slide}{Ready for the next step}
\begin{block}{What we have}
\begin{itemize}
   \item Framework for postfix function application
   \item Threading a state through the terminals
\end{itemize}
\end{block}
\pause
\begin{block}{The next step}
\begin{itemize}
   \item Embed postfix notation 
   \item Embed prefix  notation    
\end{itemize}
\end{block}
\end{slide}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Postfix notation
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Postfix notation}

\begin{slide}{Postfix and Prefix notation}
\begin{block}{Postfix notation}
\begin{itemize}
   \item Functions follow their arguments
   \item 3 5 + 2 *
   \item Also known as Reverse Polish Notation (RPN)
   \item Stack-based implementation
\end{itemize}
\end{block}
\pause
\begin{block}{Prefix notation}
\begin{itemize}
   \item Functions precedes their arguments
   \item * + 3 5 2
   \item Also known as Polish notation
   \item Haskell data contructors
\end{itemize}
\end{block}
\end{slide}

\begin{slide}{Example}
\begin{block}{Financial Combinators (S. Peyton-Jones et al.)}
> data Contract   =   Zero 
>                 |   One
>                 |   Give   Contract
>                 |   Or     Contract Contract
\end{block}
\pause
\begin{block}{Concrete syntax}
> contract   =    quote
>                    one zero or give
>                 end 
Should evaluate to: |Give (Or One Zero)|
\end{block}
\end{slide}


\begin{slide}{Systematic translation} 
\begin{block}{Redefine quotation}
> quote    ::   CPS    ()
> quote    =    lift   ()
>
> end      ::   ((), alpha)  ->    alpha
> end           ((), a    )  =     a
\end{block}
\pause
\begin{block}{Translation of constructors}
Introduce for each contructor |C :: tau1 -> ... -> taun -> tau|,
a postfix function (terminal) |c|:
>
> c ::   (((st, tau1   ), ...), taun)    ->    CPS    (st, tau)
> c      (((st, t1     ), ...), tn)      =     lift   (st, C t1 ... tn)
\end{block}
\end{slide}


\begin{slide}{Example translation}
\begin{block}{Example constructors}
> Zero, One   :: Contract
> Give        :: Contract -> Contract
> Or          :: Contract -> Contract -> Contract
\end{block}
\begin{block}{Translated postfix functions (terminals)}
>zero    ::  st                                    ->    CPS    (st, Contract)
>zero        st                                    =     lift   (st, Zero)
>
>give    ::  (st, Contract   )                     ->    CPS    (st, Contract)
>give        (st, c          )                     =     lift   (st, Give c)
>
>or      ::  ((st, Contract   ),   Contract   )    ->    CPS    (st, Contract)
>or          ((st, c1         ),   c2         )    =     lift   (st, Or c1 c2)
\end{block}
\end{slide}


\begin{slide}{Concrete example, types} 
\begin{block}{The types correspond to the stack layout}
>quote      ::                                              CPS    ()
>zero,one   ::  st                                    ->    CPS    (st, Contract)
>give       ::  (st, Contract   )                     ->    CPS    (st, Contract)
>or         ::  ((st, Contract  ),   Contract   )     ->    CPS    (st, Contract)
>end        ::  ((), alpha)                           ->    alpha
>
>quote                           :: CPS ()
>quote one                       :: CPS ((), Contract)
>quote one zero                  :: CPS (((), Contract), Contract)
>quote one zero or               :: CPS ((), Contract)
>quote one zero or give          :: CPS ((), Contract)
>quote one zero or give end      :: Contract
\end{block}
\end{slide}


%format red (s) = "\textcolor{red}{" s "}"
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

\begin{slide}{Concrete example, evaluation} 
\begin{block}{Evaluation steps}
>quote                                             =     lift   ()
>zero        st                                    =     lift   (st, Zero)
>one         st                                    =     lift   (st, One)
>give        (st, c          )                     =     lift   (st, Give c)
>or          ((st, c1         ),   c2         )    =     lift   (st, Or c1 c2)
>end         ((), a    )                           =     a
>
>rquote          rone    zero    or      give    end
>rone      s1    rzero   or      give    end
>rzero     s2    ror     give    end
>ror       s3    rgive   end
>rgive     s4    rend
>rend      s5
>s6
\end{block}
\end{slide}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Prefix notation
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Prefix notation}


\begin{slide}{Prefix notation}
\begin{block}{Embedding prefix notation}
\begin{itemize}
   \item Systematic translation for embedding data types
   \item State is a stack of pending arguments
   \item Stack is represented by a function
   \item Running example: Financial Combinators
\end{itemize}
\end{block}
\pause
\begin{block}{Concrete syntax}
> contract   =    quote
>                    give or zero one
>                 end 
Should evaluate to: |Give (Or Zero One)|
\end{block}
\end{slide}


\begin{slide}{Systematic translation} 
\begin{block}{Redefine quotation}
> quote    ::   CPS    (alpha -> alpha)
> quote    =    lift   id
>
> end      ::   alpha   ->    alpha
> end           a       =     a
\end{block}
\pause
\begin{block}{Translation of constructors}
Introduce for each contructor |C :: tau1 -> ... -> taun -> tau|,
a prefix function (terminal) |c|:
>
> c ::   (tau -> alpha)   ->   CPS    (tau1 ->   ... ->   taun   ->    alpha)
> c      ctx              =    lift   (\t1       ...      tn     ->    ctx (C t1 ... tn))
\end{block}
\end{slide}


\begin{slide}{Example translation}
\begin{block}{Example constructors}
> Zero, One   :: Contract
> Give        :: Contract -> Contract
> Or          :: Contract -> Contract -> Contract
\end{block}
\begin{block}{Translated prefix functions (terminals)}
> zero   ::   (Contract -> a)   ->   CPS     a
> zero        ctx               =    lift    (ctx Zero)
> give   ::   (Contract -> a)   ->   CPS     (Contract -> a)
> give        ctx               =    lift    (\c -> ctx (Give c))
> or     ::   (Contract -> a)   ->   CPS     (Contract -> Contract -> a)
> or          ctx               =    lift    (\c1 c2 -> ctx (Or c1 c2))
\end{block}
\end{slide}


\begin{slide}{Concrete example, types} 
\begin{block}{The types correspond to the stack layout}
> quote      ::                           CPS    (alpha -> alpha)
> zero,one   ::   (Contract -> a)   ->    CPS     a
> give       ::   (Contract -> a)   ->    CPS     (Contract -> a)
> or         ::   (Contract -> a)   ->    CPS     (Contract -> Contract -> a)
> end        ::   alpha   ->    alpha
>
> quote                                  :: CPS (alpha -> alpha)
> quote   give                           :: CPS (Contract -> Contract)
> quote   give   or                      :: CPS (Contract -> Contract -> Contract)
> quote   give   or   zero               :: CPS (Contract -> Contract)
> quote   give   or   zero   one         :: CPS (Contract)
> quote   give   or   zero   one   end   :: Contract 
\end{block}
\end{slide}


%format p1    = "\textcolor{red}{(\lambda c\ \to\ c)}"
%format p2    = "\textcolor{red}{(\lambda c\ \to\ Give\ c) }"
%format p3    = "\textcolor{red}{(\lambda c1\ c2\ \to\ Give\ (Or\ c1\ c2))}"
%format p4    = "\textcolor{red}{(\lambda c2\ \to\ Give\ (Or\ Zero\ c2))}"
%format p5    = "\textcolor{red}{(Give\ (Or\ Zero\ One))}"
%format p6    = "\textcolor{red}{Give\ (Or\ Zero\ One)}"
\begin{slide}{Concrete example, evaluation} 
\begin{block}{Evaluation steps}
> quote                         =   lift id
> zero        ctx               =   lift (ctx Zero)
> one         ctx               =   lift (ctx One)
> give        ctx               =   lift (\c -> ctx (Give c))
> or          ctx               =   lift (\c1 c2 -> ctx (Or c1 c2))
> end         a                 =   a
>
> rquote       rgive   or     zero   one    end
> rgive   p1   ror     zero   one    end
> ror     p2   rzero   one    end
> rzero   p3   rone    end
> rone    p4   rend
> rend    p5
> p6 
\end{block}
\end{slide}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% Greibach Normal Form
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Greibach Normal Form}

\begin{slide}{Ready for the real work} 
\begin{block}{Embed languages described by context-free grammars}
\begin{itemize}
   \item Grammars in Greibach Normal Form (GNF)
   \item LL(1) grammars
   \item LR(0) grammars
\end{itemize}
\end{block}
\pause
\begin{block}{A grammars is in GNF if:}
\begin{itemize}
   \item All production are of the form $N \to aB_1 \dots B_n$
   \item It cannot generate the empty word ($\epsilon$).
\end{itemize}
\end{block}
\end{slide}


\begin{slide}{Example syntax} 
\begin{block}{New syntax for Financial Combinators}
> quote        give one dollar    or   give one euro         
>                                 or   give zero
> end
\end{block}
\begin{block}{Abstract syntax}
> type Contracts   =   [Contract]
> data Contract    =   Give    Contract
>                  |   One     Currency
>                  |   Zero 
> data Currency    =   Euro | Dollar
\end{block}
\end{slide}

\begin{slide}{Corresponding grammar} 
\begin{block}{Grammar and equivalent grammar in GNF}
\only<1>{
\begin{tabular}{lrl}
S    &$\to$&quote CS end \\
CS   &$\to$&C or CS      \\
     &$||$ &C            \\
C    &$\to$&give C       \\
     &$||$ &one CU       \\
     &$||$ &zero         \\
CU   &$\to$&euro         \\
     &$||$ &dollar       \\
\end{tabular}
}
\pause
\only<2>{
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
}
\end{block}
\end{slide}

\begin{slide}{Systematic translation, step 1} 
\begin{block}{A datatype for each non-terminal: $N$}
> newtype N alpha = N (S -> alpha)
Where |S| is the type of the semantic value.
\end{block}
\pause
\begin{block}{Datatypes for the example grammar}
> newtype S    a = S    (Contracts -> a)
> newtype CS   a = CS   (Contracts -> a)
> newtype C    a = C    (Contract  -> a)
> newtype CU   a = CU   (Currency  -> a)
\end{block}
\end{slide}


\begin{slide}{Systematic translation, step 2} 
\begin{block}{For each production: $N \to aB_1 \dots B_n$}
We introduce the following function:
>   a ::   (N alpha)   ->   CPS    (B1 (         ...      (Bn alpha) ...))
>   a      (N ctx)     =    lift   (B1 (\v1 ->   ... ->   Bn (\vn -> ctx (f v1 ... vn))))
Where |f| is the semantic function.
\end{block}
\end{slide}

\begin{slide}{Example translation} 
\begin{block}{Productions of the example grammar}
\begin{tabular}{lrl}
S  &$\to$&quote C CS\\ 
CS &$\to$&or C CS\\
   &$||$ &end\\
C  &$\to$&give C\\
   &$||$ &zero\\
\end{tabular}
\end{block}
\begin{block}{Translated parsing functions (terminals)}
>quote                  =    lift    (C (\c -> CS (\cs -> c:cs)))
>or          (CS ctx)   =    lift    (C (\c -> CS (\cs -> ctx (c:cs))))
>end         (CS ctx)   =    ctx []
>give        (C  ctx)   =    lift    (C  (\c   -> ctx (Give c )))
>zero        (C  ctx)   =    lift    (ctx Zero)
\end{block}
\end{slide}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%
% LL(1) parsing
%
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{LL(1) grammars}


\begin{slide}{LL(1) grammars} 
\begin{block}{Syntax we want to embed}
> contract =   quote   give (one dollar and one euro)
>                 or   give zero
>              end
\end{block}
\begin{block}{Abstract syntax}
> data Contract   =   Or     Contract   Contract
>                 |   And    Contract   Contract
>                 |   Give   Contract
>                 |   One    Currency
>                 |   Zero 
> data Currency   =   Euro | Dollar
\end{block}
\end{slide}


\begin{slide}{Corresponding grammar} 
\begin{block}{Grammar and equivalent LL(1) grammar}
\only<1>{
\begin{tabular}{lrl}
S  &$\to$& AC or S  \\
   &$||$ & AC       \\
AC &$\to$& C and AC \\
   &$||$ & C        \\
C  &$\to$& ( S )    \\
   &$||$ & give C   \\
   &$||$ & one  CU  \\
   &$||$ & zero     \\
CU &$\to$& euro     \\
   &$||$ & dollar   \\
\end{tabular}
}
\pause
\only<2>{
\begin{tabular}{lrlllrlrl}
S  &$\to$& AC or S  &             & S   &$\to$& AC S'    &    & \\
   &$||$ & AC       &             & S' &$\to$& or AC S' &$||$& $\epsilon$\\
AC &$\to$& C and AC &             & AC  &$\to$& C AC'     &    & \\
   &$||$ & C        &$\Rightarrow$& AC' &$\to$& and C AC' &$||$& $\epsilon$\\
C  &$\to$& ( S )    &             & C   &$\to$& ( S )     &    & \\
   &$||$ & give C   &             &     &     & give C    &    & \\
   &$||$ & one  CU  &             &     &$\to$& one CU    &    & \\
   &$||$ & zero     &             &     &$||$ & zero      &    & \\
CU &$\to$& euro     &             & CU  &$\to$& euro      &    & \\
   &$||$ & dollar   &             &     &$||$ & dollar    &    & \\
\end{tabular}
}
\end{block}
\end{slide}


\begin{slide}{Architecture of an LL parser} 
\begin{block}{The parsing table}
\begin{tabular}{l||llllll}
      &S            & AC           & C            & CU \\
\hline
give  &S $\to$AC S' & AC$\to$C AC' & C$\to$give C &  \\
one   &S $\to$AC S' & AC$\to$C AC' & C$\to$one CU &  \\
euro  &             &              &              &CU$\to$euro  \\
\end{tabular}
\end{block}
\pause
\begin{block}{The parse process}
\begin{tabular}{l||l||l} 
Step &Stack                                &Input buffer     \\
\hline
1    & S, end                              & give one euro \\
2    & AC, S', end                         & give one euro \\
3    & C, AC', S', end                     & give one euro \\
4    & give, C, AC', S', end               & give one euro \\
5    & C, AC', S', end                     & one euro \\
n    & end                                 &end \\
\end{tabular}
\end{block}
\end{slide}


\begin{slide}{Translation to Haskell} 
\begin{block}{The state}
\begin{itemize}
   \item The state is a stack of pending symbols
   \item Represent a symbol by a data type  
   \item |quote| pushes the start symbol on the stack
   \item |quote = lift (S  (\x -> x))|
\end{itemize}
\end{block}
\pause
\begin{block}{Some of the data types}
For the terminal |give|: |newtype G     a =   G    a|\\
For the non-terminals |S|, |AC|, and |C|
> newtype S     a =   S    (Contract               -> a)
> newtype AC    a =   AC   (Contract               -> a)
> newtype C     a =   C    (Contract               -> a)
\end{block}
\end{slide}


\begin{slide}{Translation to Haskell} 
\begin{block}{Terminals must both}
\begin{itemize}
   \item Encode the parsing of a terminal
   \item Encode the expansion rules of the parsing table   
   \item Overloading!
\end{itemize}
\end{block}
\pause
\begin{block}{Recognition of |give| }
Introduce a class for |give|
> class Give  old new | old -> new where
>     give  :: old -> CPS new
>
And an instance, parsing |give|
> instance Give (G a) a where
>     give (G ctx) = lift ctx
\end{block}
\end{slide}


\begin{slide}{Encoding of the expansion rules}
\begin{block}{Entries in the parsing table}
\begin{tabular}{l||llllll}
      &S            & AC           & C             \\
\hline
give  &S $\to$AC S' & AC$\to$C AC' & C$\to$give C  \\
\end{tabular}
\end{block}
\begin{block}{Translated instance declarations}
>instance Give (S  a)   (C (AC' (S'  a))) where
>    give   (S  ctx)   = give (AC (\t -> S'  (\e' -> ctx (e' t))))
>
>instance Give (AC a)   (C (AC' a)) where
>    give   (AC ctx)   = give (C (\f -> AC' (\t' -> ctx (t' f ))))
>
>instance Give (C a)    (C a) where
>    give   (C ctx)    = give (G (C (\c -> ctx (Give c))))
\end{block}
\end{slide}

\begin{slide}{Conclusion}
\begin{block}{What we have done}
\begin{itemize}
   \item Developed a postfix framework
   \item Embedded postfix and prefix notation
   \item Embedded languages described by LL(1) and GNF grammars
   \item Type-checker guarantees correct syntax embedding
\end{itemize}
\end{block}
\pause
\begin{block}{Practical concerns}
\begin{itemize}
   \item Type-error messages are unreadable
   \item Haskell compilers are not designed for this
   \item Lexical syntax of functions limits the concrete syntax
\end{itemize}
\end{block}
\end{slide}

\end{document}
