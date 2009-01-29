\documentclass[xcolor=dvipsnames,sans,mathserif]{beamer}

\usepackage[english]{babel}
\usepackage{mathpazo}

%include polycode.fmt

\setbeamertemplate{navigation symbols}{}
\setlength\parindent{0.0in}
\setlength\parskip{0.25in} 

% -----------------------------------------------------------------------------

\definecolor{stress}{rgb}{0.60,0.60,1.00} 
\definecolor{c1}{rgb}{1.00,0.70,0.30} 

\setbeamercolor{title}{fg=c1}
\setbeamercolor{frametitle}{fg=c1}
\setbeamercolor{normal text}{fg=white}
\setbeamercolor{background canvas}{bg=black}

\usefonttheme[stillsansseriftext]{serif} 
\setbeamerfont{frametitle}{family=\rmfamily,shape=\itshape} 

\newcommand{\stress}[1]{\textcolor{stress}{#1}}

% -----------------------------------------------------------------------------

\begin{document}

\title{Running Haskell on the CLR}
\subtitle{The painful road to vendor lock-in.}
\author{Jeroen Leeuwestein, Tom Lokhorst\\\small\texttt{jleeuwes@@cs.uu.nl, tom@@lokhorst.eu}} 
\date{\today} 

\frame{\titlepage} 

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{Don't get your hopes up!}

  \pause

  show most complicated example (fib?)

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{Why target the CLR?}

  \only<2>
  {
    A lot of presence.
    \begin{itemize}
      \item \stress{Multiple versions} of Windows desktops.
      \item \stress{OS X} and \stress{Linux} desktops, through Mono.
      \item Web browsers, through \stress{Silverlight} and \stress{Moonlight}.
      \item Mobile devices:
        \begin{itemize}
          \item Windows \stress{Mobile}.
          \item Mono on the \stress{iPhone} and \stress{Android}.
        \end{itemize}
      \item In the cloud!
        \begin{itemize}
          \item Windows \stress{Azure}: Distributed computation environment.
        \end{itemize}
    \end{itemize}
  }

  \only<3>
  {
    Rich environment.
    \begin{itemize}
      \item \stress{Interop} with other languages.
      \item Access a huge set of \stress{libraries}.
      \item Provide libraries developed in \stress{Haskell}.
    \end{itemize}
  }

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{What is the CLR?}

  Common Language Runtime / Mono Project

  \begin{itemize}
    \item \stress{Stack-based} virtual machine.
    \item First-class support for \stress{classes} with \stress{methods}.
    \item Basic operations for \stress{reference types} and \stress{value types}.
    \item \stress{Type safe}: operations must match the exact type.
    \item Dynamic \stress{casting} is allowed.
    \item Executes \stress{Common Intermediate Language} (CIL).
    \item CIL has a concrete syntax.
      \begin{itemize}
        \item ilasm
        \item ildasm / monodis
      \end{itemize}
  \end{itemize}

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{Architecture of .NET backend}

  \$ bin/8/ehc \stress{-ccil} Test.hs\\
  \pause
  \$ ls\\
  Test.hs   \stress{Test.il}\\
  \pause
  \$ \stress{ilasm} Test.il\\
  \pause
  \$ ls\\
  \stress{Test.exe}  Test.hs   Test.il\\
  \pause
  \$ \stress{mono} Test.exe\\
  42

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Architecture of .NET backend}

Haskell package \stress{language-cil}.

Abstract syntax for the Common Intermediate Language.

With build functions and pretty printer for concrete syntax.

\pause

Future:
\begin{itemize}
  \item Support all CIL constructs
  \item Parser for concrete syntax
  \item Analysis functions
  \item Release on Hackage
\end{itemize}

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Philosophy on the Runtime System}

How to tread the RTS?
\begin{itemize}
  \item As an \stress{abstract machine}?
    \uncover<2->
    {
    \begin{itemize}
      \item simulate virtual memory
      \item simulate registers
      \item simulate functions and function calls
    \end{itemize}
    }
  \item Use it for what it was \stress{designed}
    \uncover<3->
    {
    \begin{itemize}
      \item build strongly typed objects
      \item use inheritance
      \item use method calling conventions
      \item interop with other languages
    \end{itemize}
    }
\end{itemize}

\uncover<4->
{
Look at the what other languages do (\stress{F\#}).
}

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Philosophy on the Runtime System}
\framesubtitle{Some questions}

%format data  = "\stress{\textbf{data}}"

>data List = Nil | Cons Int List

\pause

What are the \stress{types} of Nil and Cons?

What is the type of List?

And how about \stress{thunks} and \stress{partial applications}?

\end{frame} 

% -----------------------------------------------------------------------------


\begin{frame}
\frametitle{Code generation}

\begin{itemize}
  \item Generate code from GRIN
  \item Direct translation of GRIN constructs

% >global_x_3_10_0_25 <- store (Ffun_x_15_10_0_25)
% >global_x_5_15_23_0 <- store (CInt 1)
% >
% >fun_x_15_10_0_25 = unit ()
% >
% >main  
% >  = fetchfield global_x_3_10_0_25 0 ; \x ->
% >    case x of
% >     CJust
% >       -> fetchfield global_x_3_10_0_25 1 ; \y ->
% >          unit (CJust y)
% >     Ffun_x_15_10_0_25
% >       -> updateunit (CJust global_x_5_15_23_0) global_x_3_10_0_25

\end{itemize}

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Sequence}

%format stress (e) = "\stress{" e "}"

\only<1>{
>expr ; \x -> ... length x ...
}
\only<2>{
>stress expr ; \x -> ... length x ...
}
\only<3>{
>expr ; stress (\x ->) ... length x ...
}
\only<4>{
>expr ; \x -> stress (... length x ...)
}

\pause
expr\\
\pause
STLOC x\\
\pause
...\\
LDLOC x\\
CALL length\\
...\\

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Case}

\only<1>{
>case tag of
>  CNil  -> ...
>  CCons -> ...
}
\only<2>{
>stress(case tag) of
>  CNil  -> ...
>  CCons -> ...
}
\only<3>{
>case tag of
>  stress(CNil)  -> stress(...)
>  CCons -> ...
}

\pause
tag\\
\pause
L1:\\
~~DUP\\
~~ISINST CNil\\
~~BRFALSE L2\\
~~POP\\
~~...\\
L2:\\
\end{frame}


\begin{frame}
\frametitle{Future work}
\framesubtitle{Obvious enhancements}

\begin{itemize}
  \item stloc x, ldloc x
  \item more stack focussed code
    \begin{itemize}
      \item Silly-like
      \item tail calls!
    \end{itemize}
  \item remove RefObj indirection
  \item use value types
  \item more polymorphic code
    \begin{itemize}
      \item inline unboxed values
    \end{itemize}
\end{itemize}

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{Future work}
\framesubtitle{More `out there' stuff}

Simon Peyton Jones on Haskell for CLR:
\begin{itemize}
  \item Generate IL
    \begin{itemize}
      \item Runtime representation for \stress{thunks}
    \end{itemize}
  \item \stress{Interop} with .NET libraries
    \begin{itemize}
      \item No \stress{foreign import ...} for everything
    \end{itemize}
  \item Other GHC primitives:
    \begin{itemize}
      \item the I/O monad
      \item arbitrary precision arithmetic
      \item concurrency
      \item exceptions
      \item finalisers
      \item stable pointers
      \item Software transactional memory 
    \end{itemize}
  \item Existing libraries
\end{itemize}

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\begin{center}
\Huge{\textbackslash EOF}
\end{center}
\end{frame}

% -----------------------------------------------------------------------------

\end{document}

