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

  \frametitle{The simple approach}

%format public  = "\stress{\textbf{public}}"
%format static  = "\stress{\textbf{static}}"
%format class   = "\stress{\textbf{class}}"
%format extends = "\stress{\textbf{extends}}"

>public static class Node
>{
>  public int tag;
>  public Node[] payload;
>}

>public static class IntNode extends Node
>{
>  public int intVal;
>}

\end{frame}

\begin{frame}
\begin{center}
  \Huge{xs = |[1,2]|}
  \end{center}
  \end{frame}
\begin{frame}
  \includegraphics[scale=0.38]{listof12}
\end{frame}

\begin{frame}
\frametitle{hsjava}
\end{frame}

\begin{frame}
\frametitle{Silly to JVM}
\end{frame}

\begin{frame}
\frametitle{Compositionality}
\begin{itemize}
\item Local variables
\end{itemize}
\end{frame}

\begin{frame}
\frametitle{FFI}
\end{frame}

\begin{frame}
\frametitle{Demo}
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

