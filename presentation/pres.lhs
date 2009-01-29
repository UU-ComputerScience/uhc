\documentclass[xcolor=dvipsnames,sans,mathserif]{beamer}
\usepackage[english]{babel}

\usepackage{mathpazo}

%include polycode.fmt 

\setlength\parindent{0.0in}
\setlength\parskip{0.25in} 

\definecolor{stress}{rgb}{0.60,0.60,0.60} 
\definecolor{c1}{rgb}{1.00,0.70,0.30} 

\setbeamercolor{title}{fg=c1}
\setbeamercolor{frametitle}{fg=c1}
\setbeamercolor{normal text}{fg=stress}
\setbeamercolor{background canvas}{bg=black}

\usefonttheme[stillsansseriftext]{serif} 
\setbeamerfont{frametitle}{family=\rmfamily,shape=\itshape} 
\setbeamersize{text margin left=0.3cm}
\setbeamersize{text margin right=0.5cm}

\newcommand{\stress}[1]{\textcolor{white}{\textbf{#1}}}

\begin{document}

\title{Running Haskell on the Java Virtual Machine}
\subtitle{Compiling EHC Silly to JVM Bytecode}
\author{Chris Eidhof, Sebastiaan Visser\\\small\texttt{ce@@tupil.com, sfvisser@@cs.uu.nl}} 
\date{\today} 

\frame{\titlepage} 

\begin{frame}

  \frametitle{JVM}

  Java Virtual Machine:

  \begin{itemize}
    \item \stress{Stack-based} machine.
    \item Basic operations for \stress{primitives}, \stress{arrays} and \stress{classes}.
    \item \stress{Type safe}: operations must match the exact type.
  \end{itemize}

  Java \texttt{.class} file:

  \begin{itemize}
    \item Describes one \stress{single class}.
    \item All (possibly static) \stress{methods}.
    \item Possibly several \stress{inner classes}.
  \end{itemize}

\end{frame}

\begin{frame}
\frametitle{hsjava}

To build Java \texttt{.class} file we use the \stress{\texttt{hsjava}} package. \\
A \stress{high-level abstraction} for generation byte-code.

Developed by \stress{Brian Alliet} for integration into \stress{GHC}.

We use \texttt{hsjava} to build a JVM back-end for \stress{EHC}.

\end{frame}

\begin{frame}

  \frametitle{Problems}

  Java is not C, lots of things happen at \stress{runtime}.

  \begin{itemize}
    \item Array boundary checks.
    \item No mixing of integers and references in arrays.
    \item Checks for method existence.
    \item Checks for method type signature.
    \item Cast between object types.
  \end{itemize}

  Might some of these only happen at \stress{JIT} compile time?\\
  We do not know and should start \stress{profiling}.

\end{frame}

\begin{frame}

  \frametitle{Type safety}

  We have to choose one of the alternatives:

  \begin{itemize}
    \item \stress{Separate Java classes} for different data types.
    \item \stress{Box integer values} in a reference type.
  \end{itemize}

  \pause First approach is more \stress{efficient} but needs \stress{more information}. \\
  Second approach is \stress{simpler} to implement but is more \stress{expensive}.
  
\end{frame}

%format public  = "\stress{\textbf{public}}"
%format static  = "\stress{\textbf{static}}"
%format class   = "\stress{\textbf{class}}"
%format extends = "\stress{\textbf{extends}}"

\begin{frame}
\frametitle{Runtime.java}

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
  \Huge{|[1,2]|}
  \end{center}
  \end{frame}

\begin{frame}
  \includegraphics[scale=0.50]{listof12-complicated}
\end{frame}


\begin{frame}
\frametitle{Silly to JVM}
  We hook into EHC and use \stress{Silly} as input.

  Code generation from Silly to Java bytecode can be hard:
  \begin{itemize}
  \item \stress{Compositionality} of Silly does not match bytecode structure.
  \item We need \stress{both sides of an assignments} in both sides.
  \item \stress{Type information} is erased.
  \item \stress{Type inferencing} is hard because of \stress{node updates}.
  \item Conversion is often \stress{all or nothing}.
  \end{itemize}
\end{frame}

\begin{frame}
\frametitle{FFI}
\end{frame}

\begin{frame}
\frametitle{Demo}
\end{frame}

\begin{frame}
\frametitle{Future optimizations}

  \begin{itemize}
  \item One class for one node layout: \stress{no more payload indirection}.
  \item Enable integer values in layout: \stress{no more triple boxing}.
  \item Save integers in node tag: \stress{no more double boxing}.
  \item When no double boxing: \stress{unboxed primitive functions}.
  \item When no double boxing: \stress{flatten multiple allocs} (silly-205).
  \item Generate Java global variables: \stress{no more global indirection}.
  \item Generate Java RP variables: \stress{no more RP indirection}.
  \end{itemize}

\end{frame}

\begin{frame} \includegraphics[scale=0.50]{listof12-complicated} \end{frame}
\begin{frame} \includegraphics[scale=0.50]{listof12} \end{frame}
\begin{frame} \includegraphics[scale=0.50]{listof12-nopayload} \end{frame}
\begin{frame} \includegraphics[scale=0.50]{listof12-nopayloadoptimized} \end{frame}
\begin{frame} \includegraphics[scale=0.50]{listof12-intandtag} \end{frame}

% -----------------------------------------------------------------------------
\end{document}

