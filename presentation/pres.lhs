\documentclass[xcolor=dvipsnames,sans,mathserif]{beamer}
\usepackage[english]{babel}

\usepackage{mathpazo}

%include polycode.fmt 

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

\title{Running Haskell on the Java Virtual Machine}
\subtitle{Compiling EHC Silly to JVM Bytecode}
\author{Chris Eidhof, Sebastiaan Visser\\\small\texttt{ce@@tupil.com, sfvisser@@cs.uu.nl}} 
\date{\today} 

\frame{\titlepage} 

% -----------------------------------------------------------------------------

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

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{Type safety}

  We can not freely mix \stress{integers} and \stress{references} in one array.

  We have to choose one of the alternatives:

  \begin{itemize}
    \item \stress{Separate Java classes} for different data types.
    \item \stress{Box integer values} in a reference type.
  \end{itemize}

  \pause First approach is more \stress{efficient} but needs \stress{more information}. \\
  Second approach is \stress{simpler} to implement but is more \stress{expensive}.
  
\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}

%format public  = "\stress{\textbf{public}}"
%format static  = "\stress{\textbf{static}}"
%format class   = "\stress{\textbf{class}}"
%format extends = "\stress{\textbf{extends}}"

>public static class ANode {}

>public static class Node extends ANode
>{
>  public int tag;
>  public ANode[] payload;
>}

>public static class IntNode extends ANode
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
  \includegraphics[scale=0.50]{listof12}
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
\frametitle{Future optimizations}

  \begin{itemize}
  \item One class for one node layout: \stress{no more payload indirection}.
  \item Enable integer values in layout: \stress{no more triple boxing}.
  \item Save integers in node tag: \stress{no more double boxing boxing}.
  \item Generate Java global variables: \stress{no more global indirection}.
  \item Generate Java RP variables: \stress{no more RP indirection}.
  \item When no double boxing: \stress{unboxed primitive functions}.
  \item When no double boxing: \stress{flatten multiple allocs} (silly-205).
  \end{itemize}

\end{frame}

% -----------------------------------------------------------------------------
\end{document}
