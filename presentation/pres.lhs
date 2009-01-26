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

\title{Running Haskell on the CLR}
\subtitle{The painful road to vendor lock-in.}
\author{Jeroen Leeuwestein, Tom Lokhorst\\\small\texttt{jleeuwes@@cs.uu.nl, tom@@lokhorst.eu}} 
\date{\today} 

\frame{\titlepage} 

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{CLR/Mono}

  Common Language Runtime / Mono Project

  \begin{itemize}
    \item \stress{Stack-based} machine.
    \item Basic operations for \stress{reference types} and \stress{value types}.
    \item \stress{Type safe}: operations must match the exact type.
    \item However, runtime checked casting is allowed.
  \end{itemize}

  Cil \texttt{.il} file:

  \begin{itemize}
    \item Describes one \stress{module}.
    \item Multiple \stress{classes}.
    \item Classes have \stress{methods}.
    \item Possibly several \stress{inner classes}.
  \end{itemize}

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

  \pause First approach is more \stress{efficient} but needs \stress{more information}.

  Second approach is \stress{simpler} to implement but is more \stress{expensive}.
  
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
  \Huge{|[1,2]|}
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
\frametitle{Future optimizations}
\end{frame}

% -----------------------------------------------------------------------------
\end{document}

