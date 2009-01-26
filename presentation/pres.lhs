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

\title{Running Haskell on the JVM}
\subtitle{The painful road to platform independence.}
\author{Chris Eidhof, Sebastiaan Visser\\\small\texttt{ce@@tupil.nl, sfvisser@@cs.uu.nl}} 
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
  \includegraphics[scale=0.50]{listof12}
\end{frame}

% -----------------------------------------------------------------------------
\end{document}

