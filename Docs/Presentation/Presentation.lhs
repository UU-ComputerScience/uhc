\documentclass[compress,red,professionalfonts]{beamer}

\mode<presentation>

\usepackage{beamerthemeAntibes}
\usecolortheme{lily}

\title{Type systems with first class polymorphisms using Attribute Grammars}
\subtitle{Master thesis defense}
\author[Tamar]{Tamar Christina \\ \small{T.Christina@@students.uu.nl}}
\institute{University of Utrecht}
\date{\today}

\setcounter{tocdepth}{1}

%include lhs2TeX.fmt
%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include spacing.fmt

\usepackage{fancyvrb,url}
\usepackage[english]{babel}
%\usepackage[usenames]{color}
\usepackage{hyperref}
\usepackage{color}
\usepackage{textcomp}
\usepackage{parskip}
\usepackage{graphicx}
\usepackage{bussproofs}
\usepackage{amssymb}
\usepackage{amsmath}
\usepackage{latexsym}
\usepackage{hyperref}
\usepackage[xindy, toc]{glossaries}
\usepackage[shell, miktex, pdf]{dottex}
\usepackage{pgf}
\usepackage{tikz}
\usepackage{savesym}
\savesymbol{geq}
\savesymbol{leq}
\usepackage{mathabx}
\restoresymbol{TXT}{leq}
\restoresymbol{TXT}{geq}

\usepackage{bnf, float}
\usetikzlibrary{arrows,positioning}
\restylefloat{figure}

%include ..\\thesis\\Shorthands.tex

\begin{document}

\frame{\titlepage}

\section[Outline]{}
\frame{\tableofcontents}

\AtBeginSection[]{
	\begin{frame}{Outline}
		\tableofcontents[currentsection]
	\end{frame}
}

\section{Introduction}

%\frame
%{
%  \frametitle{Features of the Beamer Class}
%
%  \begin{itemize}
%  \item<1-> Normal LaTeX class.
%  \item<2-> Easy overlays.
%  \item<3-> No external programs needed.      
%  \end{itemize}
%}

% \begin{frame}[fragile]
% 	\frametitle{By example}
% 
% 	\begin{block}<+->{Consider the recursive function last:}
% 		\begin{lstlisting}
% 		last :: [a] -> a
% 		last []     = error "Last"
% 		last (x:[]) = x
% 		last (x:xs) = last xs
% 		\end{lstlisting}
% 	\end{block}
% 	
% 	Runtime two checks for the Cons (:) constructor are done.
% \end{frame}

\subsection{Type Systems}
\frame{
	\frametitle{Type inferencing}

	\begin{quotation}
    "Type inference refers to the ability to deduce automatically the type of an expression in a programming language."
    \end{quotation}
    
    \only<2->{For example the \emph{identity} function |\x->x|}
}

\frame{
	\frametitle{Hindley-Milner}
    
}

\frame{
	\frametitle{Higher-rank types}
    
}

\frame{
	\frametitle{SystemF}
    
}

\frame{
	\frametitle{The problem}
    
}

\frame{
	\frametitle{Goal}
    
}

\section{Attribute Grammars}

\subsection{Introduction}

\subsection{UUAGC}

\subsection{Ruler-Core}

\section{HML}

\subsection{Introduction}

\subsection{Types}

\subsection{Invariants}

\subsection{Semantics}

\subsection{Utility functions}

\subsection{Normal Form}

\section{Implementation}

\subsection{Inferencing}

\subsection{Unification}

\section{Additions}

\subsection{Tuples}

\subsection{Case expressions}

\section{Result}

\section{Problems and Future work}

\subsection{HML}

\subsection{Ruler-Core}

\section{Conclusion}
\end{document}
