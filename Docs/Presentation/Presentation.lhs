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

%include Content.tex

\end{document}
