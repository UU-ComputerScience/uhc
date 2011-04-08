\documentclass[twoside, titlepage, openright, a4paper]{book}

%include lhs2TeX.fmt
%include polycode.fmt
%include forall.fmt
%include greek.fmt
%include spacing.fmt

\usepackage{fancyvrb,url}
\usepackage[english]{babel}
\usepackage[usenames]{color}
\usepackage{hyperref}
\usepackage{listings}
\usepackage{color}
\usepackage{textcomp}
\usepackage{parskip}
\usepackage{graphicx}
\usepackage{bussproofs}
\usepackage{amssymb}
\usepackage{latexsym}
\usepackage{hyperref}
\usepackage{makeidx}
\usepackage[xindy, toc]{glossaries}
\usepackage{sectsty}
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

%\setlength{\parindent}{20pt} 

\makeindex
\makeglossaries

%include Shorthands.tex

\begin{document}
\input{./titlepage}

\input{./abstract}

\input{./acknowledgments}
\newpage
\thispagestyle{headings}
\mbox{}

\tableofcontents

%include Introduction.tex

%include Background.tex

%include UHC.tex

%include AttributeGrammar.tex

%% include EssentialHaskell.tex

%include HML.tex

%include Implementation.tex

%include SemiHaskell.tex

%include Conclusions.tex

\printglossaries
\printindex

\appendix

%include A-Compare.tex

%include citations.tex
\end{document}
