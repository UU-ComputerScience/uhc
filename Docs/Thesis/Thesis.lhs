\documentclass[twoside, titlepage, openright, a4paper]{book}

%include lhs2TeX.fmt
%include polycode.fmt

\usepackage{fancyvrb,url}
\usepackage[english]{babel}
\usepackage[usenames]{color}
\usepackage{hyperref}
\usepackage{color}
\usepackage{textcomp}
\usepackage{parskip}
\usepackage{graphicx}
\usepackage{bussproofs}
\usepackage{amssymb}
\usepackage{latexsym}
\usepackage{hyperref}
%\usepackage{bookman}
\usepackage{makeidx}
\usepackage[xindy, toc]{glossaries}
\usepackage{sectsty}
\usepackage[shell, miktex, pdf]{dottex}
\usepackage{pgf}
\usepackage{tikz}
\usepackage{bnf, float}
\usetikzlibrary{arrows,positioning}
\restylefloat{figure}

\newcommand{\HRule}{\rule{\linewidth}{0.5mm}}
%\setlength{\parindent}{20pt} 

\makeindex
\makeglossaries

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

%include EssentialHaskell.tex

%include HML.tex

%include Implementation.tex

%include SemiHaskell.tex

%include Conclusions.tex

\printglossaries
\printindex

\appendix

%include citations.tex
\end{document}
