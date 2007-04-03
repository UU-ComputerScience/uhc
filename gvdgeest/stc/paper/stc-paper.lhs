%
% natbib voor meer cite opmaak
% 

\documentclass[11pt, onecolumn, blockstyle]{sigplanconf}

\usepackage[english]{babel}
\usepackage[pdftex]{graphicx}
\usepackage{longtable}
\usepackage{hyperref}
\usepackage{boxedminipage}
\usepackage{latexsym}
\usepackage{epsf}
\usepackage{epsfig}
\usepackage{listings}
\usepackage{amsmath,amssymb}
\usepackage{color}

%include lhs2TeX.fmt
%include lhs2TeX.sty

\begin{document}

\title{Embedding Languages in Haskell}

\authorinfo{Gerrit van den Geest}
           {Department of Information and Computing Sciences \\ Universiteit Utrecht \\ P.O.Box 80.089, 3508 TB Utrecht, The Netherlands}
           {ggeest@@cs.uu.nl}

\toappear{This paper is written as part of the Software Technology Colloquium course of the master Software Technology at the Universiteit Utrecht.}

\maketitle

%format . = " . "
%format forall = "\forall"
%format tau1   = "\tau_1"
%format taun    = "\tau_n"
%format t1   = "t_1"
%format tn    = "t_n"
%format alpha   = "\alpha"
%format beta    = "\beta"
%format gamma   = "\gamma"
%format tau     = "\tau"

% -- Abstract
%include 00-abstract.lhs


% -- Contents
%include 01-introduction.lhs
%include 02-framework.lhs
%include 03-postfix.lhs
%include 04-prefix.lhs
%include 05-gnf.lhs
%include 06-topdown.lhs
%%%include 07-bottomup.lhs
%include 08-conclusion.lhs

% % % % % % % % % % % % %
%      End Matter       %
% % % % % % % % % % % % %

% -- Bibliography
\bibliography{references}  %{alpha}
\bibliographystyle{plain}  %{plain} %{papers}
\end{document}
