% $Id$

%include lhs2TeX.fmt
%include forText.fmt

%% configuration of what to include

%if storyPHD || storyAfpTRUU1
%let inclParts  = True
%else
%let inclParts  = False
%endif

%if storyPHD || storyAfpTRUU1
%let inclTOC    = True
%else
%let inclTOC    = False
%endif

%if storyAFP04Notes || storyPHD || storyEHIntro || storyAfpTRUU1
%let incl00     = True
%else
%let incl00     = False
%endif

%if storyAFP04Notes || storyPHD || storyAfpTRUU1
%let incl01     = True
%let incl02     = True
%let incl03     = True
%else
%let incl01     = False
%let incl02     = False
%let incl03     = False
%endif

%if storyPHD  || storyAfpTRUU1 || onlyCurrentWork
%let incl04     = True
%else
%let incl04     = False
%endif

%if storyPHD  || onlyCurrentWork
%let incl05     = True
%let incl06     = True
%let incl07     = True
%else
%let incl05     = False
%let incl06     = False
%let incl07     = False
%endif

%if storyPHD  || onlyCurrentWork
%let incl08     = True
%else
%let incl08     = False
%endif

%if storyPHD || storyExplImpl  || onlyCurrentWork
%let incl09     = True
%else
%let incl09     = False
%endif

%if storyPHD  || onlyCurrentWork
%let incl10     = True
%let incl11     = True
%else
%let incl10     = False
%let incl11     = False
%endif

%if storyPHD 
%let incl12     = True
%let incl13     = True
%let incl14     = True
%let incl15     = True
%let incl16     = True
%let incl17     = True
%let inclXX     = True
%else
%let incl12     = False
%let incl13     = False
%let incl14     = False
%let incl15     = False
%let incl16     = False
%let incl17     = False
%let inclXX     = False
%endif

%if storyPHD
%let inclApp    = True
%let inclInx    = True
%else
%let inclApp    = False
%let inclInx    = False
%endif

%if storyAFP04Notes
%let inclConcl  = True
%let inclAck    = True
%else
%let inclConcl  = False
%let inclAck    = False
%endif

%if not storyAFP04Notes
%let incl01TopicPP  		= True
%let incl01TopicErr  		= True
%let incl01TopicParsing  	= True
%else
%let incl01TopicPP  		= False
%let incl01TopicErr  		= False
%let incl01TopicParsing  	= False
%endif

%if storyExplImpl
%let chapAsArticle  = True
%else
%let chapAsArticle  = False
%endif

%let incl00TopicAGPrimer  = True


%let a4       = True
%let noprelim = True
%let color    = False

\documentclass[%
%if yesBeamer
%if asSlides
               ignorenonframetext,
%else
%if asArticle
%if llncs
               class=llncs,
%elif acm
%if icfp05
%if asDraft
               class=sigplanconf,onecolumn,11pt,preprint,
%else
               class=sigplanconf,preprint,blockstyle,
%endif %% asDraft
%else
               class=sigplan-proc,preprint,
%endif
%else
               class=article,
%endif %% llncs
%else
               class=book,
%endif %% asArticle
%endif %% asSlides
%endif %% yesBeamer
%if not (asSlides || acm)
               a4paper,
%if veryWide
               11pt,
%else
               10pt,
%endif %% veryWide
               headinclude,% new for typearea
               footexclude,% new for typearea
               nopictures%
%elif acm
%if asDraft
               onecolumn,11pt,blockstyle,preprint
%else
%endif %% asDraft
               blockstyle
%endif
]
%if yesBeamer
               {beamer}
%else
%if acm
%if useSigplanconfSty
               {sigplanconf}%{article}
%else
               {sigplan-proc}%{article}
%endif %% useSigplanconfSty
%elif asArticle
               {beamer}%{article}
%else
               {beamer}%{book}
%endif
%endif

%if not asSlides
\usepackage{kscode}
%endif

\usepackage{fancyvrb}
\usepackage{\jobname} %%% needs: fancyvrb
%\usepackage{scrtime}% needed for prelim/DRAFT warnings

\usepackage{graphicx} %%% needs: fancyvrb

%if yesBeamer
%if asSlides
\usepackage{beamerthemeafp}
\beamertemplatetransparentcovereddynamic
%else
\usepackage
%if llncs || acm
[noamsthm]
%elif acm
%endif %% llncs ...
{beamerbasearticle}
%endif %% asSlides
%else
%\usepackage[absolute]{textpos}%
\usepackage{color}
\usepackage[fleqn]{amsmath}
%if not acm
\usepackage{mparhack}% to get marginpars to show up correctly
\usepackage{natbib}
\bibpunct{(}{)}{;}{a}{}{,}
%endif %% not acm
%endif %% yesBeamer

%if not (asSlides || llncs || acm)
%if wide
%if asArticle
\usepackage[left=2.75cm,right=2.75cm,top=3cm,bottom=3cm,nohead]{geometry}
%else
\usepackage[left=2.75cm,right=2.75cm,top=3cm,bottom=3cm,asymmetric]{geometry}
%endif %% asArticle
%elif veryWide
\usepackage[left=2.25cm,right=2.25cm,top=2.5cm,bottom=2.5cm,nohead]{geometry}
%endif %% wide
%endif %% not (...)

\usepackage{pgf}
\usepackage{pgfarrows}
\usepackage{pgfnodes}

%if asSlides
%elif acm
%else
\usepackage
%if fontsite
           [sc]%
%endif
           {mathpazo}% change main font
\linespread{1.05}% larger line spacing due to palatino
\renewcommand{\bfdefault}{b}% bx palatino isn't available anyway
%if fontsite
\usepackage{grotesk}
\let\originaltextsc=\textsc
\renewcommand{\textsc}[1]{%
  \begingroup
  \fontfamily{5plj}\selectfont
  \originaltextsc{#1}%
  \endgroup}
%else
\usepackage[scaled=.95]{helvet}% change sf font
%endif %% fontsite
\usepackage{calc}
%endif %% asSlides

\usepackage{array}

% experimental \DeclareMathOperator redefinition to use sf font
%if llncs || (acm && yesBeamer)
\newcommand{\DeclareMathOperator}[2]{%
  \newcommand*{#1}{\textsf{#2}}}
%else
\renewcommand*{\DeclareMathOperator}[2]{%
  \newcommand*{#1}{\textsf{#2}}}
%endif

% this column type is for tabulars of (type) inference rules
\newcolumntype{P}{@@{}>{\let\pcr=\\\begin{center}\let\\=\pcr\(}p{\linewidth}<{\)\end{center}}@@{}}

% Hyperref should be the last package loaded. The final online
% version should use something different than the ugly boxes.

%if useHyperref
\usepackage{hyperref}
%endif

%include lhs2TeX.sty

% Done with lhs2TeX stuff ...

% operators
\DeclareMathOperator{\ftv}{ftv}

\let\norml=\empty
\let\normr=\empty
\def\resetnextsize{\def\nextsize{norm}}\resetnextsize
\def\nextbig{\def\nextsize{big}}
\def\nextBig{\def\nextsize{Big}}
\newcommand*{\MakeResizable}[1]{%
  \expandafter\let\csname fixed.\string #1\endcsname=#1%
  \def #1{%
    \let\realnextsize\nextsize
    \resetnextsize
    \def\next{\csname fixed.\string #1\endcsname}%
    \expandafter\next\expandafter[\realnextsize]}}

\newcommand*{\pars}[2][norm]{\csname #1l\endcsname(#2\csname #1r\endcsname)}
\MakeResizable\pars

%if style == newcode

%format ^^.

%else

\newcommand*{\many}[2][norm]{\csname #1l\endcsname\{#2\csname #1r\endcsname\}}
\MakeResizable\many

%endif

\newcommand{\defined}[1]{\textbf{#1}}

%if color
\newenvironment{preliminary}{\itshape\color{blue}}{}
%else
\newenvironment{preliminary}{\itshape}{}
%endif

\usepackage{infrule}

%if not asSlides
%if asArticle
\newcommand{\AddContentsLine}[1]{\addcontentsline{toc}{section}{#1}}
%else
\newcommand{\AddContentsLine}[1]{\addcontentsline{toc}{chapter}{#1}}
%endif
%endif

%if not asArticle || chapAsArticle || storyEHIntro
\let\prevSection=\section
\let\prevSubsection=\subsection
%if not chapAsArticle
\def\section{\chapter}
%endif
\def\subsection{\prevSection}
\def\subsubsection{\prevSubsection}
%endif
%if storyEHIntro
\def\paragraph{\prevSubsection}
%elif useSigplanconfSty
\let\prevParagraph=\paragraph
\def\paragraph#1{\prevParagraph{#1.}}
%endif

\usepackage{txfonts}

%if truu
\usepackage{TRtitlepage}
%endif

%if llncs
% \usepackage{makeidx}
%endif

% Specific for UHC

%format (Lst(x))    = "[" x "]"

%format ~=          = "\sim"
%format +++         = "\cup "
%format `intersect`     = "\cap "
% format :=          = "\vDash "
%format ::<         = :: "_" coco
%format **          = "\_"

%format sigma1'
%format sigma1''
%format sigma2'
%format sigma2''

%format (sigmaNN(i)(j)) = sigma "^{" i "}_{" j "}"
%format sigmar          = sigma "^{r}"
%format sigmac          = sigma "^{c}"
%format sigmap          = sigma "^{p}"
%format sigmapi         = sigmap "_{i}"
%format (sigmapN(i))    = sigmap "_{" i "}"
%format sigmap1
%format sigmap2
%format sigmae          = sigma "^{e}"
%format (sigmaeN(i))    = sigmae "_{" i "}"
%format sigmaa          = sigma "^{a}"
%format sigmaa1
%format sigmar1
%format (sigmai(i))     = sigma "^{" i "}"

%format Gammapi         = Gammap "_{i}"
%format (GammaNN(i)(j)) = Gamma "^{" i "}_{" j "}"
%format Gammap1
%format Gammap2

%format jfit                                = "\stackrel{fit}{" :- "}"

%format Bind(n)(e)  = n :-> e

%format Cnstr'
%format Cnstr''
%format (CnstrNN(i)(j))             = Cnstr "^{" i "}_{" j "}"
%format (CnstrN(i))                 = Cnstr "_{" i "}"
%format Cnstrp                      = Cnstr "^p"
%format Cnstrc                      = Cnstr "^c"
%format Cnstre                      = Cnstr "^e"
%format Cnstr0
%format Cnstr1
%format Cnstr2
%format Cnstr3
%format Cnstr4
%format (CnstrR(f)(t))              = Cnstr "_{" f .. t "}"
%format (CnstrNR(i)(f)(t))          = Cnstr "^{" i "}_{" f .. t "}"
%format (CnstrPlus(x)(y))           = x +++ "_{" Cnstr "}" y

%format coe1
%format coe2
%format coeL        = "{" coe "_{l}}"
%format coeR        = "{" coe "_{r}}"
%format coeH        = "\_ _" coe
%format coeLam(a)(b)= "\lambda_" coe a "." b
%format coeLam2(b)  = \ "n" -> "\lambda_" coe "n" "." b
%format coeApp(f)(a)= f ^^ "\$_" coe ^^ a
%format coeApp1(f)  = f ^^ "\$_" coe ^^
%format coeLet(b)(e)= "\mathbf{let}_" coe ^^ b in e
%format coeTup2(x)(y) = "(" x "," y ")_" coe
%format coeComp(f)(g)   = f "\circ_" coe g
%format Coe         = "\overline{" coe "}"
%format CoeL        = "{" Coe "_{l}}"
%format CoeL'
%format CoeL''
%format CoeR        = "{" Coe "_{r}}"
%format CoeR'
%format CoeR''
% format coeWeave    = "weave_{" coe "}"
% format coeWipe     = "wipe_{" coe "}"
% format coeCode     = "code"
% format coeEval     = "eval_{" coe "}"

%format e1
%format e2

% format expr            = e
% format expr1
% format expr2
% format expri           = expr "_i"
% format exprn           = expr "_n"
% format exprc           = expr "_c"
% format (exprNN(i)(j))  = expr "^{" i "}_{" j "}"

%format pat         = p
%format pat1
%format pat2
%format pati        = pat "_i"
%format patn        = pat "_n"

%format cod         = "{\mu}"
%format cod1
%format cod2

%format Inst(f)(e)  = "inst_{" f "}(" e ")"
%format InstArrow(i)(f)(e)  = "inst^{" i -> "}_{" f "}(" e ")"

%format Restr(e)(wh)(by) = e "\backslash^{" wh "}_{" by "}"

%format TVFree      = v
%format TVFixed     = f
%format TVAsPat     = p

%format pred        = pi
%format pred1
%format pred2
%format pred_u      = pred "_{u}"
%format predi       = pred "^i"
%format predi1
%format predi2

%format (identNN(i)(j))     = ident "^{" i "}_{" j "}"
%format identv              = i
%format identc              = I
%format (identcNN(i)(j))    = identc "^{" i "}_{" j "}"
%format unIdent(i)          = "un" i

%format rhoi        = rho "_i"

%format Card(x)     = "|" x "|"

%format alts        = "\mathbf{alts}"
%format constr      = "\mathbf{constr}"

% AG Patterns
%format AGPat(p)    = "{\color{red}" p "}"

%%% sizes

%if llncs || acm
\setlength{\mathindent}{.03\textwidth}
%else
\setlength{\mathindent}{.05\textwidth}
%endif

%if truu
\setlength{\marginparsep}{.015\textwidth}
%endif

% title
%if storyExplImpl
\title{Explicit implicit parameters}
%elif storyEHIntro
%if storyVariantETAPSLinks
\title{Essential Haskell Compiler Highlights}
%else
\title{Essential Haskell Compiler Overview}
%endif
%else
\title{Typing Haskell with an Attribute Grammar}
%endif

%if storyPHD
\author{Atze Dijkstra}
%elif acm && storyExplImpl
%if useSigplanconfSty
\authorinfo{Atze Dijkstra \and Doaitse S. Swierstra}
  {Institute of Information and Computing Sciences \\
   Utrecht University \\
   P.O.Box 80.089, 3508 TB Utrecht, The Netherlands
  }
  {\{atze,doaitse\}@@cs.uu.nl}
\toappear{Submitted to the International Conference onf Functional Programming 2005 (ICFP 2005), September 26-28, Tallin, Estonia}
%else %% useSigplanconfSty
\numberofauthors{1}
\author{
%
% The command \alignauthor (no curly braces needed) should
% precede each author name, affiliation/snail-mail address and
% e-mail address. Additionally, tag each line of
% affiliation/address with \affaddr, and tag the
%% e-mail address with \email.
\alignauthor Atze Dijkstra and Doaitse S. Swierstra\\
       \affaddr{Institute of Information and Computing Sciences}\\
       \affaddr{Utrecht University}\\
       \affaddr{P.O.Box 80.089, 3508 TB Utrecht, The Netherlands}\\
       \email{\{atze,doaitse\}@@cs.uu.nl}
}
%endif %% useSigplanconfSty
%else
\author{Atze Dijkstra and S. Doaitse Swierstra}
%endif
\date{\today}
%if llncs
\institute{Institute of Information and Computing Sciences,\\
Utrecht University,\\
P.O.Box 80.089, \\
Padualaan 14, Utrecht, Netherlands,\\
\email{@{atze,doaitse}@@cs.uu.nl@},\\
WWW home page:
\texttt{http://www.cs.uu.nl}
}
%endif

%if inclInx
\mode<article>{\makeindex}
%endif %% inclInx

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Import of all code chunks in lhs2tex'd format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\newcommand{\EHCTexVersion}{tmp-\jobname}
\newcommand{\inputEHCTex}[2]{\input #1/#2}
\inputEHCTex{\EHCTexVersion}{EHAbsSyn.tex}
\inputEHCTex{\EHCTexVersion}{EHC.tex}
\inputEHCTex{\EHCTexVersion}{EHCnstr.tex}
\inputEHCTex{\EHCTexVersion}{EHCommon.tex}
\inputEHCTex{\EHCTexVersion}{EHError.tex}
\inputEHCTex{\EHCTexVersion}{EHErrorAbsSyn.tex}
\inputEHCTex{\EHCTexVersion}{EHErrorPretty.tex}
\inputEHCTex{\EHCTexVersion}{EHExtraChecks.tex}
\inputEHCTex{\EHCTexVersion}{EHGam.tex}
\inputEHCTex{\EHCTexVersion}{EHGatherError.tex}
\inputEHCTex{\EHCTexVersion}{EHInfer.tex}
\inputEHCTex{\EHCTexVersion}{EHInferCaseExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferClass.tex}
\inputEHCTex{\EHCTexVersion}{EHInferData.tex}
\inputEHCTex{\EHCTexVersion}{EHInferExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferKiExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferPatExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferTyExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHMainAG.tex}
\inputEHCTex{\EHCTexVersion}{EHParser.tex}
\inputEHCTex{\EHCTexVersion}{EHPred.tex}
\inputEHCTex{\EHCTexVersion}{EHPretty.tex}
\inputEHCTex{\EHCTexVersion}{EHResolvePred.tex}
\inputEHCTex{\EHCTexVersion}{EHSubstitutable.tex}
\inputEHCTex{\EHCTexVersion}{EHTy.tex}
\inputEHCTex{\EHCTexVersion}{EHTyAbsSyn.tex}
\inputEHCTex{\EHCTexVersion}{EHTyCommonAG.tex}
\inputEHCTex{\EHCTexVersion}{EHTyFitsIn.tex}
\inputEHCTex{\EHCTexVersion}{EHTyFitsInCommon.tex}
\inputEHCTex{\EHCTexVersion}{EHTyFtv.tex}
\inputEHCTex{\EHCTexVersion}{EHTyInstantiate.tex}
\inputEHCTex{\EHCTexVersion}{EHTyPretty.tex}
\inputEHCTex{\EHCTexVersion}{EHTyQuantify.tex}
\inputEHCTex{\EHCTexVersion}{EHTySubst.tex}
\inputEHCTex{\EHCTexVersion}{EHUniq.tex}
%\inputEHCTex{\EHCTexVersion}{EHCoreAbsSyn.tex}
%\inputEHCTex{\EHCTexVersion}{EHCore.tex}
%\inputEHCTex{\EHCTexVersion}{EHCorePretty.tex}
%\inputEHCTex{\EHCTexVersion}{EHCoreJava.tex}
%\inputEHCTex{\EHCTexVersion}{EHGenCore.tex}

%% AG primer
%if incl00TopicAGPrimer
\inputEHCTex{\EHCTexVersion}{RepminAG.tex}
\inputEHCTex{\EHCTexVersion}{RepminHS.tex}
\inputEHCTex{\EHCTexVersion}{Expr.tex}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% rules
\input rules.tex
\input rules2.tex

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pictures
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% pgf picture
\input afp-pgf.tex

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Misc settings
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% xfig font
\gdef\SetFigFont#1#2#3#4#5{}

% the sort of paper
\newcommand{\thispaper}{%
%if llncs
these notes%
%elif storyPHD
this thesis%
%else
this paper%
%endif
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Shared global defs w.r.t. actual content
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\def\ParserCombTableHead{%
Combinator & Meaning & Result
\\ \hline
}
\def\ParserCombTableA{%
|p <*> q|                       & |p| followed by |q|               & result of |p| applied to result of |q| \\
|p <||> q|                      & |p| or |q|                        & result of |p| or result of |q| \\
|pSucceed r|                    & empty input |epsilon|             & |r| \\
|f <$> p|                       & |== pSucceed f <*> p|             & \\
|pKey "x"|                      & symbol/keyword x                  & |"x"| \\
}
\def\ParserCombTableB{%
|p <**> q|                      & |p| followed by |q|               & result of |q| applied to result of |p| \\
|p `opt` r|                     & |== p <||> pSucceed r|            & \\
|p <??> q|                      & |== p <**> q `opt` id|            & \\
|p <* q|, |p *> q|, |f <$ p|    & \parbox[t]{.3\textwidth}{variants throwing away result of angle missing side}
                                                                    & \\
|pFoldr listAlg p|              & sequence of |p|'s                 & |foldr c n (|result of all |p|'s|)| \\
|pList p|                       & |pFoldr ((:),[]) p|               & \\
|pChainr s p|                   & |p|'s (|>1|) separated by |s|'s   & result of |s|'s applied to results of |p|'s aside \\
}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual content
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\begin{document}

%if llncs
\frontmatter
%endif

%if storyAfpTRUU1
\TRtitlepage
{Typing Haskell with an Attribute Grammar (Part I)}
{Atze Dijkstra \\ Doaitse Swierstra}
{UU-CS-2004-037}
%elif storyExplImpl && truu
\TRtitlepage
{Explicit implicit parameters}
{Atze Dijkstra \\ S. Doaitse Swierstra}
{UU-CS-2004-059}
\maketitle
%else
\maketitle
%endif

%if asSlides
\frame<presentation>{\titlepage}
%endif

% Avoid indentation
%if acm
%elif llncs
\setlength{\parindent}{0mm}
\addtolength{\parskip}{0.25\baselineskip}
%elif acm && icfp05
%else
\setlength{\parindent}{0mm}
\addtolength{\parskip}{0.4\baselineskip}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstract
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if storyAfpTRUU1
\section*{Preface}
\subsection*{Type systems for functional languages (abstract)}
%endif

%if asArticle
\begin{abstract}
%endif

%if storyExplImpl
%if atze
In almost all languages arguments to functions must be given explicitly.
There exist however a few interesting exceptions to this rule.
In Haskell functions take arguments which are either passed explicitly or implicitly.
An instance of the latter is the class system in Haskell, where
dictionaries are passed as evidence for class predicates.
However, the construction as well as the passing of these dictionaries
is completely invisible to the programmer,
and worse also completely uncontrollable;
the programmer cannot provide any help if the built-in proof mechanism fails.
In \thispaper\ we propose, in the context of Haskell, a mechanism that allows the programmer
to explicitly pass
implicit parameters.
This extension blends well with existing resolution mechanisms,
since it only overrides the default behavior of
such mechanisms.
We also describe how this extension has been implemented for Haskell.
The implementation also gives us the additional bonus of partial type signatures,
liberating the programmer from the obligation to specify either a full signature
or not to specify a signature at all.
%else %% doaitse
In almost all languages arguments to functions are to be given
explicitly. The Haskell class system however is an
exception: functions can have class predicates as part of their type
signature, and dictionaries are implicitly constructed and implicitly
passed as hidden arguments, thus relieving the programmer from a lot of
clerical work and removing clutter from the program text. Unfortunately
Haskell maintains a very strict boundary between the implicit and the
explicit world; if the implicit mechanisms fail to construct the hidden
dictionaries there is no way the programmer can provide help, nor is he
able to override the choices made by the implicit mechanisms. In this
paper we describe, in the context of Haskell, a mechanism that allows
the programmer to explicitly pass implicit parameters. This extension
blends well with existing resolution mechanisms, since it only overrides
the default behavior of such mechanisms. We also describe its
implementation.
We include a
description of the use of partial type signatures, liberating the
programmer from having to choose  between specifying a complete type
signature  or no type signature at all. Finally we show how the system
can easily be extended to deal with higher-order predicates, thus
enabling the elegant formulation of some form of generic programming.
%endif %% atze
%if False
To our knowledge the implementation is the first to handle these features
in combination with existentials and higher ranked polymorphic types.
%endif
%else
A great deal has been written about type systems.
Much less has been written about implementing them.
Even less has been written about implementations of complete compilers in which
all aspects come together.
%if storyAFP04Notes
This paper fills this gap by describing the implementation
of a series of compilers
for a simplified variant of Haskell.
%else
This paper fills this gap by describing the implementation of a compiler
for a simplified variant of Haskell.
%endif
By using an attribute grammar system, aspects of a compiler implementation
can be described separately and
added in a sequence of steps,
thereby giving a series of increasingly complex (working) compilers.
Also, the source text of both this paper and the executable compilers come
from the same source files by an underlying minimal weaving system.
Therefore, source and explanation is kept consistent.
%endif
%if asArticle
\end{abstract}
%endif

%if storyAfpTRUU1
\subsection*{Context of \thispaper\}
A previous version of \thispaper\ has been presented and
distributed at the AFP2004 summerschool\footnote{The proceedings
for the AFP2004 summerschool have yet to appear.}.
This paper describes a part of a Haskell compiler under development \cite{dijkstra04ehc-web},
focusing on the type system of the langauge and its implementation.
Subsequent papers will describe the remaining parts of the implementation.

Not all parts of the implementation are explained in \thispaper.
In a subsequent paper (continuing with part II) for example data structures (|data| types, kind inference and records)
will be introduced and explained.
As a consequence at some points in \thispaper\ a forward reference will be made to material to be published
later.
This concerns mainly future design decisions which have an influence on design decisions made in \thispaper.
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Preamble
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclTOC

{\setlength{\parskip}{0cm}
\tableofcontents
\listoffigures
}

%endif %% inclTOC

\raggedbottom

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Part I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclParts
\part{Type checking, inference and polymorphism}
%endif

%if not (storyEHIntro || acm)
\frame<presentation>{
\frametitle{Topics}
\tableofcontents[hidesubsections]
}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intro
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl00

%if not storyEHIntro
%if storyPHD
\section*%
%else
\section%
%endif
{Introduction and overview}
%endif

Haskell98 \cite{peytonjones03has98-rev-rep} is a complex language,
not to mention its more experimental incarnations.
Though also intended as a research platform, realistic compilers for Haskell \cite{www04ghc}
have grown over the years
and understanding and experimenting with
those compilers is not an easy task.
Experimentation on a smaller scale usually is based upon relatively simple and restricted implementations
\cite{jones99thih}, often focusing only on a particular aspect of
the language and/or its implementation.
This paper aims at walking somewhere between this complexity and simplicity by
\begin{itemize}
\item
Describing the implementation of essential aspects of Haskell
(or any other (functional) programming language), hence the name Essential Haskell (EH) used
for simplified variants of
Haskell\footnote{The 'E' in EH might also be expanded to other aspects of the compiler, like being an \textbf{E}xample.}
in \thispaper.
\item
Describing these aspects separately in order to provide a better understanding.
\item
Adding these aspects on top of each other
in an incremental way, thus leading to a sequence of compilers, each for a larger subset of complete Haskell (and extensions).
\item
Using tools like the Utrecht University Attribute Grammar (UUAG) system
\cite{baars04ag-www},
hereafter referred to as the AG system,
to allow for separate descriptions for the various aspects.
\end{itemize}

The remaining sections of this introduction will expand on this by looking at
the intentions, purpose and limitations of \thispaper\ in more detail.
This is followed by a short description
of the individual languages for which we develop compilers throughout \thispaper.
The last part of the introduction contains a small tutorial on the AG system used 
in \thispaper.
After the introduction we continue with discussing the implementation
of the first three compilers
(sections \ref{ehc1}, \ref{ehc2} and \ref{ehc3})
out of a (currently) sequence of ten compilers.
On the web site \cite{dijkstra04ehc-web} for this project the  full distribution of the code for these compilers can be found.
We conclude \thispaper\ by reflecting upon our experiences with the AG system and the creation of \thispaper\ (\secRef{ehcConcl}).

%if storyPHD
\subsection*%
%else
\subsection%
%endif
{Purpose}

%if storyEHIntro
\frame<presentation>
{
\frametitle{Why and what?}
\begin{itemize}
\item EHC (Essential Haskell compiler) is a compiler
\begin{itemize}
\item For Haskell restricted to its essentials
\item Each language feature as general as possible
\item For experimentation and education
\end{itemize}
\item The implementation of the compiler
\begin{itemize}
\item Is partitioned into steps, building on top of each other
\item Each step adds a language feature
\item Each step implements a working compiler which can be used as starting point for changes
\end{itemize}
\item Design starting point
\begin{itemize}
\item Use explicit (type) information provided by programmer
\item And make best effort to propagate this information to where it is needed
\end{itemize}
\end{itemize}
}

%else
\frame<presentation>
{
\frametitle{What do we hope to have achieved?}
\begin{itemize}
\item Show how a compiler for an extended subset of Haskell can be implemented
\item While at the same time minimizing complexity and maximizing generality
\item By describing the implementation as a combination of separate aspects
\item Which are introduced gradually in a sequence of working compilers
\item For which the source text is derived from the same source as for these slides
\item So you can be sure we are not cheating :-)
\end{itemize}
}

\frame<presentation>
{
\frametitle{What can you learn from this talk?}
\begin{itemize}
\item How to use AG, its idiom and patterns
\item Do functional programming via AG
\item Type systems, especially how to implement one
\item Haskell from an implementors point of view
\end{itemize}
}

\frame<presentation>
{
\frametitle{What would be nice to know already?}
\begin{itemize}
\item Haskell
\item Tools from the preschool
\begin{itemize}
\item Attribute Grammar (AG) system/notation
\item Parser Combinators
\item Pretty Printing Combinators
\end{itemize}
\item Type systems
\item But: a bit of it will be explained on the fly
\end{itemize}
}

\frame<presentation>
{
\frametitle{Other similar work}
\begin{itemize}
\item Combination of theory and implementation
\begin{itemize}
\item Typing Haskell in Haskell (Mark Jones)
\item Practical type inferencing for arbitrary-rank types (Simon P Jones e.a.)
\item Types and Programming Languages (Benjamin C Pierce)
\end{itemize}
\item Usually partial descriptions/implementations
\begin{itemize}
\item Limited interaction with other features
\item Must fit in limited space available for (research) paper
\item Meant for making a particular point clear
\end{itemize}
\end{itemize}
}
%endif

\frame<presentation>
{
\frametitle{And then?}
\begin{itemize}
\item Extend the set of compilers up to a full Haskell++ compiler
\item Keep it understandable
\item So it can still be used for experimentation and education
\item Will grow over time
\begin{itemize}
\item see @http://www.cs.uu.nl/groups/ST/Ehc/WebHome@
\end{itemize}
\end{itemize}
}

%if storyEHIntro
\frame<presentation>
{
\frametitle{How is it implemented?}
\begin{itemize}
\item Tools
\begin{itemize}
\item Attribute grammar system
\item Parser combinators
\item Code weaving tools
\end{itemize}
\item Design starting points
\begin{itemize}
\item Stick to standard (Hindley/Milner) type inference
\item Use explicit (type) information provided by programmer
\item And make best effort to propagate this information to where it is needed
\item So we are not limited by type inference
\end{itemize}
\end{itemize}
}

%else

\frame<presentation>
{
\frametitle{About the slides and the paper}
\begin{itemize}
\item The content
\begin{itemize}
\item Is new
\item Builds on multiple formalisms (parsing, types, AG)
\item Which makes it confusing and complex at times
\item Also because code fragments balance between realistic (occasionally too large) and concise (occasionally ignoring details)
\end{itemize}
\item Practicalities
\begin{itemize}
\item Based on source code for working compilers
\item Which is too much to present here
\item Or is ``not interesting'' or straightforward
\item So presented code in slides has loose ends
\item Which partially can be found in the paper
\end{itemize}
\item We will primarily look at the AG for EH 1 and 2
\end{itemize}
}

\frame<presentation>
{
\frametitle{Some observations}
\begin{itemize}
\item Generated code does not know anything about AG
\begin{itemize}
\item Error messages from Haskell compilers are in terms of Haskell, not AG
\item Which makes debugging unpleasant at times
\end{itemize}
\item AG programming
\begin{itemize}
\item Structure extension (additional |DATA| variants)
\item Aspect extension (additional |ATTR| definitions)
\item Gives rise to quadratic complexity
\end{itemize}
\end{itemize}
}
%endif

For whom is this material intended?
\begin{itemize}
\item
For students who wish to learn more about the implementation of functional languages.
This paper also informally explains the required theory, in particular about type systems.
\item
For researchers who want to build (e.g.) a prototype and to experiment
with extensions to the type system and need a non-trivial and realistic starting point.
This paper provides documentation, design rationales and an implementation for such a starting point.
\item
For those who wish to study a larger example of the tools used to build the compilers in \thispaper.
We demonstrate the use of the AG system,
which allows us to separately describe the various aspects of a language implementation.
%if incl01TopicParsing
Parser combinators \cite{swierstra00parser-toytool,swierstra99parser-tutor}
are used to compactly describe executable syntax.
%endif
Other tools for maintaining consistency between different versions of the resulting compilers
and the source code text included in \thispaper\ are also used, but will not be discussed.
\end{itemize}

For this intended audience \thispaper
%if llncs
provide:
%else
provides:
%endif

\begin{itemize}
\item
A description of the implementation of a type checker/inferencer for
a subset of Haskell.
We describe the first three languages of a (currently) sequence of ten,
that end in a full implementation of an extended Haskell.
\item
A description of the semantics of Haskell, lying between the
more formal
\cite{hall96type-class-haskell,faxen02semantics-haskell}
and more implementation oriented
\cite{jones00thih,ipt:impl-func-prog-lang} and similar to other combinations of
theory and practice \cite{typing:types-prog-lang:pierce}.
\item
A gradual instead of a big bang explanation.
\item
Empirical support for the belief that the complexity of a compiler
can be managed by splitting the implementation of the compiler into separate aspects.
\item
A working combination of otherwise usually separately proven or implemented features.
\end{itemize}

We will come back to this in our conclusion (see \secRef{ehcConcl}).

We restrict ourselves in the following ways, partly because of space limitations, partly by design:

\begin{itemize}
%if storyAFP04Notes
\item
We do not discuss extensions to Haskell implemented in versions beyond the last version presented in \thispaper.
See \secRef{eh-not-described} for a preciser description of what can and cannot be found in \thispaper\
with respect to Haskell features.
%endif
\item
We concern ourselves with typing only.
Other aspects, like pretty printing and parsing, are not discussed.
However, the introduction to the AG system (see \secRef{ag-primer}) gives some examples
of the pretty printing and the interaction between parsing, AG code and Haskell code.
\item
We do not deal with type theory or parsing theory as a subject on its own.
This paper is intended to describe ``how to implement'' and
will use theory from that point of view.
Theoretical aspects are touched upon from a more intuitive point of view.
\end{itemize}

Although informally and concisely introduced where necessary,
familiarity with the following will make reading and understanding \thispaper\ easier:
\begin{itemize}
\item
Functional programming, in particular using Haskell
\item
Compiler construction in general
\item
Type systems, |lambda|-calculus
\item
Parser combinator library and AG system \cite{baars04ag-www,uust04www}
\end{itemize}

%if incl00TopicAGPrimer
For those not familiar with the AG system a short tutorial has been included at the end of
this introduction (see \secRef{ag-primer}).
It also demonstrates the use of the parser combinators used throughout the implementation of
all EH versions.
%endif

We expect that by finding a balance between theory and implementation,
we serve both those who want to learn and those who want to do research.
It is also our belief that by splitting the big problem into smaller aspects the combination can
be explained in an easier way.

In the following sections we give examples of the Haskell features
present in the
series of compilers described in
%if not incl06
the following chapters.
%else
\chapterRef{ehc1} throughout
\chapterRef{ehc6}.
%endif
Only short examples are given, so the reader gets an impression of what is explained in more detail
and implemented in the relevant versions of the compiler.

%if storyPHD
\subsection*%
%else
\subsection%
%endif
{A short tour}

Though all compilers described in \thispaper\ deal with a different issue,
they all have in common that they are based on the \IxAsIs{|lambda|-calculus},
most of the time using the syntax and semantics of Haskell.
The first version of our series of compilers therefore accepts a language that most closely resembles the
|lambda|-calculus, in particular typed |lambda|-calculus extended with |let| expressions
and some basic types and type constructors such as |Int|, |Char| and tuples.

\paragraph{EH version 1: |lambda|-calculus.}
An EH program is a single expression, contrary to a Haskell program which consists of a set of declarations forming a module.

\begin{code}
%%1srcfile(test/1-demo2.eh%%)
\end{code}

All variables need to be typed explicitly; absence of an explicit type is considered to be an error.
The corresponding compiler (EH version 1, \chapterRef{ehc1}) checks the explicit types against
actual types.
%if not storyAFP04Notes
For example:

\begin{code}
%%1srcfile(test/1-all-fail2.eh%%)
\end{code}

is not accepted.
%endif

Besides the basic types |Int| and |Char|, composite types can be formed by building tuples and defining functions:

\begin{code}
%%1srcfile(test/1-demo3.eh%%)
\end{code}

Functions accept one parameter only, which can be a pattern.
All types are monomorphic.

%if storyVariantETAPSLinks
%else
\frame<presentation>
{
\frametitle{EH version 1: |lambda|-calculus}
\begin{itemize}
\item EH program is single expression
\SafeCode{%
\begin{code}
%%1srcfile(test/1-demo2.eh%%)
\end{code}
}
\item Types |Int|, |Char|, tuples and functions
\SafeCode{%
\begin{code}
%%1srcfile(test/1-demo3.eh%%)
\end{code}
}
\end{itemize}
}

\frame<presentation>[containsverbatim]
{
\frametitle{EH version 1: type checking}
\begin{itemize}
\item Type signatures are required
\item Types are checked
\SafeCode{%
\begin{code}
%%1srcfile(test/1-all-fail2.eh%%)
\end{code}
}
gives rise to error annotated representation of program:
\begin{TT}
%%1ppfile(test/1-all-fail2.eh%%)
\end{TT}
\end{itemize}
}
%endif

%if incl02
\paragraph{EH version 2: Explicit/implicit typing.}
The next version
(EH version 2, \chapterRef{ehc2})
no longer requires the explicit type specifications, which thus may have to be inferred by the compiler.
%if not storyAFP04Notes
For example for:

\begin{code}
%%2srcfile(test/1-sig-fail.eh%%)
\end{code}

the compiler will infer the type specification |i :: %%2file(test/1-sig-fail.eh%%)|.
%endif

The reconstructed type information is monomorphic, for example the identity function in:

\begin{code}
%%2srcfile(test/2-demo1.eh%%)
\end{code}

is inferred to have the type |id :: %%2file(test/2-demo1.eh%%)|.

%if storyVariantETAPSLinks
%else
\frame<presentation>
{
\frametitle{EH version 2: Explicit/implicit typing}
\begin{itemize}
\item Type signature may be omitted
\SafeCode{%
\begin{code}
%%2srcfile(test/1-sig-fail.eh%%)
\end{code}
}
\item
Missing type is inferred: |i :: %%2file(test/1-sig-fail.eh%%)|
\item Inferred types are monomorphic
\SafeCode{%
\begin{code}
%%2srcfile(test/2-demo1.eh%%)
\end{code}
}
gives rise to type
|id :: %%2file(test/2-demo1.eh%%)|
\end{itemize}
}
%endif

%endif %% incl02

%if incl03

\paragraph{EH version 3: Polymorphism.}
The third version
(EH version 3, \chapterRef{ehc3})
performs standard
Hindley-Milner type inferencing \cite{ipt:type-infer-milner,damas82principal-type}
which also supports parametric polymorphism.
For example,
\begin{code}
let  id = \x -> x
in   id 3
\end{code}
is inferred to have type |id :: %%3(let id = \x -> x in id%%)|.

%if not storyAFP04Notes
A type for a value can also be specified explicitly
\begin{code}
let  id :: a -> a
     id = \x -> x
in   id 3
\end{code}
The type signature is checked against the inferred type.
%endif

%if storyVariantETAPSLinks
%else
\frame<presentation>
{
\frametitle{EH version 3: Polymorphism}
\begin{itemize}
\item
Polymorphism a la Haskell (i.e. Hindley/Milner)
\item For example
\SafeCode{%
\begin{code}
let  id = \x -> x
in   id 3
\end{code}
}
gives type |id :: %%3(let id = \x -> x in id%%)|
\item Type signature may be given
\SafeCode{%
\begin{code}
let  id :: a -> a
     id = \x -> x
in   id 3
\end{code}
}
\item Type signature can further constrain a type
\SafeCode{%
\begin{code}
let  id :: Int -> Int
     id = \x -> x
in   id 3
\end{code}
}
\end{itemize}
}
%endif

%endif %% incl03

%if incl04 || storyEHIntro
\paragraph{EH version 4: Higher ranked types.}
Standard Hindley-Milner type inferencing cannot infer polymorphic parameters:
so-called rank-2 polymorphism.
In general, this is a hard thing to do and even impossible for rank-3 (and higher) types
\cite{jim95rank,kfoury94direct,kfoury99rank2-decid,kfoury03rank2-princ},
so the fourth version
(EH version 4,
%if storyAFP04Notes
describe elsewhere \cite{dijkstra04thag-part1}
%else
\chapterRef{ehc4}%
%endif
)
does not infer this type information but
allows explicitly specified polymorphism for (e.g.) parameters.

For example, the following is allowed.
\begin{code}
let  f :: (forall a . a -> a) -> (Int,Char)
     f = \i -> (i 3, i 'x')
in   f
\end{code}
Note that the type signature thus is required here.

This version also provides some notational sugaring by allowing one to omit
the explicit quantifiers from
the type signature.
For example, if the |forall| in the previous example is omitted
the correct location for the quantifier is inferred,
based on the occurrences of type variables in a type expression:
\begin{code}
let  f :: (a -> a) -> (Int,Char)
     f = \i -> (i 3, i 'x')
in   f
\end{code}
infers |f :: %%4(let  f :: (a -> a) -> (Int,Char) in f%%)|

Specifying a complete type signature can be difficult for complicated types,
so it is also permitted to leave argument and results of a function unspecified
using a \IxAsDef{partial type signature}.
\begin{code}
%%4srcfile(test/4-ty-wild1.eh%%)
\end{code}
For |f| only the part that cannot be inferred is given in the signature.

Finally, type information can be hidden, or encapsulated,
by using existential quantification:
\begin{code}
%%4srcfile(test/4-demo1.eh%%)
\end{code}
The tuple |xy| contains an |Int| and a function making an |Int| from the
value of which the type has been hidden.
Access to the elemnts of such a tuple is done by pattern matching, as in the argument position of the function |ixy|.
The attempt to construct |pq| fails.

When a value of an existentially quantified type is opened, that is,
identifiers are bound to its components,
the hidden type becomes visible in the form of a fresh type constant.
The explicit |exists| may also be omitted, for example
|xy :: (a, a->Int)| is interpreted as |xy :: exists a . (a, a->Int)|.


%if storyVariantETAPSLinks
\frame<presentation>
{
\frametitle{EH version 4: Higher ranked types}
\begin{itemize}
\item
Type signatures for quantifiers on argument (higher ranked) positions
\SafeCode{%
\begin{code}
%%4srcfile(test/4-demo2.eh%%)
\end{code}
}
\begin{itemize}
\item Notational sugaring allows omission of quantifier
\item Partial specification of type signature (rest is inferred)
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{EH version 4: Higher ranked types}
\begin{itemize}
\item
Impredicativity
\SafeCode{%
\begin{code}
%%4srcfile(test/4-impred1.eh%%)
\end{code}
}
\begin{itemize}
\item Propagation of explicit type information
\item Which cannot (easily) be reconstructed
\end{itemize}
\end{itemize}
}

%else
\frame<presentation>
{
\frametitle{EH version 4: Higher ranked types}
\begin{itemize}
\item
Type signatures for quantifiers on argument (higher ranked) positions
\SafeCode{%
\begin{code}
let  f :: (forall a . a -> a) -> (Int,Char)
     f = \i -> (i 3, i 'x')
in   f
\end{code}
}
\item Notational sugaring allows omission of quantifier
\SafeCode{%
\begin{code}
let  f :: (a -> a) -> (Int,Char)
     f = \i -> (i 3, i 'x')
in   f
\end{code}
}
\end{itemize}
}

\frame<presentation>
{
\frametitle{EH version 4: Existential types}
\begin{itemize}
\item
Existential quantification: hiding/forgetting type information
\SafeCode{%
\begin{code}
%%4srcfile(test/4-demo1.eh%%)
\end{code}
}
\end{itemize}
}

\frame<presentation>
{
\frametitle{EH version 4: Existential types}
\begin{itemize}
\item Notational sugaring allows omission of quantifier
\begin{itemize}
\item
|xy :: (a, a->Int)| is interpreted as
\item
|xy :: exists a . (a, a->Int)|
\end{itemize}
\item Interprets type structure to find suitable location for quantifier
\begin{itemize}
\item |a| occurs in |sigma1| and |sigma2| in |sigma1 -> sigma2| and not outside: |forall|
\item |a| occurs in |sigma1| and |sigma2| in |(sigma1,sigma2)| and not outside: |exists|
\end{itemize}
\end{itemize}
}
%endif %% storyVariantETAPSLinks

%endif %% incl04

%if incl05 || (storyEHIntro && not storyVariantETAPSLinks)

\paragraph{EH version 5: Data types.}
The fifth version (EH version 5, \chapterRef{ehc5})
adds |data| types and opening/unpacking/scrutinizing
a data type value by means of a |case| expression.
\begin{code}
%%5srcfile(test/5-list.eh%%)
\end{code}

\frame<presentation>
{
\frametitle{EH version 5: Data types}
\begin{itemize}
\item
User defined data types
\SafeCode{%
\begin{code}
%%5srcfile(test/5-list.eh%%)
\end{code}
}
\item Unpacking via case expression
\end{itemize}
}

%endif %% incl05

%if incl06 || storyEHIntro

\paragraph{EH version 6: Kinding.}
The previous version allows incorrect programs because
data types can be used incorrectly:
\begin{code}
%%6srcfile(test/5-list-wrong.eh%%)
\end{code}
The type of |v| is not a type of a value, and thus the type of |v|
itself is not well-typed.
The sixth version (EH version 6, \chapterRef{ehc6})
adds kind (that is, the type of a type) inferencing.
%if noAFP04
For example, the previous example gives
\begin{TT}
%%6ppfile(test/5-list-wrong.eh%%)
\end{TT}
%endif

With the notion of the kind of a type also comes
the notion of polymorphism for kinds,
which this version allows:
\begin{code}
let  data Eq a b = Eq (forall f . f a -> f b)
     id = \x -> x
in   Eq id
\end{code}
infers for type constructor |Eq|
\begin{TT}
%%6ppinline(let data Eq a b = Eq (forall f . f a -> f b) in 3%%)
\end{TT}

%if storyVariantETAPSLinks
%else
\frame<presentation>
{
\frametitle{EH version 6: Kinds}
\begin{itemize}
\item
Type expressions can be incorrectly used
\SafeCode{%
\begin{code}
%%6srcfile(test/5-list-wrong.eh%%)
\end{code}
}
\item
Requires type system for types (similar to type system for values)
\item
Type of a type: kind
\item Examples
\begin{itemize}
\item Kind of |Int :: *|
\item Kind of |List a :: *|
\item Kind of |List ::* -> *|
\end{itemize}
\item
Kind inferencing/checking for types
(similar to type inferencing/checking for values)
\begin{itemize}
\item Values must have type with kind |:: *|
\end{itemize}
\end{itemize}
}
%endif

\frame<presentation>
{
\frametitle{EH version 6: Kind polymorphism}
\begin{itemize}
\item
Kind signatures for types (similar to type signatures for values)
\SafeCode{%
\begin{code}
%%6srcfile(test/6-expl-ki.eh%%)
\end{code}
}
\item
Polymorphic kinds can also be inferred
\SafeCode{%
\begin{code}
%%6srcfile(test/5-all-ok2.eh%%)
\end{code}
}
infers kind @Eq :: Forall a . a -> a -> *@
\end{itemize}
}

%endif %% incl06

%if incl07 || (storyEHIntro && not storyVariantETAPSLinks)
\frame<presentation>
{
\frametitle{EH version 7: Non extensible records}
\begin{itemize}
\item Replacement for tuples
\SafeCode{%
\begin{code}
%%7srcfile(test/7-demo1.eh%%)
\end{code}
}
\end{itemize}
}

%endif %% incl07

%if incl08 || (storyEHIntro && not storyVariantETAPSLinks)
\frame<presentation>
{
\frametitle{EH version 8: Code generation}
\begin{itemize}
\item In phases
 \begin{itemize}
 \item to core representation (removing syntactic sugar, ...)
 \item via transformations (lambda lifting, ...)
 \item to code for abstract sequential machine
 \end{itemize}
\item Interpreter for abstract sequential machine
\end{itemize}
}
%endif %% incl08

%if incl09 || storyEHIntro
\frame<presentation>
{
\frametitle{EH version 9: Class system, explicit implicit parameters}
\begin{itemize}
\item Class system
\item + named instances
\item + explicit dictionary passing
\item + scoping for instances
\item + coercions
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{EH version 9: explicit parameter}
\SafeCode{%
\begin{code}
%%9srcfile(eh-frags/9-eq-nub.eh%%)
\end{code}
}
}

%if storyVariantETAPSLinks
\frame<presentation>[plain]
{
\frametitle{EH version 9: higher order predicate}
\SafeCode{%
\begin{code}
%%9srcfile(test/9-snd-order1.eh%%)
\end{code}
}
}

%if False
\frame<presentation>[plain]
{
\frametitle{EH version 9: implementation}
\begin{itemize}
\item does not become simpler...
\[
\rulerCmdUse{rules2.expr9.base.e-app}
\]
\end{itemize}
}
%endif

%endif %% storyVariantETAPSLinks

%endif %% incl09

%if incl10 || (storyEHIntro && not storyVariantETAPSLinks)
\frame<presentation>
{
\frametitle{EH version 10: Extensible records}
\begin{itemize}
\item Flexibility w.r.t. presence of labels
\SafeCode{%
\begin{code}
%%10srcfile(test/10-demo1.eh%%)
\end{code}
}
\item More general tuple access
\SafeCode{%
\begin{code}
%%10srcfile(test/10-snd.eh%%)
\end{code}
}
\end{itemize}
}
%endif %% incl10

%if incl11 || (storyEHIntro && not storyVariantETAPSLinks)
\frame<presentation>
{
\frametitle{EH version [11..]: ...}
\begin{itemize}
\item (Student) projects
\begin{itemize}
\item Support for Attribute Grammars
\item Efficient code generation
\end{itemize}
\end{itemize}
}
%endif %% incl11

%if storyPHD
\subsection*%
%else
\subsection%
%endif
{Haskell language elements not described}
\label{eh-not-described}

As mentioned before, only a subset of the full sequence of compilers is described in \thispaper.
Currently, as part of an ongoing work \cite{dijkstra04ehc-web},
in the compilers following the compilers described in \thispaper, the following Haskell features are dealt with:

\begin{description}
%if not incl04
\item[EH 4.] Quantifiers everywhere: higher ranked types
\cite{shan04sexy-types,peytonjones04pract-inf-rank,botlan03ml-power-f,odersky97putting-ann}
and existentials \cite{perry91phd,laufer94poly-absdata,mitchell88absty-exist}.
See also the longer version of \thispaper\ handed out during the AFP04 summerschool
\cite{dijkstra04thag-part1}.
%endif
%if not incl05
\item[EH 5.] Data types.
%endif
%if not incl06
\item[EH 6.] Kinds, kind inference, kind checking, kind polymorphism.
%endif
%if not incl07
\item[EH 7.] Non extensible records, subsuming tuples.
%endif
%if not incl08
\item[EH 8.] Code generation for a GRIN (Graph Reduction Intermediate Notation) like backend
\cite{boquist96grin-optim,boquist99phd-optim-lazy}.
%endif
%if not incl09
\item[EH 9.] Class system, explicit implicit parameters \cite{dijkstra04expl-impl-param-tr}.
%endif
%if not incl10
\item[EH 10.] Extensible records \cite{gaster96poly-ext-rec-var,jones99lightweight-ext-rec}.
%endif
\end{description}

Also missing are features which fall in the category syntactic sugar, programming in the large and the like.
Haskell incorporates many features which make programming easier and/or manageable.
Just to mention a few:
\begin{itemize}
\item
Binding group analysis
\item
Syntax directives like infix declarations
\item
Modules
\cite{diatchki02hask-module,shields01first-class-mod}.
\item
Type synonyms
\item
Syntactic sugar for |if|, |do|, list notation and comprehension.
\end{itemize}

We have deliberately not dealt with these issues.
Though necessary and convenient we feel that these features should be added after all else has been dealt with,
so as not to make understanding and implementating essential features more difficult.

%if False
%if storyPHD
\subsection*%
%else
\subsection%
%endif
{Untackled newly arisen issues}
The extra features on occasions also add new issues...

\paragraph{Kind checking required during type inferencing}

\TBD{}

Kinds may be different than tacitly assumed.

\begin{code}
let  data Eq a b = Eq (forall f . f a -> f b)
     data L a = N | C a (L a)
     undefined :: forall a . a
in   let  maf :: (a -> Eq a a) -> Eq L L
          maf = \g -> g undefined
     in   3
\end{code}

\paragraph{Co/contra variance analysis/inference/checking}

\TBD{}
%endif

%if False
%if storyPHD
\subsection*%
%else
\subsection%
%endif
{About the presented code}

The fact that multiple versions of a compiler are described and
form the basis around which the explanation has been woven brings
some consequences
\begin{itemize}
\item
Versions are built on top of each other.
However, in practice this meant that
after a next version was constructed, refactoring of earlier versions was in general necessary.
As such what  we describe is ``an optimal line of development''.
It is not the case that the versions represent a timeline,
a tale of how the different versions came into being.
It also means that all versions are dependent on each other and
are designed as a whole.
Any desired change in the last version may imply a change in the first version.
Metaphorically speaking, in order to change the grown-up compiler you may have to tweak
its childhood.

In an ideal situation this would not have been necessary;
but it would be unfair if we did not mention the work that went into getting
all the laysers to work as neatly as they do now.
\item
Since we do not only want to sketch the approach but want to present
a complete compiler we also have to deal with many non-interesting details.
However, for \thispaper\ we have chosen to only incorporate aspects directly related to typing
and omit other aspects like pretty printing, parsing and error reporting.
The complete compiler text can be found on the website accompanying
\thispaper \cite{dijkstra04ehc-web}.
\end{itemize}
%endif %% False


%if incl00TopicAGPrimer
%if storyPHD
\subsection*%
%else
\subsection%
%endif
{An AG mini tutorial}
The remaining part of the introduction contains a small tutorial on the AG system.
The tutorial explains the basic features of the AG system.
The explanation of remaining features is postponed to its first use throughout the main text.
These places are marked with |AGFeature|.
The tutorial can safely be skipped if the reader is already familiar with the AG system.


\inputEHCTex{\EHCTexVersion}{AGMiniPrimer.tex}
%endif

%if not omitLitDiscuss
%if storyPHD
\subsection*%
%else
\subsection%
%endif
<article>{Literature}

\TBD{}

here????
Or merged with previous.

Similar, implementation describing

Rules only

Haskell
%endif

%endif %% incl00

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl01

\section{EH 1: typed |lambda|-calculus}
\label{ehc1}

\frame<presentation>{\tableofcontents[current,hidesubsections]}

In this section we build the first version of our series of compilers:
the typed |lambda|-calculus packaged in Haskell syntax in which
all values need to explicitly be given a type.
The compiler checks if the specified types are in agreement with actual
value definitions.
For example

\begin{code}
%%1srcfile(test/1-demo2.eh%%)
\end{code}

is accepted, whereas

\begin{code}
%%1srcfile(test/1-all-fail2.eh%%)
\end{code}
produces a pretty printed version of the erroneous program,
annotated with errors:
\begin{TT}
%%1ppfile(test/1-all-fail2.eh%%)
\end{TT}

Type signatures have to be specified for identifiers bound in a |let| expression.
For |lambda|-expressions the type of the parameter can be extracted from
these type signatures unless a |lambda|-expression occurs at the position of an applied function.
In that case a type signature for the |lambda|-expression is required in the expression itself.
This program will not typecheck because this EH version does not allow polymorphic types in general and
on higher ranked (that is, parameter) positions in particular.

\begin{code}
%%1srcfile(test/1-polylam1.eh%%)
\end{code}

%if incl01TopicPP

Although the implementation of a type system will be the main focus of this section,
any such implementation
lives in co-existence with the complete environment/framework needed to build a compiler.
Therefore, aspects conveniently omitted from subsequent sections \cite{dijkstra04thag-part1}, like parsing, connection with
the abstract syntax, the use of the AG system and error reporting will also be
touched upon here.

First we start with the EH language elements implemented and how they correspond to
abstract syntax, followed by the translation done by parsing from concrete syntax to abstract syntax.
Our first aspect described using the AG notation will be a pretty printed representation of the abstract syntax tree,
reflecting the original input sufficiently close.
The second aspect concerns type checking, which involves the introduction of
several attributes for computing a type 
associated with parts of the abstract syntax tree.

%else %% incl01TopicPP

The implementation of a type system will be the main focus of this and following sections.
As a consequence the full environment/framework needed to build a compiler will not be discussed.
This means that error reporting, generation of a pretty printed annotated output,
parsing and the compiler driver are not described.

We start with the definition of the AST and how it relates to concrete syntax,
followed by the introduction of several attributes required for the implementation
of the type system.

%endif %% incl01TopicPP

\subsection{Concrete and abstract syntax}
The \IxAsDef{concrete syntax} of a (programming) language describes the structure of acceptable
sentences for that language, or more down to earth, it describes what a compiler for that language
accepts with respect to the textual structure.
On the other hand, \IxAsDef{abstract syntax} describes the structure used by the compiler itself for
analysis and code generation.
Translation from the more user friendly concrete syntax to the machine friendly abstract syntax is done by a
parser; from the abstract to the concrete representation is done by a pretty printer.

Let us focus our attention first on the abstract syntax for EH1, in particular the part
defining the structure for expressions (the remaining syntax can be found in \figRef{abs-syn-eh1}).
%if not incl00TopicAGPrimer
A |DATA| definition in the AG (Attribute Grammar) language corresponds closely to a Haskell |data| definition,
and defines a part of the abstract syntax.
%endif

\chunkCmdUseMark{EHAbsSyn.1.Expr}


\frame<presentation>[t]
{
\frametitle{Abstract syntax for expressions}
\begin{itemize}
\item Expression abstract syntax:
\begin{columns}[onlytextwidth,t]
 \begin{column}{.75\textwidth}
  \chunkCmdFrameUse{EHAbsSyn.1.Expr}
 \end{column}
 \begin{column}{.2\textwidth}
  \SafeCode{%
  \begin{code}
  3
  'x'
  (...,...)
  v
  f a
  let ... in ...
  \... -> ...
  \end{code}
  }
 \end{column}
\end{columns}
\item
|AppTop| has no concrete counterpart
\item
|HsName = String|
\end{itemize}
}

%if incl00TopicAGPrimer
Integer constants are represented by |IConst|, lowercase (uppercase) identifier occurrences by |Var| (|Con|),
an |App| represents the application of a function to its argument, |Lam| and |Let| represent lambda expressions and
let expressions.
%else
The notation of the AG system is used to define an expression |Expr|
to be a range of alternatives (or productions);
for example a single variable |Var| represents the occurrence of an identifier
(referring to a value introduced by
a declaration),
or |App| represents the application of a function to an argument.
%endif

%{
%format < 		= "{\langle}"
%format > 		= "{\rangle}"
\begin{AGFeature}{ag-type-syn}{Type synonyms (for lists)}
The AG notation allows type synomyms for one special case, AG's equivalent
of a list.
It is an often occurring idiom to encode a list of nodes, say |DATA L| with elements |<node>| as:
\begin{code}
DATA L
  | Cons  hd  :  <node>
          tl  :  L
  | Nil
\end{code}
AG allows the following notation as a shorthand:
\begin{code}
TYPE L = [<node>]
\end{code}
\end{AGFeature}
%}

The EH fragment (which is incorrect for this version of because type signatures are missing)
\begin{code}
%%1srcfile(afp-eh/02.eh%%)
\end{code}
is represented by the following piece of abstract syntax tree:
\begin{TT}
%%1astfile(afp-eh/02.eh%%)
\end{TT}
The example also demonstrates the use of patterns, which is
almost the same as in Haskell:
EH does not allow a type signature for the elements of a tuple.

\mode<all>{%
\newcommand{\PGFAppAA}[3]{%
 \begin{center}
  \begin{pgfpicture}{0cm}{0cm}{4cm}{2.5cm}
   \pgfnodebox{App1}[virtual]{\pgfxy(2.5,2)}{App}{2pt}{2pt}
   \pgfnodebox{App2}[virtual]{\pgfxy(1.75,1.25)}{App}{2pt}{2pt}
   \pgfnodebox{Fun}[virtual]{\pgfxy(1,0.5)}{#1}{2pt}{2pt}
   \pgfnodebox{Arg1}[virtual]{\pgfxy(2.5,0.5)}{#2}{2pt}{2pt}
   \pgfnodebox{Arg2}[virtual]{\pgfxy(3.25,1.25)}{#3}{2pt}{2pt}
   \pgfnodeconnline{Fun}{App2}
   \pgfnodeconnline{Arg1}{App2}
   \pgfnodeconnline{App2}{App1}
   \pgfnodeconnline{Arg2}{App1}
  \end{pgfpicture}
 \end{center}
}
}

\frame<presentation>
{
\frametitle{Expressions}
\begin{itemize}
\item Basic values
\begin{itemize}
\item Constants: |IConst|, |CConst|
\item Identifiers: |Var|, |Con| (constructors)
\item Only constructor is tuple constructor: |Con ",<arity>"|
\end{itemize}
\item Applications: |App|, left-associative
\item |f a b| \\
   \PGFAppAA{Var "f"}{Var "a"}{Var "b"}
\item |(5,'x')| \\
   \PGFAppAA{Con ",2"}{IConst 5}{CConst 'x'}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Expressions varieties}
\begin{itemize}
\item Applications of values, patterns and types share same basic structure
\item Pattern |(i,c)|
   \PGFAppAA{Con ",2"}{Var "i"}{Var "c"}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Pattern expression}
\begin{itemize}
\item Pattern expression abstract syntax:
\chunkCmdFrameUse{EHAbsSyn.1.PatExpr}
\item Similar to expressions (and type expressions)
\end{itemize}
}

\frame<presentation>[t,containsverbatim]
{
\frametitle{Abstract syntax example}
\begin{columns}[onlytextwidth,t]
 \begin{column}{.5\textwidth}
  \SafeCode{%
\begin{code}
%%1srcfile(afp-eh/02.eh%%)
\end{code}
  }
 \end{column}
 \begin{column}{.5\textwidth}
\begin{TTtiny}
%%1astfile(afp-eh/02.eh%%)
\end{TTtiny}
 \end{column}
\end{columns}
}

\frame<presentation>
{
\frametitle{Type expression}
\begin{itemize}
\item Type |(Int,Int)|
   \PGFAppAA{Con ",2"}{Con "Int"}{Con "Int"}
\item Type |Int -> Char|
   \PGFAppAA{Con "|->|"}{Con "Int"}{Con "Char"}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Type expression}
\begin{itemize}
\item Type expression abstract syntax:
\chunkCmdFrameUse{EHAbsSyn.1.TyExpr}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Declarations}
\begin{itemize}
\item Declaration abstract syntax:
\chunkCmdFrameUse{EHAbsSyn.1.Decl}
\item Restriction on combination of signature and corresponding value declaration
\begin{itemize}
\item Type signature may only define signature for top level identifier in pattern
\item Ok
\SafeCode{%
\begin{code}
let  x        ::  ...
     x@(...)  =   ...
\end{code}
}
\item Not ok
\SafeCode{%
\begin{code}
let  x          ::  ...
     (..,x,..)  =   ...
\end{code}
}
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Program}
\begin{itemize}
\item Additional node for top expression:
\chunkCmdFrameUse{EHAbsSyn.1.AGItf}
\item
Used for (e.g.) attribute initialization
\item
Node in tree used as interface to Haskell world (via AG |WRAPPER| construct)
\item
Groups of attributes
\savecolumns
\chunkCmdFrameUse{EHAbsSyn.1.AllExpr}
\restorecolumns
\chunkCmdFrameUse{EHAbsSyn.1.AllPatExpr}
\restorecolumns
\chunkCmdFrameUse{EHAbsSyn.1.AllTyExpr}
\restorecolumns
\chunkCmdFrameUse{EHAbsSyn.1.AllNT}
\item
Used for defining attributes for group
of nodes/nonterminals
\end{itemize}
}


\begin{Figure}{Abstract syntax for EH (without Expr)}{abs-syn-eh1}
\savecolumns
\chunkCmdUse{EHAbsSyn.1.AGItf}
\restorecolumns
\chunkCmdUse{EHAbsSyn.1.Decl}
\restorecolumns
\chunkCmdUse{EHAbsSyn.1.PatExpr}
\restorecolumns
\chunkCmdUse{EHAbsSyn.1.AllPatExpr}
\restorecolumns
\chunkCmdUse{EHAbsSyn.1.TyExpr}
\restorecolumns
\chunkCmdUse{EHAbsSyn.1.AllTyExpr}
\restorecolumns
\chunkCmdUse{EHAbsSyn.1.AllExpr}
\restorecolumns
\chunkCmdUse{EHAbsSyn.1.AllNT}
\end{Figure}

Looking at this example and the rest of the abstract syntax in \figRef{abs-syn-eh1} we can make several
observations of what one is allowed to write in EH and what can be expected from the implementation.
\begin{itemize}
\item
There is a striking similarity between the structure of expressions |Expr|
and patterns |PatExpr| (and as we will see later type expressions |TyExpr|):
they all contain |App| and |Con| variants.
This similarity will sometimes be exploited to factor out common code, and, if
factoring out cannot be done, leads to similarities between pieces of code.
This is the case with pretty printing%
%if not incl01TopicPP
 (not included in \thispaper)%
%endif
, which is quite similar for the different kinds of
constructs.
%if not incl00TopicAGPrimer
\item
In the abstract syntax an alternative belongs to a nonterminal (or |DATA|), for
example ``|App| of |Expr|''.
On request the AG system generates corresponding Haskell data types with
the same name as the |DATA| defined, alternatives are mapped to constructors
with the name of the |DATA| combined with the name of the alternative,
separated by an underscore `|_|' as in |Expr_App|.
If necessary, the same convention will be used when referring to an
alternative\footnote{In this way we overcome a problem in Haskell,
where it is required that all constructors for all data types to be different.}.
%endif
\item
Type signatures (|Decl_TySig|) and value definitions (|Decl_Val|) may be freely mixed.
However, type signatures and value definitions for the same identifier are still related.
%if False
For this version of EH, each identifier introduced by means of a value definition must
have a corresponding type signature specification.
%endif
\item
Because of the textual decoupling of value definitions and type signatures,
a type signature may specify the type for an identifier occurring inside a pattern:
\begin{code}
let  a      ::  Int
     (a,b)  =   (3,4)
in   ...
\end{code}
Currently we do not allow this, but the following however is:
\begin{code}
let  ab        ::  (Int,Int)
     ab@(a,b)  =   (3,4)
in   ...
\end{code}
because the specified type for |ab| corresponds to the top of a pattern of a value definition.
\item
In EH composite values are created by tupling, denoted by |(..,..)|.
The same notation is also used for patterns (for unpacking a composite value) and
types (describing the structure of the composite).
In all these cases the corresponding AST consists of a |Con| applied to the elements
of the tuple.
For example, the value |(2,3)| corresponds to
\begin{code}
Expr_App (Expr_App (Expr_Con ",2") (Expr_IConst 2)) (Expr_IConst 3)
\end{code}
\item
For now there is only one value constructor: for tuples.
The EH constructor for tuples also is the one which needs special treatment because it
actually stands for a infinite family of constructors.
This can be seen in the encoding of the name of the constructor which is composed of
a |","| together with the arity of the constructor.
For example, the expression |(3,4)| is encoded as an application |App| of |Con ",2"|
to the two |Int| arguments: (,2 3 4).
In our examples we will follow the Haskell convention, in which we write (,) instead of `,2'.
By using this encoding we also get the unit type |()| as it is encoded
by the name |",0"|.
\item
The naming convention for tuples and other naming conventions are available through the following definitions
for Haskell names |HsName|.

\chunkCmdUseMark{EHCommon.1.HsName.type}
\chunkCmdUseMark{EHCommon.1.HsName.Base}

\item
Each application is wrapped on top with an |AppTop|.
This has no meaning in itself but
%if incl01TopicPP
as we will see in section~\ref{sec-pretty1}
%else
it
%endif
simplifies the pretty printing of expressions\footnote{As it also complicates parsing it may disappear in future versions of EH.}.
We need |AppTop| for patterns, but for the rest it can be ignored.
\item
The location of parentheses around an expression is remembered by a |Parens| alternative.
We need this for the reconstruction of the parenthesis in the input.
\item
|AGItf| is the top of a complete abstract syntax tree.
%if incl00TopicAGPrimer
As noted in the AG primer this is the place where interfacing with the `outside' Haskell world takes place.
%else
The top of an abstract syntax tree is the place where interfacing
(hence the convention |Itf|)
with the outside, that is, the Haskell world takes place.
At |AGItf| 
  \begin{itemize}
  \item Initialization of inherited attributes takes place.
  \item Synthesized attributes are routed back into the tree as inherited attributes.
  \item Synthesized attributes are passed to the outside, to the Haskell world.
  \end{itemize}
%endif
It is a convention in \thispaper\ to give all nonterminals in the abstract syntax a
name with |AGItf| in it,
if it plays a similar role.
%if not incl00TopicAGPrimer
\item
The remaining alternatives for the non-terminal |Expr|
stand for their EH counterparts, for example |IConst| for an |Int| constant,
and |Lam| for a |lambda|-expression.
\item
In the AG system sets of nonterminals may be given a name through the |SET| directive.
For example, |AllNT| (All NonTerminals) is the name for all nonterminals occurring in the abstract syntax.
This provides an indirection when referring to nonterminals in attribute definitions (|ATTR|).
Later compiler versions can change the definition for |AllNT| without the need to
also modify the list of nonterminals for which an attribute definition |ATTR| is
declared.
%endif
\end{itemize}

%if incl01TopicParsing

\subsection{Parsing: from concrete to abstract (syntax)}
An abstract syntax tree is obtained as the result of parsing.
The parser combinator library used (see \cite{uust04www} and \figRef{parser-combinators}) to specify the parser for EH allows
us to simultaneously define the syntactic structure and the connection to
the abstract syntax.
For example, the parser for the simplest of expressions

\savecolumns
\chunkCmdUse{EHParser.1.pExprBase}
\restorecolumns
\chunkCmdUse{EHParser.1.pExprBaseParenProd}

recognises variables via |pVar| and integer constants via |pInt|.
These parsers are based on parsers defined in a general purpose scanner library \cite{uust04www}
about which
we will say no more than that it provides basic parsers tailored towards the recognition
of Haskell lexical elements:

\chunkCmdUse{EHParser.1.scanWrappers}

All parsers from this library return their result as a string,
which,
after additional postprocessing,
can conveniently be passed as an argument to the semantic function.
For example, for a variable, the semantic function |sem_Expr_Var| is applied to the result of |pVarid| packaged as a |HsName|.
The semantic function is generated by the AG system from the attribute grammar.

The use of semantic functions deserves some closer inspection.
The attribute grammar associates semantics with each abstract syntax
tree, which is represented by a function from inherited to synthesized
attributes. The semantic functions correspondings to alternatives in the
grammar map the semantics of the non-terminals occuring in the right
hand side of the production to the semantics of the nonterminal in the
left hand side.






For each of the alternatives of a non-terminal the AG system generates a
\IxAsDef{semantic function},
that takes as argument the semantic functions corresponding to its right hand side,
and constructs a function mapping inherited to synthesized attributes.
The structure of such a function, that is, its type is similar to its related and also to its
generated datatype.
Similar in the sense that both take their components as their first arguments.
For example, both |Expr_Var| as a data constructor and |sem_Expr_Var| take
a string as their first argument.
It is this similarity we exploit:
instead of first building the abstract syntax tree and subsequently analysing
it we directly map semantics associated with the children onto the
semantics of the father.
For all practical purposes, for now, this is all we need in order to be able to use
these functions
(see also \cite{swierstra99comb-lang}).

\begin{Figure}{Parser combinators}{parser-combinators}
\begin{tabular}{lll}
\ParserCombTableHead
\ParserCombTableA
\ParserCombTableB
\end{tabular}
\end{Figure}

\frame<presentation>
{
\frametitle{Parsing expressions}
\begin{itemize}
\item Basic expressions
\chunkCmdFrameUse{EHParser.1.pExprBase}
\chunkCmdFrameUse{EHParser.1.pExprBaseParenProd}
\item |pInt| (etc.): parsers for lexical tokens
\item |sem_XXX|: AG generated functions
 \begin{itemize}
 \item taking abstract syntax tree children as arguments
 \item yielding functions for attribute computations
 \end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Semantic functions}
\begin{itemize}
\item An (edited) example of an AG generated semantic function
\SafeCode{%
\begin{code}
data Expr    = Expr_IConst (Int)
             | ...
type T_Expr  =  ... -> ... -> ... ->
                ... -> ... -> ... ->
                (  ..,..,..,..
                ,  (PP_Doc),..,..,..)
sem_Expr     :: (Expr) -> (T_Expr)
sem_Expr ((Expr_IConst (_int))) =
    (sem_Expr_IConst (_int))
sem_Expr_IConst :: (Int) -> (T_Expr)
sem_Expr_IConst (int_) =
    \ ... ... ... ... ... ... ->
       let  (_pp@_) = text (show int_)
       in   ( ..,..,..,..,_pp@_,..,..,.. )
\end{code}
}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Parsing combinators}
\begin{itemize}
\item Basic parser combinators
\begin{center}
\begin{tabular}{llp{.3\textwidth}}
\ParserCombTableHead
\ParserCombTableA
\end{tabular}
\end{center}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Parsing expressions}
\begin{itemize}
\item Composite expressions
\chunkCmdFrameUse{EHParser.1.pExpr}
\end{itemize}
}

From |pExprBase| the parser |pExpr| for the more complex expressions is built.

\savecolumns
\chunkCmdUseMark{EHParser.1.exprAlg}
\restorecolumns
\chunkCmdUseMark{EHParser.1.pExpr}
\restorecolumns
\chunkCmdUseMark{EHParser.1.pExprApp}
\restorecolumns
\chunkCmdUseMark{EHParser.1.pExprPrefix}
\restorecolumns
\chunkCmdUseMark{EHParser.1.pExprPrefixLam}

An application |pExprApp| is a juxtapositioning of a non empty series
of |pExprBase|'s.
A |pExprApp| itself can be prefixed with parsers making the expression
into a |let|- or |lambda|-expression.
The parser |pExprPrefix| recognizes this prefix
and returns a function
that maps the semantics of the expression that follows it into the
semantics of the corresponding construct.
The result of |pExprPrefix| is a function and can therefore be applied immediately to |pExprApp| by the
sequencing combinator |<*>|.

Parser |pExprApp| is defined in terms of an algebra |exprAlg| and an
abstract parser recognising applications

\chunkCmdUse{EHParser.1.pApp}

First, let us look at the \IxAsIs{algebra} |exprAlg|, again referring to
Swierstra \cite{swierstra99comb-lang} for a more thorough
treatment.
The abstract syntax of EH (see \figPageRef{abs-syn-eh1})
has |Con|, |App| and |AppTop| alternatives for |Expr|, |TyExpr| and |PatExpr|.
For all three kinds of expressions an application |f x y| maps
to the same structure |AppTop (App (App f x) y)|,
where for a 2-tuple pattern |f == Con ",2"|,
and for a function type |f == Con  "->"| (representing |x -> y|).
The |Con|, |App| and |AppTop| depend on the particular kind
of expression.
The following function |mkApp| does this job of
creating the application given a list of elements (here [f,x,y]) to make
the application from, and the specific variants for |Con|, |App| and |AppTop|.
Note that no |AppTop| is placed around a singleton list since this is not an application
and the function |mkApp| is never used on an empty list.

\chunkCmdUseMark{EHCommon.1.MkConApp}
\chunkCmdUseMark{EHCommon.1.mkApp.Base}
\chunkCmdUseMark{EHCommon.1.mkApp.mkArrow}
\chunkCmdUseMark{EHCommon.1.mkApp.mkConApp}
\chunkCmdUseMark{EHCommon.1.mkApp.mkProdApp}

\frame<presentation>[plain]
{
\frametitle{Exploiting |App| similarities}
\begin{itemize}
\item Abstraction for parsing |App| structures
\chunkCmdFrameUse{EHParser.1.pApp}
\item |App| building functions:
\chunkCmdFrameUse{EHCommon.1.MkConApp}
\chunkCmdFrameUse{EHCommon.1.mkApp.Base}
\item Parameterized with ``what to build'' (as an algebra)
\end{itemize}
}

%{
%format Top = "Top"
\frame<presentation>[plain]
{
\frametitle{Sidetrack: algebra example}
\begin{itemize}
\item |Expr| variation to show usual use of algebra's
\SafeCode{%
\begin{code}
data Expr  = Con String | App Expr Expr | Top Expr | Int Int

type MkConApp t = (String -> t,t -> t -> t,t -> t,Int -> t)

eCata  :: MkConApp t -> Expr -> t
eCata  alg@(  con  ,  _    ,  _    ,  _    )   (Con     s       )  = con  s
eCata  alg@(  _    ,  app  ,  _    ,  _    )   (App     e1  e2  )  = app  (eCata alg e1)
                                                                          (eCata alg e2)
eCata  alg@(  _    ,  _    ,  top  ,  _    )   (Top     e       )  = top  (eCata alg e)
eCata  alg@(  _    ,  _    ,  _    ,  int  )   (Int     i       )  = int  i

e1 = Top (App (App (Con "+") (Int 2)) (Int 3))
c1 = eCata  (  id                      ,  \v1 v2 -> v1 ++ " " ++ v2
            ,  \v -> "(" ++ v ++ ")"   ,  show
            )
            e1
\end{code}
}
\item |c1| gives @"(+ 2 3)"@
\end{itemize}
}
%}

It is precisely the group of functions specifying what should be done
for |Con|, |App| and |AppTop| which is called an algebra.

Parsing parenthesized constructs deserves some special attention: in the
case of expresssions () stands for a single value, in the case of (expr)
parentheses just tell us how to parse a more complicated expression, and
in the case of (expr, expr, ...) they correspond to the constructor for a
cartesian product. Since this parttern also pops up in the case of types
the parser |pParenProd| which recognises these three alternatives is also 
parameterized with the algebra:

\chunkCmdUse{EHParser.1.pParenProd}

\frame<presentation>[plain]
{
\frametitle{Parsing parenthesized/product expressions}
\begin{itemize}
\item Abstraction for expressions inside @'('@ and @')'@
\chunkCmdFrameUse{EHParser.1.pParenProd}
\chunkCmdFrameUse{EHCommon.1.mkApp.mkConApp}
\chunkCmdFrameUse{EHCommon.1.mkApp.mkProdApp}
\item E.g.
\begin{itemize}
\item |()| gives |Con ",0"|, unit
\item |(e)| gives e, expression itself
\item |(e1,e2)| gives |AppTop (App (App (Con ",2") e1) e2)|, a 2-tuple
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Parsing combinators}
\begin{itemize}
\item Derived parser combinators
\begin{center}
\begin{tabular}{llp{.3\textwidth}}
\ParserCombTableHead
\ParserCombTableB
\end{tabular}
\end{center}
\end{itemize}
}

The definition for |pParenProd| quite literally follows this enumeration of alternatives.
Note that the parser is in a left factorized form
in order to make parsing take linear time.

The parsers for patterns and type expressions also use these abstractions,
for example, the parser for type expressions:

\savecolumns
\chunkCmdUse{EHParser.1.tyExprAlg}
\restorecolumns
\chunkCmdUse{EHParser.1.pTyExprBase}
\restorecolumns
\chunkCmdUse{EHParser.1.pTyExpr}

\frame<presentation>[plain]
{
\frametitle{Parsing type/pattern expressions}
\begin{itemize}
\item Similar to normal expressions
\item Type expressions (for example)
\chunkCmdFrameUse{EHParser.1.tyExprAlg}
\chunkCmdFrameUse{EHParser.1.pTyExprBase}
\chunkCmdFrameUse{EHParser.1.pTyExpr}
\end{itemize}
}

defines a |tyExprAlg| to be used to recognise parenthesized and tupled type expressions.
The parser for |pTyExpr| uses |pChainr| to recognise a list
of more elementary types separated by |->|.

The parser for patterns is, because of its similarity with the previous expression
parsers, given without further explanation:

\savecolumns
\chunkCmdUse{EHParser.1.patExprAlg}
\restorecolumns
\chunkCmdUse{EHParser.1.pPatExpr}
\restorecolumns
\chunkCmdUse{EHParser.1.pPatExprBase}
\restorecolumns
\chunkCmdUse{EHParser.1.pPatExprBase.prod}

Finally, the parsers for the program itself and declarations should have few surprises by now,
except for the use of |pBlock| recognising a list of more elementary parsers
where the offside rule of
Haskell applies.
We will not look at this any further.

\chunkCmdUse{EHParser.1.pDecl}
\chunkCmdUse{EHParser.1.pAGItf}

Once all parsing results are passed to semantic functions we enter the world of attributes
as offered by the AG system.
The machinery for making the compiler actually produce some output is not explained here but
can be found in the sources.
From this point onwards we will forget about that and just look at computations performed on
the abstract syntax tree through the separate views as provided by the AG system.

%endif %% incl01TopicParsing

%if incl01TopicPP

\subsection{Pretty printing: from abstract to concrete}
\label{sec-pretty1}
The first aspect we define is |pp| that constructs the pretty printed representation
for the abstract syntax tree, starting with the definition for |Expr|:

\savecolumns
\chunkCmdUseMark{EHPretty.1.Base.ExprSimple}
\restorecolumns
\chunkCmdUseMark{EHPretty.1.ExprExtra}
\restorecolumns
\chunkCmdUseMark{EHPretty.1.Base.ExprComplex}

\frame<presentation>[t,containsverbatim]
{
\frametitle{A first AG attribution: Pretty printing}
\begin{columns}[onlytextwidth,t]
\begin{column}{.4\textwidth}
\begin{itemize}
\item Input
\SafeCode{%
\begin{code}
%%1srcfile(test/1-all-fail2.eh%%)
\end{code}
}
\end{itemize}
\end{column}
\begin{column}{.6\textwidth}
\begin{itemize}
\item Abstract syntax
\begin{TT}
%%1astfile(test/1-all-fail2.eh%%)
\end{TT}
\end{itemize}
\end{column}
\end{columns}
}

\frame<presentation>[t,containsverbatim]
{
\frametitle{Pretty printing}
\begin{itemize}
\item Output
\begin{TT}
%%1ppfile(test/1-all-fail2.eh%%)
\end{TT}
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: Compose All}
\begin{itemize}
\item Similar to |fold|
\PGFPatComposition
\item Here: a = pp, combine = pretty print combinators
\item (instances of AG patterns are indicated like |AGPat(Compose All)|)
\item Supported by AG's copy rule and |USE| construct
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Pretty printing}
\begin{itemize}
\item AG attribution and attribute definitions
\chunkCmdFrameUse{EHPretty.1.Base.ExprSimple}
\chunkCmdFrameUse{EHPretty.1.ExprExtra}
\item (and more, but left out)
\end{itemize}
}

The |pp| attribute is defined (via |ATTR|)
as a synthesized (because it is preceded by two | || | symbols) attribute for all nonterminals of the abstract syntax.
This output attribute will also be passed to ``the outside world'' (because it is also defined for |AGItf|).
If a rule for |pp| for an alternative of a nonterminal is missing, the default definition
is based on |USE| which provides a default value for alternatives without nonterminals as children (here: |empty|)
and a function used to compose new values based on the values of the children (here: |>-<|).

\begin{Figure}{Pretty printing combinators}{pretty-printing-combinators}
\begin{center}
\begin{tabular}{ll}
Combinator & Result
\\ \hline
|p1 >||< p2| & |p1| besides |p2|, |p2| at the right \\
|p1 `ppBesideSp` p2| & same as |>||<| but with an additional space in between \\
|p1 >-< p2| & |p1| above |p2| \\
|pp_parens p| & |p| inside parentheses \\
|text s| & string |s| as |PP_Doc| \\
|pp x| & pretty print |x| (assuming instance |PP x|) resulting in a |PP_Doc| \\
\end{tabular}
\end{center}
\end{Figure}

The attribute |pp| is defined as a local (to a production alternative)
attribute (by means of the |loc| keyword) so it can be referred to by
other attribute computations.
The copy rules of the AG system automatically insert code to copy the local attribute to
the |pp| synthesized attribute; the definition |lhs.pp = @pp| is inserted by the AG system.

Attributes can be referred to using the notation |@nt.attr|.
|nt| can refer to a child node in the syntax tree (e.g. |decls|) or to
a parent node (by means of keyword |lhs|).
If |nt| is omitted as in |@attr| the |attr| refers to a locally defined attribute (e.g. |lValGam|)
or a value embedded in a node (e.g. |nm|).

The computation of the |pp| attribute is straightforward in the sense that
only a few basic pretty printing combinators are needed
(see \cite{uust04www} and \figRef{pretty-printing-combinators}).
The additional complexity arises from
the encoding for applications.
The problem is that
straightforward printing prints |(2,3)| as |,2 2 3|.
This is solved at the
|AppTop| alternative by gathering all arguments (here: |[2,3]|)
for a function (here: tuple constructor |,2|, or |(,)|) into |appArgPPL|:

\chunkCmdUseMark{EHPretty.1.Expr.ConNm}

The attributes |appArgPPL| and |appFunNm| only are relevant for the |Con| and |App| alternatives.
For the remaining alternatives a default definition is provided.
The AG system allows to refer to all alternatives by means of a set notation where `|*|' means all alternatives and
`|-|' means set subtraction.
Juxtapositioning of alternatives means set union.
Hence |* - Con App| stands for all alternatives except |Con| and |App|.
The use of this notation is convenient as it relieves us from the obligation to specify default values for
new alternatives that will be added in the future.

The |appArgPPL| and the two other required attributes |appFunNm| for the name and
|appFunPP| for the pretty printing of the applied function
are all used to gather information from deeper within the syntax tree
for further processing higher up in the tree by |ppAppTop|:

\chunkCmdUseMark{EHCommon.1.PP.ppAppTop}
\chunkCmdUseMark{EHCommon.1.PP.NeededByExpr}

The value for |pp| defined in |App| is not directly used for the pretty printed output
as result of the definition of |pp| at |AppTop|.
However, because the |pp| defined in |App| more closely resembles the structure of
the syntax tree it is used for producing error messages
%if incl01TopicErr
(see section~\ref{sec-error1})%
%endif
.

We will not look into the pretty printing for
patterns and type expressions because it is almost an exact replica of the code for expressions.
The remainder of the |pp| related attribute computations is also omitted as
they are rather straightforward uses of |>||<| and |>-<|.

%endif %% incl01TopicPP

\subsection{Types}

We will now turn our attention to the way the type system is incorporated into EH1.
We focus on the pragmatics of the implementation and less on the corresponding type theory.

\TBD{better intro here}

\subsubsection{What is a type}

Compiler builders consider a \IxAsDef{type} to be a description of the interpretation of a value
whereas a value is to be understood as a bitpattern.
This means that machine operations such as integer addition,
are only applied to patterns that are to be interpreted as integers.
More generally, we want to prevent unintended interpretations of bitpatterns,
which might
likely lead to the crash of a program.

The flow of values, that is, the copying between memory locations,
through the execution of a program may
only be such that a copy is allowed only if the corresponding types
relate to each other in some proper fashion.
A compiler uses a type system to analyse this flow and to make sure that built-in functions are only
applied to patterns that they are intended to work on.
The idea is
that if a compiler cannot find an erroneous flow of values, with the notion of
erroneous defined by the type system, the program is guaranteed not to crash
because of unintended use of bitpatterns.

\mode<all>{%
\def\EHCOneTyLangA{%
\begin{code}
sigma  =  Int | Char
       |  (sigma,...,sigma)
       |  sigma -> sigma
\end{code}
}
\def\EHCOneTyLangB{%
\begin{code}
sigma  =  Int | Char | ^^ -> ^^ | ^^ , ^^ | ^^ ,, ^^ | ^^ ...
       |  sigma ^^ sigma
\end{code}
}
}
\frame<presentation>
{
\frametitle{Type system}
\begin{itemize}
\item Type: description of the interpretation of a bitpattern
\item Type system: types + rules how types may be combined/used
\item Compiler uses a type system to guarantee proper flow of bitpatterns
\begin{itemize}
\item proper: not leading to crash
\end{itemize}
\item Compiler uses 
\begin{itemize}
\item type information explicitly specified by programmer
\item type information implicitly inferred/reconstructed from
(non type related) program text
\end{itemize}
\item Types are described by type language, here:
\SafeCode{\EHCOneTyLangA}
\end{itemize}
}

In this section we start by introducing a type language in a more formal setting 
as well as a more practical setting.
The formal setting uses
typing rules to specify the static semantics of EH whereas in the practical setting the AG system is used, providing an implementation.
In the following section we discuss the typing rules, the mechanism for enforcing the equality of types (called \IxAsDef{fitting})
and the checking itself.
Types will be introduced informally,
instead of taking a more formal approach
\cite{fp:type-theory:func-prog,wadler89theorems-for-free,typing:types-prog-lang:pierce,ipt:theory-of-objects}.

Types are described by a type language.
The type language for EH1
allows some basic types and two forms of composite types, functions and tuples,
and is described by the following grammar:
\EHCOneTyLangA
\frame<presentation>
{
\frametitle{Type language}
\begin{itemize}
\item .. or more general, alike the implementation
\SafeCode{\EHCOneTyLangB}
\item AG type representation
\chunkCmdFrameUse{EHTyAbsSyn.1}
\item |Any| (also denoted by |ANY|) encodes ``don't know'' type
\end{itemize}
}
The following definition however is closer to the one used in our implementation:

\EHCOneTyLangB

The latter definition also introduces the possibility of describing types like |Int Int|.
We nevertheless use this one since it is 
used in the implementation of later versions
of EH where it will prove useful in expressing the application of type constructors to types.
Here we just have to make sure no types like |Int Int| will be created;
in a (omitted) later version of EH we perform kind inferencing/checking to prevent the creation of such types from showing up.

The corresponding encoding using AG notation differs in the
presence of an |Any| type, also denoted by |ANY|.
In \secRef{sec-check-type} we will say more about this.
It is used to smoothen the type checking by (e.g.) limiting the propagation of
erroneous types:

\savecolumns
\chunkCmdUseMark{EHTyAbsSyn.1.TyAGItf}
\restorecolumns
\chunkCmdUseMark{EHTyAbsSyn.1.Ty}

\frame<presentation>[containsverbatim]
{
\frametitle{Type language}
\begin{itemize}
\item Haskell data type generated by AG compiler
\begin{TT}
-- Ty ----------------------------
data Ty = Ty_Any 
        | Ty_App (Ty) (Ty)
        | Ty_Con (String)
        deriving ( Eq,Show)
-- TyAGItf -----------------------
data TyAGItf = TyAGItf_AGItf (Ty)
             deriving ( Eq,Show)
\end{TT}
\item
Allows manipulation as Haskell datastructures
\item
AG computations via semantic functions
\end{itemize}
}

The formal system and implementation of this system use different symbols to refer to the same concept.
For example, |Any| in the implementation is the same as |ANY| in the typing rules.
Not always is such a similarity pointed out explicitly but instead a notation |name1 =@= name2|
is used to simultaneously refer to both symbols |name1| and |name2|,
for example |Any =@= ANY|.
The notation also implies that the identifiers and symbols separated by '|=@=|' are referring
to the same concept.

The definition of |Ty| will be used in both the Haskell world and the AG world.
In Haskell we use the corresponding |data| type generated by the AG compiler,
for example in the derived type |TyL|:

\chunkCmdUseMark{EHTy.1.TyL}

The data type is used to construct type representations.
In the AG world we define computations over the type structure in terms of attributes.
The corresponding semantic functions generated by the AG system can then be applied to Haskell values.

%if incl01TopicPP

\subsubsection{Type pretty printing}
As before, |TyAGItf| is the place where interfacing with the Haskell world takes place.
The AG system also introduces proper data types to be used in the Haskell world and the following
data type is generated by the AG system:
\begin{TT}
-- Ty ----------------------------
data Ty = Ty_Any 
        | Ty_App (Ty) (Ty)
        | Ty_Con (String)
        deriving ( Eq,Show)
-- TyAGItf -----------------------
data TyAGItf = TyAGItf_AGItf (Ty)
             deriving ( Eq,Show)
\end{TT}
This allows us to create types as plain Haskell values, and to define functionality
in the AG world which uses types represented by Haskell data type values.
As a demonstration of how this is done, pretty printing is used once again.
Using the same design and tools as used for pretty printing |Expr| (and pattern/type expressions)
abstract syntax we can define pretty printing for a type:

\chunkCmdUseMark{EHTyPretty.1.pp}

Previously, |AppTop| was used to remember where in the abstract syntax tree (of EH) the
top of an application could be found.
Here we need an additional local attribute |isSpineRoot| holding the boolean value telling us whether we are
at the top of the spine of |App|'s.
It uses its position |appSpinePos| in the spine to determine if this is the case:

\savecolumns
\chunkCmdUseMark{EHTyCommonAG.1.ConNm}
\restorecolumns
\chunkCmdUseMark{EHTyCommonAG.1.ConNm.ConApp}
\restorecolumns
\chunkCmdUseMark{EHTyCommonAG.1.ConNm.Ty}
\chunkCmdUseMark{EHTyCommonAG.1.appSpinePos}

\paragraph{Parenthesis.}
Because no explicit |Parens| alternative is present in the type structure,
appropriate places to insert parenthesis have to be computed.
This is solved by passing the need for parentheses
as contextual information using the inherited attribute |parNeed|.
However, as this part is not necessary for understanding
the implementation of a type system its reading can safely be
skipped.

\chunkCmdUseMark{EHTyPretty.1.ParNeed}

And supporting Haskell definitions:

\chunkCmdUseMark{EHCommon.1.ParNeed}

The idea here is that a |Ty| and its context together determine whether parentheses are needed.
This is encoded in |ParNeed|, the contextual need is passed to |ppParNeed| as the parameter |globNeed|
and the local need as |locNeed|.
In |ppParNeed| parentheses are added if the contextual need is higher than the local need.
For example, at |AppTop| the printing of a tuple always adds parentheses via function |ppAppTop|,
so no additional parentheses are needed.
This is reflected in the result of |parNeedApp| which gives us the following information back in the order of the elements of
the returned tuple:
\begin{itemize}
\item The local need for parentheses, that is `here' in the alternative.
\item The need for parenthesis for the arguments of a type.
\end{itemize}

The structure of the solution, that is, gather information bottom to top (e.g. |appArgPPL|) and use it higher up in the tree,
is a recurring pattern.
The other way around, for example |parNeedL| distributing the need for parentheses over the
arguments of an application, is also a recurring pattern.

\paragraph{Tying in with the Haskell world.}
What remains, is the way the functionality is made available to the Haskell world:

\chunkCmdUseMark{EHTyPretty.1.ppTy}

To make this work, we need to tell the AG system to generate a wrapper function
and datatypes |Inh_TyAGItf| and |Syn_TyAGItf| with selector functions for
defined attributes to pass inherited and synthesized attributes respectively:
\TBD{Update AG manual, pg 21 w.r.t. wrap}

\chunkCmdUseMark{EHTyPretty.1.WRAPPER}

In the code that is generated by the AG system
inherited attributes become function arguments and
synthesized attributes components of a cartesian
product; in both cases they are identified by position.
This makes interfacing to the Haskell world cumbersome.
In order to overcome these problems so-called wrapper
functions can be generated, which make it possible to
access result e.g. by using functions generated
according to a fixed naming convention. An example of
such a fuction is |pp_Syn_TyAGItf|, which, when applied to |t|,
accesses the
synthesized |pp| attribute at an attributed non-terminal
|t| of type |TyAGItf|.

%endif %% incl01TopicPP


\subsection{Checking types}
\label{sec-check-type}
The type system of a programming language is described by typing rules.
A \IxAsDef{typing rule}
\begin{itemize}
\item
Relates language constructs to types.
\item
Constrains the types of these language constructs.
\end{itemize}

\subsubsection{Type rules}
For example, the following is the typing rule
(taken from \figRef{rules.expr1A})
for function application
\[
\rulerCmdUse{rules.expr1A.e-app1}
\]
It states that an application of |e1| to |e2| has type |sigma|
provided that the argument has type |sigmaa| and
the function has a type |sigmaa -> sigma|.

\rulerCmdUse{rules.expr1A}

All rules we will use are of the form
\[
\infrule{rule-name}{prerequisite_1 \\ prerequisite_2 \\ ...}{consequence}
\]
with the meaning that if all $prerequisite_i$ can be proven we may conclude the $consequence$.

A |prerequisite| can take the form of any logical predicate or has 
a more structured form, usually called a \IxAsDef{judgement}:
\[
|context |\stackrel{|judgetype|}{|:-|}| construct : property ~> more ^^ results |
\]
The part ``|~> more ^^ results|'' needs not always be present if there are no more results for
a judgement.
The notation reads as
\begin{quote}
In the interpretation |judgetype| the |construct| has property |property| assuming
|context| and with optional additional |more ^^ results|.
\end{quote}

If the |context| or |more ^^ results| itself consists of multiple parts, these parts are separated by
a semicolon '|;|'.
An underscore '|_|' has a similar role as in Haskell to indicate a property is not relevant for a type rule
(see \ruleRef{e-app1B}, \figRef{rules.expr1B})

Although a rule formally is to be interpreted purely equational, it may help to realise
that from an implementors point of view this (more or less)
corresponds to an implementation template, either in the form of a function |judgetype|:

\begin{code}
judgetype =  \construct ->
             \context -> ... (property,more_results)
\end{code}
or a piece of AG:
\begin{code}
ATTR judgetype [  context: ... | |
                  property: ...  more_results: ... ]

SEM judgetype
  |  construct
       lhs.(property,more_results) = ... @lhs.context ...
\end{code}
Typing rules and implementation templates
differ in that the latter prescribes the order in which the computation of
a property takes place, whereas the former simply postulates
relationships between parts of a rule.
In general typing rules presented
throughout \thispaper\ will be rather explicit in the flow of information
and thus be close to the actual implementation.

\frame<presentation>
{
\frametitle{Checking types}
\begin{itemize}
\item Proper use of values is described by type rules
\item E.g. type rule for function application:
\[
\rulerCmdUse{rules.expr1A.e-app1}
\]
\item Expected function argument and given argument are constrained to be equal
\end{itemize}
}

\frame<presentation>
{
\frametitle{Rules and judgements}
\begin{itemize}
\item Form of a rule:
\[
\infrule{rule-name}{prerequisite_1 \\ prerequisite_2 \\ ...}{consequence}
\]
\item where |prerequisite| and |consequence| can be
\begin{itemize}
\item any predicate
\item or a more specific predicate called |judgement|:
\[
|context |\stackrel{|judgetype|}{|:-|}| construct : property ~> more ^^ results |
\]
\end{itemize}
\item Rules are used in logic(s) to reason about properties described by judgements
\item Here
\begin{itemize}
\item rules are used to describe what proper use of values means
\item by constraining types of values
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Rules and judgements implementation}
\begin{itemize}
\item Rules partially describe an implementation
\begin{itemize}
\item Haskell:
\SafeCode{
\begin{code}
judgetype =  \construct ->
             \context -> ... (property,more_results)
\end{code}
}
\item AG:
\SafeCode{
\begin{code}
ATTR judgetype [  context: ... | |
                  property: ...  more_results: ... ]

SEM judgetype
  |  construct
       lhs.(property,more_results) = ... @lhs.context ...
\end{code}
}
\end{itemize}
\item Implementation in addition describes order in which constraints are to be enforced
\end{itemize}
}

\frame<presentation>
{
\frametitle{Rules and judgements implementation}
\begin{itemize}
\item Rule structure correspond almost 1-1 to AG structure
\item For example, rule for application of expression:
\[
\rulerCmdUse{rules.expr1A.e-app1}
\]
\item corresponding AG structure:
\SafeCode{
\begin{code}
DATA Expr
  | App           func          : Expr
                  arg           : Expr
\end{code}
}
\item where |e1 = func, e2 = arg|
\end{itemize}
}

\frame<presentation>
{
\frametitle{Rules and judgements implementation}
\begin{itemize}
\item Rule has
\begin{itemize}
\item<1-> prerequisite judgements about properties for each child in abstract syntax tree
\item<2-> and consequence judgement about properties for composition of children
\item<3-> \alert<3>{constraints} for properties
\end{itemize}
\item AG implementation has
\begin{itemize}
\item<1-> attributes for properties for each child in abstract syntax tree
\item<2-> attributes for properties for composition of children
\item<3-> attribute definitions specifying how these attributes are \alert<3>{computed}
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Rules and judgements implementation}
\begin{itemize}
\item Rule:
\[
\rulerCmdUse{rules.expr1A.e-app1}
\]
\item AG:
\chunkCmdFrameUse{EHInferExpr.1.App}
\item (ignoring details, for now)
\end{itemize}
}

\subsubsection{Environment}
The rules in \figRef{rules.expr1A} refer to |Gamma|,
which is often called \IxAsDef{assumptions}, \IxAsDef{environment} or
\IxAsDef{context} because it provides information about what may
be assumed about identifiers.
Identifiers |ident| are distinguished on the case of the first character,
capitalized |I|'s starting with an uppercase, uncapitalized |i|'s otherwise
\begin{code}
ident  =  identv
       |  identc
\end{code}
For type constants we will use capitalized identifiers |identc|,
whereas for identifiers bound to an expression in a |let|-expression
we will use lower case identifiers |(identv, j, ...)|.

An environment |Gamma| is a vector of bindings, a partial finite map from identifiers to types:
\begin{code}
Gamma = Vec(ident :-> sigma)
\end{code}

Concatenation of such collections as well as scrutinizing a collection is denoted with a comma '|,|'.
For example, `|identv :-> sigma, Gamma|' represents a concatenation as well as a pattern match.
For rules this does not make a difference, for the implementation there is a direction involved as we either construct
from smaller parts or deconstruct (pattern match) into smaller parts.

If shadowing is involved, that is duplicate entries are added, left/first (w.r.t. to the comma '|,|') entries shadow right/later entries.
When we locate some variable in a |Gamma| the first occurrence will be taken.

If convenient we will also use a list notation:
\begin{code}
Gamma = [ident :-> sigma]
\end{code}
This will be done if specific properties of a list are used or if we borrow from Haskell's repertoire of list functions.
For simplicity we also use (assocation) lists in our implementation.
% In later versions of EH |FiniteMap|'s will be used instead for maps like this.

A list structure suffices to encode the presence of an identifier in a |Gamma|, but it
cannot be used to detect multiple occurrences caused by duplicate introductions.
Thus in our implementation we use a stack of lists instead:

\chunkCmdUseMark{EHCommon.1.AssocL}
\chunkCmdUseMark{EHGam.1.Base.type}
\chunkCmdUseMark{EHGam.1.Base.sigs}
\chunkCmdUseMark{EHGam.1.Base.funs}

Entering and leaving a scope is implemented by means of pushing and popping a |Gamma|.
Extending an environment |Gamma| will take place on the top of the stack only.
A |gamUnit| used as an infix operator will print as |`gamUnit`|.

A specialization |ValGam| of |Gam| is used to store and lookup the type of value identifiers.

\chunkCmdUseMark{EHGam.1.ValGam.Base}

The type is wrapped in a |ValGamInfo|.
Later versions of EH can add additional fields to this data type.

\chunkCmdUseMark{EHGam.1.valGamLookup}
\chunkCmdUseMark{EHGam.1.valGamLookupTy}

Later the variant |valGamLookup| will do additional work, but for now it
does not differ from |gamLookup|.
The additional variant |valGamLookupTy| is specialized further to produce
an error message in case the identifier is missing from the environment.

\subsubsection{Checking Expr}

The rules in \figRef{rules.expr1A} do not provide much information about how
the type |sigma| in the consequence of a rule
is to be computed; it is just stated that it should relate in some way
to other types.
However, type information can be made available to parts of the abstract syntax tree, either
because the programmer has supplied it somewhere or because the compiler can reconstruct it.
For types given by a programmer the compiler has to check if such a type correctly
describes the value of an expression for which the type is given.
This is called \IxAsDef{type checking}.
If no type information has been given for a value,
the compiler needs
to reconstruct or infer this type based on the structure of the abstract syntax
tree and the semantics of the language as defined by the typing rules.
This is called \IxAsDef{type inferencing}.
In EH1 we exclusively deal with type checking.

\rulerCmdUse{rules.expr1B}

We now can tailor the type rules in \figRef{rules.expr1A} towards an implementation
which performs type checking, in \figRef{rules.expr1B}.
We also start with the discussion of
the corresponding AG implementation.
The rules now take an additional context, the expected (or known) type |sigmak|
(attribute |knTy|, simultaneously referred to by |sigmak =@= knTy|)
as specified by the programmer, defined in terms of AG as follows:

\chunkCmdUseMark{EHInferExpr.1.knTy}

%{
%format lhs = "lhs"
The basic idea underlying this implementation for type checking, as well as in later versions of EH also
for type inferencing, is that
\begin{itemize}
\item
A \IxAsDef{known} (or \IxAsDef{expected})
type |sigmak =@= knTy| is passed top-down through the syntax tree of an expression,
representing the maximal type (in terms of |<=|, see \figRef{rules.fit1} and discussion below) the type of an expression can be.
At all places where this expression is used it also is assumed that the type of this expression equals |sigmak|.
\item
A result type |sigma =@= ty| is computed bottom-up for each expression,
representing the minimal type (in terms of |<=|) the expression can have.
\item
At each node in the abstract syntax tree it is checked whether |sigma <= sigmak| holds.
The result of |lhs <= rhs| is |rhs| which is subsequently used by the type checker,
for example to simply return or use in constructing another, usually composite, type.
\item
In general, for |lhs <= rhs| the |rhs| is an expected type whereas |lhs| is the bottom-up computed result type.
\end{itemize}
%}

\rulerCmdUse{rules.fit1}

An additional judgement type named |fit| (\figRef{rules.fit1}) is needed to check an actual type against an expected (known) type.
The judgement specifies the matching |sigma1 <= sigma2| of two types |sigma1| and |sigma2|.
The meaning of |<=| is that the left hand side (lhs) type |sigma1| of |<=| can be used where the right hand side (rhs)
type |sigma2| is expected.
Expressed differently, |<=| checks whether a value of type |sigma1| can flow (that is, be stored) into a memory location
of type |sigma2|.
This is an asymmetric relation because ``a value flowing into a location'' does not imply that it can flow
the other way,
so |<=| conceptually has a direction, even though in the
rules in \figRef{rules.fit1} |<=| is a test on equality of the two type arguments.

The rules for |<=| also specify a result type.
Strictly this result is not required for the |fit| judgement to hold but in the implementation it is convenient
to have the implementation |fitsIn| of |<=| return the smallest type |sigma| for which of |sigma1 <= sigma| and |sigma2 <= sigma| hold.
This is useful in relation to the use of |ANY| in
in \ruleRef{f-anyl1} and \ruleRef{f-anyr1}; we will come back to this later. 

For example, |<=| is used in \ruleRef{e-int1B} which checks that its actual |Int| type matches the
known type |sigmak|.
The implementation of the type \ruleRef{e-int1B} performs this check and returns the type |sigma| in attribute |ty|:

\chunkCmdUseMark{EHInferExpr.1.Const}

\begin{AGFeature}{ag-set-notation}{Set notation for variants}
The rule for (e.g.) attribute |fo| is specified for |IConst| and |CConst| together.
Instead of specifying only one variant a whitespace separated list of variant names
may be specified after the vertical bar '| || |'.
It is also allowed to specify this list relative to all declared variants by specifying for which
variants the rule should \emph{not} be declared.
For example: |* - IConst CConst| if the rule was to be defined for all variants except |IConst| and |CConst|.
\end{AGFeature}

\begin{AGFeature}{ag-loc-attr}{Local attributes}
The attribute |fTy| is declared locally.
In this context `local' means that the scope is limited to the variant of a node.
Attribute |fTy| defined for variant |IConst| is available only for other attribute
rules for variant |IConst| of |Expr|.

Note that no explicit rule for synthesized attribute |ty| is required;
a copy rule is inserted to use the value of the locally declared attribute |ty|.
This is a common AG idiom when a value is required for later use as well or
needs to be redefined in later versions of EH.
\end{AGFeature}

Some additional constants representing built-in types are also required:

\chunkCmdUseMark{EHTy.1.tyConsts}

The local attribute |fTy| (by convention) holds the type
as computed on the basis of the abstract syntax tree.
This type |fTy| is subsequently compared to the expected type |lhs.knTy|
via the implementation |fitsIn| of the rules for |fit =@= <=|.
In infix notation |fitsIn| prints as |<=|.
The function |fitsIn| returns a |FIOut| (\textbf{f}its\textbf{I}n \textbf{out}put) data structure in attribute |fo|.
|FIOut| consists of a record containing amongst other things field |foTy|:

\chunkCmdUseMark{EHTyFitsInCommon.1.FIOut}
\chunkCmdUseMark{EHTyFitsInCommon.1.foHasErrs}

Using a separate attribute |fTy| instead of using its value directly has been
done in order to prepare for a redefinition of |fTy| in later versions\footnote{This will happen with other attributes as well.}.

|Ty_Any =@= Any =@= ANY| plays a special role.
This type appears at two places in the implementation of the type system
as a solution to the following problems:

\begin{itemize}
\item
Invariant to our implementation is the top-down passing of an expected type.
However, this type is not always fully known in a top-down order.
For example, in \ruleRef{e-app1B} (\figRef{rules.expr1B}) the argument of the expected function type
|ANY -> sigmak| is not known because this information is only available from the environment |Gamma| which is
used further down in the AST via \ruleRef{e-ident1B}.
In this use of |ANY| it represents a ``dont't know'' of the type system implementation.
As such |ANY| has the role of a type variable (as introduced for type inferencing in \secRef{ehc2}).
\item
An error occurs at a place where the implementation of the type system needs a type to continue (type checking) with.
In that case |ANY| is used to prevent further errors from occurring.
In this use of |ANY| it represents a ``dont't care'' of the type system implementation.
As such |ANY| will be replaced by more a more specific type as soon as it matches (via |<=|) such a type.
\end{itemize}

In both cases |ANY| is a type exclusively used by the implementation to smoothen type checking.
The rules for |<=| for |ANY| in \figRef{rules.fit1} state that |ANY| is equal to any type.
The effect is that the result of |<=| is a more specific type.
This suits our ``dont't know'' and ``dont't care'' use.
Later, when discussing the AG implementation for these rules this issue reappears.
In later EH versions we will split the use of |ANY| into the proper use of a type lattice,
and will it thus disappear.

The role of |ANY| may appear to be similar to |Top| and |Bot| known from type theory.
However, |ANY| is used only as a mechanism for the type system implementation.
It is not offered as a feature to the user (i.e. the EH programmer) of the type system.


|Ty_Any =@= Any =@= ANY| is also used at the top level where the actual expected type of the expression neither is
specified nor matters
because it is not used:

\chunkCmdUseMark{EHInferExpr.1.knTy.AGItf}

The \ruleRef{f-arrow1} in \figRef{rules.fit1} for comparing function types compares the
types for arguments in the opposite direction.
Only in
%if storyAFP04Notes
later versions of EH
%else
\secPageRef{ehc4-fitsin}
%endif
when |<=| really behaves asymmetrically we will discuss this aspect
of the rules which is named \IxAsDef{contravariance}.
In the rules in \figRef{rules.fit1} the direction makes no difference;
the correct use of the direction for now only anticipates issues yet to come.

\frame<presentation>
{
\frametitle{Type checking for expressions}
\begin{itemize}
\item Structure of judgements
\[
\rulerCmdUse{rules.expr1B.scheme}
\]
\item Structure/attribution of AG
\PGFExprIBScheme
\begin{itemize}
\item Environment |Gamma =@= valGam| holds assumptions (types) about value identifiers
\item Expected/known/top-down type |sigmak =@= knTy| together with bottom-up type |sigma =@= ty| from child expression
compute type of expression,
\item but: here mostly known type is used: all types are explicitly specified
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: DeCompose}
\begin{itemize}
\item |knTy| follows pattern, similar to |unfold|
\PGFPatDeComposition
\item where decomposition follows tree structure
\SafeCode{%
\begin{code}
decombine1 :: A -> (A,A,A)
decombine2 :: A -> (A,A)
\end{code}
}
\item if decomposition fails: pass ``will do'' value
\begin{itemize}
\item here: |Ty_Any|
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Fitting known type and type}
\begin{itemize}
\item According to rule
\[
\rulerCmdUse{rules.expr1B.e-int1B}
\]
\item |ty|
\chunkCmdFrameUse{EHInferExpr.1.Const}
\item must fit in |knTy|
\chunkCmdFrameUse{EHInferExpr.1.knTy}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Fitting}
\begin{itemize}
\item Fitting constraint |`fitsIn`| is specified by rules of the form
\[
\rulerCmdUse{rules.fit1.scheme}
\]
\item And implemented by |fitsIn| which returns
\chunkCmdFrameUse{EHTyFitsInCommon.1.FIOut}
\begin{itemize}
\item a type to continue checking with
\item errors (if any) to incorporate in pretty printed program
\end{itemize}
\item Result |sigma|
\begin{itemize}
\item Is in principle unnecessary (we are just checking)
\item But need to know the `maximal' type for |sigmal <= ANY| to continue type checking with
\item Is already a bit of type inferencing
\end{itemize}
\end{itemize}
}

The Haskell counterpart of |jfit sigma1 <= sigma2 : sigma|
is implemented by |fitsIn|:

\savecolumns
\chunkCmdUseMark{EHTyFitsIn.1.fitsIn.Base}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.1.fitsIn.AppRest}

\frame<presentation>
{
\frametitle{fitsIn}
\begin{itemize}
\item (E.g.) rule
\[
\rulerCmdUse{rules.fit1.f-con1}
\]
\item Where |I| denotes a constructor identifier
\item Is implemented by
\chunkCmdFrameUse{EHTyFitsIn.1.fitsIn.Base}
\end{itemize}
}

\frame<presentation>
{
\frametitle{fitsIn}
\begin{itemize}
\item Product type
\[
\rulerCmdUse{rules.fit1.f-prod1}
\]
\item Function type
\[
\rulerCmdUse{rules.fit1.f-arrow1}
\]
\item Function arguments are fitted other way around
\begin{itemize}
\item known as |contravariance|
\item here: irrelevant (|<=| is symmetric)
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{fitsIn}
\begin{itemize}
\item ... and its implementation
\chunkCmdFrameUse{EHTyFitsIn.1.fitsIn.AppRest}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Known type and type: identifiers}
\begin{itemize}
\item Type of identifier is retrieved from |Gamma|
\[
\rulerCmdUse{rules.expr1B.e-ident1B}
\]
\item AG:
\chunkCmdFrameUse{EHInferExpr.1.Var}
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: Distribution}
\begin{itemize}
\item |valGam| is distributed
\item Special case of |AGPat(DeCompose)| pattern.
\PGFPatDistribute
\item Passing unmodified, done by AG's copy rules
\item Distribution ``as is'', tree structure is not used for steering the decomposition
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Known type and type: products}
\begin{itemize}
\item Rule
\[
\rulerCmdUse{rules.expr1B.e-prod1B}
\]
\item Constructor for tuple is function
\chunkCmdFrameUse{EHInferExpr.1.Con}
\item Extracted from |knTy|
\item For example
\begin{itemize}
\item |@lhs.knTy = ANY -> ANY -> (t1,t2)|
\item |resTy = (t1,t2)|
\item |@loc.ty = t1 -> t2 -> (t1,t2)|
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Type decomposing utilities}
\begin{itemize}
\item Decompose into constituents
\chunkCmdFrameUse{EHTy.1.unMkTy.sigs.tyArrow}
\item In case of failure, return ``will do'' values:
\begin{itemize}
\item |Ty_Any| (for |Ty|)
\item |[]| (for |TyL|)
\end{itemize}
\end{itemize}
}


The function |fitsIn| checks whether the |Ty_App| structure
and all type constants |Ty_Con| are equal.
If not, a non-empty list of errors is returned as well as type |Ty_Any =@= Any =@= ANY|.
Matching a composite type is split in two cases for |Ty_App|, one for function types (the first case),
and one for the remaining type applications
(the second case).
For the current EH version the second case only concerns tuple types.
Both matches for composite types use |comp| wich performs multiple |<=|'s and combines the results.
The difference lies in the treatment of contravariant behavior as discussed earlier.

The type rules leave in the open how to handle a situation when a required
constraint is broken.
For a compiler this is not good enough, being the reason |fitsIn| gives a ``will-do'' type
|Ty_Any| back together with an error for later processing%
%if incl01TopicErr
 (in section~\ref{sec-error1})%
%endif
.
Errors themselves are also described via AG:

\chunkCmdUseMark{EHErrorAbsSyn.1.UnifyClash}
\chunkCmdUseMark{EHErrorAbsSyn.1.NamesNotIntrod}

The |Err| datatype is available as a datatype in the same way |Ty| is.
The error datatype is also used for signalling undeclared identifiers: 

\chunkCmdUseMark{EHInferExpr.1.Var}

%{

%format < 		= "{\langle}"
%format > 		= "{\rangle}"
\begin{AGFeature}{ag-lhs-pat}{Left hand side patterns}
The simplest way to define a value for an attribute is to define one value for one attribute
at a time.
However, if this value is a tuple, its fields are to be extracted and assigned to individual attributes
(as in |tyArrowArgRes|).
AG allows a pattern notation of the form(s) to make the notation for this situation more concise:
\begin{code}
| <variant>  ^^^^  <node> . ( <attr1>   , <attr2>            , ...  )  = 
| <variant>  ^^^^  ( <node1> . <attr1>  , <node1> . <attr2>  , ...  )  = 
\end{code}
\end{AGFeature}
%}

Again, the error condition is signalled by a non empty list of errors
if a lookup in |Gamma| fails.
%if incl01TopicErr
Later, in \secRef{sec-error1}, these
%else
These
%endif
errors are gathered so they can be incorporated into an annotated pretty printed
version of the program.


Typing \ruleRef{e-ident1B} uses the environment |Gamma| to retrieve the type
of an identifier.
This environment |valGam| for types of
identifiers simply is declared as an inherited attribute,
initialized at the top of the abstrcat syntax tree.
It is only extended with new bindings for identifiers at a declaration of an identifier.

\chunkCmdUseMark{EHInfer.1.valGam}

One may wonder why the judgement |jfit sigma1 <= sigma2 : sigma|
and its implementation |fitsIn| returns a type at all;
the idea of checking was to only pass explicit type information |sigmak| (or |knTy|)
from the top of the abstract syntax tree to the leaf nodes.
Note that this idea breaks when we try to check the expression |id 3| in

\begin{code}
let  id :: Int -> Int
     id = \x -> x
 in  id 3
\end{code}

What is the |knTy| against which |3| will be checked?
It is the argument type of the type of |id|.
However, in \ruleRef{e-app1B} and its AG implementation,
the type of |id| is not the (top-to-bottom travelling) |sigmak =@= knTy|, but it
will be the argument part of the (bottom-to-top travelling) resulting function type of
%{
%format e1
|e1 =@= func.ty|:
%}

\chunkCmdUseMark{EHInferExpr.1.App}

The idea here is to encode the partially
known function type as |ANY -> sigmak| (passed to |fun.knTy|)  and let
|fitsIn| fill in the missing details, that is to find a type for |ANY|.
This is the place where it is convenient to have |fitsIn| return a type in which
|ANY =@= Ty_Any|'s are replaced by a more concrete type.
From that result the known/expected type of the argument can be extracted.

Note that we are already performing a little bit of type inferencing.
This is however only done locally to |App| as the |ANY| in
|ANY -> sigmak| is guaranteed to have disappeared in the result type of |fitsIn|.
If this is not the case, the EH program contains an error.
This is a mechanism we repeatedly use, so we summarize it here:

\begin{itemize}
\item
Generally, the semantics of the language requires a type |sigma| to be of a specific form.
Here |sigma| equals the type of the function (not known at the |App| location in the AST)
which should have the form |ANY -> sigmak|.
\item
The specific form may contain types about which we know nothing, here encoded by
|ANY|, in later EH versions by type variables.
\item
|fitsIn =@= <=| is used to enforce |sigma| to have the right form.
Here this is done by pushing the form as |sigmak| down the AST for the function (attribute |func.knTy|).
The check |sigma `fitsIn` sigmak| is then performed in the |Var| variant of |Expr|.
\item
Enforcing may or may not succeed.
In the latter case error messages are generated and the result of
enforcing is |ANY|.
\end{itemize}

The type construction and inspection done in the |App| variant of |Expr|
requires some additional type construction functions, of which we only include |mkTyArrow|:

\chunkCmdUseMark{EHTy.1.mkTyArrow}

The function is derived from a more general function |mkArrow|:

\chunkCmdUseMark{EHCommon.1.MkConApp}
\chunkCmdUseMark{EHCommon.1.mkApp.mkArrow}
\chunkCmdUseMark{EHCommon.1.mkApp.Base}

A |MkConApp| contains four functions, for constructing a value similar to |Con|, |App|, |AppTop| and |IConst| respectively.
These functions are used by |mkApp| to build an |App| like structure and by |mkArrow| to build function like structures.
The code for (e.g.) parsers (omitted from \thispaper),
uses these functions parameterized with the proper four semantics functions as generated by the AG system.
So this additional layer of abstraction improves code reuse.
Similarly, function |mkTyProdApp| constructs a tuple type out of types for the elements.

The functions used for scrutinizing a type are given names in which (by convention)
the following is encoded:

\begin{itemize}
\item
What is scrutinized.
\item
What is the result of scrutinizing.
\end{itemize}

For example, |tyArrowArgRes| dissects a function type into its argument and result type.
If the scrutinized type is not a function, ``will do'' values are returned:

\chunkCmdUseMark{EHTy.1.unMkTy.sigs.tyArrow}
\chunkCmdUseMark{EHTy.1.unMkTy.tyArrowArgRes}

Similarly |tyProdArgs| is defined to return the types of the elements of a tuple type.
The code for this and other similar functions have been omitted for brevity.

\paragraph{Constructor |Con|, tuples.}

Apart from constructing function types only tupling allows us
to build composite types.
The \ruleRef{e-prod1B} for tupling
has no immediate counterpart in the implementation
because a tuple |(a,b)| is encoded as the application |(,) a b|.
The alternative |Con| takes care of producing a type |a -> b -> (a,b)| for |(,)|.

\chunkCmdUseMark{EHInferExpr.1.Con}

This type can be constructed from |knTy| which by definition
has the form |ANY -> ANY -> (a,b)| (for this example).
The result type of this function type is taken apart and used to produce
the desired type.
Also by definition (via construction by the parser) we know the arity is correct.

Note that, despite the fact that the cartesian product constructors are
essentially polymorphic, we do not have to do any kind of unification
here, since they either appear in the right hand side of declaration
where the type is given by an explcit type declaration, or they occur at
an argument position where the type has been implicitly specified by the
function type.
Therefore we indeed can use the |a| and |b| from type |ANY -> ANY -> (a,b)|
to construct the type |a -> b -> (a,b)| for the constructor |(,)|.

\frame<presentation>
{
\frametitle{Known type and type: function (application)}
\begin{itemize}
\item Application
\[
\rulerCmdUse{rules.expr1B.e-app1B}
\]
\item Function, |lambda|-expression
\[
\rulerCmdUse{rules.expr1B.e-lam1B}
\]
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Known type and type: function application}
\begin{itemize}
\item Rule:
\[
\rulerCmdUse{rules.expr1B.e-app1B}
\]
\item Application, AG:
\chunkCmdFrameUse{EHInferExpr.1.App}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Known type and type: function}
\begin{itemize}
\item Rule:
\[
\rulerCmdUse{rules.expr1B.e-lam1B}
\]
\item Function, |lambda|-expression, AG:
\chunkCmdFrameUse{EHInferExpr.1.Lam}
\item |->| structure is enforced via |funTy| and |fitsIn|
\end{itemize}
}

\paragraph{|lambda|-expression |Lam|.}

For \ruleRef{e-lam1B} the check whether |knTy| has the form |sigma1 -> sigma2|
is done by letting |fitsIn| match the |knTy| with |ANY -> ANY|.
The result (forced to be a function type) is split up by
|tyArrowArgRes| into argument and result type.
The function |gamPushNew| opens a new scope on top of |valGam| so as to be able
to check duplicate names introduced by the pattern |arg|:

\chunkCmdUseMark{EHInferExpr.1.Lam}

\paragraph{Type annotations (for |lambda|-expression).}

In order to make |lambda|-expressions typecheck correctly it is the responsibility of
the EH programmer to supply the correct type signature.
The |TypeAs| variant of |Expr| takes care of this by simply passing the type signature as the expected type:

\chunkCmdUseMark{EHInferExpr.1.TypeAs}

The obligation for the EH programmer to specify a type is dropped in later versions of EH.

\subsubsection{Checking PatExpr}

Before we can look into more detail at the way new identifiers are
introduced in |let|- and |lambda|-expressions
we take a look at patterns.
The \ruleRef{e-let1B} is too restrictive for the actual language construct supported by EH
because the rule only allows a single identifier to be introduced.
The following program allows inspection of parts of a composite value by
naming its components through pattern matching:

\begin{code}
let  p :: (Int,Int)
     p@(a,b) = (3,4)
in   a
\end{code}

The \ruleRef{e-let1C} from \figRef{rules.expr1B.C}
together with the rules for patterns from \figRef{rules.pat1}
reflects the desired behaviour.
These rules differ from those in \figRef{rules.expr1B}
in that a pattern instead of a single identifier is allowed in a
value definition and the parameter position of a |lambda|-expression.

\rulerCmdUse{rules.expr1B.C}
\rulerCmdUse{rules.pat1}

Again the idea is to distribute a known type over the pattern
by dissecting it into its constituents.
However, patterns do not return a type but
type bindings for the identifiers inside a pattern instead.
The new bindings are subsequently used in
|let|- and |lambda|-expressions bodies.

A tuple pattern with \ruleRef{p-prod1} is encoded in the same way
as tuple expressions; that is, pattern |(a,b)| is encoded as
an application |(,) a b| with an |AppTop| on top of it.
We dissect the known type of a tuple in \ruleRef{p-prod1} into its element types
at |AppTop| using function |tyProdArgs|.
For this version of EH
we only have tuple patterns; we can indeed assume that we are dealing with a tuple type.

\chunkCmdUseMark{EHInferPatExpr.1.knTy}
\chunkCmdUseMark{EHInferPatExpr.1.knTy.App}

The list of these elements is passed through attribute |knTyL| to all |App|'s of the pattern.
At each |App| one element of this list is taken as the |knTy| of the element AST.

The complexity in the |AppTop| alternative
of |PatExpr| arises
from repair actions in case the arity of the pattern and its known type do not match.
In that case the subpatterns are given as many |ANY|'s as known type as necessary.

Finally, for the distribution of the known type throughout a pattern we
need to properly initialize |knTyL|:

\chunkCmdUseMark{EHInferPatExpr.1.knTy.Init}

The arity of the patterns is needed as well:

\chunkCmdUseMark{EHInferPatExpr.1.arity}

As a result of this unpacking, at a
|Var| alternative attribute |knTy| holds the type of the variable name introduced.
The type is added to attribute |valGam| that is threaded through the pattern for gathering
all introduced bindings:

\chunkCmdUseMark{EHInferPatExpr.1.valGam}

The addition to |valGam| is encoded in the attribute |addToGam|, a function
which only adds a new entry if the variable name is not equal to an underscore '|_|'
and
has not been added previously via a type signature for the variable name, signalled
by attribute |inclVarBind| (defined later).

\subsubsection{Checking declarations}

In a |let|-expression type signatures, patterns and expressions do meet.
\RuleRef{e-let1C} from \figRef{rules.expr1B.C} shows that the idea is straightforward:
take the type signature, distribute it over a pattern to extract bindings
for identifiers and pass both type signature (as |knTy|) and bindings (as |valGam|)
to the expression.
This works fine for single combinations of type signature and the corresponding value definition for
a pattern.
However, it does not work for:
\begin{itemize}
\item
Mutually recursive value definitions.
\begin{code}
let  f :: ...
     f = \x -> ... g ...
     g :: ...
     g = \x -> ... f ...
in   ...
\end{code}
In the body of |f| the type |g| must be known and vice-versa.
There is no ordering of what can be defined and checked first.
In Haskell |f| and |g| together would be in the same binding group.
\item
Textually separated signatures and value definitions.
\begin{code}
let  f :: ...
     ...
     f = \x -> ...
in   ...
\end{code}
Syntactically the signature and value definition for an identifier need not be defined
adjacently or in any specific order.
\end{itemize}
In Haskell dependency analysis determines that |f| and |g| form a so-called
\IxAsDef{binding group},
which contains declarations that have to be subjected to type analysis together.
However, due to the obligatory presence of the type signatures in this version of EH
it is possible to first gather all signatures
and only then type check the value definitions.
Therefore, for this version of EH it is not really an issue as we always require a signature to be defined.
For later versions of EH it actually will become an issue, so for simplicity all bindings
in a |let|-expression are analysed together as a single (binding) group.

Though only stating something about one combination of type signature 
and value definition, \ruleRef{e-let1C} still describes the
basic strategy. 
First extract all type signatures, then distribute those signatures over patterns followed by expressions.
The difference lies in doing it simultaneously for all declarations in a |let|-expression.
So, first all signatures are collected:
\TBD{AG pattern: walk over tree, put result back into tree}
\TBD{AG pattern: gather, walk, update (via gamAdd)}

\savecolumns
\chunkCmdUseMark{EHInfer.1.gathTySigGam}

Attribute |gathTySigGam| is used to gather type signatures.
The gathered signatures are then passed back into the declarations.
Attribute |tySigGam| is used to distribute the gathered type signatures over the declarations.

\restorecolumns
\chunkCmdUseMark{EHInfer.1.tySigGam.TysigLet}

At a value declaration we extract the the type signature from |tySigGam| and
use it to check whether a pattern has a type signature:

\restorecolumns
\chunkCmdUseMark{EHInfer.1.tySigGam.Val}

This type signature is then
used as the known type of the pattern and the expression.

\chunkCmdUseMark{EHInfer.1.tyInstKnown}

The flag |hasTySig| is used to signal the presence of a type signature for a value and a correct
form of the pattern.
We allow patterns of the form `|ab@(a,b)|' to have a type signature associated with |ab|.
No type signatures are allowed for `|(a,b)|' without the `|ab@|' alias (because there is no way to refer to the
anonymous tuple) nor is it allowed to specify type signature for the fields of the tuple (because of simplicity,
additional plumbing would be required).

\chunkCmdUseMark{EHInfer.1.mbTopNm}

The value of |hasTySig| is also used to decide
on the binding of
the top level identifier of a pattern,
via |inclVarBind|.

\chunkCmdUseMark{EHInfer.1.inclVarBind}

If a type signature for an identifier is already defined there is no need
to rebind the identifier by adding one more binding to |valGam|.

New bindings are not immediately added to |valGam| but are first gathered in a
separately threaded attribute |patValGam|, much in the same way
as |gathTySigGam| is used.

\chunkCmdUseMark{EHInfer.1.patValGam}
\chunkCmdUseMark{EHInfer.1.Let}

Newly gathered bindings are stacked on top of the inherited |valGam| before passing them
on to both declarations and body.

Some additional functionality for pushing and popping the stack |valGam| is also needed:

\chunkCmdUseMark{EHGam.1.Rest.sigs}
\chunkCmdUseMark{EHGam.1.Rest.funs}

Extracting the top of the stack |patValGam| gives all the locally introduced
bindings in |lValGam|.
An additional error message is produced
%if these
(later on, section~\ref{sec-error1})%
%endif
if any duplicate bindings are present in |lValGam|.

\subsubsection{Checking TyExpr}

All that is left to do now is to use the type expressions to extract type signatures.
This is straightforward as type expressions (abstract syntax for what the programmer specified)
and types (as internally used by the compiler) have almost the same structure:

\chunkCmdUseMark{EHInferTyExpr.1.ty}

Actually, we need to do more because we also have to check whether a type is defined.
A variant of |Gam| is used to hold type constants:

\chunkCmdUseMark{EHGam.1.TyGamInfo}
\chunkCmdUseMark{EHGam.1.TyGam}
\chunkCmdUseMark{EHGam.1.tyGamLookup}

This |Gamma| is threaded through |TyExpr|:

\chunkCmdUseMark{EHInferTyExpr.1.tyGam}

At the root of the AST |tyGam| is initialized with the fixed set of types available
in this version of the compiler:

\chunkCmdUseMark{EHInfer.1.initTyGam}

Finally, at the |Con| alternative of |TyExpr| we need to check if a type is defined:

\chunkCmdUseMark{EHInferTyExpr.1.check}

\frame<presentation>
{
\frametitle{Declarations}
\begin{itemize}
\item Rule for |let|-expression
\[
\rulerCmdUse{rules.expr1B.e-let1B}
\]
\item is more complicated in the implementation
\begin{itemize}
\item Patterns allow introduction of |>1| identifier
\SafeCode{%
\begin{code}
let ab@(a,b) = ...
\end{code}
}
\item Definitions may be used mutually recursive
\SafeCode{%
\begin{code}
let  f = ... g ...
     g = ... f ...
\end{code}
}
\end{itemize}
\item Solution:
\begin{itemize}
\item Gather and distribute in multiple passes
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Gathering and distribution for let}
\begin{itemize}
\item
\chunkCmdFrameUse{EHInfer.1.gathTySigGam}
\chunkCmdFrameUse{EHInfer.1.tySigGam.TysigLet}
\item Gather in |gathTySigGam|, distribute in |tySigGam|
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Environment |Gamma|}
\begin{itemize}
\item Holding assumptions |v| about |k|
\chunkCmdFrameUse{EHCommon.1.AssocL}
\chunkCmdFrameUse{EHGam.1.Base.sigs}
\item Stack like structure
\begin{itemize}
\item To match scope introduced by |let| and |\| expressions
\item So duplicate identifiers can be detected
\end{itemize}
\item Variant specialized for (type) information about values
\chunkCmdFrameUse{EHGam.1.ValGam.Base}
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: State, State Gather}
\begin{itemize}
\item Simulation of global variable via threading
\PGFPatThread
\item Generated by AG's copy rules if |a| is
both inherited and synthesized
\item Update by intervening definition
\begin{itemize}
\item here: |AGPat(State Gather)| gathers set of values in |gathTySigGam|, |upd=| addition to |Gamma|
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG patterns and monads}
\begin{itemize}
\item |AGPat(DeCompose)|: reader monad (class |MonadReader|)
\item |AGPat(Compose)|: writer monad (class |MonadWriter|)
\item |AGPat(State)|: state monad (class |MonadState|)
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: Multipass}
\begin{itemize}
\item Iteration over structure
\PGFPatMultipass
\item Fixed number of times
\item Each pass via different attribute, possibly different role
\item Here:
\begin{itemize}
\item |a1 = gathTySigGam| gathers type signatures
\item |a2 = tySigGam| distributes type signatures of identifiers
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Let}
\begin{itemize}
\item Pattern in |let|-expression
\[
\rulerCmdUse{rules.expr1B.C.e-let1C}
\]
\item also requires |AGPat(State Gather)| to gather identifiers introduced in a pattern, e.g. for |a,b|:

\SafeCode{%
\begin{code}
let  v         ::  Int
     v         =   a
     ab        ::  (Int,Int)
     ab@(a,b)  =   ...
\end{code}
}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Patterns}
\begin{itemize}
\item Rules for patterns describe the gathering if given a known type
\[
\rulerCmdUse{rules.pat1.p-var1}
\]
\item |sigmak =@= knTy| is decomposed via |AGPat(DeCompose)| pattern
\[
\rulerCmdUse{rules.pat1.p-prod1}
\]
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Patterns: Var}
\begin{itemize}
\item |valGam| is used for |AGPat(State Gather)| of identifier bindings
\chunkCmdFrameUse{EHInferPatExpr.1.valGam}
\item |inclVarBind| inhibits double occurrences of type of complete pattern
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Patterns: Product}
\begin{itemize}
\item |knTy| used in |AGPat(DeCompose)|
\chunkCmdFrameUse{EHInferPatExpr.1.knTy}
\chunkCmdFrameUse{EHInferPatExpr.1.knTy.App}
\item Uses also |AGPat(DeCompose)| for |knTyL|
\item For |AGPat(DeCompose)| of product |knTy| the arity of the product expression is required
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: Listlike Spine}
\begin{itemize}
\item Extracting info from left/right balanced list like structure
\PGFPatListSpine
\item |2 x ^ AGPat(Compose All)| over |X|, for a value related to the
\begin{itemize}
\item last (|Nil|) element: |n|
\item spine (|Cons|) + elements: |c|
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Patterns: Product arity}
\begin{itemize}
\item Here:
\begin{itemize}
\item |App| structure is left balanced
\item |X = PatExpr|,\\ |Cons = App|, |Nil = ...|,\\ |c = arity| (i.e. length),\\ |combine = \c _ -> c+1|
\end{itemize}
\item AG:
\chunkCmdFrameUse{EHInferPatExpr.1.arity}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Declarations}
\begin{itemize}
\item Mutual recursiveness of value definitions necessitates gathering
bindings for identifiers (|AGPat(State Gather)| via |patValGam|) first, passing the bindings back for type checking
(|AGPat(Multipass)| into |valGam|)
\item AG:
\chunkCmdFrameUse{EHInfer.1.patValGam}
\chunkCmdFrameUse{EHInfer.1.Let}
\end{itemize}
}

%if incl01TopicErr

\subsection{Reporting program errors}
\label{sec-error1}

Errors too are defined using AG%
%if storyAFP04Notes
.
%else
(see also \pageRef{EHErrorAbsSyn.1.UnifyClash}).
%endif

\chunkCmdUseMark{EHErrorAbsSyn.1.Rest}

Errors are gathered, grouped at convenient places and made part of the
pretty printing attribute |pp|.
This is only shown for expressions:

\chunkCmdUseMark{EHGatherError.1.GatherExpr}
\chunkCmdUseMark{EHError.1.mkNestErr}

Pretty printed collected errors are made available for
inclusion in the pretty printing of the abstract syntax tree%
%if storyAFP04Notes
.
%else
(see \pageRef{EHPretty.1.Base.ExprSimple}).
%endif
We have chosen to only report the collected errors at
the end of a group of declarations or at a specific
declaration. Many different choices are possible here,
depending on the output medium.

\chunkCmdUseMark{EHGatherError.1.PP}

Errors themselves also need to be pretty printed, but we have omitted this part.
The issue here is to keep the generated output reasonably compact because the nesting
can give a long trace of locations where the error did occur.

\frame<presentation>
{
\frametitle{Errors}
\begin{itemize}
\item Errors are described by AG too:
\chunkCmdFrameUse{EHErrorAbsSyn.1.UnifyClash}
\chunkCmdFrameUse{EHErrorAbsSyn.1.NamesNotIntrod}
\chunkCmdFrameUse{EHErrorAbsSyn.1.Rest}
\chunkCmdFrameUse{EHErrorAbsSyn.1.MissingSig}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Gathering errors}
\begin{itemize}
\item Errors are collected into set of errors
\item Annotated with context in which error occurred (|NestedIn|):
\chunkCmdFrameUse{EHGatherError.1.GatherExpr}
\item With pretty printing and use as annotation of pretty printed output
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: Compose Gather}
\begin{itemize}
\item Gathering errors follows special case of |AGPat(Compose All)| where
\begin{itemize}
\item |combine = union| (of lists/sets)
\item Leafs of tree: empty list/set
\item Supported by AG's |USE| construct
\end{itemize}
\item Difference with |AGPat(State Gather)|
\begin{itemize}
\item |AGPat(State Gather)| corresponds to tree walk, individual children's contribution cannot be differentiated
\item |AGPat(Compose Gather)| corresponds to postfix tree walk, individual children's contribution accessible
\end{itemize}
\end{itemize}
}

%endif %% incl01TopicErr

%if not incl00TopicAGPrimer

\subsection{Tying it all together}

Finally, all needs to be tied together to obtain a working program.
This involves a bit of glue code to (for example) combine scanner (not discussed here),
parser, semantics, passing compiler options and the generation of output.
For brevity, these details are omitted.

%endif %% incl00TopicAGPrimer

\frame<presentation>
{
\frametitle{Tying things together}
\begin{itemize}
\item For example the use of pretty printing a type as defined by an AG
\item Define the AG
\SafeCode{%
\begin{code}
...
\end{code}
} 
\item Tell AG to create a Haskell wrapper function
\chunkCmdFrameUse{EHTyPretty.1.WRAPPER}
\begin{itemize}
\item takes care of mapping attribute names to/from argument/result tuple positions in
generated functions
\end{itemize} 
\item Use wrapping function to define Haskell function
\chunkCmdFrameUse{EHTyPretty.1.ppTy}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Making it work}
\begin{itemize}
\item Involves many remaining other (logistical) details
\item At the code level
\begin{itemize}
\item Compiler driver
\item Options to compiler
\item Connecting AG generated Haskell and remaining Haskell
\end{itemize}
\item At the tool/compilation level
\begin{itemize}
\item Make system
\item etc. etc.
\end{itemize}
\item Not shown here
\begin{itemize}
\item See WWW site
\end{itemize}
\end{itemize}
}


%if not omitLitDiscuss
\subsection<article>{Literature}

\TBD{}

Local type inference \cite{pierce00local-type-inference} also has top-down, bottom-up mixing.
%endif

%endif %% incl01

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl02

\section{EH 2: monomorphic type inferencing}
\label{ehc2}

\frame<presentation>{\tableofcontents[current,hidesubsections]}

The next version of EH drops the requirement that all value definitions
need to be accompanied by an explicit type signature.
For example, the example from the introduction:

\begin{code}
%%2srcfile(test/1-sig-fail.eh%%)
\end{code}
is accepted by this version of EH:
\begin{TT}
%%2ppfile(test/1-sig-fail.eh%%)
\end{TT}

\frame<presentation>[containsverbatim]
{
\frametitle{EH 2: monomorphic type inferencing}
\begin{itemize}
\item Type signature may be omitted
\item For example
\SafeCode{%
\begin{code}
%%2srcfile(test/1-sig-fail.eh%%)
\end{code}
} 
\item gives
\begin{TT}
%%2ppfile(test/1-sig-fail.eh%%)
\end{TT}
\item Type is inferred/reconstructed
\end{itemize}
}

The idea is that the type system implementation has an internal representation
for ``knowing it is a type, but not yet which one'' which can be replaced
by a more specific type if that becomes known.
The internal representation for a yet unknown type
is called a \IxAsDef{type variable}, similar to mutable variables
for (runtime) values.

The implementation attempts to gather as much information as possible
from a program
to reconstruct (or infer) types for type variables.
However, the types it can reconstruct are limited to those allowed by
the used type language, that is, basic types, tuples and functions.
All types are assumed to be monomorphic, that is, polymorphism is not yet allowed.
The next version of EH deals with polymorphism.

So
\begin{code}
%%2srcfile(test/2-demo1.eh%%)
\end{code}
will give
\begin{TT}
%%2ppfile(test/2-demo1.eh%%)
\end{TT}

\frame<presentation>[containsverbatim]
{
\frametitle{Monomorphism}
\begin{itemize}
\item Functions can only be used for one type, not many (polymorphism)
\item For example
\SafeCode{%
\begin{code}
%%2srcfile(test/2-demo1.eh%%)
\end{code}
}
\item infers
\begin{TT}
%%2ppfile(test/2-demo1.eh%%)
\end{TT}
\end{itemize}
}

If the use of |id| to define |v| is omitted,
less information (namely the argument of |id| is an int) to infer a type for |id| is available.
Because no more specific type information for the argument (and result) of |id| could be retrieved 
the representation for ``not knowing which type'', that is, a type variable, is shown:

\begin{TT}
%%2ppinline(let id = \x -> x in id%%)
\end{TT}

On the other hand, if contradictory information is found we will have
\begin{TT}
%%2ppfile(test/2-id-intchar.eh%%)
\end{TT}
However, the next version of EH dealing with Haskell style polymorphism
(\chapterRef{ehc3}) accepts this program.

\frame<presentation>[containsverbatim]
{
\frametitle{Monomorphism}
\begin{itemize}
\item Polymorphic use leads to errors
\item For example
\SafeCode{%
\begin{code}
%%2srcfile(test/2-id-intchar.eh%%)
\end{code}
}
\item gives
\begin{TT}
%%2ppfile(test/2-id-intchar.eh%%)
\end{TT}
\end{itemize}
}

Partial type signatures are also allowed.
A partial type signature specifies a type only for a part, allowing
a co\"operation between the programmer who specifies what is (e.g.) already
known about a type signature and the type inferencer filling in the unspecified details.
For example:

\begin{code}
%%2srcfile(test/2-ty-wild.eh%%)
\end{code}

The type inferencer pretty prints the inferred type instead of the explicity type signature:

\begin{TT}
%%2ppfile(test/2-ty-wild.eh%%)
\end{TT}

The discussion of the implementation of this feature is postponed until
\secRef{ehc2partial-sig} in order to demonstrate the effects of an additional feature
on the compiler implementation in isolation.

\frame<presentation>
{
\frametitle{Partial type signature}
\begin{itemize}
\item Type need not be specified completely
\SafeCode{%
\begin{code}
%%2srcfile(test/2-ty-wild.eh%%)
\end{code}
}
\item
Only the part which needs to be specified or constrained further
\item
As an afterthought to see how well an additional feature can be added
\end{itemize}
}

\frame<presentation>[containsverbatim,plain]
{
\frametitle{Partial type signature}
\begin{itemize}
\item Pretty printing shows inferred types
\begin{TT}
%%2ppfile(test/2-ty-wild.eh%%)
\end{TT}
\item Shows inferred type only if partially specified
\end{itemize}
}

\subsection{Type variables}

In order to be able to represent yet unknown types the type language needs
\IxAsDef{type variable}s to represent this:

\begin{code}
sigma  =  Int | Char
       |  (sigma,...,sigma)
       |  sigma -> sigma
       |  tvarv
\end{code}

The corresponding type structure |Ty| needs to be extended with an alternative for a variable:

\chunkCmdUseMark{EHTyAbsSyn.2}

%if incl01TopicPP

The AG system allows us to separately describe the extension with a new variant as well
as describe separately the additionaly required attribution,
for example the pretty printing of the type

\chunkCmdUseMark{EHTyPretty.2}

%endif %% incl01TopicPP

A type variable is identified by a unique identifier, a |UID|:

\chunkCmdUseMark{EHCommon.2.UID.Base}
\chunkCmdUseMark{EHCommon.2.UID.UIDL}
\chunkCmdUseMark{EHCommon.2.UID.Show}
\chunkCmdUseMark{EHTy.2.TyVarId.Base}
\chunkCmdUseMark{EHTy.2.TyVarId.Rest}

The idea is to thread a counter as global variable through the AST,
incrementing it whenever a new unique value is required.
The implementation used throughout all EH compiler versions is more complex because
an |UID| actually is a hierarchy of counters, each level counting in the context of an outer level.
This is not discussed any further;
we will ignore this aspect and just assume a unique |UID| can be obtained.
However, a bit of its implementation is visible in the pretty printed representation as a underscore separated
list of integer values,
occasionaly visible in sample output of the compiler.

\frame<presentation>
{
\frametitle{Type language: type variables}
\begin{itemize}
\item Type language
\SafeCode{%
\begin{code}
sigma  =  Int | Char
       |  (sigma,...,sigma)
       |  sigma -> sigma
       |  tvarv
\end{code}
}
\item Type variable |tvarv| represents yet unknown type
\item AG extension of type structure:
\chunkCmdFrameUse{EHTyAbsSyn.2}
\chunkCmdFrameUse{EHCommon.2.UID.Base}
\chunkCmdFrameUse{EHTy.2.TyVarId.Base}
\item (more about |UID|'s later)
\end{itemize}
}

\subsection{Constraints}

Although the typing rules at \figPageRef{rules.expr1B.C} still hold
we need to look at the meaning of |<=| (or |fitsIn|) in the presence of
type variables.
The idea here is that what is unknown may be replaced by that which is known.
For example, when the check |tvarv <= sigma| is encountered,
the easiest way to make |tvarv <= sigma| true
is to state that the (previously) unknown type |tvarv| equals |sigma|.
An alternative way to look at this is that |tvarv <= sigma| is true under the
constraint that |tvarv| equals |sigma|.

\subsubsection{Remembering and applying constraints}

Next we can observe that once a certain type |tvarv| is declared to be
equal to a type |sigma| this fact has to be remembered.

\begin{code}
Cnstr                       =  [tvarv :-> sigma]
\end{code}

A set of \IxAsDef{constraint}s |Cnstr| (appearing in its non pretty printed form as @Cnstr@ in the source text)
is a set of bindings for type variables,
represented as an association list:

\chunkCmdUseMark{EHCnstr.2.Cnstr.Base}
\chunkCmdUseMark{EHCnstr.2.Cnstr.emptyCnstr}
\chunkCmdUseMark{EHCnstr.2.Cnstr.cnstrTyUnit}

If |cnstrTyUnit| is used as an infix operator it is printed as |`cnstrTyUnit`| in
the same way as used in type rules.

Different strategies can be used to cope with constraints
\cite{heeren02hm-constr,sulzmann97constrained-type}.
Here
constraints |Cnstr| are used to replace all other
references to |tvarv| by |sigma|,
for this reason often named a \IxAsDef{substitution}.
In this version of EH the replacement of type variables
with newly types is done immediately after constraints are obtained as
to avoid finding a new and probably conflicting constraint for
a type variable.
Applying constraints means substituting type
variables with the bindings in
the constraints, hence the class
|Substitutable|
for those structures which have references to type
variables hidden inside and can replace, or substitute those type variables:

\chunkCmdUseMark{EHSubstitutable.2.Substitutable}

The operator | ||=>| applies constraints |Cnstr| to a
|Substitutable|.
Function |ftv| extracts the free type variable references as a set of
|TVarId|'s.

A |Cnstr| can be applied to a type:

\chunkCmdUseMark{EHSubstitutable.2.SubstitutableTy}

This is another place where we use the AG notation and the automatic propagation of values
as attributes throughout the type representation to make the description of the application of
a |Cnstr| to a |Ty| easier.
The function |tyAppCnstr| is defined in terms of the following AG.
The plumbing required to provide the value of attribute |repl| (|tvs|) available as
the result of Haskell function |tyAppCnstr| (|tyFtv|)
has been omitted:

\chunkCmdUseMark{EHTySubst.2.TySubst}
\chunkCmdUseMark{EHTyFtv.2.TyFtv}

%{

%format < 		= "{\langle}"
%format > 		= "{\rangle}"

\begin{AGFeature}{ag-self-attr}{Attribute of type SELF}
The type of an attribute of type |SELF| depends on the node in which a rule is defined for
the attribute.
The generated type of an attribute |<attr>| for |<node>| is equal to the generated Haskell datatype
of the same name |<node>|.
The AG compiler inserts code for building |<node>|'s from the |<attr>| of the children and other fields.
Insertion of this code can be overridden by providing a definition ourselves.
In this way a complete copy of the AST can be built as a Haskell value.
For example, via attribute |repl| a copy of the type is built which only differs (or, may differ) in the original
in the value for the type variable.
\end{AGFeature}

\begin{AGFeature}{ag-use-attr}{Attribute together with USE}
A synthesized attribute |<attr>| may be declared together with |USE {<op>} {<zero>}|.
The |<op>| and |<zero>| allow the insertion of copy rules which behave similar to Haskell's |foldr|.
The first piece of text |<op>| is used to combine the attribute values of two children by textually placing this text as an
operator between references to the attributes of the children.
If no child has an |<attr>|, the second piece of text |<zero>| is used as a default value for |<attr>|.
For example, @tvs USE {`union`} {[]}@ (appearing in pretty printed form as |tvs USE {`union`} {[]}|)
gathers bottom-up the free type variables of a type.
\end{AGFeature}

%}

The application of a |Cnstr| is straightforwardly lifted to lists:

\chunkCmdUseMark{EHSubstitutable.2.SubstitutableList}
\chunkCmdUseMark{EHCommon.2.unionL}

A |Cnstr| can also be applied to another |Cnstr|:

\chunkCmdUseMark{EHSubstitutable.2.SubstitutableCnstr}

Substituting a substitution is non-commutative as constraints |s1| in |s1 ||=> s2| take precedence
over |s2|.
To make this even clearer all constraints for type variables in |s1| are removed from |s2|,
even though for a list implementation this would not be required.

\frame<presentation>
{
\frametitle{Type inference}
\begin{itemize}
\item The idea (employed by most type inferencers)
\begin{itemize}
\item If no type is known for identifier, bind it to a fresh type variable
\item Look at how this type variable is used
\item In particular if an assumption about it is made
\item That constrains the possible types for the type variable
\item Leading to a more specific type for the type variable
\item So all occurrences of the type variable must be replaced by this more specific type
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Constraints}
\begin{itemize}
\item New constraint for type variable
\SafeCode{%
\begin{code}
Cnstr                       =  [tvarv :-> sigma]
\end{code}
}
\begin{itemize}
\item Set of bindings/mappings for type variables
\item Usually called |Substitution|
\end{itemize}
\item Implemented (here) as association list
\chunkCmdFrameUse{EHCommon.1.AssocL}
\chunkCmdFrameUse{EHCnstr.2.Cnstr.Base}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Applying constraints}
\begin{itemize}
\item Replacement of type variables in a type
\begin{itemize}
\item Denoted
\SafeCode{%
\begin{code}
Cnstr |=> sigma
\end{code}
}
\item or even shorter as function application
\SafeCode{%
\begin{code}
Cnstr sigma
\end{code}
}
\end{itemize}
\item Constraints/substitution can be applied to |Substitutable|
\chunkCmdFrameUse{EHSubstitutable.2.Substitutable}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Type substitution}
\begin{itemize}
\item Replacing type variables in a |Ty|:
\chunkCmdFrameUse{EHSubstitutable.2.SubstitutableTy}
\chunkCmdFrameUse{EHTySubst.2.TySubst}
\chunkCmdFrameUse{EHTyFtv.2.TyFtv}
\item Other datatypes (like |Cnstr|) are instances of |Substitutable| too
\end{itemize}
}

\subsubsection{Computing constraints}

The only source of constraints is the check |fitsIn| which determines
whether one type can flow into another one.
The previous version of EH could only do one thing in case a type could not fit
in another: report an error.
Now,
if one of the types is unknown, which means that it is a type variable, we have the additional possibility of
returning a constraint on that type variable.
The implementation |fitsIn| of |<=| additionaly has to return constraints:

\savecolumns
\chunkCmdUseOnPrev{EHTyFitsInCommon.1.FIOut}{EHTyFitsInCommon.2.FIOut}
\restorecolumns
\chunkCmdUseMark{EHTyFitsInCommon.2.FIOut.empty}

Computation and proper combination of constraints necessitates
|fitsIn| to be rewritten:

\savecolumns
\chunkCmdUseOnPrev{EHTyFitsIn.1.fitsIn.Base}{EHTyFitsIn.2.fitsIn.Base}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.2.fitsIn.Bind}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.2.fitsIn.app}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.2.fitsIn.BotCon}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.2.fitsIn.Var}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.2.fitsIn.AppRest}

Although this version of the implementation of |fitsIn| resembles the previous one
it differs in the following aspects:

\begin{itemize}
\item
The datatype |FIOut| returned by |fitsIn| has an additional field |foCnstr| holding found constraints.
This requires constraints to be combined for composite types
like the |App| variant of |Ty|.
\item
The function |bind| creates a binding for a type variable to a type.
The use of |bind| is shielded by |occurBind| which checks if the type variable for
which a binding is created does not occur free in the bound type too.
This is to prevent (e.g.) |a <= a -> a| to succeed.
This is because it is not clear if |a :-> a -> a| should be the resulting constraint
or |a :-> (a -> a) -> (a -> a)| or one of infinitely many other possible solutions.
A so called \IxAsDef{infinite type}
like this is inhibited by the so called \IxAsDef{occurs check}.
\item
An application |App| recursively fits its components with components of another |App|.
The constraints from the first fit |ffo| are applied immediately to the following component
before fitting that one.
This is to prevent |a -> a <= Int -> Char| from finding two conflicting
constraints
|[a :-> Int,a :-> Char]| instead of properly reporting an error.
\end{itemize}

\frame<presentation>[plain]
{
\frametitle{Computing constraints}
\begin{itemize}
\item Types are constrained (e.g. to be equal) in type rules
\item Enforced by |fitsIn| in the implementation 
\item Idea: if (at least) one of two compared types is a type variable |v|,
|fitsIn| returns a |Cnstr| mapping |v| to the other type 
\item Additional cases in |fitsIn|
\chunkCmdFrameUse{EHTyFitsIn.2.fitsIn.Var}
\item |fitsIn| returns the more specific resulting type
\begin{itemize}
\item Usually called |unified| type
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Binding type variables}
\begin{itemize}
\item |fitsIn| binds a type variable |v| by returning
a |Cnstr = [v :-> ...]| for it.
\item Result of |fitsIn| has an additional |foCnstr :: Cnstr|
\chunkCmdFrameUse{EHTyFitsInCommon.2.FIOut}
\item Binding
\chunkCmdFrameUse{EHTyFitsIn.2.fitsIn.Bind}
\begin{itemize}
\item prevents recursive types as in |v1 `fitsIn` v1 -> v1|
\item checks if to be bound type variable occurs in the other type
\item called \emph{occur check}
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Applying constraints/substitutions}
\begin{itemize}
\item Strategy/invariant: all already known constraints are applied
to a type variable |v| when |v| is used
\begin{itemize}
\item If not: contradictory constraints may arise
\end{itemize}
\item For example in fitting two |Ty_App|'s
\chunkCmdFrameUse{EHTyFitsIn.2.fitsIn.app}
\item Forgetting to apply constraints is a generous source of (mysterious :-)) errors
\end{itemize}
}

\subsection{Reconstructing types for Expr}

Constraints are used to make knowledge found about previously unknown
types explicit.
The typing rules in \figRef{rules.expr1A} (and \figRef{rules.expr1B}, \figRef{rules.expr1B.C})
in principle do not need to be changed.
The only reason to adapt some of the rules to the variant in
\figRef{rules.expr2}
is to clarify the way constraints are used.

\rulerCmdUse{rules.expr2}

The type rules in \figRef{rules.expr2} enforce an order in which
checking and inferring types has to be done.

\TBD{...}

Actually, the rules in \figRef{rules.expr2} should be even more
specific in how constraints flow around if we want to be closer to
the corresponding AG description.
The AG specifies a |Cnstr| to be threaded instead of just returned bottom-up:

% \chunkCmdUsePrevLit{EHInferExpr.1.App}
\chunkCmdUseMark{EHInferExpr.2.tyCnstr}

Its use in an expression application is as follows:

\chunkCmdUseOnPrev{EHInferExpr.1.App}{EHInferExpr.2.App}

\begin{AGFeature}{ag-redef-rule}{Redefining an attribute value}
Normally a value for an attribute may be associated with an attribute only once,
using |=| in a rule.
It is an error if multiple rules for an attribute are present.
If |:=| is used instead, any previous definition is overridden and no error message is generated.
In this context previous means ``textually occurring earlier''.
Because the AG system's flexibility finds its origin in the independence of textual locations of
declarations and definitions, |:=| should be used with care.
For \thispaper\ the order in which redefinitions appear is the same as their textual appearance
in \thispaper, which again is the same as the sequence of versions of EH.
\end{AGFeature}

This definition builds on top of the previous version by
redefining some attributes (indicated by |:=| instead of |=|).
If this happens a reference to the location (in \thispaper) of the code on top of which
the new code is added can be found\footnote{This is not an ideal solution to display combined fragments.
A special purpose editor would probably do a better
job of browsing textually separated but logically related pieces of code.}.

To correspond better with the related AG code the \ruleRef{e-app2} should be:

\[
\rulerCmdUse{rules.expr2B.e-app2B}
\]

The flow of constraints is made explicit as they are passed through the rules,
from the context (left of |:-|) to a result (right of |~>|).
We feel this does not benefit clarity, even though it is correct.
It is our opinion that
typing rules serve their purpose best by providing a basis for proof as well
as understanding and discussion.
An AG description serves its purpose best by showing how it really is implemented.
Used in tandem they strengthen each other.

An implementation by necessity imposes additional choices, in order to make a typing
rule into an algorithmic solution.
For example, our AG description preserves the following invariant:
\begin{itemize}
\item
A resulting type has all known constraints applied to it, here |ty|.
\end{itemize}
but as this invariant is not kept for |knTy| and |valGam| it requires to
\begin{itemize}
\item
Explicitly apply known constraints to the inherited known type |knTy|.
\item
Explicitly apply known constraints to types from a |Gamma|, here |valGam|.
\end{itemize}

The type rules in \figRef{rules.expr2} do not mention the last two constraint applications
(\ruleRef{e-app2B} does),
and this will also be omitted for later typing rules.
However, the constraint applications are shown by the AG code for
the |App| alternative and the following |Var| alternative:

\chunkCmdUseOnPrev{EHInferExpr.1.Var}{EHInferExpr.2.Var}

The rules for constants all resemble the one for |Int|, \ruleRef{e-int2}.
Their implementation additionaly takes care of constraint handling:

\chunkCmdUseOnPrev{EHInferExpr.1.Const}{EHInferExpr.2.Const}

The handling of products does not differ much from the previous
implementation.
A \ruleRef{e-con2} has been included in the typing rules,
as a replacement for \ruleRef{e-prod1B} (\figRef{rules.expr1B}) better
resembling its implementation.
Again the idea is to exploit that in this version of EH tupling is the
only way to construct an aggregrate value.
A proper structure for its type is (again) enforced by |fitsIn|.

\chunkCmdUseOnPrev{EHInferExpr.1.Con}{EHInferExpr.2.Con}

Finally, 

\chunkCmdUseOnPrev{EHInferExpr.1.Lam}{EHInferExpr.2.Lam}

which uses some additional functions for creating type variables

\chunkCmdUseMark{EHTy.2.NewTyVar}
\chunkCmdUseMark{EHTy.2.NewTyVarL}

Some observations are in place:
\begin{itemize}
\item
The main difference with the previous implementation is the use
of type variables to represent unknown knowledge.
Previously |ANY| was used for that purpose, for example,
the \ruleRef{e-lam2} and its implementation show that fresh
type variables |tvari| in |tvar1 -> tvar2| are used instead
of |ANY -> ANY| to enforce a |.. -> ..| structure.
If |ANY| still would be used, for example in:
\begin{code}
let  id = \x -> x
in   id 3
\end{code}
the conclusion would be drawn that |id :: ANY -> ANY|,
whereas |id :: tvarv -> tvarv| would later on have bound |tvarv :-> Int| (at the application |id 3|).
So, |ANY| represents ``unknown knowledge'',
a type variable |tvarv| represents ``not yet known knowledge''
to which the inferencing process later has to refer to make it ``known knowledge''.
\item
Type variables are introduced under the condition that they are
\Ix{fresh type variable}``fresh''.
For a typing rule this means that these type
variables are not in use elsewhere,
often more concretely specified with a condition |tvarv `notElem` ftv(Gamma)|.
Freshness in the implementation is implemented via unique identifiers UID.
\end{itemize}

\frame<presentation>
{
\frametitle{Type inferencing expressions}
\begin{itemize}
\item Rules assume constraints are `magically' applied, e.g. for
application
\[
\rulerCmdUse{rules.expr1A.e-app1}
\]
\item but implementation has to manipulate constraints explicitly
\[
\rulerCmdUse{rules.expr2.e-app2}
\]
\end{itemize}
}

\frame<presentation>
{
\frametitle{Type inferencing expressions}
\begin{itemize}
\item<+-> Here: walk a middle way to avoid cluttered rules
\begin{itemize}
\item Rules are kept relatively simple to help understanding
\item AG code incorporates the implementation 'magic'
\end{itemize}
\onslide<+-> 
\item So
\begin{itemize}
\item In rules constraints are computed bottom-up a la |AGPat(Compose Gather)|
\item But in the AG implementation a |AGPat(State Gather)| is used
\onslide<+-> 
\item In rules constraints are magically applied to context |Gamma|, |sigmak|
\item But in the AG implementation constraints are applied just before using context
\item Which can be done as all constraints are available via threading
\onslide<+-> 
\item In both rules and implementation the resulting type |sigma| has all constraints already applied to
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Type inferencing expressions}
\begin{itemize}
\item Previous |App|'s AG:
\chunkCmdUsePrevLit{EHInferExpr.1.App}
\item Additional |App|'s AG:
\chunkCmdFrameUse{EHInferExpr.2.tyCnstr}
\chunkCmdFrameUse{EHInferExpr.2.App}
\chunkCmdFrameUse{EHTy.2.NewTyVar}
\item Uses AG's @:=@ to redefine/override (textually) previous definition
\end{itemize}
}

\frame<presentation>
{
\frametitle{Type inferencing expressions}
\begin{itemize}
\item |App|'s rule:
\[
\rulerCmdUse{rules.expr2.e-app2}
\]
\item |v| fresh
\begin{itemize}
\item means |v| must be unique, different from any other type variable
\item often alternatively specified by |v `notElem` ftv(Gamma)|
\end{itemize}
\item Here: how do we obtain a unique value
\begin{itemize}
\item avoiding Haskell's |STRef| hidden state constructs
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{AG pattern: Uniq}
\begin{itemize}
\item<+-> Special case of |AGPat(Global Var)|
\item Threaded counter solution
\SafeCode{%
\begin{code}
ATTR AllNT [ | gUniq: Int | ]
SEM Expr
  | App  loc    .  lUniq    = @lhs.gUniq
         func   .  gUniq    = @lhs.gUniq + 1
\end{code}
}
\begin{itemize}
\item Must be threaded through all functions needing unique value
\item Additional input counter and tupled output counter
\onslide<+-> 
\item Clutters code
\onslide<+-> 
\item Creates attribute dependencies which can easily cause cycles if used
together with |AGPat(Multipass)|
\item Unless each pass has own counter
\end{itemize}
\end{itemize}
}

%if not dist
\frame<presentation>[plain]
{
\frametitle{Cyclic AG}
\begin{itemize}
\item<1-> Alternatives of a case expression (from version 5)
\item<2-> |u|: unique counter
\item<3-> |cp|: constraints found in patterns (attribute |patTyCnstr|)
\item<4-> |ce|: constraints found in expressions (attribute |tyCnstr|)
\end{itemize}

\begin{overprint}
\onslide<1>
\includegraphics[height=20cm]{figs/cycle-case1.pdf}
\onslide<2>
\includegraphics[height=20cm]{figs/cycle-case2.pdf}
\onslide<3>
\includegraphics[height=20cm]{figs/cycle-case3.pdf}
\onslide<4>
\includegraphics[height=20cm]{figs/cycle-case4.pdf}
\onslide<5>
\includegraphics[height=20cm]{figs/cycle-case4b.pdf}
\onslide<6>
\includegraphics[height=20cm]{figs/cycle-case5.pdf}
\onslide<7>
\includegraphics[height=20cm]{figs/cycle-case6.pdf}
\onslide<8>
\includegraphics[height=20cm]{figs/cycle-case7.pdf}
\end{overprint}
}
%endif

\frame<presentation>
{
\frametitle{Cycles}
\begin{itemize}
\item Cycles need not be problem
\begin{itemize}
\item If value in cycle is never used
\item If cycle goes through intermediate constructor
\end{itemize}
\item AG can detect cycles
\item AG can also be fooled into detecting cycles
\SafeCode{%
\begin{code}
ATTR AllNT [ | u1: Int ^^^ u2: Int| ]
SEM Expr
  | App         lhs     .   (u1,u2)     = id (@func.u1,@func.u2)
  | AppTop      expr    .   u2          = @expr.u1
\end{code}
}
\begin{itemize}
\item |u1| depends on |u2| and vice versa
\item AG compiler does not analyse Haskell part of attribute definitions
\item Makes cycle checker less useful
\end{itemize}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{AG pattern: Uniq}
\begin{itemize}
\item Threaded seed solution
\begin{itemize}
\item Threads seed instead of counter
\item Each seed has subcounter for a private `infinite' sequence of unique UID's
\item Each node takes as many seeds as required
\end{itemize}
\item E.g. for expressions
\chunkCmdFrameUse{EHUniq.2.Expr}
\item UID
\chunkCmdFrameUse{EHCommon.2.UID.Base}
\chunkCmdFrameUse{EHCommon.2.UID.mkNewLevUID}
\end{itemize}
}

\subsection{Reconstructing types for PatExpr}

In the previous version of EH we were only interested in bindings for
identifiers in a pattern.
The type of a pattern was already known via a corresponding type signature.
For this version this is no longer the case so the structure of a pattern
reveals already some type structure.
Hence we compute types for patterns too and use this type as the known type
if no type signature is available.

\rulerCmdUse{rules.pat2}

Computation of the type of a pattern is similar to and yet more straightforward than
for expressions.
The \ruleRef{e-pat2} from \figRef{rules.pat2} binds the identifier
to the known type and if no such known type is available it invents a fresh one,
by means of |tyEnsureNonAny|:

\chunkCmdUseMark{EHInferPatExpr.2.Var}
\chunkCmdUseMark{EHTy.2.tyEnsureNonAny}

For tuples we again make use of the fact that the |Con| alternative will always
represent a tuple.
%if not incl05
When datatypes are introduced (not part of \thispaper) this will no longer be the case.
%else
From \chapterRef{ehc5} when datatypes are introduced
and onwards this will no longer be the case.
%endif
Here, we already make the required \ruleRef{p-con2} more general
than is required here because we already prepare for datatypes.

A pattern can be represented by a function |sigma -> (sigma1,...)| taking a value of some type |sigma| and
dissecting it into a tuple |(sigma1,...)| containing all its constituents.
For now, because we have only tuples to dissect, the
function returned by the |Con| alternative is just the identity
on tuples of the correct size.
The application \ruleRef{p-app2} consumes an element of this tuple representing
the dissected value and uses it for checking and inferring the constituent.

The implementation of this representation convention returns the dissecting function type
in |patFunTy|:

\chunkCmdUseMark{EHInferPatExpr.2.patFunTy}

The dissecting function type |patFunTy| is constructed from fresh type variables.
Each occurrence of a tuple pattern deals with different unknown types and hence fresh type variables are needed.
The availability of polymorphism in later versions of EH allows us to describe this in a more general way.

At |AppTop| of |PatExpr| the function type |sigma -> (sigma1,...)| describing the dissection is split into
the type |sigma| (attribute |knResTy|) of the pattern and the tuple type |(sigma1,...)| (attribute |knProdTy|)
holding its constituents.
The distribution of the types of the fields of |knProdTy| was described in the previous version of EH.

\chunkCmdUseOnPrev{EHInferPatExpr.1.knTy.App}{EHInferPatExpr.2.knTy}

Finally, the type itself and additional constraints are returned:

\chunkCmdUseMark{EHInferPatExpr.2.Rest}

The careful reader may have observed that the direction of |<=|
for fitting actual (synthesized, bottom-up) and known type (inherited, top-down)
is the opposite of the direction used for expressions.
This is a result of a difference in the meaning of an expression and a pattern.
An expression builds a value from bottom to top as seen in the context of an abstract syntax
tree.
A pattern dissects a value from top to bottom.
The flow of data is opposite, hence the direction of |<=| too.

\subsection{Declarations}

Again, at the level of declarations all is tied together.
Because we first gather information about patterns and then about expressions
two separate threads for gathering constraints are used, |patTyCnstr|
and |tyCnstr| respectively.

\chunkCmdUseOnPrev{EHInfer.1.Let}{EHInfer.2.Let}
\chunkCmdUseOnPrev{EHInfer.1.valGam,EHInfer.1.patValGam}{EHInfer.2.Rest}

If a type signature has been given it is used as the known type for both
expression and pattern. If not, the type of a pattern is used as the known type for an expression.

\chunkCmdUseOnPrev{EHInfer.1.tyInstKnown}{EHInfer.2.tyInstKnown}

\frame<presentation>
{
\frametitle{Declarations}
\begin{itemize}
\item Parallel |AGPat(Multipass)| to |valGam| and |patValGam| for
constraints |tyCnstr| and |patTyCnstr|
\chunkCmdFrameUse{EHInfer.2.Let}
\chunkCmdFrameUse{EHInfer.2.Rest}
\item Patterns are also used to extract type information (omitted)
\end{itemize}
}

%if inclOmitted
\subsection{Omitted}
Details concerning new errors (occur check), gathering of errors, and pretty printing
have been omitted.

Generation of unique identifiers has been omitted here, but
can be found in \appRef{app-ag-pattern-uid}.
%endif

\frame<presentation>[plain]
{
\frametitle{Partial type signatures}
\begin{itemize}
\item ``Impact analysis'' of change
\item Abstract syntax
\chunkCmdFrameUse{EHAbsSyn.2.TyExpr}
\item Parser for type expressions (|pTyExprBase|)
\chunkCmdFrameUse{EHParser.2.pTyExprBase}
\item (scanner configuration omitted)
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Partial type signatures}
\begin{itemize}
\item Type of
\chunkCmdFrameUse{EHInferTyExpr.2.tyVarGather}
\chunkCmdFrameUse{EHInferTyExpr.2.ty}
\item Pretty printing, |pp| part
\chunkCmdFrameUse{EHPretty.2.Wild.pp}
\item Uniq
\chunkCmdFrameUse{EHUniq.2.Wild}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Partial type signatures}
\begin{itemize}
\item Gather wild type vars
\chunkCmdFrameUse{EHInferTyExpr.2.tyVarWildL}
\item Distributing final type info
\chunkCmdFrameUse{EHInfer.2.finValGam}
\end{itemize}
}

\subsection{Partial type signatures: a test case for extendibility}
\label{ehc2partial-sig}

Partial type signatures allow the programmer to specify only a part of a type 
in a type signature. The description of the implementation of this feature is
separated from the discussion of other features to show the effects of an additional
feature on the compiler.
In other words, the following is an impact analysis.

First, both abstract syntax and the parser
%if not incl01TopicParsing
(not included in \thispaper)
%endif
contain an additional alternative for parsing the "@...@" notation
chosen for unspecified type information designated by |Wild| for wildcard:

\chunkCmdUseMark{EHAbsSyn.2.TyExpr}
%if incl01TopicParsing
The parser for |pTyExprBase| needs an additional alternative:
\chunkCmdUseMark{EHParser.2.pTyExpr}
%endif

A wildcard type is treated in the same way as a type variable as it also represents unknown 
type information:

\chunkCmdUseMark{EHInferTyExpr.2.tyVarGather}
\chunkCmdUseMark{EHInferTyExpr.2.ty}

Changes also have to be made to the omitted parts of the implementation, in particular the pretty printing
of the AST
and generation of unique identifiers.
We mention the necessity of this but omit the relevant code.

The pretty printing of a type signature is enhanced a bit further by either printing the type signature
(if no wildcard types are present in it)
or by printing the type of the type signature combined with all found constraints.
The decision is based on the presence of wildcard type variables in the type signature:

\chunkCmdUseMark{EHInferTyExpr.2.tyVarWildL}

The set of all constraints is retrieved at the root of the AST and passed back into the tree:

\chunkCmdUseMark{EHInfer.2.finValGam}

%endif %% incl02

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl03

\section{EH 3: polymorphic type inferencing}
\label{ehc3}

\frame<presentation>{\tableofcontents[current,hidesubsections]}

The third version of EH adds polymorphism, in particular so-called parametric polymorphism
which allows functions to be used on arguments of differing types.
For example
\begin{code}
%%srcfile(afp-eh/04.eh%%)
\end{code}
gives |v :: %%3file(afp-eh/04.eh%%)|
and |id :: %%3(let id = \x -> x in id%%)|.
The polymorphic identity function |id| accepts a value of any type |a|,
giving back a value of the same type |a|.
Type variables in the type signature are used to specify polymorphic types.
Polymorphism of a type variable in a type is made explicit in the type
by the use of a universal quantifier @forall@, or |forall|.
The meaning of this quantifier is that a value with a universally quantified
type can be used with different types for the quantified type variables.

\TBD{more on this..., mention: specialization, instantiation, impredicativeness}

\frame<presentation>
{
\frametitle{Polymorphism}
\begin{itemize}
\item Function may be applied to arguments of different types
\item E.g.
\SafeCode{%
\begin{code}
%%srcfile(afp-eh/04.eh%%)
\end{code}
}
\item |a -> a| means |forall a . a -> a|
\end{itemize}
}

The type signature may be omitted, and in that case the same type will still be inferred.
However, the reconstruction of the type of a value for which
the type signature is omitted has its limitations,
the same as for Haskell98 \cite{peytonjones03has98-rev-rep}.
Haskell98 also restricts what can be described by type signatures.

Polymorphism is allowed for identifiers bound by a |let|-expression,
not for identifiers bound by another mechanism such as parameters of a
lambda expression.
The following variant of the previous example is therefore not to be considered
correct:

\begin{code}
%%3srcfile(test/3-mono-arg.eh%%)
\end{code}

It will give the following output:

\begin{TT}
%%3ppfile(test/3-mono-arg.eh%%)
\end{TT}

The problem here is that the polymorphism of |f| in |a| means that the caller
of |f| can freely choose what this |a| is for a particular call.
However, from the viewpoint
of the body of |f| this limits the choice of |a| to no choice at all.
If the caller has all the freedom to make the choice, the callee has none.
In our implementation this is encoded as a type constant @c_@ chosen for |a| during type checking
the body of |f|.
This type constant by definition is a type a programmer can never define or denote.
The consequence is that an attempt to use |i| in the body of |f|, which has type @c_..@| -> |@c_..@ cannot
be used with an |Int|.
The use of type constants will be explained later.

\frame<presentation>
{
\frametitle{Polymorphism}
\begin{itemize}
\item Arguments may not be polymorphic
\item Incorrect:
\SafeCode{%
\begin{code}
%%3srcfile(test/3-mono-arg.eh%%)
\end{code}
}
\item Incorrect:
\SafeCode{%
\begin{code}
%%3srcfile(test/3-mono-arg3.eh%%)
\end{code}
}
\item Only identifiers bound by |let|
\end{itemize}
}

Another example of the limitations of polymorphism in this version of EH is the following
variation:
\begin{code}
%%3srcfile(test/3-mono-arg2.eh%%)
\end{code}
for which the compiler will infer the following types:
\begin{TT}
%%3ppfile(test/3-mono-arg2.eh%%)
\end{TT}

\frame<presentation>
{
\frametitle{Polymorphism and inferencing}
\begin{itemize}
\item<+-> Type inferencer deduces polymorphism at |let| bindings only
\item Bindings can only be polymorphically used in body of |let|:
\SafeCode{%
\begin{code}
%%3srcfile(test/3-mono-arg2.eh%%)
\end{code}
}
\item Type of \alert<+>{|f :: %%3file(test/3-mono-arg2.eh%%)|}
\onslide<+->
\item If used in same group of bindings:
\SafeCode{%
\begin{code}
%%3srcfile(test/3-mono-arg4.eh%%)
\end{code}
}
\item Type of \alert<+>{|f :: %%3file(test/3-mono-arg4.eh%%)|}
\item<+-> Analysis per \emph{binding group}
\end{itemize}
}

EH version 3 allows parametric polymorphism but not yet polymorphic parameters.
The parameter |i| has a monomorphic type, which is made even more clear when
we make an attempt to use this |i| polymorphically in:
\begin{code}
%%3srcfile(test/3-mono-arg3.eh%%)
\end{code}
about which the compiler will complain:
\begin{TT}
%%3ppfile(test/3-mono-arg3.eh%%)
\end{TT}

Because |i| is not allowed to be polymorphic it can either be used on |Int| or |Char|, but
not both.

These problems can be overcome by allowing higher ranked polymorphism in type signatures.
Later versions of EH deal with this problem%
%if incl04
(\chapterRef{ehc4}).
%else
, but this is not included in \thispaper.
%endif
This version of EH resembles Haskell98 in these restrictions.

%if False
The ``monomorphic parameter'' problem could have been solved by allowing a programmer to explicitly specify a
|forall| for the type of the parameter |i| of |f|.
The type of |f| would then be |(forall a . a -> a) -> (Int,Char)| instead of
|forall a . (a -> a) -> (Int,Char)|.
In this version of EH (resembling Haskell98) it
is not permitted to specify polymorphism explicitly,
but the next version of EH does permit this.
%endif

The reason not to allow explicit types to be of assistance to the type inferencer
is that Haskell98 and this version of EH have as a design principle
that all explicitly specified types in a program are redundant.
That is, after removal of explicit type signatures,
the type inferencer can still reconstruct all types.
It is guaranteed that all reconstructed
types are the same as the removed signatures or more general, that is,
the type signatures are a special case of the inferred types.
This guarantee is called the principal type property
\cite{damas82principal-type,milner78type-poly,hindley69princ-type}.
However, type inferencing also has its limits\TBD{[cite...]}.
In fact, the richer a type system becomes, the more difficult it is for a type inferencing algorithm
to make the right choice for a type without the programmer specifying additional helpful type information.

\subsection{Type language}

The type language for this version of EH adds quantification by means of the universal quantifier |forall|:
\begin{code}
sigma  =  Int | Char
       |  (sigma,...,sigma)
       |  sigma -> sigma
       |  tvarv | tvarf
       |  forall ^ alpha . sigma
\end{code}

\frame<presentation>
{
\frametitle{Type language}
\begin{itemize}
\item Additional variant for |forall|
\SafeCode{%
\begin{code}
sigma  =  Int | Char
       |  (sigma,...,sigma)
       |  sigma -> sigma
       |  tvarv | tvarf
       |  forall ^ alpha . sigma
\end{code}
}
\onslide
\item AG:
\chunkCmdFrameUse{EHTyAbsSyn.3}
\item (more about |tvarf =@= Fixed| later)
\end{itemize}
}

A |tvarf| stands for
a fixed type variable,
a type variable which may not be constrained but still stands for
an unknown type.
A |tvarv| stands for a plain type variable as used in the previous EH version.
A series of consecutive quantifiers in |forall ^ alpha1 . forall ^ alpha2 . ... sigma|
is abbreviated to |forall ^ Vec(alpha) . sigma|.

The type language suggests that a quantifier may occur anywhere in a type.
This is not the case, quantifiers may only be on the top of a type;
this version of EH takes care to ensure this.
A second restriction is that quantified types
are present only in a |Gamma| whereas no |forall|'s are
present in types used throughout type inferencing expressions and patterns.
This is to guarantee the principle type property. \TBD{more refs [..]}

The corresponding abstract syntax for a type needs
additional alternative to represent a quantified type.
For a type variable we also have to remember to which category it belongs,
either \IxAsDef{plain} or \IxAsDef{fixed}:

\chunkCmdUseMark{EHTyAbsSyn.3}

together with convenience functions for constructing these types:

\chunkCmdUseMark{EHTy.3.mkTyVar}
\chunkCmdUseMark{EHTy.3.mkTyQu}

We will postpone the discussion of type variable categories
until \secRef{ehc3instantiation}.

The syntax of this version of EH only allows type variables to be specified as part
of a type signature.
The quantifier |forall| cannot be explicitly denoted.
We only need to extend the abstract syntax for types with an alternative for type variables:

\chunkCmdUseMark{EHAbsSyn.3}

%if incl01TopicParsing
As a consequence the parser for type expressions has to include an alternative
in |pTyExprBase| to parse type variables.

\chunkCmdUseMark{EHParser.3}
%else
%endif

\subsection{Type inferencing}

Compared to the previous version the type inferencing process does not change much.
Because types used throughout the type inferencing of expressions and patterns
do not contain |forall| quantifiers, nothing has to be changed there.

Changes have to be made to the handling of declarations and identifiers though.
This is because polymorphism is tied up with the way identifiers for
values are introduced and used.

\frame<presentation>
{
\frametitle{Type inference}
\begin{itemize}
\item Type inferencing still only infers monomorphic types
\item For which polymorphism may be concluded at |let| bindings
\begin{itemize}
\item Stored as quantified type (aka \emph{type scheme}) in |Gamma|
\item When retrieved from |Gamma| immediately unquantified/instantiated:
\[
\rulerCmdUse{rules.expr2.3.e-ident3}
\]
\end{itemize}
\item Standard Hindley/Milner type inferencing
\item (other rules and code omitted)
\end{itemize}
}

\rulerCmdUse{rules.expr2.3}

A quantified type, also often named \IxAsDef{type scheme},
is introduced in \ruleRef{e-let3} and \ruleRef{e-let-tysig3} and instantiated
in \ruleRef{e-ident3}, see \figRef{rules.expr2.3}.
We will first look at the \IxAsIs{instantiation}.

\subsubsection{Instantiation}
\label{ehc3instantiation}

A quantified type is introduced in the type inferencing process whenever a value identifier having that type is occurs
in an expression:

\chunkCmdUseOnPrev{EHInferExpr.1.Var,EHInferExpr.2.Var}{EHInferExpr.3.Var}

We may freely decide what type the quantified type variables may have as long
as each type variable stands for a monomorphic type.
However, at this point it is not known which type a type variable
stands for, so fresh type variables are
used instead.
This is called \IxAsDef{instantiation}, or \IxAsIs{specialization}.
The resulting instantiated type partakes in the inference process as usual.

The removal of the quantifier and replacement of all quantified type variables
with fresh type variables
is done by |tyInst|:

\chunkCmdUseMark{EHTyInstantiate.3.tyInst}

Function |tyInst| strips all quantifiers and substitutes the quantified type variables with fresh ones.
It is assumed that quantifiers occur only at the top of a type.

\subsubsection{Quantification}

The other way around, quantifying a type, happens when a type is
bound to a value identifier and added to a |Gamma|.
The way this is done varies with the presence of a type signature.
\RuleRef{e-let3} and \ruleRef{e-let-tysig3} (\figRef{rules.expr2.3})
specify the respective variations.

A type signature itself is specified without explicit use of quantifiers.
These need to be added for all introduced type variables, except the ones specified
by means of `@...@' in a partial type signature:

\chunkCmdUseOnPrev{EHInfer.1.gathTySigGam}{EHInfer.3.TySig}

A type signature simply is quantified over all free type variables in the type using

\chunkCmdUseMark{EHTyQuantify.3.tyQuantify}
% \chunkCmdUseMark{EHTyQuantify.3.tyQuantifyClosed}

Type variables introduced by a wildcard may not be quantified over because the type inferencer will fill in the
type for those type variables.

We now run into a problem which will be solved no sooner than the next version of EH.
In a declaration of a value (variant |Val| of |Decl|)
the type signature acts as a known type |knTy| against which checking of the value expression takes place.
Which type do we use for that purpose, the quantified |sigTy| or the unquantified |tyExpr.ty|?
\begin{itemize}
\item
Suppose the |tyExpr.ty| is used.
Then, for the erroneous
\begin{code}
let  id :: a -> a
     id = \x -> 3
in   ...
\end{code}
we end up with fitting |tvarv1 -> Int <= a -> a|.
This can be accomplished via constraints |[tvarv1 :-> Int, a :-> Int]|.
However, |a| was supposed to be chosen by the caller of |id|.
Now it is constrained by the body of |id| to be an |Int|.
Somehow constraining |a| whilst being used as part of a known type for the body of
|id| must be inhibited.
\item
Alternatively, |sigTy| may be used.
However, the inferencing process and the fitting done by |fitsIn| cannot (yet) handle
types with quantifiers.
\end{itemize}

For now, this can be solved by replacing all quantified type variables of a known type
with type constants:

\chunkCmdUseOnPrev{EHInfer.1.tyInstKnown}{EHInfer.3.tyInstKnown}

by using a variant of |tyInst|:

\chunkCmdUseMark{EHTyInstantiate.3.tyInstKnown}

This changes the category of the fresh type variable replacing the quantified type variable to `fixed'.
A \IxAsDef{fixed type variable} is like a plain type variable but may not be constrained,
that is, bound to another type.
This means that |fitsIn| has to be adapted to prevent this from happening.
The difference with the previous version only lies in the handling of type variables.
Type variables now may be bound if not fixed, and to be equal only if their categories match too.
For brevity the new version of |fitsIn| is omitted.


\subsubsection{Generalization/quantification of inferred types}

How do we determine if a type for some expression bound to an identifier in
a value declaration is polymorphic?
If a (non partial) type signature is given, the signature itself describes the
polymorphism via type variables explicitly.
However, if for a value definition a corresponding type signature is missing,
the value definition itself gives us all the information we need.
We make use of the observation that a binding for a value identifier
acts as a kind of boundary for that expression.
\begin{code}
let  id = \x -> x
in   ...
\end{code}
The only way the value associated with |id| ever will be used outside the
expression bound to |id|, is via the identifier |id|.
So, if the inferred type |tvarv1 -> tvarv1| for the expression |\x -> x| has free type variables
(here: |[tvarv1]|)
and these type variables are not used in the types of other bindings, in particular those
in the global |Gamma|,
we know that the expression |\x -> x| nor any other type will constrain those free type variables.
The type for such a type variable apparently can be freely chosen by
the expression using |id|, which is exactly the meaning
of the universal quantifier.
These free type variables are the candidate type variables over which quantification can take place,
as described by the typing rules for |let|-expressions in \figRef{rules.expr2.3} and its implementation:

\chunkCmdUseOnPrev{EHInfer.1.Let,EHInfer.2.Let}{EHInfer.3.Let}

\frame<presentation>[plain]
{
\frametitle{Declarations (example of incremental AG)}
\chunkCmdUsePrevLit{EHInfer.1.Let}
\chunkCmdUsePrevLit{EHInfer.2.Let}
\chunkCmdFrameUse{EHInfer.3.Let}
}


All available constraints in the form of |decls.tyCnstr| are applied to both global (|gValGam|)
and local (|lValGam|) |Gamma|.
All types in the resulting local |lSubsValGam| are then quantified over their free type variables,
with the exception of those referred to more globally, the |gTyTvL|.
We use |valGamQuantify| to accomplish this:

\chunkCmdUseMark{EHGam.3.valGamQuantify}
\chunkCmdUseMark{EHGam.3.valGamMapTy}
\chunkCmdUseMark{EHGam.3.gamMap}
\chunkCmdUseMark{EHGam.3.gamMapElts}

The condition that quantification only may be done for type variables not occurring in
the global |Gamma| is a necessary one.
For example:
\begin{code}
%%3srcfile(test/3-mono-glob.eh%%)
\end{code}

If the type |g :: a -> (a,a)| would be concluded, |g| can be used with |y| an |Int| parameter, as
in the example. Function |f| can then be used with |x| a |Char| parameter.
This would go wrong because |h| assumes the types of its parameters |x| and |y| are equal.
So, this justifies the error given by the compiler for this version of EH:

\begin{TT}
%%3ppfile(test/3-mono-glob.eh%%)
\end{TT}

All declarations in a |let|-expression together form what in Haskell is called a binding group.
Inference for these declarations is done together and all the types of all identifiers
are quantified together. The consequence is that a declaration that on its own would be polymorphic,
may not be so in conjunction with an additional declaration which uses the previous declaration:

\begin{code}
%%3srcfile(test/3-mono-bind.eh%%)
\end{code}

The types of the function |id1| and value |v1| are inferred in the same binding group.
However, in this binding group the type for |id1| is |tvarv1 -> tvarv1| for some type variable |tvarv1|,
without any quantifier around the type.
The application |id1 3| therefore infers an additional constraint |tvarv1 :-> Int|, resulting
in type |Int -> Int| for |id1|

\begin{TT}
%%3ppfile(test/3-mono-bind.eh%%)
\end{TT}

On the other hand, |id2| is used after quantification, outside the binding group, with
type |forall a . a -> a|.
The application |id2 3| will not constrain |id2|.

In Haskell binding group analysis will find groups of mutually dependent definitions,
each of these called a binding group. These groups are then ordered
according to ``define before use'' order.
Here, for EH, all declarations in a |let|-expression
automatically form a binding group, the ordering of two binding groups |d1| and |d2| has
to be done explicitly using sequences of |let| expressions: |let d1 in let d2 in ...|.

Being together in a binding group can create a problem for inferencing mutually recursive definitions,
for example:

\begin{code}
%%3srcfile(test/3-mut-rec.eh%%)
\end{code}

This results in

\begin{TT}
%%3ppfile(test/3-mut-rec.eh%%)
\end{TT}

For |f1| it is only known that its type is |tvarv1 -> tvarv2|.
Similarly |g1| has a type |tvarv3 -> tvarv4|.
More type information cannot be constructed unless more information
is given as is done for |f2|.
Then also for |g2| may the type |forall a.a->a| be reconstructed.

\TBD{example polymorphic recursion}

\subsubsection{Type expressions}

Finally, type expressions need to return a type where all
occurrences of type variable names (of type |HsName|) coincide with type variables (of type |TyVarId|).
Type variable names are identifiers just as well so 
a |TyGam| similar to |ValGam|
is used to map type variable names to freshly created type variables.

\chunkCmdUseMark{EHInferTyExpr.3}

Either a type variable is defined in |tyGam|, in that case the type bound to the identifier is used,
otherwise a new type variable is created.

%if inclOmitted
\subsection{Omitted}
Details concerning pretty printing
have been omitted.

Generation of unique identifiers for type variables
in a type for pretty printing
has been omitted here, but
can be found in \appRef{app-ag-pattern-uid}.
%endif

%if not omitLitDiscuss
\subsection<article>{Literature}

\TBD{}

Polymorphic recursion
\cite{kfoury93recursivetype,hallet04polyrec-ex,vasconcellos03polyrec-impl,henglein91polyrec-infer,figueiredo01polyrec-princ}
%endif


%endif %% incl03



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl04

\section{EH 4: |forall| and |exists| everywhere}
\label{ehc4}

\frame<presentation>{\tableofcontents[current,hidesubsections]}

This version of EH adds explicit types with quantifiers at all positions in a type,
existential quantification and an interpretation of unquantified types to
quantified types.

\paragraph{Higher ranked explicit polymorphism.}
For example in
\begin{code}
%%4srcfile(test/3-poly-rank.eh%%)
\end{code}
|f| has type |f :: %%4file(test/3-poly-rank.eh%%)|,
which means that |i| has type |forall a. a ->a| in the body of |f|.
Therefore |i| can be used polymorphically in the body of |f|.

%{
%format forall1
%format forall2
%format forall3
%format forall4
%format forall5
\frame<presentation>
{
\frametitle{Higher ranked explicit polymorphism}
\begin{itemize}
\item Only if explicitly specified
\SafeCode{%
\begin{code}
%%4srcfile(test/3-poly-rank.eh%%)
\end{code}
}
\item Quantifier may be at any position in type
\SafeCode{%
\begin{code}
forall1 a . a -> (forall2 b . b -> b)
  -> ((forall3 c . c -> c) -> (forall4 d . d -> d))
  -> forall5 e . e -> e -> a
\end{code}
}
\begin{itemize}
\item |rank = 1|: |forall1|, |forall5|
\item |rank = 2|: |forall2|, |forall4|
\item |rank = 3|: |forall3|
\end{itemize}
\item Rank: ...
\item No type inferencing to infer higher ranked types, only ``not forgetting'' quantifier information
\end{itemize}
}

The quantifier |forall| in the type of |f| is on a so called higher ranked position
in the type, rank 1 being in front of the type or in front of a result of a function type.
A rank 2 position is in front of the argument of a function type.
Higher ranked positions are defined recursively by the same definition.
So, in
\begin{code}
forall1 a . a -> (forall2 b . b -> b) -> ((forall3 c . c -> c) -> (forall4 d . d -> d)) -> forall5 e . e -> e -> a
\end{code}
|forall1| and |forall5| are on a rank 1 position, |forall2| and |forall4| on rank 2 and |forall3| on rank 3.
%}

Standard Hindley-Milner inferencing as described for the previous version of EH can infer rank 1 universal quantifiers.
We will not do better than that, but the inferencer described for this version of EH will
not throw away any type information about higher ranked quantifiers defined via a type signature.
No attempt is made to be smart in reconstructing higher ranked types, only smartness is
implemented by not forgetting higher ranked type information.

\paragraph{Existentially quantified types.}
Quantifiers on higher ranked positions are also necessary to make existential types useful

\begin{code}
%%4srcfile(test/4-demo1.eh%%)
\end{code}

An existentially quantified type \Ix{existential quantification}
is specified with keyword @exists@, or |exists|.
The type variable which is existentially quantified represents a type but we
do not know which one.
Existential quantification hides, or forgets, more specific type information.
In the given example |xy| is a tuple for which we have forgotten
that its first component is an |Int|.
All we know is that we can apply the second component to the first component
(as done by |ixy|).
This constraint is not upheld for |pq|, so an error is produced:

\begin{TT}
%%4ppfile(test/4-demo1.eh%%)
\end{TT}

\frame<presentation>
{
\frametitle{Existential types}
\begin{itemize}
\item ``It exists, but I've forgotten what it was''
\SafeCode{%
\begin{code}
%%4srcfile(test/4-demo1.eh%%)
\end{code}
}
\item |xy| created with |a = Int|, quantifier hides this information
\item |ixy| can still use |xy| because type of |v| matches |f|'s argument type
\end{itemize}
}

\frame<presentation>[containsverbatim,plain]
{
\frametitle{Existential types}
\begin{TT}
%%4ppfile(test/4-demo1.eh%%)
\end{TT}
}

\paragraph{Opening an existentially quantified type.}
The inverse of existential quantification of a type variable is often called `opening'.
It means that we get to know the original type.
This of course cannot \TBD{(???? type carrying code, analysis, etc)} be done
as this information was forgotten in the first place.
Instead, the compiler `invents' a fresh new type, a type constant,
which acts as a placeholder for the opened type variable.
This fresh type is guaranteed to be different from any type the programmer can construct.
Only if functions accepting parameters of this type are available anything useful can be done with it,
as in the example.

It also means that we can create values with which we cannot do much useful.
For example

\begin{code}
%%4srcfile(test/4-ex-extr3.eh%%)
\end{code}

gives

\begin{TT}
%%4ppfile(test/4-ex-extr3.eh%%)
\end{TT}

\frame<presentation>[containsverbatim,plain]
{
\frametitle{Opening an existential}
\begin{itemize}
\item Existential is opened/instantiated when bound to an identifier
\begin{itemize}
\item Often done explicitly via language construct
\item Instantiated type variables are fresh type constants
\end{itemize}
\item Once opened, always open
\SafeCode{%
\begin{code}
%%4srcfile(test/4-ex-extr3.eh%%)
\end{code}
}
\item
\begin{TT}
%%4ppfile(test/4-ex-extr3.eh%%)
\end{TT}
\end{itemize}
}

Both examples also demonstrate the place where opening an existentially quantified type is done.
Opening an existential type is done when the type is bound to an identifier.
However, only the type variables for the top level existential quantifiers are opened.
If an existentially quantified type is buried in a composite type it will only be opened if
bound to a value identifier.
For example:

\begin{code}
%%4srcfile(test/4-ex-extr4.eh%%)
\end{code}

gives

\begin{TT}
%%4ppfile(test/4-ex-extr4.eh%%)
\end{TT}

\frame<presentation>[containsverbatim]
{
\frametitle{Opening an existential}
\begin{itemize}
\item Nested existentials are retained
\SafeCode{%
\begin{code}
%%4srcfile(test/4-ex-extr4.eh%%)
\end{code}
}
\item
\begin{TT}
%%4ppfile(test/4-ex-extr4.eh%%)
\end{TT}
\end{itemize}
}

Also, opening the same type twice, as done in the given example for |v1|,
will twice give fresh type constants too.

This behavior simplifies the use of existential types in that no additional
language construct for opening is necessary.
%if not omitTBD
More about this in \TBD{[literature ...]}.
%endif

\paragraph{Guessing locations for quantifiers.}
Quantifiers need not always be specified.
For example in

\begin{code}
%%4srcfile(test/4-ex-extr2.eh%%)
\end{code}

no quantifiers are specified in the type signatures, for |ixy| a type signature is even absent.
The following interpretation of the meaning of a type is used to determine
where a quantifier should be inserted when a type is quantified.

\begin{itemize}
\item
If a type variable |a| occurs somewhere in |sigma1| and |sigma2| in |sigma1 -> sigma2| but not outside the function type,
|a| will be universally quantified, i.e. with |forall|.
\item
If a type variable |a| occurs somewhere in |sigma1| and |sigma2| in |(sigma1,sigma2)| but not outside the tuple type,
|a| will be existentially quantified, i.e. with |exists|.
\end{itemize}

The idea is that the first rule covers the notion that if an |a| is passed in and comes out a function,
this function will work for all |a|, hence the universal quantification.
On the other hand, the second rule covers the notion that if an |a| is stored together with another and nothing
more is known about |a| we might as well hide |a|, hence the existential quantification.
More rules are needed but we will look into this further when we look at the implementation.

For the given example the following will be concluded

\begin{TT}
%%4ppfile(test/4-ex-extr2.eh%%)
\end{TT}

\frame<presentation>
{
\frametitle{Quantifier location}
\begin{itemize}
\item Notational sugar/convention
\SafeCode{%
\begin{code}
%%4srcfile(test/4-ex-extr2.eh%%)
\end{code}
}
\item Interprets type structure to find suitable location for quantifier
\begin{itemize}
\item |a| occurs in |sigma1| and |sigma2| in |sigma1 -> sigma2| and not outside: |forall|
\item |a| occurs in |sigma1| and |sigma2| in |(sigma1,sigma2)| and not outside: |exists|
\end{itemize}
\item For type signatures and inferred types
\end{itemize}
}

\frame<presentation>[containsverbatim,plain]
{
\frametitle{Quantifier location}
\begin{TT}
%%4ppfile(test/4-ex-extr2.eh%%)
\end{TT}
}

Note that an explicit type is needed to hide type information (as for |xy|),
but is not required to pass it to a function expecting an existential.

\paragraph{Outline of the rest.}
First we will look at the type structure (\secRef{ehc4-type-lang}).
The |fitsIn| function will have to be redesigned almost completely (\secRef{ehc4-fitsin}),
partly because of the presence of quantifiers everywhere,
partly because |fitsIn| now really is asymmetric because (e.g.)
forgetting type information is one-way only.
Quantification as well as instantiation become more complicated because
of the presence of quantifiers anywhere in a type (\secRef{ehc4-quant}, \secRef{ehc4-inst}).
Finally type inferencing expressions and patterns will have to be modified (\secRef{ehc4-ty-infer}),
luckily not much because the hard work is done in |fitsIn|.

\subsection{Type language}
\label{ehc4-type-lang}

The type language for this version of EH adds quantification with the existential quantifier |exists|
\begin{code}
sigma  =  Int | Char | tcon
       |  (sigma,...,sigma)
       |  sigma -> sigma
       |  tvar
       |  Qu alpha . sigma, Qu `elem` {forall, exists}
tvar   =  tvarv | tvarf
\end{code}
We also need an infinite supply of type constants |tcon|.
Quantifiers |Qu| may now appear anywhere in a type.

The |Quant| alternative of the type structure has to made more general
to be able to encode |exists| too.

\chunkCmdUseMark{EHTyAbsSyn.4}

Some additional functions on quantifiers |TyQu| are defined here too.
These may seem unnecessary but the extra layer of abstraction
is convenient when the range of quantifiers is extended
%if not incl06
later on (not included in \thispaper):
%else
in \chapterRef{ehc6}:
%endif

\chunkCmdUseMark{EHTy.4.tyquMisc}

These functions inquire the kind of quantifier and flip between universal and existential quantifier.

\subsection{Fitting, subsumption}
\label{ehc4-fitsin}

First we will look at the issues arising with the presence of quantifiers in a type.
Next we will look at the implementation.

\subsubsection{Issues}

\paragraph{|<=| is a partial ordering.}
With the presence of quantifiers during type inferencing,
the function |fitsIn|, implementating the so called \IxAsDef{subsumption}
relation |<=| between types, now
becomes a partial ordering on types.
For example,
\label{ehc4-exists-sub}
\begin{code}
%%srcfile(test/4-exists-sub.eh%%)
\end{code}

makes the type inferencer check |Int <= exists a . a|,
if the type of the actual value |3| fits in the specified type |exists a . a|.
This corresponds to the flow of |3| into some location, which in turn is then later on used elsewhere.
This checks out well, but not the other way around because we cannot
assume that some unknown type is an |Int|.

Similarly for universal quantification
the check |forall a . a -> a <= Int -> Int|
holds as we can use a more generally applicable function |id|
in the more restricted required setting |Int -> Int|.
\label{ehc4-forall-sub}
\begin{code}
%%srcfile(test/4-forall-sub.eh%%)
\end{code}

Losing generality, or specialization/instantiation to a specific type is ok,
but the other way around, for the definition of |id| it is not.

So, in a nutshell
\[
|forall a . a <= sigma <= exists a . a|
\]
meaning that a universally quantified type can be used at
a place where any other type is expected, say some type |sigma| (e.g. an |Int|),
which in turn can be used at a place where all is forgotten about a type.
Or, in other words, first we choose some type for |a|, then we forget this choice.

Let us look at some other examples to get a better idea of what |fitsIn =@= <=|
has to deal with.

\frame<presentation>
{
\frametitle{Changes to the inferencing}
\begin{itemize}
\item Quantified types are instantiated as late as possible
\begin{itemize}
\item In order to preserve type information
\end{itemize}
\item Quantified types are encountered during type inferencing/checking
\begin{itemize}
\item |fitsIn =@= <=| compares types with quantifiers
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Issue: asymmetry |fitsIn =@= <=|}
\begin{itemize}
\item Forgetting type information is ok, but irreversible
\SafeCode{%
\begin{code}
%%srcfile(test/4-exists-sub.eh%%)
\end{code}
}
\item Check done
\begin{itemize}
\item |Int <= exists a . a| (for |v1|): ok
\item |exists a . a <= Int| (for |v2|): fail
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Issue: asymmetry |fitsIn =@= <=|}
\begin{itemize}
\item Instantiation of polymorphism is ok,
but the reverse only if it can be guaranteed no additional
constraints will be found for the quantified type variable
\SafeCode{%
\begin{code}
%%srcfile(test/4-forall-sub.eh%%)
\end{code}
}
\item Check done
\begin{itemize}
\item |forall a . a -> a <= Int -> Int| (for |ii|): ok
\item |Int -> Int <= forall a . a -> a| (for |id|): fail
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Issue: asymmetry |fitsIn =@= <=|}
\begin{itemize}
\item Partial ordering of types where
\[
|forall a . a <= sigma <= exists a . a|
\]
\item |Bot :: forall a . a|
\begin{itemize}
\item @undefined@|= Bot|
\item Whatever the expected type, a |forall a . a| can be used
\item But its use leads to program crash
\end{itemize}
\item |Top :: exists a . a|
\begin{itemize}
\item Represents the most specific type
\item If the expected type expects nothing, a |exists a . a| can be used
\item But nothing can be done with it
\end{itemize}
\end{itemize}
}

\paragraph{Impredicativeness.}
The following example (from Botlan \cite{botlan03ml-power-f})

\label{ehc4-choose}
\begin{code}
%%srcfile(test/4-choose.eh%%)
\end{code}

demonstrates a choice we can make.
This choice coincides with the question what the type of |v| is.
\begin{itemize}
\item
|v :: forall b . (b -> b) -> b -> b|.
This is Haskell's answer.
This answer is given because all types are monomorphic, that is,
without quantifiers during inferencing.
The type variable |a| of the type of |choose| is bound to the instantiated
type if |id|.
So, |id| is instantiated to |tvarv1 -> tvarv1|, giving |choose id :: (tvarv1 -> tvarv1) -> tvarv1 -> tvarv1|
resulting in the given quantified type.
Function |v| can safely be applied to a function of type |Int -> Int|.
\item
|v :: (forall c . c -> c) -> (forall d . d -> d)|.
This is system-F's answer \cite{reynolds74type-struct-sysF}.
This answer is given because the type variable |a| of |choose|
is bound to the type of the parameter as it is known, with quantifiers, in its
uninstantiated original form.
Now |v| cannot be applied to a function of type |Int -> Int| because this function is
not general enough to be used as if it were of type |forall a . a -> a|.
Paradoxically enough a more general function will be returned;
this relates to a phenomenon called \IxAsDef{contravariance} which we will discuss later.
\end{itemize}

Allowing type variables to be bound to, or instantiated with,
quantified types is called \IxAsDef{impredicativeness} \TBD{[cite...]}.

The critical observation for this version of EH is that it is difficult \TBD{[cite...]} to
infer that a type variable should be bound to
a quantified type,
but that it is relatively easy not to forget that a type is quantified if
we already knew in the first place that it was quantified.
The latter is what we do except in situations where it would break Haskell's choice,
thereby still inferring types in a standard Hindley-Milner way but
using |fitsIn =@= <=| to allow richer types still to match properly.

\frame<presentation>
{
\frametitle{Impredicativeness}
\begin{itemize}
\item Quantified types throughout type inferencing can/cannot
be bound to type variables?
\SafeCode{%
\begin{code}
%%srcfile(test/4-choose.eh%%)
\end{code}
}
\item Haskell: |v :: forall b . (b -> b) -> b -> b|
\begin{itemize}
\item |v| can be applied to a |Int -> Int|
\item During type inferencing type variable |b| in |v|'s type
is instantiated and can be freely bound to |Int|
\end{itemize}
\item Impredicative: |v :: (forall c . c -> c) -> (forall d . d -> d)|
\begin{itemize}
\item |v| cannot be applied to a |Int -> Int|
\item Is not polymorphic enough: |Int -> Int </= forall a . a -> a|
\end{itemize}
\end{itemize}
}

\paragraph{Subsumption |<=| needs to instantiate types.}
These examples also demonstrate that |fitsIn| needs to instantiate types.
In the previous version of EH all types partaking in the inference process
were expected to be fully instantiated.
By definition this can no longer be done if as much as possible type information
is to be retained.
Still, at some point instantiation has to be done,
at the latest moment possible.
This latest moment is the place where a type
really is compared with another one, in |fitsIn|.

\paragraph{Subsumption |<=| depends on context.}
|fitsIn| is used as a tool to enforce the |<=| relation between types.
It is used to check the type of an actual parameter with its expected one,
as in the previous example for |choose| (\pageRef{ehc4-choose}).
It is also used to check the type of a value against its known type as in
the earlier example with |ii| and |id| (\pageRef{ehc4-forall-sub}).
However, the precise use of |fitsIn| in these contexts differs slightly
in the way instantiation is done.

\begin{itemize}
\item
For an application |f a| of |f :: sigma2 -> ...| to |a :: sigma1|
we have
to check the type |sigma1| of an actual argument against 
the type |sigma2| of an expected
via |sigma1 <= sigma2|.
As we learned from looking at the |choose| example, Haskell's convention
is to instantiate |sigma1| before binding it to a type variable |tvarv|
in the case |sigma2 == tvarv|.
This information is passed as an additional parameter to |fitsIn|, notated
by |instLFIOpts|, named an \IxAsDef{instantiating context}.
\item
For checking an expression |e :: sigma2| of a declaration |v :: sigma1; v = e| to its known type
|sigma1|
we check |sigma2 <= sigma1|.
In this case we want to avoid instantiating |sigma2|.
The necessity of avoiding this becomes clear if we look at a situation where
|sigma2 == (exists a . a, ...)| and |sigma1 == (tvarv,...)|,
coinciding with a situation where an explicit type signature is absent.
Now, if |exists a . a| is instantiated with type constants before it is bound to |tvarv|
all information about the existential is irretrievably lost,
something we do only when an existential is bound to an identifier.
So, in this case we say that |fitsIn| needs to told
it is checking types in a \IxAsDef{strong context}, notated by |strongFIOpts|.
\item
A third context will also be mentioned here for completeness,
a so called \IxAsDef{weak context} |weakFIOpts|.
It is used whenever an expression can have |>1| alternative expressions as a result,
which is the case for |case|-expressions,
to be dealt with no sooner than the introduction of datatypes in the next version of EH
%if not incl05
(not included in \thispaper).
%else
(\chapterRef{ehc5}).
%endif
\end{itemize}

\label{ehc4-fitsIn-strength}
|fitsIn =@= <=| therefore needs an option |fiopt| to describe these variations
\begin{code}
fiopt  =  strongFIOpts  ^^ -- strong context
       |  instLFIOpts   ^^ -- instantiating context, for expr |App|
       |  instFIOpts    ^^ -- instantiating context, for patterns
\end{code}

These contextual variations actually are
configurations of lowlevel boolean flags for |fitsIn|

\begin{code}
fioBindRFirst   =   fioBindRFirstY       ^^ -- prefer binding of a rhs tvar over instantiating
                |   fioBindRFirstN
fioBindLFirst   =   fioBindLFirstY       ^^ -- prefer binding of a lhs tvar over instantiating
                |   fioBindLFirstN
fioLeaveRInst   =   fioLeaveRInstY       ^^ -- leave rhs (of fitsIn) instantiated
                |   fioLeaveRInstN
\end{code}

where the |+| variants stand for |True|.
A |True| value for the flag |fioBindRFirst| states that in case of |sigma <= tvarv|
a constraint |(tvarv :-> sigma)| will result,
otherwise first |sigma| will be instantiated and |tvarv| be bound to
the instantiated |sigma|.
Similary we have |fioBindLFirst| for |tvarv <= sigma|.
Finally, |fioLeaveRInst| determines if an instantiation done for |sigma|
in |... <= sigma| will return the instantiated |sigma| or |sigma| itself.
Summarizing, |fioBindRFirst| and |fioBindLFirst| turn off greedy
instantiating and |fioLeaveRInst| leaves visible what has been instantiated.

Context variations and these flags relate as follows

\begin{tabular}{llll}
 & |fioBindRFirst| & |fioBindLFirst| & |fioLeaveRInst| \\
\hline
|strongFIOpts| & |fioBindRFirstY| & |fioBindLFirstY| & |fioLeaveRInstN| \\
|instLFIOpts| & |fioBindRFirstN| & |fioBindLFirstY| & |fioLeaveRInstN| \\
|instFIOpts| & |fioBindRFirstN| & |fioBindLFirstN| & |fioLeaveRInstY| \\
\end{tabular}

So, for example the |instFIOpts| context variant used for an expression application
has as its effect that instantiation will be done a la Hindley-Milner.

Finally, all of this is encoded as follows

\savecolumns
\chunkCmdUseMark{EHCommon.4.FIOpts.hd}
\restorecolumns
\chunkCmdUseMark{EHCommon.4.FIOpts.tl}
\chunkCmdUseMark{EHCommon.4.FIOpts.defaults}

\paragraph{Co- and contravariance.}
For tuples the check |(sigma1,sigma2) <= (sigma3,sigma4)|
will break down into |sigma1 <= sigma3| and |sigma2 <= sigma4|
because conceptually a tuple value can be put into
a location if that location expects a tuple and
the elements of the tuple also fit.

For functions this works differently.
Checking |sigma1 -> sigma2 <= sigma3 -> sigma4| means that
a |f :: sigma3 -> sigma4| is expected but a |g :: sigma1 -> sigma2| is available.
This happens for example in

\begin{code}
let  g :: sigma1 -> sigma2
     f :: sigma3 -> sigma4
     f = g
     a :: sigma3
in   f a
\end{code}

So, what happens when |f| is called and what does it mean in terms of types |sigma|?
The caller of |f| in the application |f a| expects that the function |f|
accepts a |sigma3|.
However, |g| is invoked instead, so a value of type |sigma3| is passed to a function
expecting a |sigma1|, which in terms of fitting means |sigma3 <= sigma1|.
The observation here is that the direction of |<=| for fitting the
argument types of |f| and |g| is opposite to the direction
of fitting |f| and |g|.
This behavior is called \IxAsDef{contravariance}.

In general, fitting a composite type breaks down into fitting the components of the composite type.
If the direction of |<=| for fitting a component is the same as for the composite type,
it is said that that component of the type is \IxAsDef{covariant},
if the direction is opposite the component is
\IxAsDef{contravariant}.

The following notation is used to denote this variance

\begin{code}
coco                =   CoVariant           ^^ -- CoVariant
                    |   ContraVariant       ^^ -- ContraVariant
                    |   CoContraVariant     ^^ -- CoContraVariant (both co/contra)
\end{code}

with a corresponding Haskell definition

\chunkCmdUseMark{EHCommon.4.CoContraVariance}

in which the same notation is used for the alternatives of |CoContraVariance|.

\subsubsection{Implementation}

\paragraph{Typing rules.}
Let us look more precisely at |<=| which we now also will
describe with rules,
in \figRef{rules.fit4.quant} and \figRef{rules.fit4.app}.
\RuleRef{f-prod4} and \ruleRef{f-arrow4} both follow the discussion about co- and
contravariance.
These rules are both instances of the by now usual |App| structure which will
be used by |fitsIn|.

The fine detail here lies with \ruleRef{f-arrow4} which specifies
a strong context |strongFIOpts| for fitting its arguments.
This means that for higher ranked positions in a type any implicit
instantiation of types is inhibited, that is, it is not visible
for the inference process.
\TBD{more explanation why...}

\rulerCmdUse{rules.fit4.app}
\rulerCmdUse{rules.fit4.quant}

The rules for quantified types also deserve a bit more attention.

\begin{itemize}
\item
\RuleRef{f-forall-r2} applies in
\begin{code}
%%srcfile(test/4-forall-sub.eh%%)
\end{code}
to the check |Int -> Int <= forall a . a -> a| which has to be done for |id = ii|.
If |forall a . a -> a| is instantiated with type variables |tvarv|,
the check would succeed with a constraint |(tvarv :-> Int)|.
This is not correct.
Recall that by succesfully passing this check |Int -> Int| will be used
as if it were a |forall a . a -> a|, which definitely will not work for
all types.
So, in order to let this check fail we instantiate with the fixed variant |tvarf|
of type variables, indicated by |instf|.
These fixed type variables cannot be further constrained but quantification
over them is allowed (see ...).

\item
Dually, \ruleRef{f-exists-r2} applies in
\begin{code}
let  f :: (exists a . a) -> Int
     v = f 3
in   v
\end{code}
to the check |Int <= exists a . a| for the application |f 3|.
The body of |f| only knows it gets passed a value of some unknown type,
no assumptions about it are made in the body of |f|.
Consequently, type variable may be instantiated with any type by the caller of
|f|.
This is simulated by instantiating with fresh constrainable type variables |tvarv|,
via |instv|.
In that case we also are not interested in any found constraints
concerning the fresh type variables, so these are removed.
\end{itemize}

\paragraph{Co- and contravariance.}
The encoding of co- and contravariance behavior is solved a bit more general then really
is required at this point. The idea is that for a type application |tcon a b ...|
the |tcon| determines how the fitting of its arguments should be done.
For example, for |tcon == ->|,
the first argument should be fitted with the opposite variance and
options |fiopt| should be made strong. This is described via a environment
encoding this information

\chunkCmdUseMark{EHTyFitsInCommon.4.AppSpine}
\chunkCmdUseMark{EHOpts.4.fioMkStrong}
\chunkCmdUseMark{EHGam.4.AppSpineGam}
\chunkCmdUseMark{EHGam.4.appSpineGam}

It also shows that only for function and tuple types we know what to do in such a situation.
Complications in this area will arise with the introduction of datatypes
%if not incl05
later on (not included in \thispaper).
%else
in \chapterRef{ehc5}.
%endif

\paragraph{|fitsIn| parameters and result.}
So, let us know finally look at the implementation of |<=|,
and redo the implementation of |fitsIn|.
First, |fitsIn| needs to pass information both up and downwards.
Upwards was already implemented via

\savecolumns
\chunkCmdUseMark{EHTyFitsInCommon.4.FIOut}
\restorecolumns
\chunkCmdUseMark{EHTyFitsInCommon.4.FIOut.tl}

which is extended with |CoContraVariant| information and threads a UID value
needed for instantiating types together with the downward information stored in

\savecolumns
\chunkCmdUseMark{EHTyFitsIn.4.FIIn.hd}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.FIIn.tl}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.FIIn.emptyFI.hd}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.FIIn.emptyFI.tl}

The parameter |fiCoContra| is used to indicate if the comparison |<=| is flipped.

Finally, an environment is passed to |fitsIn|:

\chunkCmdUseMark{EHTyFitsIn.4.FIEnv}

In this version |FIEnv| acts as a placeholder for use in later versions.
The intention of a |FIEnv| is to pass environmental information needed by |fitsIn|,
usually stored in |Gamma|'s throughout the type rules and attribute grammar implementation.

\paragraph{The fitting.}
The preliminaries of |fitsIn| have not changed much compared to
the previous version (\pageRef{EHTyFitsIn.2.fitsIn.Base}).
All internally defined functions now take an additional top-down |fi :: FIIn|
parameter and some work has to be done for extracting and passing variance information
(in function |res|).

\savecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.Prelim}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.bind}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.allowBind}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.allowImpredTVBind}

The fitting of quantified types uses |unquant| which removes all top level quantifiers.

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.unquant}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.FOUtils}

The instantiation is parameterized a flag |hide|
telling if any found constraints for the fresh
type variables |rtvs| should be hidden.
A second parameter |howToInst :: HowToInst|
specifies how to instantiate.
When discussing the implementation
for quantifiers we will look at this further.

The first cases of the actual implementation of |<=| are similar
to the previous version with the exception
of an alternative for flipping
|t1 <= t2| into |t2 <= t1| if the variance is |ContraVariant|,
and
an additional guard on |fioBindLFirst| and |fioBindRFirst|.

Finally, before looking at the case analysis, some convenience functions for
updating the output |FIOut| are introduced:

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.foCmb}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.foCmbApp}

These functions straightforwardly combine fields from two different |FIOut|'s.
These combination functions specify how aspects of |fitsIn| should be combined
for |Ty_App|. For example |foCmbCnstr| applies the constraints resulting
from fitting the second subtype |afo| (of |Ty_App|) to the constraints coming out
of the first subtype |ffo|.
The overal combination |foCmbApp| used by the fitting of
two |Ty_App|'s is the composition of all the smaller combinations.

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.Base}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.Var1}

The order in which all these case are listed is now important
as the cases for |fioBindLFirstN| and |fioBindRFirstN| will
be executed only after types are stripped of their top level quantifiers.

Compared to the rules in \figRef{rules.fit4.quant}
an additional case has been included for an exact match of
two quantified types when we want |t1 <= t2| and |t2 <= t1| both
to hold. We will postpone discussion until
%if not incl05
later (not included in \thispaper).
%else
\chapterRef{ehc5}.
%endif

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.QLR}

The function |unquant| has to be told how to do the instantiation,
this is specified by a function which creates a type from a type
variable and a quantifier.

\chunkCmdUseMark{EHTyInstantiate.4.HowToInst}

The rules in \figRef{rules.fit4.quant} indicate for different combinations of options
and quantifiers how to instantiate type variables.
For example,
the first case of

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.QR}

implements \ruleRef{f-forall-r1} and \ruleRef{f-exists-r1}.
The behavior with respect to the different ways of instantiating is encoded
in |instCoConst| which tells us that the universally quantified types
should be instantiated with type variables,
and existentially quantified types with type constants.
The second case similarly covers \ruleRef{f-forall-r2} and \ruleRef{f-exists-r2}
while

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.QL}

covers the remaining \ruleRef{f-forall-l} and \ruleRef{f-exists-l}.

Checking two application of types, implementing both \ruleRef{f-prod4} and \ruleRef{f-arrow4},
has changed with respect to the handling of co- and contravariance.
From the resulting |foCoContraL| the first element describes the
co- and contravariance behavior, as such it is used to update
the |fi :: FIIn| accordingly.

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.App}

All is left of |fitsIn| are the remaining cases for type variables, now for the
|fioBindLFirstN| and |fioBindRFirstN| cases

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.Var2}

and starting it all

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.DefaultCase}
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.SetupAndResult}

\subsection{Instantiation}
\label{ehc4-inst}

Function |fitsIn| is
now one of the two places
where instantiation of a type occurs.

\begin{itemize}
\item
In |fitsIn| the
instantiation of a quantified type now is entangled with
matching two types.
|fitsIn| peels off top level quantifiers layer by layer during
the matching of types.
\item
When a type is bound to an identifier,
we have to instantiate top level |exists|'s to
open the type.
\end{itemize}

These variants are implemented by |tyInst1Quants|
and |tyInst1Exists| respectively:

\chunkCmdUseMark{EHTyInstantiate.4.tyInst}

An additional |onlyExists :: Bool| is passed to the more general function |tyInst|
to inhibit quantification of |forall|'s.

Note that in  the previous version of EH instantiation was done explicitly,
as indicated by type rules.
In this version instantiation is done implicitly by |fitsIn|.

\subsection{Quantification}
\label{ehc4-quant}

Quantification is more complicated because the place of omitted quantifiers
has to be guessed.
This guessing is done according to the rules in
\figRef{rules.qu4}.
The structure of a type and the occurrences of
type variables are used to determine where which quantifier is inserted
during the quantification of a type.

\rulerCmdUse{rules.qu4}
\rulerCmdUse{rules.quGam4}

Informally, the following rules are obeyed
\begin{itemize}
\item
The first step is to find for a type variable |a| the smallest part of a type
where |a| occurs.
For example, for |a -> (a,b,b,b->c->(b,c))| this is the complete
type for |a|, |(a,b,b,b->c->(b,c))| for |b| and |c->(b,c)| for |c|.
\item
If this smallest part is a function type it is assumed
that universal quantification is the right choice (\ruleRef{q-arrow}).
\item
If this smallest part is a tuple type
the type variable is existentially quantified (\ruleRef{q-prod}).
\item
For the remaining cases the position of the smallest part as part
of a function type determines which quantifier is put in front.
For example, in |a -> b| the type variable |a| occurs in a contravariant
(argument) position, |b| in a covariant position.
Here the observation is that |a| apparently is not used at all by
the corresponding function because it does not show up in the result type.
So, we might as well hide |a|, hence the |exists| in \ruleRef{q-var-contra}.
Conversely, the choice of what |b| is apparently is up to the caller of
the function, hence the |forall| in \ruleRef{q-var-co}.
\item
The \ruleRef{q-app} covers remaining type constructors,
which is irrelevant for this version of EH as there are no other type constructors.
It becomes relevant for the next version of EH,
when datatypes are introduced
%if not incl05
later on (not included in \thispaper).
%else
(\chapterRef{ehc5}).
%endif
\end{itemize}

Because these rules represent a form of syntactic sugar they always
can be overruled by explicit quantifiers, as indicated by \ruleRef{q-quant}.
The quantification is lifted to a |Gamma| in a straightforward way as
specified by \figRef{rules.quGam4}.

The implementation, in terms of AG, follows the rules with the exception of
some details. First all free type variables are gathered:

\chunkCmdUseMark{EHTyQuantify.4.frTvLL}
\chunkCmdUseMark{EHCommon.4.listCombineUniq}

At the top of a type application, say |App (App (Con "->") (Var 1)) (Var 1)| representing
|a -> a| we need to be able to determine which type variables occur in both arguments of
the type constructor |->|.
Therefore a list |frTvLL: [TyVarIdL]| of type variables is gathered, each element corresponding
with the free type variables of an argument.
This list corresponds to the |fv|'s in the rules in \figRef{rules.qu4}.
For the given example this would be |[[1],[1]]|.

Next we determine which locations in the type structure are a candidate
for quantification:

\chunkCmdUseMark{EHTyQuantify.4.coco}
\chunkCmdUseMark{EHTyQuantify.4.isQuLoc}

Quantifiability of a location is based
on co- and contravariance information as passed from top to bottom,
as prescribed by the rules in \figRef{rules.qu4}.
We also need to know what an |App| represents, that is, if
it is a function type (|appIsArrow|) or tuple type (|appIsLikeProd|):

\chunkCmdUseMark{EHTyCommonAG.4.whereIAm}

If a location in the type structure is a place where
quantification may take place, the candidate free type
variables |qHereTvL| are computed:

\chunkCmdUseMark{EHTyQuantify.4.qHereTvL}
\chunkCmdUseMark{EHTyQuantify.4.tvarOccurCount}

The auxiliary function |tvarOccurGE2| selects those
type variables which occur at least twice in the arguments
of a type constructor.

From the top of the type downwards then the function |tvIsBound|
is constructed,
ensuring that candidate free type variables are not in the
|bv| of the rules in \figRef{rules.qu4}.

\chunkCmdUseMark{EHTyQuantify.4.tvIsBound}
\chunkCmdUseMark{EHTyQuantify.4.tvarsToQuant}

The resulting selection of type variables |qBndTvL|
is then used with the quantifier |hereQu|
which in turn is based on |qExists|,
telling us if it is a location where |exists| is to be used:

\chunkCmdUseMark{EHTyQuantify.4.hereQu}

Finally the quantified type |quTy| is constructed:

\chunkCmdUseMark{EHTyQuantify.4.quTy}

concluding with wrapping the AG functionality in the
function |tyQuantify| which can be used in
the Haskell world:

\chunkCmdUseMark{EHTyQuantify.4.tyQuantify}

\subsection{Type inference}
\label{ehc4-ty-infer}

Type inferencing for this version of EH and the previous version are very
similar.
\FigRef{rules.expr4} holds the adapted rules for expressions,
\figRef{rules.pat4} for patterns.
The main differences are as follows:

\rulerCmdUse{rules.expr4}
\rulerCmdUse{rules.pat4}

\begin{itemize}
\item
All rules are passed an additional context parameter indicating
the way |<=| has to be done with respect to strength |fiopt|.
See \pageRef{ehc4-fitsIn-strength} for the relevant discussion.
\item
The |fiopt| is mostly passed on unchanged, except in the argument
of an expression application (\ruleRef{e-app4}) and a pattern
|Con| (\ruleRef{p-con4}).
The latter is due to a different way of handling tuple constructors.
Instantiation in a pattern \ruleRef{p-con4} instantiates as greedily as possible.
\TBD{needs more discussion}
\item
Types of tuple constructors (and destructors) are now stored in the |Gamma| for
types, |valGam|.
The lookup for types in |valGam| (|valGamLookup|) now takes care of
returning a proper quantified type for tuples, thereby resembling more the
normal retrieval of types from a |Gamma|.
The \ruleRef{p-var4} now covers the case for (tuple)constructors too.
This change also prepares for the introduction of datatypes in the next version of EH.
\item
A |let|-expression in \ruleRef{e-let4} and \ruleRef{e-let-tysig4} quantify
bindings via the rules in \figRef{rules.quGam4} and \figRef{rules.qu4}.
Additionaly, to take care of always opening existentially quantified types
bound by a value identifier, a function |instE| is used.
Function |instE| corresponds to |tyInst1Exists|.
\end{itemize}

Changes in the implementation are also small, mostly to take care
of the additional parameters to |fitsIn| (|fiopt|, a |UID| for instantiations)
and the previous remarks.

\subsubsection{Handling of tuples}

The alternative for |Con| looks up the value associated with
the tuple constructor name in |valGam|.

\chunkCmdUseMark{EHInferExpr.4.Con}

Previously, the type was constructed in situ,
now it is delegated to |valGamLookup|:

\chunkCmdUseMark{EHGam.4.valGamLookup}

This modification also introduces a new convention where |valGam|
contains for a value constructor |X| a binding for the type
of the function which constructs the value,
and a type of the function which dissects the value into
a tuple of all fields of the value.
The convention is that the constructor has name |X|, the dissector/deconstructor
has name |unX|.
For tuples these bindings are created on the fly.
For example,for a 3-tuple the following
bindings are simulated to be present in |valGam|:

\begin{code}
,3    :: forall a . a -> forall b . b -> forall c . c -> (a,b,c)
un,3  :: forall a b c . (a,b,c) -> (a,b,c)
\end{code}

The |unX| binding corresponds to the type created in the \ruleRef{p-con2} (\figPageRef{rules.pat2}).
The |Con| alternative now also uses the |valGam| to find a binding for a tuple dissector/destructor:

\chunkCmdUseMark{EHInferPatExpr.4.patFunTy}

\subsubsection{Declarations and options}

Declarations als need some modifications to take care of the quantification and instantiation
of toplevel existential quantifiers as specified in \ruleRef{e-let4} and \ruleRef{e-let-tysig4}:

\chunkCmdUseMark{EHInfer.4}

Setting up proper values for the ``strength'' |fiopt| is also done here.

\subsubsection{Type expressions}

Type signatures may include quantifiers.
This requires additional abstract syntax for type expressions:

\chunkCmdUseMark{EHAbsSyn.4}

and additional parsing

\chunkCmdUseMark{EHParser.4.pTyExprPrefix}
\chunkCmdUseMark{EHParser.4.pTyExpr}

The parser |pTyExpr| is slightly complicated because of the right associativity of 
the function type constructor |->| in combination with quantifiers.
For example, the type
\[
|forall a . a -> forall b . b -> (a,b)|
\]
parses to an abstract syntax fragment corresponding to
\[
|forall a . (a -> (forall b . (b -> (a,b))))|
\]

Rewriting to a form similar to the parser for expressions, with a prefix would lead
to a parser with common prefixes (the |pTyExprBase|) in its alternatives.
For LL(k) parsers such as the parser combinators used here this is not a good idea.
Hence the construction where the quantifier is parsed as a prefix of
the parts between |->| but still applies right associatively.

\chunkCmdUseMark{EHInferExpr.4.Var}
\chunkCmdUseMark{EHInferExpr.4.Rest}

\chunkCmdUseMark{EHInferPatExpr.4.Rest}


\TBD{previous should be redone.}

%if onlyCurrentWork
\subsection{Propagation of impredicativity}

Our solution for the use of higher ranked types is based on:
\begin{itemize}
\item The obligation for a programmer to specify the type information the type inferencer is not capable of inferring.
\item The obligation of the type inferencer to not forget this information.
\end{itemize}

The type inferencer uses standard Hindley-Milner type inferencing extended with the possibility to bind type variables to
quantified types, usually named \IxAsDef{impredicativity}.
This allows the type inferencer to propagate quantified types instead of instantiating these types to a monomorphic type
for which it is then impossible to find back forgotten polymorphism.
This idea works well for the examples encountered so far, for example:

\begin{code}
%%4srcfile(test/3-poly-rank.eh%%)
\end{code}

Polymorphism for |i| has been declared explicitly before any use of this information in de type checking of the body of |f| is done or any parameter is
passed to |f|.
Because we allow type variables to be bound to quantified types the following example also infers |f :: (forall a . a -> a) -> Int| correctly:

\begin{code}
%%4srcfile(test/4-impred2.eh%%)
\end{code}

This works because initially we assign a type variable to the type of |h| which is later bound to |forall a . a -> a| when it is used as an argument of |g|.
However, the following example breaks because we first bind the type of |h| to a monomorphic type:

\begin{code}
%%4srcfile(test/4-impred1.eh%%)
\end{code}

This example breaks at three different places:
\begin{itemize}
\item
The first use of |h| for the computation of |x1| concludes |h :: Int -> v_7|. This conflicts with the second use in the computation of |x2| where
|h| is expected to accept a |Char|.
\item
|h| is also not polymorphic enough to be passed as a parameter to |g|.
\item
The type inferencer will conclude |f :: (Int -> forall a . a) -> Int| (or something similar) which is not polymorphic enough in its argument
to be able to accept |id| as its parameter.
\end{itemize}

These problems are caused by the interaction of the following design choices:
\begin{itemize}
\item
If the type inferencer finds more information about a type variable it immediately applies this knowledge to all types.
This is done in a left-to-right order through the abstract syntax tree.
\item
No polymorphism for parameters is inferred. See ... for the a discussion of the reasons to avoid the complexity of ... .
\end{itemize}

In other words, once a type is monomorphic we don't allow it to become polymorphic, not even if we encounter the `right' to do so elsewhere in
a program.
We will not introduce inferencing polymorphism for parameters in our inferencing machinery because of its complexity, so we cannot repair the problem
by inventing polymorphism whenever it would be convenient to do so.
However, the problem could be fixed because in our example program the use of |h| as a parameter to |g| tells us that |h| must be polymorphic anyway.
If only this information could be available in an earlier stage of type inferencing,
or alternatively, if only the decision to let |h| be monomorphic could be delayed for a while!
We choose the latter, that is, we introduce a way of delaying a binding decision for a type variable.

In order to be able to rebind a type variable to a more polymorphic we may not forget to which type variable a type was assigned.
This can be remembered by just relating the type variable to its type(s):

\begin{code}
sigma  =  ...
       |  tvarv//Vec(sigma)
\end{code}

The notation |tvarv//Vec(sigma)| associates to a set of types |Vec(sigma)|.
The type variable |tvarv| is bound to each of them during type inferencing, hence the name \IxAsDef{bind type} for this type variant.
The idea is that as soon as an attempt is made to bind |tvarv| to a polymorphic
type we check if all types in |Vec(sigma)| are an instance of the polymorphic type.
If this is the case we can forget all types |Vec(sigma)| and go on with the polymorphic type.

The rules for |<=| in \figRef{rules.fit4.bind} make this more precise.
A bind type is introduced in \ruleRef{f-var-l1}.
The introduction is also influenced by the context in which |<=| is used; this is expressed by
the boolean flag |fioBindToTyAltsY|, part of the set of options |fiopt|.
The modified \ruleRef{e-ident4B} \figRef{rules.expr4.B} for checking the type of an identifier sets this flag.

A bind type can only be introduced when checking an identifier.
Traditionally, this is the place where a quantified type is instantiated when it is extracted from an environment |Gamma|.
Quantified types usually live in a |Gamma| as a so called \IxAsDef{type scheme} and are introduced into the type checking/inferencing world
by instantiating the type scheme to a monomorphic type.
Here, in a similar manner, if nothing is known about an identifier, its type variable will be bound to a bind type which will hold
all possible instantiations found during type inferencing.
The remaining rules of \figRef{rules.fit4.bind} specify what should be done if a bind type is encountered in |<=|.

Some additional notation for manipulating vectors is used as well.
A vector |Vec(x)| of |x|'s is alternatively notated as |VecI(x)(i)| where |i| implicitly ranges over all indices referring to
an element of the vector. Any predicate referring to |i| has an implicit quantifier |forall ^ i| in front of it.
Extraction of an individual element of the array with index |i| is notated by |VecI(x)(..,i,..)|.
A predicate referring to this |i| has an implicit |exists ^ i| in front of it.

Some additional options need to be passed as well:

\chunkCmdUseMark{EHTyFitsIn.6.2.FIOpts.defaults}

\rulerCmdUse{rules.fit4.bind}
\rulerCmdUse{rules.expr4.bind}
\rulerCmdUse{rules.elimb4}
\rulerCmdUse{rules.elimbGam4}

The following example really is responsible for delaying subsumption checks:

\begin{code}
%%4srcfile(test/4-impred4.eh%%)
\end{code}

There is no single usage of |h| which enforces |h :: forall a . forall b . (a,b) -> (a,b)|,
the meet of |forall a . (Int,a) -> (Int,a)| and |forall a . (a,Int) -> (a,Int)| done at the generalization of |f|
computes this type.
Newfound polymorphism (as in |g1 h|) can be used to deduce a more general type for (e.g.) |(Int,Int) -> v| found in |h (3,4)|...

???? Meet instead of subsumption

\subsection{Propagation of impredicativity + predicates + coercions}

Some examples:

\begin{code}
let  g  ::  (forall a . A a => a -> a) -> Int
     f  =   \h ->  let  x1 = g h
                        x2 = h 3
                   in   ...
in   ...
\end{code}

Function |h| has type |h :: forall a . A a => a -> a|.
However, it cannot be instantiated immediately in its use in |h 3| because later on |h| might turn out
to be more polymorphic. Here it does not matter because |h| already is polymorphic enough...

%endif % onlyCurrentWork








%if inclOmitted
\subsection{Omitted, more of the same}
Substitution, error gathering, pretty printing, uniq, common
%endif

%if not omitLitDiscuss
\subsection<article>{Literature}

\TBD{}

Higher ranked types, \cite{peytonjones04pract-inf-rank,botlan03ml-power-f}

Cannot do inference for rank3, \cite{jim95rank,kfoury94direct,kfoury99rank2-decid,kfoury03rank2-princ}

Existentials, via universal \cite{laufer96class-existential}

%endif




%endif %% incl04


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl05

%if inclParts
\part{Structure types}
%endif

%if asSlides
\frame<presentation>{
\tableofcontents[hidesubsections]
}
%endif

\section{EH 5: data types}
\label{ehc5}

%if forAfpHandout
This part is not included in this version of \thispaper.
%else

Topics: user defined type (constructors) with value constructors for that type

Examples


Additional AST
\chunkCmdUseMark{EHAbsSyn.5}

Additional parsing
\chunkCmdUseMark{EHC.5}

\subsection{Co contra variant}

Asymmetry of subsumption: forgetting val is |Int| is ok, other way around not

Contravariance of arg of arrow

Subsumption must switch direction

Data type co/contra inference (not done here??, perhaps EHC6)

\subsubsection{Notation}

\begin{code}
coco    =  CoVariant        -- co variant
        |  CoVariantn       -- contra variant
        |  CoVariantCon     -- both/neither
        |  cocovar          -- variant variable
\end{code}

\subsubsection{Examples}

\begin{code}
data X a = X a
X ::< c -> c
\end{code}

\begin{code}
data X a b c = X ((a -> b) -> c)
X ::< c -> CoVariantOpp c -> c -> c
\end{code}

\begin{code}
data X a = X (a -> a)
X ::< CoVariantCon -> CoVariantCon
\end{code}

\begin{code}
data X a b = X (a b)
X ::< (c -> d) -> c -> d
\end{code}

\subsection{Type inference}

\subsubsection{Data}

\begin{RulesFigure}{|TypeDD()(data ..)(Gamma)()|}{Type of data}{type-data}
\infrule{d-data-type}
  {|TypeDC(identc (Vec(tvar)))(constr (identcNN()(i)) (Vec(sigma)))(GammaNN()(i))()|
  }
  {|TypeDD()(data identc (Vec(tvar)) = Vec(identcNN()(i) (Vec(sigma))))(+++ GammaNN()(i))()|}
\end{RulesFigure}


\begin{RulesFigure}{|TypeDC(sigmaNN(d)())(constr ..)(Gamma)()|}{Type of data constructor}{type-data-constr}
\infrule{d-data-constr-type}
  {|QuantT([],CoVariant)(sigma1 -> .. -> sigmaNN()(n) -> sigmaNN(d)())(sigmaNN(mk)())(**)|\\
   |QuantT([],CoVariant)(forall (ftv(sigmaNN(d)())) . sigmaNN(d)() -> (sigma1,..,sigmaNN()(n)))(sigmaNN(un)())(**)|\\
   |i `elem` 1..n|
  }
  {|TypeDC(sigmaNN(d)())(constr identc (Vec(sigmaNN()(i))))([Bind(identc)(sigmaNN(mk)()),Bind(unIdent(identc))(sigmaNN(un)())])()|}
\end{RulesFigure}

\subsubsection{Case}

\begin{RulesFigure}{|TypeCA(Gamma,sigmac,sigmak)(e)(sigma)(Cnstrp,Cnstre)(cod)|}{Inference for alternatives of case expression}{inference-case-rules-alts}
\infrule{e-alts-nil}
  {
  }
  {|TypeCA(Gamma,**,sigmak)(alts [])(sigmak)([],[])|}
\\
\infrule{e-alts-cons}
  {|TypeCA(Gamma,sigmap,sigmae)(alts (Vec(pati -> expri)))(sigmaeN(i))(CnstrNN(p)(i),CnstrNN(e)(i))|\\
   |TypeE(Gammap +++ Gamma,sigmak)(expr)(sigmae)(Cnstre)()|\\
   |TypeP(Gamma,sigmac)(pat)(sigmap,Gammap)(Cnstrp)|
  }
  {|TypeCA(Gamma,sigmac,sigmak)(alts (pat -> expr) : (Vec(pati -> expri)))(sigmaeN(i))(CnstrNN(p)(i) Cnstrp,CnstrNN(e)(i) Cnstre)|}
\end{RulesFigure}

\subsubsection{Expr}

\begin{RulesFigure}{|TypeE(Gamma,sigmak)(e)(sigma)(Cnstr)(cod)|}{Inference for expressions}{inference-expr-rules-data}
\infrule{e-case-known}
  {|TypeCA(Gamma,sigmac,Cnstrp sigmak)(alts (Vec(pati -> expri)))(sigmae)(Cnstrp,Cnstre)|\\
   |TypeE(Gamma,Top)(exprc)(sigmac)(Cnstrc)()|
  }
  {|TypeE(Gamma,sigmak)(case exprc of Vec(pati -> expri))(sigmae)(Cnstre Cnstrp Cnstrc)|}
\\
\infrule{e-let-data}
  {|TypeE(GammaNN(d)() ++ Gamma,sigmak)(expr)(sigmae)(Cnstr)()|\\
   |TypeDD()(data identc (Vec(tvar)) = Vec(identcNN()(i) (Vec(sigma))))(GammaNN(d)())()|
  }
  {|TypeE(Gamma,sigmak)(let data identc (Vec(tvar)) = Vec(identcNN()(i) (Vec(sigma))) in expr)(sigmae)(Cnstr)()|}
\end{RulesFigure}

Representing a data type as constructors + unconstructors

\chunkCmdUseMark{EHInferData.5}

\subsubsection{Implementing type constraints}
Expr, checking, together with type rules
\chunkCmdUseMark{EHInferExpr.5}

CaseExpr, checking, together with type rules
\chunkCmdUseMark{EHInferCaseExpr.5}

PatExpr, checking, together with type rules
\chunkCmdUseMark{EHInferPatExpr.5}

TyExpr, extracting the type
\chunkCmdUseMark{EHInferTyExpr.5}

Tying it
\chunkCmdUseMark{EHInfer.5}

\subsubsection{valGam and tuples}

\chunkCmdUseMark{EHGam.5.valGamLookup}

%if inclOmitted
\subsection{Omitted, more of the same}
Error gathering, pretty printing, uniq

\subsection{Literature}
%endif

\TBD{}

%endif

%endif %% incl05





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl06

\section{EH 6: kind inference}
\label{ehc6}

%if forAfpHandout
This part is not included in this version of \thispaper.
%else

Topics: by introducing user definable type constructors and expressions using them,
user errors can be made, hence checking types of types.

No additional syntax required

Kind of a type
\chunkCmdUseMark{EHCommon.6}
\chunkCmdUseMark{EHTy.6}

\subsection{Kind inference}

\subsubsection{Environment/Gamma/Assumptions}
\chunkCmdUseMark{EHGam.6}

\subsubsection{Implementing kind constraints}

\chunkCmdUseMark{EHTyFitsIn.6}

TyExpr, extracting the type
\chunkCmdUseMark{EHInferTyExpr.6}

Data, checking, together with type rules
\chunkCmdUseMark{EHInferData.6}

Tying it
\chunkCmdUseMark{EHInfer.6}


\subsection<article>{Omitted, more of the same}
Error gathering, pretty printing, uniq

\subsection<article>{Literature}

\TBD{}

%endif

%endif %% incl06

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl07

\section{EH 7: non extensible records}
\label{ehc7}

\frame<presentation>
{
\frametitle{Records}
\begin{itemize}
\item
\SafeCode{%
\begin{code}
%%7srcfile(test/7-all-ok.eh%%)
\end{code}
}
\end{itemize}
}

\subsection{Current implementation examples}

\begin{code}
%%7srcfile(test/7-all-ok.eh%%)
\end{code}

with output

\begin{TT}
%%7ppfile(test/7-all-ok.eh%%)
\end{TT}

\subsection{Choices already made}

\paragraph{Syntax: delimiters.}
The notation for tuples with parenthesis |(...)| is reused, no curly braces are used |{...}|.
\begin{itemize}
\item
Curly braces are used for an alternative way of specifying list usually speficied via the layout rule (declarations, case alternatives).
Especially the case alternative would lead to parsing problems with the matching of records.
\item
Tuples now automatically are records with a numerical selection as field selection
\begin{code}
%%7srcfile(test/7-tup-sel.eh%%)
\end{code}
\end{itemize}

Rows themselves are delimited by |(||...||)|, variants by |(<...>)|.

A row/record is always based on (or an extension of) an empty row/record.
For a following version allowing extensible records this restriction (obviously) is removed.

\paragraph{Case expression, punning.}
Pattern matching a record is done via case:

\begin{code}
%%7srcfile(test/7-pun.eh%%)
\end{code}

In these examples, the matching for |v1| is done on the basis of field names at the left of |=|.
The corresponding field value is made available via the name at the right of |=|.
If the latter name is omitted, so called punning takes place where both names are base on the single identifier
occurring on a field position.
The second example |v2| demonstrates this feature.

However, for tuples we run into a problem if we try to pun because the names for fields of a tuple
default to |[1..]|.
Tuples now would require explicit field name matching whereas (for compatibility/convenience reasons) the
use as with |v3| is desirable.
To fix this a somewhat arguable choice has been made:
\begin{itemize}
\item
If at least one field is matched via explicit field matching (as in |v2|) punning is done for the fields not
matched via an explicit field match.
\item
If no field is matched explicitly, all fields are implicitly matched on their positional label, that is |[1..]|.
\end{itemize}

This design decision causes |v4| and |v5| to produce error messages:

\begin{TT}
%%7ppfile(test/7-pun.eh%%)
\end{TT}

\paragraph{Relation with other proposals.}
For the rest the proposal by Jones \cite{jones99lightweight-ext-rec} is followed.

This proposal also deviates from TRex (by extending from left to right) and is incompatible
with Haskell because of the use of @'.'@ as the builtin selection operator.

\subsection{Issues, partly resolved or previous}

Merge with tuples, merge with data (which is tagged record).

Follow \cite{jones99lightweight-ext-rec}.

But what about tuple notation? Take theirs or Haskell98/EH(untilnow)?
Or different notation for rows, swap @()@ and @{}@?

@{}@ is also used for decls and case alternatives, so conflicts with rec match
\begin{code}
let v :: {a :: Int, b :: Char, c :: a -> a}
    vc = case v of
           {a = aa, b = bb, c = cc} -> aa
...
\end{code}
The @{@ is parsed as the beginning of an alternative.

Punning
\begin{code}
let (x,y) =
\end{code}
means matching on field label, not just position.

Default names, when absent (not for patterns): "fld001" and further,
but this limits range to 1000 fields.
Otherwise, "fld1" and further breaks canonical sorting order, that is
|"fld11" < "fld2"| instead of other way around.
Order is needed later on for extensible records.

Doaitse:
Parsing of
\begin{code}
    vc =  case v of
            {{a = aa, b = bb,cc} -> aa}
\end{code}
creates a bit of a problem with |cc|, which is 
a pattern with an optional prefix |pVarid <* pKey "="|.
The optionality cannot be expressed with |`opt`| but must be done
with |pSucceed|, probably because of greediness.

Allowing field labels to be absent and having default names is ok
for values and patterns because only a @{}@ notation is used.
For rows however:
\begin{code}
v1 :: (Int,Char)
\end{code}
it is ambiguous. Is |v1| a tuple or a row?
Solutions:
\begin{itemize}
\item
Forbid absent labels in a row.
\item
Different syntax, but which, taking into account the need to notate 
implicit arguments in a type with e.g. @<>@.
\end{itemize}

Problem: open/close pairs of symbols:

\begin{center}
\begin{tabular}{lllll}
Open & Close & Expr & PatExpr & TyExpr \\
\hline
@[@ & @]@ & list & list & list \\
@(@ & @)@ & tuple & tuple & tuple, row \\
@{@ & @}@ & record, layout & record & record \\
@<@ & @>@ & operator & & implicit parameter \\
 &  & variant & variant & variant \\
 &  & implicit parameter & & \\
\end{tabular}
\end{center}

The braces @{}@ are used ambiguously, variants and implicit parameters cannot be expressed.
Alternative scheme, merging tuples with records:

\begin{center}
\begin{tabular}{lllll}
Open & Close & |Expr| & |PatExpr| & |TyExpr| \\
\hline
@[@ & @]@ & list & list & list \\
@(@ & @)@ & record/tuple & record (field match) & record/tuple \\
@{(@ & @)}@ &  & tuple (position match) & row \\
@{[@ & @]}@ & variant & variant & variant \\
@{<@ & @>}@ & implicit parameter & & implicit parameter \\
@<@ & @>@ & operator & & \\
\end{tabular}
\end{center}

Consequence: scanner requires adaptation, @{(@ and @)}@ are used inconsistently in
|PatExpr| and |TyExpr|, keeping backwards compatibility for tuple alike (i.e. positional) match.

\subsection{Rules}

\rulerCmdUse{rules.expr9.rec}


\subsection<article>{Literature}

%endif %% incl07



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl08

\section{EH 8: code generation}
\label{ehc8}

Coercion (of records), related to subsumption

\subsection{Extended GRIN}

%{
%format unit    = "\mathbf{unit}"
%format store   = "\mathbf{store}"
%format fetch   = "\mathbf{fetch}"
%format update  = "\mathbf{update}"
%format rec     = "\mathbf{rec}"
%format ffi     = "\mathbf{ffi}"
%format eval    = "\mathbf{eval}"
%format apply   = "\mathbf{eval}"

\begin{Figure}{Extended GRIN syntax}{grin-ext}
\begin{code}
prog        ::=     { binding } +                   --  program

binding     ::=     var {var}* = exp                --  function/CAF definition
            |       rec {binding}+                  --  mutual recursive bindings

exp         ::=     sexp ; \lpat -> exp             --  sequencing
            |       case val of {cpat -> expr} +    --  case
            |       sexp                            --  operation

sexp        ::=     var {sval}+                     --  application
            |       ffi var {sval}+                 --  foreign function application
            |       apply var {sval}+               --  unknown function application
            |       unit val                        --  return value
            |       store val                       --  allocate new heap node
            |       eval var                        --  evaluate variable
            |       fetch var {[n]}                 --  load heap node
            |       update var val                  --  overwrite heap node
            |       (exp)

val         ::=     (tag {sval}+)                   --  complete node (constant tag)
            |       (var {sval}+)                   --  complete node (variable tag)
            |       (var | adapt,{adapt}* )         --  record adaptation
            |       tag                             --  single tag
            |       ()                              --  empty
            |       sval                            --  simple value

adapt       ::=     offset += sval                  --  insertion at offset
            |       offset := sval                  --  update at offset
            |       offset -=                       --  delete at offset

sval        ::=     literal                         --  constant, basic value
            |       var                             --  variable

offset      ::=     sval                            --  offset in record

literal     ::=     posint                          --  integer literal
            |       negint
            |       #posint / tagcateg / var        --  tag constant

posint      ::=     0..maxint                       --  positive integer literal

negint      ::=     minint..-1                      --  negative integer literal

tagcateg    ::=     C                               --  constructor
            |       F                               --  saturated function call
            |       P posint                        --  partially applied function call
            |       A                               --  application of unknown function
            |       R                               --  record
            |       H                               --  hole for mutual recursive values

var         ::=     $ chars                         --  identifier
            |       alphanumchars

lpat        ::=     val                             --  lambda pattern

cpat        ::=     (tag {var}*)                    --  constant node pattern
            |       (var | split,{split}* )         --  record split
            |       tag                             --  constant tag pattern
            |       literal                         --  constant

split       ::=     var = offset                    --  extraction at offset
\end{code}
\end{Figure}
%}

\subsection<article>{Literature}

%endif %% incl08


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl09

%if inclParts
\part{Implicitness}
%endif

%if not storyExplImpl
\section{EH 9: explicit implicitness, classes}
\label{ehc9}
%endif

%{
%format pred        = "\mathbf{pred}"
%format rule        = "\mathbf{rule}"
%format pia         = pi "^a"
%format piasigma    = pia "_{" sigma "}"

%if storyExplImpl
\subsection{Introduction}

The Haskell class system, originally introduced by both Wadler \cite{wadler88how-ad-hoc-poly}
and Kaes \cite{kaes88parametric-overl},
offers a powerful abstraction mechanism
for dealing with overloading (ad-hoc polymorphism).
The basic idea is to restrict the polymorphism of a parameter by specifying
that some predicates have to be satisfied when the function is called:

\begin{code}
f  ::    Eq a =>  a ->  a ->  Int
f  =   \          x     y ->  if x == y then 3 else 4
\end{code}

In this example the type signature for |f| specifies that any type |a| can be passed as an argument
as long as it satisfies the predicate |Eq a|.
Such predicates are introduced by \IxAsDef{class declaration}s,
for example the following version of Haskell's |Eq| class:

\begin{code}
class Eq a where
  (==) :: a -> a -> Bool
\end{code}

This class declaration specifies a collection of function and value types which can only be used
on a type |a| for which the predicate |Eq a| holds.
A class declaration alone is not sufficient: \IxAsDef{instance declarations}
specify for which types the predicate holds,
simultaneously providing an implementation for the class functions:

\begin{code}
instance Eq Int where
  x == y = primEqInt x y

instance Eq Char where
  x == y = primEqChar x y
\end{code}

The equality functions for |Int| and |Char| are here expressed
by primitives |primEqInt| and |primEqChar|.
The compiler turns these declarations into records (dictionaries) containing the functions as fields.
An explicit version of this internal machinery reads:

\begin{code}
data EqD a  = EqD ^^ {eqEqD :: a -> a -> Bool}  -- class Eq
eqDInt      = EqD primEqInt                     -- Eq Int
eqDChar     = EqD primEqChar                    -- Eq Char
\end{code}

Inside a function the elements of the predicate's dictionaries
are available, as if they were defined as top-level variables.
This is accomplished by passing a dictionary
for
each predicate in the type of the function.
So the actual implementation of |f| (apart from all kinds optimisations) is:

\begin{code}
f  ::         EqD a ->  a ->  a ->  Int
f  =   \  ^^  dEq       x     y ->  if (eqEqD dEq) x y then 3 else 4
\end{code}

At the call site of the function |f| the dictionary
that corresponds to the actual type of the polymorphic argument must be passed.
Thus the expression 
|f 'a' 'b'| can be seen as an abbreviation for the semantically more complete |f eqDChar 'a' 'b'|.

\paragraph{Haskell's point of view}
Haskell's class system has turned out to be theoretically sound and complete \cite{jones94phd-qual-types}
as well as flexible enough to incorporate many useful extensions \cite{jones93constr-class,jones00class-fundep}.
Its role in Haskell has been described in terms of an implementation \cite{jones99thih}
as well as its semantics \cite{hall96type-class-haskell,faxen02semantics-haskell}.
Nevertheless, the following observation with respect to its design and implementation can be made.

The compiler is fully in control of which dictionary to pass for a predicate,
determined as part of the resolution of overloading.
This behavior is the result of the combination of the following list of design choices:

\begin{itemize}
\item
A class definition introduces a record type (for the dictionary) associated with a predicate over type variables.
\item
An instance definition introduces a value for the record type for the class predicate specialized for a specific type
(or combination of types in the case of multiparameter type classes).
\item
The type of a function specifies the predicates for which dictionaries have to be passed at the call site of the function.
\item
Which dictionary is passed at the call site of a function is determined by the compiler based on
 \begin{itemize}
 \item
 required dictionaries at the call site of a function;
 this is determined by the predicates in the instantiated type of the called function.
 \item
 the available dictionaries introduced by instance definitions.
 \end{itemize}
Internally the compiler uses predicate proving machinery and heuristics
\cite{jones00thih,peytonjones97typecl-explore,faxen02semantics-haskell} to compute the proper dictionaries.
\item
Which dictionaries to be passed to a function is fully defined in the language definition.
\item
The language definition uses a fixed set of dictionaries introduced by instance definitions and a fixed algorithm for determining
which dictionaries need to be passed.
\end{itemize}

The result of this is both a blessing and a curse.
A blessing because it silently solves a problem (i.e. overloading), a curse
because as a programmer we cannot easily override the choices made in the design of the language
(i.e. via Haskell's default
mechanism), and worse,
cannot help if no unique solution according to the language semantics exists.
For example, overlapping instances occur when more than one choice
for a dictionary is possible.
Smarter, more elaborate versions of the decision making algorithms can and do help
\cite{heeren05class-direct},
but
in the end it is only the programmer who can fully express his intentions.
The system at best can only make a guess.

The issue central to this paper is that Haskell demands from a program that all choices about which dictionaries
to pass can be made automatically and uniquely,
whereas we also want to be able to specify this ourselves explicitly.

\paragraph{Our contribution}
Our approach takes explicitness as a design starting point, as opposed to the described implicitness
featured by the Haskell language definition.
To make the distinction between our and Haskell's approach clear in the remainder of \thispaper,
we call our explicit language and its implementation Explicit Haskell (EH)
whereas we refer to Haskell language and its implementations by just Haskell.

\begin{itemize}
\item
In principle, all aspects of a EH program can be explicitly specified, in particular
the types of functions, other values,
and the manipulation of dictionaries without the use of the class system.
\item
The programmer is allowed to omit explicit specification of some program aspects;
EH then does its utmost to infer the missing information.
\end{itemize}

The difference is that our approach
allows programmer and EH system to jointly construct the completely
explicit version of a program;
an implicit approach inhibits all explicit programs which the type inferencer cannot infer but would
otherwise be valid.
If the type inferencer cannot infer what a programmer expects it to infer,
then the programmer provides the required information in the form of type annotations.
In this sense we take the best of two worlds:
the simplicity
of systems like system F \cite{girard72system-f,reynolds74type-struct-sysF}
and Haskell's ease of programming.

In \thispaper\ explicitness takes the following form:

\begin{itemize}
\item
Dictionaries introduced by instance definitions can be named;
the dictionary can be accessed by name as a record value.
\item
The set of class instances and associated dictionaries to be used by
the proof machinery can used as normal values,
and vice versa, normal (record) values can be used as dictionaries for predicates.
\item
The automatic choice for a dictionary at the call site of a function can be overruled.
\item
Types can be composed of the usual base types, predicates and quantifiers
(both universal and existential) 
in arbitrary combinations.
\item
Types can be partially specified, thus having the benefit of explictness as well as inference,
but not the obligation of the ``all or nothing''
explicitness usually enforced upon the programmer.
\end{itemize}

We will focus on the first three items of the preceding list,
the explicit passing of values for implicit parameters.
Although explicit typing forms the foundation on which we build,
we discuss it only as much as is required for
our discussion of explicit implicit parameters.
We only note that by allowing the programmer to specify that which a type inferencer cannot infer,
we avoid proving common type inferencing properties like its soundness, completeness and principality
of inferred types relative to a fully explicit language.

We view Haskell's class system as syntactic and semantic sugar
on top of explicit parameter passing.
In this view, parameters need not be passed explicitly;
it can be determined automatically based upon
class and instance declarations provided by the programmer.
If it cannot be determined uniquely which parameters need to be passed
because of lacking or contradictory information,
the programmer can always provide the required parameters explicitly.

Related to programming languages in general,
our contribution, though inspired by and executed in the context of Haskell,
offers language designers a mechanism for more sophisticated control over parameter passing,
by allowing a mixture of explicit and implicit parameter passing.


\paragraph{Outline of \thispaper}
In \thispaper\ we focus on the exploration of explicit implicit parameters,
to be presented in the context of EH, a
Haskell variant
\cite{dijkstra04ehc-web,dijkstra04thag,dijkstra04thag-part1}
in which all features described in \thispaper\ are implemented.
In \secRef{ehc09-prelim} we start with some preliminaries required for understanding the remainder of \thispaper.
In \secRef{ehc09-implparam} we present examples of what we can express in EH.
The use of partial type signatures and their interaction with
predicates is demonstrated in \secRef{ehc09-partialtysig}.
In \secRef{ehc09-implem} we present a carefully selected part of our implementation,
highlighting the distinguishing aspects as compared to known implementations.
In \secRef{ehc09-discussion} we discuss some remaining design issues and related work.
We conclude in \secRef{ehc09-concl}.


\subsection{Preliminaries}
\label{ehc09-prelim}

Intended as a platform for both education and research, EH offers advanced features
like higher ranked types, existential types, partial type signatures and records.
Syntactic sugar has been kept to a minimum in order to ease experimentation with and understanding
of the implementation; other mechanisms like syntax macro's \cite{baars02www-syn-macro}
provide the means for including additional syntax into the language without having to change the compiler.
The compiler for EH actually is a series of ten compilers, each of which adds features to
the previous one.
The features presented in \thispaper\ are part of the ninth version.

\begin{figure}
\begin{center}
\begin{tabular}%
%if useSigplanconfSty
{r@@{\;}c@@{\;}ll}
\multicolumn{4}{l}{Values (expressions, terms):} \\
%else
{||r@@{\;}c@@{\;}ll||}
\multicolumn{4}{||l||}{Values (expressions, terms):} \\
\hline
%endif
|e| & |::=| &
|int || char |
 & literals
 \\
& | || | &
|identv|
 & value variable
 \\
& | || | &
|e e|
 & application
 \\
& | || | &
|e (! e <: pi !)|
 & explicit implicit application
 \\
& | || | &
|\i -> e|
 & abstraction
 \\
& | || | &
|\(! i !) -> e|
 & explicit implicit abstraction
 \\
& | || | &
|let Vec(d) in e|
 & binding
 \\
& | || | &
|(lbl = e,...)|
 & record
 \\
& | || | &
|(e || lbl = e,...)|
 & record extension
 \\
& | || | &
|(e || lbl := e,...)|
 & record update
 \\
%if useSigplanconfSty
\multicolumn{4}{l}{} \\
\multicolumn{4}{l}{Declarations of bindings:} \\
%else
\multicolumn{4}{||l||}{} \\
\multicolumn{4}{||l||}{Declarations of bindings:} \\
%endif
|d| & |::=| &
|identv = e|
 & value binding
 \\
& | || | &
|identv :: sigma|
 & value type signature
 \\
& | || | &
|data sigma = Vec(identc ^^ Vec(sigma))|
 & data type
 \\
& | || | &
|class Vec(pi) => pi where Vec(d)|
 & class
 \\
& | || | &
|instance Vec(pi) => pi where Vec(d)|
 & introduced instance
 \\
& | || | &
|instance identv <: Vec(pi) => pi where Vec(d)|
 & named introduced instance
 \\
& | || | &
|instance identv :: Vec(pi) => pi where Vec(d)|
 & named instance
 \\
& | || | &
|instance e <: pi|
 & value introduced instance
 \\
%if useSigplanconfSty
\multicolumn{4}{l}{} \\
\multicolumn{4}{l}{Identifiers:} \\
%else
\multicolumn{4}{||l||}{} \\
\multicolumn{4}{||l||}{Identifiers:} \\
%endif
|ident| & |::=| &
|identv|
 & lowercase: (type) variables
 \\
& | || | &
|identc|
 & uppercase: (type) constructors
 \\
& | || | &
|lbl|
 & field labels
 \\
%if not useSigplanconfSty
\hline
%endif
\end{tabular}
\end{center}
\caption{EH terms}
\label{exim-eh-lang-terms}
\end{figure}

\figRef{exim-eh-lang-terms} and \figRef{exim-eh-lang-types} show the terms and types featured in EH.
Throughout \thispaper\ all language constructs will be gradually introduced and explained.
In general, we designed EH to be as upwards compatible as possible with Haskell and
as simple as possible.
Unfortunately these design constraints are contradictory;
we point out some aspects required for understanding the discussion in the next section:

\begin{itemize}
\item
An EH program is single standalone term.
All types required in subsequent examples are either silently assumed to be similar to Haskell or
will be introduced explicitly.
\item
All bindings in a |let| expression are analysed together;
in Haskell this would constitute a binding group.
\item
We represent dictionaries by records.
Records are denoted as parenthesized comma separated sequences of field definitions.
Extensions and updates to a record |e| are denoted as |(e || ...)|, with |e| in front of the vertical bar `| || |'.
The notation and semantics is based on existing work on extensible records \cite{gaster96poly-ext-rec-var,jones99lightweight-ext-rec}.
\end{itemize}

The universe of types as used by EH are shown in \figRef{exim-eh-lang-types}.
A programmer can specify types using the same syntax.
We mention this because often types 
are categorized based on the presence of (universal) quantifiers and predicates
\cite{hindley69princ-type,peytonjones04pract-inf-rank}.
However, we allow quantifiers everywhere in our types and to a lesser degree predicates as well.
For example, the following is a valid type denotation in EH:

\begin{code}
(forall ^ a . a -> a) -> (forall ^ b . b -> b)
\end{code}

This higher ranked example specifies a function which takes a polymorphic identity function and returns an identity function.
%if False
The second example describes an existential type for a value of which any type information for |a| has been erased but which still provides us
with a function for observing an |Int| value of it.
%endif
Existential types are part of EH, they are omitted because we will not use them in \thispaper.
Quantification has lower priority than the other composite types,
so in a type denotation without parenthesis the scope of the quantifier extends to the right of the type denotation.
%if False
EH allows the omission of quantifiers; some 
The same types are allowed to be denoted more concisely by omitting the quantifiers:

\begin{code}
(a -> a) -> (b -> b)
(a -> Int, a)
\end{code}

Quantifiers are inserted automatically as a form of syntactic sugar,
based on a few simple rules which use the occurrence of type variables relative to the type constructors and their meaning.
For example, the rule for the insertion of the |forall| quantifier informally states the following:

\begin{quote}
If a type variable |a| occurs freely on both sides of the `|->|' type constructor but not elsewhere,
|a| is universally quantified.
\end{quote}

For the insertion of an existential quantifier |exists| a similar rule, relating type variables to tupling, is used;
we will mention additional rules whenever the need arises.
These rules and related issues like impredicativity,
the checking of such types, and their use in combination with standard Hindley-Milner type inferencing
\cite{hindley69princ-type}
are ignored in the remainder of
\thispaper, but (partially) discussed elsewhere \cite{dijkstra04thag-part1}.
%endif

We make no attempt to infer higher ranked types
\cite{kfoury94direct,kfoury99rank2-decid,jim95rank};
instead we propogate explicitly specified types as good as possible to wherever this information is needed.

\begin{figure}
\begin{center}
\begin{tabular}%
%if useSigplanconfSty
{r@@{\;}c@@{\;}ll}
\multicolumn{4}{l}{Types:} \\
%else
{||r@@{\;}c@@{\;}ll||}
\hline
\multicolumn{4}{||l||}{Types:} \\
%endif
|sigma| & |::=| &
|Int || Char|
 & literals
 \\
& | || | &
|tvarv|
 & variable
 \\
& | || | &
|sigma -> sigma|
 & abstraction
 \\
& | || | &
|pi -> sigma|
 & implicit abstraction
 \\
& | || | &
|sigma ^^ sigma|
 & type application
 \\
& | || | &
|forall ^ alpha . sigma|
 & universally quantified type
 \\
%if False
& | || | &
|exists ^ alpha . sigma|
 & existentially quantified type
 \\
%endif
& | || | &
|(lbl :: sigma,...)|
 & record
 \\
%if useSigplanconfSty
\multicolumn{4}{l}{} \\
\multicolumn{4}{l}{Predicates:} \\
%else
\multicolumn{4}{||l||}{} \\
\multicolumn{4}{||l||}{Predicates:} \\
%endif
|pi| & |::=| &
|identc ^^ Vec(sigma)|
 & predicate
 \\
& | || | &
|pi => pi|
 & predicate transformer/abstraction
 \\
%if not useSigplanconfSty
\hline
%endif
\end{tabular}
\end{center}
\caption{EH types}
\label{exim-eh-lang-types}
\end{figure}



\subsection{Implicit parameters}
\label{ehc09-implparam}

In this section we give EH example programs demonstrating most of the features
related to implicit parameters.
After pointing out these features
we continue with exploring the finer details.

\paragraph{Basic explicit implicit parameters}
Our first demonstration EH program
contains the definition of the standard Haskell function |nub| which removes duplicate
elements from a list.
A definition for |List| has been included; definitions for |Bool|, |filter| and |not| are omitted.
We continue to use the |List| type in later program
fragments.
Notice that a separate |nubBy| is no longer needed:

\begin{code}
%%9srcfile(eh-frags/9-eq-nub.eh%%)
\end{code}

This example demonstrates the use of the two basic ingredients required for being explicit in the use
of implicit parameters (the list items correspond to the commented number in the example):

\begin{enumerate}
\item
The notation |<:| binds an identifier, here |dEqInt|, to the dictionary representing the instance.
The record |dEqInt| from now on is available as a normal value.
\item
Explicitly passing a parameter is syntactically denoted by an expression between
|(!| and |!)|.
The predicate after the |<:| explicitly states the predicate for which the expression is an 
instance dictionary (or \IxAsDef{evidence}).
The dictionary expression in the example itself is formed by updating a field of the already existing dictionary for |Eq Int|.
\end{enumerate}

This example demonstrates our view on implicit parameters:
\begin{itemize}
\item
Program values live in two, possibly overlapping, worlds, \IxAsDef{explicit} and \IxAsDef{implicit}.
\item
Parameters are either passed explicitly, by the juxtapositioning of explicit function and argument expressions from the explicit world,
or passed implicitly (invisible in the program text) to an explicit function value.
In the implicit case the language definition determines which value to take from the implicit world.
\item
Switching between the explicit and implicit world is accomplished by means of additional notation.
We go from
implicit to explicit by instance definitions, and in the reverse direction by means of the |(! ^^ !)| construct.
\end{itemize}

Note that this example specifies |ne| for class |Eq|.
For simplicity, later examples of class |Eq| will not include a |ne| member.

\paragraph{Higher order predicates}
We also allow the use of higher order predicates.
Higher order predicates are already available in the form of instance declarations.
For example, the following program fragment defines the instance for |Eq (List a)|
(the code for the body of |eq| has been omitted):

%if False
We also allow higher order predicates, called \IxAsDef{dictionary transformers} in the explicit world, to be used.
This is demonstrated by our second large example at which we will look after briefly recapitulating the implementation for
instances requiring context (later we will come back to this).

In the following program fragment the instance for |Eq (List a)| is defined:
%endif

\begin{code}
instance dEqList <: Eq a => Eq (List a) where
  eq = \x y -> ...
\end{code}

The important observation is that in order to be able to construct the dictionary for |Eq (List a)| we
need a dictionary for |Eq a|.
This corresponds to a reading of |Eq a => Eq (List a)| that states that |Eq (List a)| can be proven if |Eq a| holds.
The implementation for this instance is a function taking the dictionary for |Eq a| and constructing
the dictionary for |Eq (List a)|.
This function is called a \IxAsDef{dictionary transformer}.

Higher order predicates can be passed as implicit arguments provided this is specified explicitly.
For example, in |f| we can abstract from the dictionary transformer for |Eq (List a)|,
which can then be passed either implicitly or explicitly:

%% 9-eq6.eh
\begin{code}
f  ::  (forall a . Eq a => Eq (List a))   =>  Int ->  List Int  -> Bool
f  =   \                                      p       q         -> eq  (Cons p Nil) q
\end{code}

The effect is that the creation of the dictionary for |Eq (List Int)|
is now delayed until |f| is evaluated.
It is computed as part of the body of |f|,
whereas without the use of this construct the
dictionary would be computed only globally by:

\begin{code}
let  dEqListInt = dEqList dEqInt
\end{code}

The need for higher order predicates really becomes apparent
when genericity is implemented using the class system.
The following example is taken from Hinze \cite{hinze00derive-type-class}:

\begin{code}
%%9srcfile(eh-frags/9-snd-order1.eh%%)
\end{code}

Hinze's solution essentially relies on the higher order predicate |Binary b => Binary (f b)| in the context of
|Binary (GRose f a)|.
The rationale for this particular code fragment falls outside the scope of this paper,
but the essence of its necessity lies in the definition of the |GRose| data type which uses a type constructor |f| to construct
the type |(f (GRose f a))| of the second member of |GBranch|.
When constructing an instance for |Binary (GRose f a)| an instance for this type is required,
but since |f| is not fixed we
cannot provide an instance for |Binary (f (GRose f a))| in the context of the instance.
However, given |Binary b => Binary (f b)| and the instance for |Binary (GRose f a)| being created,
we can construct the required instance by applying the dictionary transformer associated with the higher order predicate
to the dictionary under construction.
The type of |v1| in the example instantiates to |GRose List Int|; the required dictionary
for the instance |Binary (GRose List Int)| can be computed from |dBI| and |dBL|.

%if False
Note that our syntactic sugar for the insertion of universal quantifiers automatically interprets
the higher order predicate |Binary b => Binary (f b)| as |forall ^ b . Binary b => Binary (f b)|,
that is, universally quantified over the |b| which does not appear elsewhere in the context.
%endif

\paragraph{The finer details}
For our discussion we take the following fragment as our starting point:

\begin{code}
let  f = \p q r s -> (eq p q, eq r s)
in   f 3 4 5 6
\end{code}

Haskell infers the following type for |f|:

\begin{code}
f :: forall a b . (Eq b, Eq a) => a -> a -> b -> b -> (Bool,Bool)
\end{code}

On the other hand, EH would infer for |f|:

\begin{code}
f :: forall a . Eq a => a -> a -> forall b . Eq b => b -> b -> (Bool,Bool)
\end{code}

EH not only inserts quantifiers as close as possible to the place where the quantified type variables occur,
but does this for the placement of predicates in a type as well.
The idea is to instantiate a quantified type variable or pass an implicit parameter
corresponding to a predicate as late as possible, where later is defined as the
order in which arguments are passed.

The Haskell type inferred for |f| also shows that the set of required predicates for |f|
is an unordered set. Haskell does not prescribe an order.
However, if implicit parameters are to be passed explicitly, the order of the predicates
is important, since it tells us on which argument position a value for a predicate is expected:
in EH, the order of the predicates in a type signatures specifies the order
in which the corresponding implicit parameters are to be passed.
In case we want to explicitly pass an argument,
we require a type signature for the called function,
so we know the order of the implicit parameters.
For example, if the dictionary corresponding to the predicate over the
type of the third and fourth parameter of |f| needs to be passed first,
as suggested by the inferred Haskell type,
its type signature has to be specfied explicitly:

\begin{code}
f :: forall a b . (Eq b, Eq a) => a -> a -> b -> b -> (Bool,Bool)
\end{code}

If, for example, the dictionary for the first and second argument needs to be passed first,
this is specified by swapping the two predicates:

\begin{code}
f :: forall a b . (Eq a, Eq b) => a -> a -> b -> b -> (Bool,Bool)
\end{code}

When explicitly
passing an implicit parameter we make use of the fact that predicate instances also stand for actual values in the implementation.
A class declaration introduces a record type for the dictionary
corresponding to the predicate introduced by the class declaration.
For example, the class declaration for |Eq| introduces the record type |(eq :: a -> a -> Bool)|
(record with one field with label |eq|) as the type
of the dictionary to be passed when an implicit parameter for predicate |Eq a| is required.
Now, instead of automatically determining which implicit parameter to pass we construct
a dictionary ourselves, in this case for the second |Eq| predicate of |f|:

%% test/9-eq2.eh
\begin{code}
let  f :: forall a . Eq a => a -> a -> forall b . Eq b => b -> b -> (Bool,Bool)
     f = \p q r s -> (eq p q, eq r s)
in   f  ^                              3 4
        (! (eq = eqMod2) <: Eq Int !)  5 6
\end{code}

The constructed dictionary must be of the expected dictionary type.
This condition is made explicit by means of |<:| (appearing in the source text as @<:@).
The notation |(! e <: p !)| suggests a combination of ``is of type'' and ``is evidence for''.
Here ``is of type'' means that the dictionary |e| must be of the record type introduced by the class declaration
for the predicate |p|.
The phrase ``is evidence for'' means that the dictionary |e| is used
as the proof of the existence of the implicit argument to the function |f|.
By default, this value is computed by the predicate proving machinery of EH
\cite{jones94phd-qual-types}.

By explicitly providing a dictionary the default decision made by EH is overruled.
This is useful in situations where no unique choice is possible, for
example in the presence of overlapping instances:

%% test/9-eq3.eh
\begin{code}
let  instance dEqInt1 <: Eq Int where
       eq = primEqInt
     instance dEqInt2 <: Eq Int where
       eq = dEqMod2
     f = ...
in   f  (! dEqInt1 <: Eq Int !) 3 4
        (! dEqInt2 <: Eq Int !) 5 6
\end{code}

The two instances for |Eq Int| overlap, but we still can refer to each associated dictionary individually,
because of the names |dEqInt1| and |dEqInt2| given to the dictionaries.
Thus ambiguity caused by overlapping
instances can be avoided by letting the programmer
decide which dictionaries to pass to
the call |f 3 4 5 6|.

Overlapping instances can also be avoided by not introducing them in the first place.
However, this conflicts with our goal of allowing the programmer to use different instances at different places
in a program.
This problem can be overcome by stating explicitly that only one instance participates
in the predicate proving machinery,
and by inhibiting participation of the remaining instances,
by using `|::|' instead of `|<:|':

\begin{code}
instance dEqInt2 :: Eq Int where
  eq = \_ _ -> False 
\end{code}

The naming of a dictionary by means of |<:| actually does two things.
It binds the name to the dictionary and it specifies to use this dictionary as the default instance for
|Eq Int|
for use in its proof process.
The notation |::| only binds the name and does not introduce it into proving predicates.
If one at a later point wants to introduce the dictionary nevertheless,
possibly overriding an earlier choice,
this may done by specifying:

\begin{code}
instance dEqInt2 <: Eq Int
\end{code}

In combination with a scoping mechanism for instances
this mechanism allows the programmer to fully specify which instances are
active at any point in the program text:

\begin{code}
let  instance dEqInt1  <:  Eq Int where ...
     instance dEqInt2  ::  Eq Int where ...
in   let  g  = \x y -> eq x y
in   let  v1 =  g 3 4
          v2 =  let  instance dEqInt2 <: Eq Int
                in   g 3 4
in   ...
\end{code}

The value for |v1| is computed with |dEqInt1| as evidence for |Eq Int|,
whereas |v2| is computed with |dEqInt2| as evidence.
Instances are introduced in a scoped regime:
instances introduced in an inner enclosing scope take precedence over the ones introduced
in an outer scope.

As we mentioned earlier,
the declaration of an instance with a context actually introduces a function taking dictionaries
as arguments:

%% test/9-eq4.eh
\begin{code}
let  instance dEqInt <: Eq Int where
       eq = ...
     instance dEqList <: Eq a => Eq (List a) where
       eq = ...
     f :: forall a . Eq a => a -> List a -> Bool
     f = \p q -> eq (Cons p Nil) q
in   f 3 (Cons 4 Nil)
\end{code}

In terms of predicates the instance declaration states that given a proof
for the context |Eq a|, the predicate |Eq (List a)| can be proven.
In terms of values this translates to a function which takes the evidence of the
proof of |Eq a|, a dictionary record |(eq :: a -> a -> Bool)|,
to evidence for the proof of |Eq (List a)|
\cite{jones94phd-qual-types}:

\begin{code}
dEqInt   ::  (eq :: Int -> Int -> Bool)
dEqList  ::  forall a .  (eq :: a -> a -> Bool)
                           -> (eq :: List a -> List a -> Bool)
\end{code}

With these values, the body of |f| is mapped to:

\begin{code}
f = \dEq_a p -> eq (dEqList dEq_a) (Cons p Nil)
\end{code}

This translation can now be expressed explicitly as well;
a dictionary for |Eq (List a)| is explicitly constructed and passed to |eq|:

%% 9-eq5.eh
\begin{code}
f :: forall a . Eq a  =>  a ->  List a  -> Bool
f = \(! dEq_a <: Eq a !)
                      ->  \p    q       -> eq  (! dEqList dEq_a <: Eq (List a) !)
                                               (Cons p Nil) q
\end{code}

The notation |Eq a => Eq (List a)| in the instance declaration for |Eq (List a)| introduces
both a predicate transformation for a predicate (from |Eq a| to |Eq (List a)|),
to be used for proving predicates,
as well
as a corresponding dictionary transformer function.
Such transformers can also be made explicit in the following variant:

%% 9-eq6.eh
\begin{code}
f  ::  (forall a . Eq a => Eq (List a))  =>  Int ->  List Int  -> Bool
f  =   \(! dEq_La <: Eq a => Eq (List a) !)
          ->  \p  q  -> eq  (! dEq_La dEqInt <: Eq (List Int) !)
                            (Cons p Nil) q
\end{code}

Instead of using |dEqList| by default, an explicitly specified implicit predicate transformer, bound to |dEq_La| is used
in the body of |f| to supply |eq| with a dictionary for |Eq (List Int)|.
This dictionary is explicitly constructed and passed to |eq|; both the construction and binding to |dEq_La| may be omitted.
We must either pass a dictionary for |Eq a => Eq (List a)| to |f| ourselves explicitly or let it happen automatically;
in both cases |dEqList| is the only choice possible.

\subsection{Partial type signatures}
\label{ehc09-partialtysig}

Explicitly specifying complete type signatures can be a burden for the programmer,
especially when
types become large and only a specific part of the type needs to be specified
explicitly. EH therefore allows partial type signatures.
We will show its use based on the function:

\begin{code}
f = \p q r s -> (eq p q, eq r s)
\end{code}

for which we infer the following type if no specification of its type is given:

\begin{code}
f :: forall a   .    Eq a            => a -> a -> forall b . Eq b => b -> b -> (Bool,Bool)
\end{code}

\textbf{Variation 1:}
Now, if we want to make clear that the dictionary for |b| should be passed before any of the |a|'s we write:

\begin{code}
f :: forall    b . (Eq b,  ...   ) => ...  -> ...  -> b -> b -> ...
-- INFERRED:
f :: forall a  b . (Eq b,  Eq a  ) => a    -> a    -> b -> b -> (Bool,Bool)
\end{code}

The parts indicated by `|...|' are inferred.

\textbf{Variation 2:}
The dots `|...|' in the type signature specify parts of the signature to
be filled by the type inferencer.
The inferred type may be polymorphic if no restrictions on its type are found by the type inferencer,
or it may be monomorphic as for |r :: Int| in:

\begin{code}
f  ::  forall a   . (  Eq a,  ...   )  =>     a ->  a ->  ...
f  =                                       \  p     q     r       s               ->  (eq p q  ,  eq r 3  )
-- INFERRED:
f  ::  forall a   .    Eq a            =>     a ->  a ->  Int ->  forall b . b    ->  (Bool    ,  Bool    )
\end{code}

\textbf{Variation 3:}
%if False
f  ::  forall a   .    Eq a            =>     a ->  a ->  Int ->  (exists b . b)  ->  (Bool    ,  Bool    )
For |s| any value can be passed; this is encoded by the existential quantification.
The introduction of the existential quantifier is the result of the a quantifier insertion rule which states
that for a single type variable on a contravariant position an |exists| is inserted.
%endif
If instead we still want |s| to have the same type as |r| we can use a more general variant of `|...|' in which
we can refer to the inferred type using a type variable prefixed with a percent symbol '|%|',
called a \IxAsDef{named wildcard}:

\begin{code}
f  ::  forall a   . (  Eq a,  ...   )  =>     a ->  a ->  %b   ->  %b              ->  ...
f  =                                       \  p     q     r        s               ->  (eq p q  ,  eq r 3  )
-- INFERRED:
f  ::  forall a   .    Eq a            =>     a ->  a ->  Int  ->  Int             ->  (Bool    ,  Bool    )
\end{code}

For the remainder of \thispaper\ we mainly use `|...|', called a \IxAsDef{type wildcard},
or \IxAsDef{predicate wildcard}
in predicate positions.
Although the given example suggests that a wildcard may be used anywhere in a type,
there are some restrictions:
\begin{itemize}
\item
A named wildcard |%a| cannot be used as a predicate wildcard,
because it does not make sense to pass the same dictionary twice.
\item
A type wildcard can occur at an argument or result position of a function type.
A type wildcard itself may bind to a polymorphic type with predicates.
In other words, impredicativeness is allowed.
This is particularly convenient for type wildcards on a function's result position.
For example, the type wildcard |%b| in
\begin{code}
f :: forall a . Eq a => a -> a -> %b
\end{code}
is bound to
\begin{code}
forall b . Eq b => b -> b -> (Bool,Bool)
\end{code}
after further type inferencing.
\item
For the non wildcard part of a type signature
all occurrences of
a type variable in the final type must be given.
This is necessary because the type signature will be quantified over explicitly introduced
type variables.
\item
A sequence of explicit predicates may end with a predicate wildcard, standing for
an optional collection of additional predicates.
Multiple occurrences of a predicate wildcard or between explicit predicates would defeat the purpose
of being partially explicit. For example, for the type signature |(Eq b, ..., Eq c) => ...|
the argument position of |Eq c|'s dictionary cannot be predicted by the programmer.
\item
The absence of a predicate wildcard in front of a type
means \emph{no} predicates are allowed.
The only exception to this rule is a single type variable
or a type wildcard,
since these may be bound to a type which itself
contains predicates.
\end{itemize}

\subsection{Implementation}
\label{ehc09-implem}

We implemented the described extension in the EH compiler (EHC)
\cite{dijkstra04ehc-web,dijkstra04thag,dijkstra04thag-part1}.
Because of space limitations we focus on the distinguishing characteristics
of our implementation.

%{
%format ln      = "l_n"
%format sigman  = sigma "_n"
%format Transle = Transl "_e"
%format Translp = Transl "_{" pi "}"
%format pvark   = pvar "^k"
%format tvark   = tvarv "^k"
%format pia     = pi "_a"
%format pid     = pi "_d"
%format piG     = pi "_{" Gamma "}"
%format piak    = pi "_a^k"
%format piik    = pi "_i^k"
%format sigmaa  = sigma "_{a}"
%format sigmad  = sigma "_{d}"
%format sigmag  = sigma "_{" Gamma "}"
%format sigmark = sigma "_r^k"
%format Translik = Transl "_i^k"
%format Transla  = Transl "_a"
%format Transl1
%format Transl2
%format instpi  = inst "_{" pi "}"

The type system is given in \figRef{rules2.exprEv.base}
which describes the relationship between types in the type language in
\figRef{exim-eh-lang-types}.
Our |sigma| types allow for the specification of the usual base types (|Int, Char|) and type variables (|tvarv|) as well
aggregrate types like normal abstraction (|sigma -> sigma|),
implicit abstraction (|pi -> sigma|),
(higher ranked) universal quantification (|forall ^ alpha . sigma|),
%if False
as well as existential quantification (|exists ^ alpha . sigma|),
%endif
predicates (|pi|)
and their transformations (|pi -> pi|).
Translations |Transl| represent code resulting from the transformation from implicit parameter
passing to explicit parameter passing.
An environment |Gamma|
binds value identifiers to types and predicates to translations (dictionary evidence) paired with their type:

\rulerCmdUse{rules2.exprEv.base}

\begin{code}
bind   =  ident :-> sigma |  pi :> Transl : sigma
Gamma  =  Vec(bind)
\end{code}

We use vector notation for any ordered collection, denoted with a horizontal bar on top.
Concatenation of vectors and pattern matching on a vector is denoted by a comma ','.

Type rules in (e.g.) \figRef{rules2.exprEv.base}
read like this: given contextual information |Gamma| it can be proven (|:-|) that
term |e| has (:) type |sigma| and some additional (|~>|) results, which in our case is the code |Transl| in which passing
of implicit parameters has been made explicit.
Later type rules incorporate more properties; these are then separated by a semicolon ';'.
If some properties do not matter or are not used, an underscore '|_|' is used to indicate this.
Rules are labeled with names of the form $x-variant_{version}$ where |x| is a single character indicating the syntactic element,
|variant| its variant and |version| a particular version of the type rule.
For example, \ruleRef{e-app} with `|Ev|' version in \figRef{rules2.exprEv.base} has an extended version `|9|' in
\figRef{rules2.expr9.baseExplImpl} incorporating more implementation
aspects\footnote{The numbered versions correspond to the numbered EH versions \cite{dijkstra04ehc-web,dijkstra04thag}.}.
We have only included those type rules that are directly relevant to the passing of implicit parameters and have omitted
those dealing with the introduction of classes and instances; these are all standard \cite{faxen02semantics-haskell}.

The conciseness of \ruleRef{e-pred} suggests that its implementation should not
pose much of a problem, but the opposite is true.
Unfortunately, in their current form the rules do not fully specify how to combine in order to build a complete proof tree.
This is especially true for the last \ruleRef{e-pred}, since it is not associated with
a syntactic construct of the source language.
Algorithmic variants of the rules have two pleasant properties:

\begin{itemize}
\item
The syntax tree determines how to combine the rules.
\item
By distributing data over a larger set of variables an order in which to compute them becomes visible.
\end{itemize}
The first property is taken care of by the parser, and based on the second property we can implement rules
straightforwardly using an attribute grammar, mapping individual variables in a rule to attributes.
%if False
In general, typing rules give us equations which should hold but unfortunately do not tell us
how to find out whether and under what conditions those rules hold.
Algorithmic variants of typing rules usually are closely connected to the syntactic
structure of a source language,
so it is clear which rule applies for a particular language construct.
Such algorithmic variants of typing rules usually also incorporate additional information which
is passed from and to the premises and conclusions of a rule.
This additional information corresponds to information being passed up and down a syntax tree,
or in terms of an attribute grammar, this information is encoded as synthesized and inherited attributes.
Finding a suitable algorithm for explicit implicit parameters
is further complicated due to a combination of several factors:
%endif
Our situation is complicated due to a combination of several factors:

\begin{itemize}
\item
The structure of the source language cannot be used to determine whether \ruleRef{e-pred} should be applied:
the term |e| in the premise and the conclusion is the same.
Furthermore, the predicate |pi| is not mentioned in the conclusion so discovering whether this rule should be applied
depends completely on the typing rule.
Thus the necessity to pass an implicit parameter may spontaneously pop up at any expression.
\item
In the presence of type inferencing nothing may be known about |e| at all, let alone which implicit parameters it
may take.
This information usually only becomes available after generalization of the inferred type.
\item
These problems are usually circumvented by limiting the type language for types used during
inferencing to predicate-free types.
By effectively stripping a type from both its predicates and quantifiers standard Hindley-Milner type
inferencing becomes possible.
However, we allow predicated as well as quantified types to participate in type inferencing.
Consequently, predicates as well as quantifiers can be present in any type encountered during
type inferencing.
\end{itemize}

So, the bad news is that we do not know where implicit parameters need to be passed;
the good news is that if we represent this lack of knowledge explicitly we can still figure out
if and where implicit parameters need to be passed.
This is not a new idea, because type variables are usually used to refer to
a particular type about which nothing is known.
In a later stage of a type inferencing algorithm such type variables are
replaced by more accurate knowledge, if any.
In our approach we employ also the notion of variables, called \IxAsDef{predicate wildcard variable}s,
representing a yet unknown collection of implicit parameters, or,
more accurately their corresponding predicates.
These predicate wildcard variables are used in a type inferencing/checking algorithm which explicitly
deals with expected (or known) types |sigmak| as well as extra inferred type information.

\begin{figure}
\begin{center}
\begin{tabular}{ll}
%if not useSigplanconfSty
\hline
%endif
Notation & Meaning \\
\hline
|sigma|
 & type
 \\
|sigmak|
 & expected/known type
 \\
|pi|
 & predicate
 \\
|pvar|
 & predicate wildcard (collection of predicates)
 \\
|tvarv|
 & type variable
 \\
|Transl|
 & translated code
 \\
|ident|
 & identifier
 \\
|identv|
 & value identifier
 \\
|identc|
 & constructor/predicate identifier
 \\
|Gamma|
 & assumptions, environment, context
 \\
|Cnstr|
 & constraints, substitution
 \\
|Cnstr|$_{k..l}$
 & constraint composition of |Cnstr|$_k ...$ |Cnstr|$_l$
 \\
|<=|
 & subsumption, ``fits in'' relation
 \\
|fiopt|
 & options to |<=|
 \\
%if not useSigplanconfSty
\hline
%endif
\end{tabular}
\end{center}
\caption{Legenda of type related notation}
\label{exim-eh-legenda-symbols}
\end{figure}

\rulerCmdUse{rules2.exprEvK.pred}

These key aspects are expressed in the adapted rule for predicates shown
in \figRef{rules2.exprEvK.pred}.
This rule makes two things explicit:

\begin{itemize}
\item
The context provides the expected (or known) type |sigmak| of |e|.
Jointly operating, all our rules maintain the invariant that |e| has a type |sigma|
which is a subtype of |sigmak|, denoted by |sigma <= sigmak| (|sigma| is said to be subsumed by |sigmak|),
enforced by a |fit| judgement.
The \ruleRef{e-id} in \figRef{rules2.exprEvK.pred} for variables demonstrates the use of a |fit| judgement;
the handling of |sigmak| in remaining rules and
the use of the |fit| judgement are postponed until the discussion of \figRef{rules2.expr9.baseExplImpl}.
\item
An implicit parameter can be passed anywhere; this is made explicit by stating that
the known type of |e| may start with a sequence of implicit parameters.
This is expressed by letting the expected type in the premise be |pvar -> sigmak|.
\end{itemize}

A predicate wildcard variable makes explicit that we can expect a (possibly empty)
sequence of implicit parameters
and at the same time gives an identity to this sequence.
The type language for predicates thus is extended with a predicate wildcard variable |pvar|,
corresponding to the dots `|...|' in the source language for predicates:

\begin{code}
pi     ::=  I (Vec(sigma))
       |    pi => pi
       |    pvar
\end{code}

In terms of an algorithm, the expected type |sigmak| travels top-to-bottom in the
abstract syntax tree and is used for type checking, whereas |sigma| travels bottom-to-top
and holds the inferred type.
If a fully specified expected type |sigmak| is passed downwards, |sigma| will turn out to be equal to this type.
If a partially specified type is passed downwards the unspecified parts may be filled in by the
type inferencer.

\rulerCmdUse{rules2.expr9.baseExplImpl}

The adapted typing \ruleRef{e-pred} in \figRef{rules2.exprEvK.pred}
still is not much of a help as to when it should be applied.
However, as we only have to deal with a limited number of language constructs,
we can use case analysis on the source language constructs.
In \thispaper\ we only deal with function application, for which the relevant rules are shown in their full glory
in \figRef{rules2.expr9.baseExplImpl}.
The rules in \figRef{rules2.expr9.baseExplImpl} look complex.
The reader should realize that the implementation is described using an attribute grammar system
\cite{dijkstra04thag,baars04ag-www} which allows the independent specification of all aspects
which now appear together in a condensed form in \figRef{rules2.expr9.baseExplImpl}.
The tradeoff is between compact but complex type rules and more lengthy but understandable attribute grammar notation.

The versions for \ruleRef{e-app} and \ruleRef{e-lam} in \figRef{rules2.expr9.baseExplImpl}
are directed towards an implementation; additional information flows through the rules to
provide extra contextual information.
The additional parameter |fiopt| influences certain aspects of subsumption |<=| which we will further ignore
in \thispaper.
Also, the rule is more explicit in its handling of constraints computed by the rule labeled |fit|
for the subsumption |<=|;
a standard substitution mechanism constraining the different variable variants is
used for this purpose:

\begin{code}
bindv  =  tvarv :-> sigma | pvar :-> pi , pvar | pvar :-> pempty
Cnstr  =  Vec(bindv)
\end{code}

The mapping from type variables to types |tvarv :-> sigma| constitutes the usual substitution for type variables.
The remaining alternatives map an predicate wildcard variable to a possibly empty list of predicates.

From bottom to top, \ruleRef{e-app} in \figRef{rules2.expr9.baseExplImpl} reads as follows
(to keep matters simple we do not mention the handling of constraints |Cnstr| and the use of |fiopt|).
The result of the application is expected to be of type |sigmak|,
which in general will have the structure |pvark -> tvark|.
This structure is enforced and checked by the subsumption check described
by the rule |fit|;
the rule binds |pvark| and |tvark| to the matching parts of |sigmak| similar to pattern matching.
We will not look into the |fit| rules for |<=|;
for this discussion it is only relevant to know that if a |pvar| cannot be matched to
a predicate it will be constrained to |pvar :-> pempty|.
In other words, we start with assuming that implicit parameters may occur everywhere and subsequently we try
to proof the contrary.
The subsumption check |<=| gives a possible empty sequence of predicates |Vec(piak)| and the
result type |sigmark|.
The result type is used to construct the expected type |pvar -> tvarv -> sigmark| for |e1|.
The application |e1 ^^ e2| is expected to return a function which can be passed evidence for |Vec(piak)|.
We have to create fresh identifiers |Vec(Translik)| bound to these predicates.
Function |instpi| provides these names bound to the instantiated variants |Vec(piik)| of |Vec(piak)|.
The names |Vec(Translik)| are used in the translation, which is a lambda expression accepting |Vec(piak)|.
The binding |Vec(piik :> Translik)| is used to extend the type checking environment |Gamma| for
|e1| and |e2| which both are allowed to use these predicates in any predicate proving taking place in these expressions.
The judgement for |e1| will give us a type |Vec(pia) -> sigmaa -> sigma|, of which |sigmaa|
is used as the expected type for |e2|.
The predicates |Vec(pia)| need to be proven and evidence computed; the top judgement |pred| does this.
Finally, all the translations together with the computed evidence forming the actual implicit parameters |Vec(pia)|
are used to compute a translation for the application which accepts the implicit parameters it is supposed to accept.
The body |Transl1 ^ Vec(Transla) ^ Transl2| of this lambda expression contains the actual application itself.
The implicit parameters are passed before the argument.

Even though the rule for implicitly passing an implicit parameter already provides a fair amount of detail,
some issues remain hidden.
For example, the typing judgement for |e1| gives a set of predicates |pia| for which the corresponding
evidence is passed by implicit arguments.
The rule suggests that this information is readily available in an actual implementation of the rule.
However, assuming |e1| is a |let| bound function for which the type is currently being inferred,
this information will only become available
when the bindings in a |let| expression are generalized \cite{jones99thih},
higher in the corresponding abstract syntax tree.
Only then the presence and positioning of predicates in the type of |e1| can be determined.
This complicates the implementation because this information has to be redistributed over
the abstract syntax tree.

\RuleRef{e-lam} for lambda expressions from \figRef{rules2.expr9.baseExplImpl} follows a similar strategy.
At the bottom of the list of premises we
start with an expected type |sigmak| which by definition has to accept a normal parameter and a
sequence of implicit parameters.
This is enforced by the judgement |fit| which gives us back predicates |Vec(pia)| used in a similar fashion as in
\ruleRef{e-app}.

\rulerCmdUse{rules2.expr9.explimpl}

Whereas the rules in \figRef{rules2.expr9.baseExplImpl} describe the implicit passing of parameters,
the rules \figRef{rules2.expr9.explimpl} describe their explicit counterpart, that is,
the use of the |(! ... !)| notation.
Because we require the explicit specification of predicates inside |(! ... !)| the
rules in \figRef{rules2.expr9.explimpl} actually are simpler than the rules for normal application.
For example, in \ruleRef{e-iapp} we do not perform any proving of predicates but query the environment
directly to obtain the dictionary type |sigmad| for the predicate |pid|.
Judgement |fit| is then used to propagate type information from the predicate to the dictionary type.
The dictionary type |sigmad| is then used for further type checking.

EH's proof machinery required for predicates is standard
\cite{faxen02semantics-haskell,jones94phd-qual-types,jones00thih} except for the scoping mechanism introduced.
We only note that the proof machinery must now take into account the scoped availability of instances and can no longer assume
their global existence.

\subsection{Discussion and related work}
\label{ehc09-discussion}

\paragraph{How much explicitness is needed}
Being explicit by means of the |(! ... !)| language construct very soon becomes cumbersome because
our current implementation requires full specification of all predicates involved inside |(! ... !)|.
Can we do with less?

\begin{itemize}
\item
\RuleRef{e-iapp} from \figRef{rules2.expr9.explimpl} uses the predicate |pi2| in |(! e2 <: pi2 !)|
directly, that is, without
any predicate proving, to obtain |pid| and its corresponding dictionary type |sigmad|.
Alternatively we could interpret |(! e2 <: pi2 !)| as an addition of |pi2| to the set of predicates used
by the predicate proving machinery for finding a predicate whose dictionary matches the type
of |e2|.
However, if insufficient type information is known about |e2| more than one solution may be found.
Even if the type of |e2| would be fully known, its type could be coerced in dropping record fields so as to match different
dictionary types.
\item
We could drop the requirement to specify a predicate: |(! e2 !)| instead of |(! e2 <: pi2 !)|.
In that case we would need a mechanism to find a predicate for the type of the evidence provided by
|e2|.
This is most likely to succeed in the case of a class system as the functions introduced by a class need to have
globally unique names.
For other types of predicates like those for dynamically scoped values this is less clear.
By dropping the predicate in |(! e2 !)| we also loose our advocated advantage of explicitness because we can no longer
specify type related information.
\item
The syntax \ruleRef{e-ilam} requires a predicate |pi| in its implicit argument |(! p <: pi !)|.
It is sufficient to either specify a predicate for this form of a lambda expression or to specify a predicate
in a corresponding type annotation.
\end{itemize}

It is yet unclear which of these routes lead to a useful and workable solution for the programmer.
The current solution may at times be cumbersome but one can live without it.
If the need arises our solution gives the programmer the full power of being explicit in what is required.

\paragraph{Binding time of instances}
One other topic deserves attention, especially since it deviates from the
standard semantics of Haskell.
In the example for |nub|, the invocation of |nub| is parameterized with a modified record:

\begin{code}
nub  (! (dEqInt | eq := ...) <: Eq Int !)
     (Cons 3 (Cons 3 (Cons 4 Nil)))
\end{code}

In our implementation |Eq|'s function |ne| invokes |eq|, the one provided by
means of the explicit parameterization.
This corresponds a late binding, much in the style employed by object oriented languages.
This is a choice out of (at least) three equally expressive alternatives:

\begin{itemize}
\item Our current solution, late binding as described. The consequence is that
all class functions now take an additional (implicit) parameter, namely the dictionary where
this dictionary function has been retrieved from.
\item Haskell's solution, where we bind all functions at instance creation time.
In our |nub| example this would mean that |ne| still will use |dEqInt|'s |eq| instead of the |eq|
provided in the updated |(dEqInt || eq := ...)|.
\item A combination of these solutions, for example, default definitions use late binding, instances use Haskell's
binding.
\end{itemize}

It is yet unclear which solution to take as the default case,
but we notice that whatever approach is taken, the programmer has all the means available to
express his differing intentions.


%}

\paragraph{Extensible records}
\label{ehc09-others}

Implicit parameters not only implement the passing of dictionaries as evidence
for predicates. In Haskell, extensible records (if implemented)
also use the available predicate proving machinery:
integer offsets into records are the evidence for so called lacking predicates describing
where a value for a labeled field should be inserted
\cite{jones94phd-qual-types,gaster96poly-ext-rec-var,jones99lightweight-ext-rec}.

\paragraph{Dynamically scoped variables}
GHC \cite{www04ghc} enables the passing of plain values as
dynamically scoped variables (also confusingly known as implicit parameters).
It is possible to model this effect
\cite{jones99impl-param,lewis00implicit-param,www04ghc}
with the concepts described thus far.
For example, the following program uses dynamically scoped variable |?x|:

\begin{code}
let  f   ::  (?x :: Int) =>  ...
     f   =   \               ... -> ... ?x + 2 ...
     ?x  =   3
in   f ...
\end{code}

The signature of |f| specifies a predicate |?x :: Int|,
meaning that |f| can refer to dynamically scoped variable |x| with type |Int|.
Its value is introduced as a binding in a |let| expression and used in the body
of |f| by means of |?x|.
This can be encoded using the class system:

\begin{code}
let  class Has_x a where
       value_x :: a
     f  ::  (Has_x Int) =>  ...
     f  =   \               ... -> ... value_x + 2 ...
     instance Has_x Int where
       value_x = 3
in   f ...
\end{code}

We only mention briefly some issues with this approach:

\begin{itemize}
\item
The type for which an instance without context is defined usually is specified explicitly.
This is  no longer the case for |?| predicates if an explicit type signature for
e.g. |let ?x = 3| is omitted.
\item
GHC \cite{www04ghc} inhibits dynamically scoped variable predicates in the context of instance declarations because it is unclear
which scoped variable instance is to be taken.
Scoping for instances as available in EHC may well obviate this restriction.
\item
Use of records for dictionaries can be optimized away because each class contains a single field only.
\end{itemize}

\paragraph{Named instances}
Scheffczyk has explored named instances as well
\cite{kahl01named-instance,scheffczyk01mth-namedinst}.
Our work differs in several aspects:
\begin{itemize}
\item
Scheffczyk partitions predicates in a type signature into ordered and unordered ones.
For ordered predicates one needs to pass an explicit dictionary, unordered ones are those
participating in the normal predicate proving by the system.
Instances are split likewise into named and unnamed instances.
Named instances are used for explicit passing and do not participate in the predicate proving.
For unnamed instances this is the other way around.
Our approach allows a programmer to make this partitioning explicitly, by stating which
instances should participate in the proof process.
In other words, the policy of how to use the implicit parameter passing mechanism
is made by the programmer.
\item
Named instances and modules populate the same name space, separate from
the name space occupied by normal values.
This is used to implement functors as available in ML \cite{leroy94manif-ty-mod,leroy95appl-func-mod}
and as described by Jones \cite{jones96paramsig-mod} for Haskell.
Our approach is solely based on normal values already available.
%if False
In Scheffczyk work
named instances populate a module name space.
Vice versa, modules can be used as implicit parameters.
Our approach does not introduce a new name space, but uses normal values.
These values are explicitly lifted to the `evidence for predicate' domain of the compiler.
No special syntax is required to perform computations on these values.
%endif
\item
Our syntax is less concise than the syntax used by Scheffczyk.
This is probably difficult to repair because of the additional notation
required to lift normal values to the evidence domain. 
\end{itemize}

The type inferencing/checking algorithm employed in \thispaper\ is described
in greater detail in
\cite{dijkstra04thag,dijkstra04thag-part1}
and its implementation is publicly available \cite{dijkstra04ehc-web},
where it is part of a work in progress.
Similar strategies for coping with the combination of inferencing and checking
are described by Pierce
\cite{pierce00local-type-inference}
and Peyton-Jones
\cite{peytonjones04pract-inf-rank}.

\subsection{Conclusion}
\label{ehc09-concl}

Allowing explicit parameterization for implicit parameters gives the programmer an
additional mechanism for reusing existing functions.
It also makes explicit what otherwise remains hidden inside the bowels of a compiler.
We feel that this a 'good thing': it should be possible to override automatically made decisions.

%if False
The approach taken in \thispaper\ still leaves much to be sorted out.
In particular the relation with functional dependencies of multiparameter type classes,
existentials.
%endif

We have implemented all features described in \thispaper\ in the context of the ninth of a series
of ten compilers for EH.
Part of these compilers are described more extensive elsewhere \cite{dijkstra04thag-part1,dijkstra04thag};
in this paper we have presented the relevant part concerning explicit implicit parameters in an as compact
form as possible.
To our knowledge our implementation is the first combining language features like
higher ranked types, existentials, class system, explicit implicit parameters and extensible records
into one package together with a description of its implementation.
We feel that this has only been possible thanks to the use of an attribute grammar system which
allows us to independently describe all the separate aspects.

On a metalevel one can observe that the typing rules incorporate many details,
up to a point where their simplicity may easily get lost.
A typing rule serves well as a specification of the semantics of a language construct,
but as soon as a typing rule evolves towards an algorithmic variant
it may well turn out that other ways of describing, in particular attribute grammars,
are a better vehicle for expressing implementation aspects.

%else

Extensible records as case study

Subsumption and proof of impl params

Prolog alike proof system/rules combined with coercion terms ????

Key sentence: instead of leaving the location implicit parameters open we make possible locations for
impl params explicit.
Absence of a possible location now explicitly means no impl param is allowed instead of 'maybe allowed'.


\subsection{Proposal}

An implicit parameter is like a normal parameter but the actual passing of it
may be omitted.
If an implicit parameter is passed explicitly the compiler will make an attempt
to guess the actual value to be passed, based on some rules.
The underlying idea/rationale is that classes, extensible records can be modelled
with this mechanism.
This section contains some fantasy examples as well as choices which can be made.

\paragraph{Syntax.}
On the type level an implicit parameter can be specified with

\begin{code}
let  f  ::  r lacks l =>  (r | l :: a)  -> a
     f  =                 \r            -> r.l
\end{code}

Alternatively a more explicit notation could be employed:

\begin{code}
let  f  ::  (! r lacks l !) ->  (r | l :: a)  -> a
     f  =                       \r            -> r.l
     g  ::  (! Eq a !) ->  [a] ->  [a] -> Bool
     g  =   \(! eq !)      \x      \y  -> (==) (!eq!) ... && (==) ...
\end{code}

The latter approach has a couple of advantages.
\begin{itemize}
\item
LL parsing is made easier.
\item
The same notation can be used for expressions (value terms) and patterns.
\end{itemize}

The obvious syntactic sugar can be added, e.g. |(! p, q !) ->| for |(! p !) -> (! q !) ->|

The idea is that two worlds of terms co-exist, one for normal (explicit) values and one for implicit values.
Implicit values are associated with a predicate.
Explicit values always are passed to functions explicitly, whereas implicit values are not necessarily passed
explicitly.
If an implicit parameter is required but not given rule based proof machinery will try to find
an appropriate implicit value from the world of implicit values.
On the other hand, a value given as a parameter is added to the implicit value world so
the proof machinery can use this value too to determine the appropriate parameter value.
The following typing rule attempts to express this:
\[
\rulerCmdUse{rules.expr9.app.e-app9-expl-expl}
\]

This involves some additional notation:
\begin{itemize}
\item
In this rule we assume that each value term is translated to a form
where implicit parameters are explicit. This translation is denoted by |Transl|.
\item
We also assume that |Gamma| holds values from the implicit world
by means of bindings of the form |[pi :~> e]|, associating predicates |pi| to implicit values |e|.
\item
The type language has an additional alternative:
\begin{code}
sigma  =  ..
       |  (! pi !)
       |  (! ... !)
pi     =  r lacks \
       |  C ^^ Vec(sigma)
       |  v = sigma
       |  pivar
\end{code}
The alternatives for |pi| respectively denote the lacking constraint for extensible records, class constraint and equality constraint
(for use by generalized data types).
Partial type signatures w.r.t. predicates are denoted by |(! ... !)|.
Alternatively, the more concise denotations |pi| and |pvar| are used for |(! pi !)| and |(! ... !)| respectively.
A predicate var |(! pivar !)| is shorthanded by |pivar|.
\item
The constraint language has to deal with additional constraints on predicates:
\begin{code}
Cnstr  =  ..
       |  pivar  :-> pi
       |  pvar   :-> pi , pvar
       |  pvar   :-> pempty
\end{code}
These additional
alternatives deal with 
which predicate may replace a predicate variable |p| or how much predicates may replace a wildcard |(! ... !)| or |pvar|.
The constraint |p :-> pi| is similar to type variables; the |pvar :-> | constraints limit the number of
predicates.
\item The environment |Gamma| now also may contain evidence for predicates:
\begin{code}
Gamma  =  ..
       |  pi :~> Transl
\end{code}
|Transl| is a piece of code representing the proof evidence for the predicate.
A |Transl| may contain holes referring to not yet resolved predicates, allowing
delay of proving predicates.
A substitution mechanism substitutes these holes with the actual proof evidence.
\end{itemize}

The idea here is that if an implicit parameter |pia| is expected, whereas an expression translating to |Transl2| is given,
this |Transl2| is related (in the world of implicit values) to a predicate |piasigma| which can be used to
prove |pia|.
The translation |Transl2pi| of |pia| is then passed to the function.

The implementation of this typing rule probably will have to deal with:
\begin{itemize}
\item
Explicitly given parameters on an implicit position take preference in the computation of the proof.
This implies a priority mechanism to disambiguate overlapping predicates, i.e. multiple proofs.
\item
The given rule assumes the type of the function is known.
In this situation first unification should be applied to the type structure without implicit parameters,
then proving predicates takes place.
Proving should not yield additional constraints on type variables because of the expected complexity/backtracking.
\item
If the type of the function is not/partially known,
inferencing will take place.
Either it is known that an implicit parameter is given, in which case we have to find the associated predicate.
Or only after the complete context has been inferred it becomes clear that an additional implicit parameter should have been
passed.
\end{itemize}

\subsection{Specifying rules}

Predicates corresponding to class declarations are introduced by:
\begin{code}
pred  Eq a :~> ( eq :: a -> a -> Bool )
pred  Eq a :~> x => Ord a :~> ( lt :: a -> a -> Bool, Eq :: (! Eq a !) ) = ( r | Eq = x )
\end{code}
This should be read as:
\begin{itemize}
\item
A predicate |Eq a| has as its proof/evidence/witness object a record with a function |eq|.
\item
A predicate |Ord a| has as its proof a record with a function |lt| and another record |Eq| (for the superclass).
The proof object for |Eq| value is declared implicit.
However, additional evidence can be specified, here |Eq a :~> x|.
Additionally it can be used to partially specify its initialization.
In this way also default fields should be specifiable.
\\
Issues: translation to function taking |x| as well as |r| as offset params as parameter? Can this work, is it a good idea at all?
Or just only use the notation but under the hood use record structure as being known anyway.
\end{itemize}

Rules, corresponding to instance declaratations, populate the world of predicates:
\begin{code}
rule Eq Int = ( eq = primEqInt )
rule Eq a :~> e => Eq [a] :~> l =  ( eq = \a -> \b ->  eq (! e !) (head a) (head b) &&
                                                       eq (! l !) (tail a) (tail b)
                                   )
\end{code}
This should be read as:
\begin{itemize}
\item
A predicate |Eq Int| is a given 'fact', combined with how to build it, i.e. its translation |Transl|.
\item
A predicate for |Eq [a]| on lists can be constructed if a |Eq a| is given.
In the example the implicit parameters are explicitly given, in principle this can be deduced.
\end{itemize}

The introduction of |Ord a| also requires an additional rule for retrieving the superclass:
\begin{code}
rule Ord a :~> o => Eq a :~> e = o.Eq
\end{code}

Implementation issues:
\begin{itemize}
\item All |=>|'s translate to function |->|'s.
\item The |=>| of |pred| and |rule| are accumulative.
\item Proof machinery, cycles, ...
\end{itemize}

\paragraph{Giving names to rules.}
Rules can be given names
\begin{code}
rule eqInt1 :: Eq Int = ( eq = primEqInt )
\end{code}

\paragraph{Implicit parameter expressions.}
Sofar an explicit parameter passed to a function just consisted of an identifier.
Implicit parameter application could/should also be allowed
\begin{code}
rule eqList1 :: Eq a :~> e => Eq [a] :~> l =  ( eq = \a -> \b ->  eq (! e !) (head a) (head b) &&
                                                                  eq (! l !) (tail a) (tail b)
                                              )

... eq (! eqList1 eqInt1 !) ...
\end{code}

\paragraph{Eq as predicate.}

Generalized abstract data types, Arthur's stuff.

\paragraph{Multiparameter type classes.}

Combination with existentials.
Functional dependencies lead to additional existentials?

\paragraph{Overlapping resolutions/instances.}

Partially solve by cost model?
Cost increases compositionally over constructs  which  involve additional runtime computation.

\subsection{Implementation of proof machinery}

It would be very nice of the proof machinery could be parameterized with ``how a single predicate is computed/simplified''.
Perhaps something like
\begin{code}
class OnePredProof pr where
  resolve :: PredOcc pr -> [ProvenPred] -> PrfState

data PredOcc a
  =  PredOcc
       { pr :: a, ref :: UID }
       
data ProvenPred
  =  ProvenPred
       { pr :: Pred, transl :: Transl, ref :: UID, cost :: Int }

data PrfState
  =  PrfState
       { introProven  :: [ProvenPred]
       , intermProven :: [ProvenPred]
       , proven       :: [ProvenPred]
       }
\end{code}

For each type of predicate a one step proof step is factored out.
This step is given the world of available/proven predicates and will yield additional proven predicates
as |PrfState|.
A |PrfState| holds the discharged/proven predicates in |proven|.
For example |Eq Int| if this known (i.e. |`elem` ProvenPred|).
Other predicates certainly cannot be proven, e.g. |Eq a|.
Returned in |introProven| it will and up as an implicit parameter passed to the function for which the proof takes place.
Finally, simplification may yield intermediate steps in |intermProven|, for example |Eq [a]| might be deduced from |Ord [a]|,
but also deduced from |Eq a| combined with |Eq a => Eq [a]|.

It probably would be even nicer if the specification of |OnePredProof| could be done
by the Haskell programmer itself.

\subsection{Expr rules}

\paragraph{Starting point.}
If type specifies an implicit parameter is expected, give it the corresponding translation/evidence |Transl| for it
as a parameter:

\[
\rulerCmdUse{rules.expr9A.e-pred9A}
\]

Issues/problems:
\begin{enumerate}
\item
Type inference means we do not yet know if a predicate must be passed.
\item
Yet the explicit passing via |(! expr !)| requires the presence of an implicit parameter (i.e. predicate in the corresponding type).
\item
If given a |(! expr !)| how do we find the corresponding predicate |pi| for which |expr| is the evidence/proof?
\end{enumerate}

Corresponding solutions:
\begin{enumerate}
\item
Make the possible locations for implicit parameters explicit.
\item
A possible location can be encoded as an 'predicate wildcard variable' |pvar|, which can be constrained to be a set of predicates:
\[
\rulerCmdUse{rules.expr9B.e-pred9B}
\]
Fitting |<=| will take care of constraining |pvar| so the problem is (more or less) reduced to determine where a known type
|sigmak| may be given possible implicit parameters |pvar|, denoted by |pvar -> sigmak|.
In principle, always an implicit parameter may be taken except when specified otherwise by an explicit type signature.
This means that in |let| expressions (yes/no type sig for val decl) and applications (yes/no type for arg) a choice between
no/yes |pvar -> sigmak| as known type to be passed downwards has to be made.
\item
\begin{enumerate}
\item
Matching with the type of evidence, but this is most likely not 1-1.
It might work for classes because the corresponding record is unique (names in it may not be re-used in other classes).
But for |Int| offsets for lacking constraints?
\item
Require a more explicit notation where each evidence has an evidence type.
For example, the evidence of each instance would have type |data Dict c r = Dict r| with |r| a record
and |c| a phantom type equal to the class name.
The type |Dict| is only used for class instances so if a |Dict Eq (...)| value is found we know it
deals with instances of class |Eq|.
\end{enumerate}
\end{enumerate}

\paragraph{Case analysis on context.}

We have to look at the three base cases, lambda expression, application and the atomic expressions (identifier, ...).
We now assume that the context of a rule specifies if implicit variables are allowed, encoded as discussed earlier by means
of a predicate wildcard variable |pvar|. The basic idea is to match a fresh type containing a |pvar| via |fitsIn| to the known type.
The type rule for identifiers reflects this:

\[
\rulerCmdUse{rules.expr9.part2.e-ident9}
\]

In principle, we could do the same for the other cases were it not for the fact that the coercions computed by
|fitsIn| cannot use of not yet inferred information about predicates.
This would lead to many lambda abstractions and applications which could be removed by |eta|-reduction in a later stage,
but clutter resulting translations in the meantime.
So, for now, both the rules for application and lambda abstraction introduce predicate wildcard variables which can be referred to
later on to find out which implicit values need to passed.
Only after the type inferencer is ready this information becomes available.

Now let us first look at the rule for lambda abstraction:

\[
\rulerCmdUse{rules.expr9.part2.e-lam9}
\]

The use of |pvar| allows for implicit parameters in front of the first argument.
The fitting gives us the actual list of implicit parameters.
The typing rule  assumes this list is fully known but in the implementation this only
partially true, a part of the implicit parameters is known and can be used as the context for
typing the body, whereas additional implicit parameters may be inferred later on.
This is ignored in the rule.

Similarly for an application, both the applied function as well as the result may accept implicit parameters:

\[
\rulerCmdUse{rules.expr9.app.e-app-impl9-impl}
\]

However, the implicit parameters for the function need to be passed as arguments to
the function, whereas the implicit parameters for
the result are assumed via the context and thus passed as as arguments of a lambda expression to the complete expression.
The translation of the application reflects this.

\rulerCmdUse{rules.expr9.app}
\rulerCmdUse{rules.expr9.part2}

\rulerCmdUse{rules2.expr4}
\rulerCmdUse{rules2.expr3}
\rulerCmdUse{rules2.expr2}
\rulerCmdUse{rules2.expr1}
\rulerCmdUse{rules2.exprB1}
\rulerCmdUse{rules2.exprA1}
\rulerCmdUse{rules2.pat4}
\rulerCmdUse{rules2.pat2}
\rulerCmdUse{rules2.pat1}
\rulerCmdUse{rules2.fit9.base}
\rulerCmdUse{rules2.fit1.base}




\subsection{Fitting rules}

\rulerCmdUse{rules.fit9.app}
\rulerCmdUse{rules.fit9.predSymmetric}
\rulerCmdUse{rules.fit9.predAsymmetric}
\rulerCmdUse{rules.fit9.rec}



\subsection<article>{Literature}

Named instances \cite{kahl01named-instance}.

%endif %% storyExplImpl

%}

%endif %% incl09

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl10

\section{EH 10: extensible records}
\label{ehc10}

\subsection{Coercion}

\begin{center}
\begin{tabular}{l||||p{.12\textwidth}p{.14\textwidth}||p{.12\textwidth}p{.12\textwidth}||p{.12\textwidth}p{.12\textwidth}}
|<=|            & |(x)| & |(x,y)|   & |(c2||x)| & |(c2||x,y)|   & |(r2||x)| & |(r2||x,y)|   \\
\hline
\hline
|(x)|           &
& |fail|        &
|fail| & |fail|         &
|r2 :-> ()| & |fail|                                \\
|(x,y)|         &
|\r -> (r.x)| &     &
|fail| & |fail|         &
|r2 :-> (y)| & |r2 :-> ()|                              \\
\hline
|(c1||x)|       &
|\r -> (r.x)| & |fail|      &
|c1 == c2| & |fail|         &
|r2 :-> c1| & |fail|                        \\
|(c1||x,y)|     &
|\r -> (r.x)| & |\r -> (r.x,r.y)|       &
|c1 == c2|, |\r -> r - y| & |c1 == c2|      &
|r2 :-> (c1||y)| & |r2 :-> c1|                              \\
\hline
|(r1||x)|       &
|\r -> (r.x)| & |r1 :-> (r'||y)|, |\r -> (r.x,r.y)|     &
|r1 :-> c2| & |r1 :-> (c2||y)|          &
|r1 :-> r2| & |r1 :-> (r2||y)|                              \\
|(r1||x,y)|     &
|\r -> (r.x)| & |\r -> (r.x,r.y)|       &
|r1 :-> c2|, |\r -> r - y| & |r1 :-> c2|            &
|r2 :-> (r1||y)| & |r1 :-> r2|                              \\
\end{tabular}
\end{center}

Notes:
\begin{itemize}
\item
By rebuilding a record if the |rhs| of |<=| is fully known,
we can be more liberal in constraining the |lhs|.
For example, in |(r1||x) <= (x)| it is not necessary to restrict |r1 :-> ()|.
\end{itemize}

\subsection{Low level code for update/extend}

\begin{TT}
Node* old[] ;
Node* new[] ;
struct{enum {UPD;EXT} what; int offset; Node* val} upd[] ;
for ( o = 0, n = 0, u = 0 ; u < upd.length ; ) {
  for ( ; o < upd[u].offset ; ) {
    new[n++] = old[o++]
  }
  switch( upd[u].what ) {
    case EXT : new[n++] = upd[u].val ; break ;
    case UPD : new[n++] = upd[u].val ; o++ ; break ;
  }
  u++ ;
}
for ( ; o < old.length ; ) {
  new[n++] = old[o++] ;
}
\end{TT}

Notes:
\begin{itemize}
\item
All offsets are expressed in terms of the original/old record.
\item
All offsets relate to labels for which it can be assumed that they only occur once.
In particular for equal offsets for insertions we may assume that those offsets occur in the order
of their corresponding labels.
\item
The @upd[..].offset@'s should be sorted.
If this can statically be determined, loop unrolling can be done.
This cannot be determined statically for label polymorphism.
Sorting is done on offset, and where equal extensions before updates.
This allows the mentioned algorithm.
\end{itemize}

\subsection<article>{Literature}

%endif %% incl10




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl11

\section{EH 11: generalized abstract data types}
\label{ehc11}

\subsection{Type language}

\begin{code}
sigma  =  ...
       |  tvare /=/ sigma
\end{code}

\subsection{Type rules}

A few notes:
\begin{itemize}
\item
Threading of constraints/substitution until now has been implicit.
Only result constraints have been compositionally described.
This bites back now because a more precise manipulation of constraints is required.
The implementation correctly expresses the required threading but the type rules here/currently handle it via the
immediate application of constraints to |Gamma| and the expected type |sigmak|.
This requires fixing in all type rules dealing with constraints and/or see remarks in \secRef{ehc2}.
\item
A pattern in a lambda expression require similar treatment as a pattern in a case alternative.
This has been omitted.
\end{itemize}

\rulerCmdUse{rules.fit11.varGADT}
\rulerCmdUse{rules.fit11.gadt}
\rulerCmdUse{rules.pat11}
\rulerCmdUse{rules.casealt11}
\rulerCmdUse{rules.casealts11}
\rulerCmdUse{rules.expr9.case}

\subsection<article>{Literature}

%endif %% incl11




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 12
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl12

\section{EH 12: type synonyms}
\label{ehc12}

with explicit kinding, plus checking thereof

as lambda's on type level, effect on fitsIn (where expansion takes place).

\begin{code}
L :: * -> *
data L a = N | C a (L a)
\end{code}

\subsection<article>{Literature}

%endif %% incl12



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl13

\section{EH 13: modules}
\label{ehc13}

built on records

\subsection<article>{Literature}

\cite{mitchell88absty-exist,leroy94manif-ty-mod,leroy95appl-func-mod}
\cite{laufer96class-existential}

%endif %% incl13


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 14
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl14

\section{EH 14: type property propagation}
\label{ehc14}

co-contra variance

uniqueness??

coercions

\subsection<article>{Literature}

%endif %% incl14


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 15
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if incl15

\section{EH 15: syntax macro's}
\label{ehc15}

or other rewritings in the form a pre parser ??

do notation

list comprehension

if expression

case expression pattern reordering/compiler

infix declarations

binding group analysis, dependency analysis

\subsection<article>{Literature}

%endif %% incl15

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC ??
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclXX

\section{EH ??: genericity, |deriving|}

|deriving| for data structures

%endif %% inclXX


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC XX-1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclXX

\section{EH XX-1: data structure optimizing}
\label{ehcXX1}

Converting O(n) structures to faster (i.e., [] -> Set, assoc list -> Map)

\subsection<article>{Literature}

%endif %% inclXX



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC XX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclXX

\section{EH XX: Haskell front end}
\label{ehcXX}

UHC ??

error messaging, line/col position, comment ????

\subsection<article>{Literature}

%endif %% inclXX






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Conclusion
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclConcl

\section{Remarks, experiences and conclusion}
\label{ehcConcl}

\paragraph{AG system.}
At the start of \thispaper\ we did make a claim that our ``describe separately'' approach contributes
to a better understood implementation of a compiler, in particular a Haskell compiler.
Is this true?
We feel that this is the case, and thus the benefits outweigh the drawbacks, based on some observations made during this project:

The AG system provides mechanisms to split a description into smaller fragments, combine those fragments and redefine part
of those fragments.
An additional fragment management system did allow us to do the same with Haskell fragments.
Both are essential in the sense that the simultaneous `existence' of a sequence of compiler versions,
all in working order when compiled, with all aspects described with the least amount of duplication,
presentable in a consistent form in \thispaper\ could not have been achieved without these mechanisms and supporting tools.

The AG system allows focusing on the places where something unusual needs to be done, similar to other approaches
\cite{laemmel03boilerplate}.
In particular, copy rules allow us to forget about a large amount of plumbing.

The complexity of the language Haskell, its semantics, and the interaction between features is not reduced.
However, it becomes manageable and explainable when divided into small fragments.
Features which are indeed independent can also be described independently of each other by different attributes.
Features which evolve through different versions, like the type system, can also be described separately,
but can still be looked upon as a group of fragments.
This makes the variation in the solutions explicit and hence increases the understanding of what really makes the difference
between two subsequent versions.

On the downside, fragments for one aspect but for different compiler versions end up in different sections of \thispaper.
This makes their understanding more difficult because one now has to jump between pages.
This is a consequence of the multiple dimensions we describe: variation in language elements (new AST), additional semantics (new attributes) and
variation in the implementation.
Paper, on the other hand, provides by definition a linear, one dimensional rendering of this multidimensional view.
We can only expect this to be remedied by the use of proper tool support (like a fragment editor or browser).
On paper, proper cross referencing, colors, indexing or accumulative merging of text are most likely to be helpful.

The AG system, though in its simplicity surprisingly usable and helpful, could be improved in many areas.
For example, no type checking related to Haskell code for attribute definitions is performed,
nor will the generated Haskell code when compiled by a Haskell compiler produce sensible error messages in terms of the
original AG code.
The AG system also lacks features necessary for programming in the large.
For example, all attributes for a node live in a global namespace for that node instead of being packaged in some form of module.

Performance is expected to give problems for large systems.
This seems to be primarily caused by the simple translation scheme in which all attributes together live in a tuple just until the program
completes.
This inhibits garbage collection of intermediate attributes that are no longer required.
It also stops GHC from performing optimizations;
informal experimentation with a large AG program resulted in GHC taking approximately 10 times more time with optimization flags on.
The resulting program only ran approximately 15\% faster.
The next version of the AG system will be improved in this area \cite{saraiva99phd-funcimpl-ag}.

\paragraph{AG vs Haskell.}
Is the AG system a better way to do Haskell programming? In general, no, but for Haskell programs
which can be described by a catamorphism the answer is yes (see also \secRef{ag-primer}).
In general, if the choices made by a function are mainly driven by some datastructure,
it is likely that this datastructure can be described by an AST and the function can be described by the AG's attribution.
This is the case for an abstract syntax tree or analysis of a single type.
It is not the case for a function like |fitsIn| (\secPageRef{EHTyFitsIn.1.fitsIn.Base}) in which
decisions are made based on the combination of two (instead of just one) type.

\paragraph{About \thispaper\, EH and its code.}
The linear presentation of code and explanation might suggest that this is also
the order in which the code and \thispaper\ came into existence.
This is not the case.
A starting point was created by programming a final version (at that time EH version 6, not included in \thispaper).
From this version the earlier versions were constructed.
After that, later versions were added.
However, these later versions usually needed some tweaking of earlier versions.
The consequence of this approach is that the rationale for design decisions in earlier versions become clear only
in later versions.
For example, an attribute is introduced only so later versions only need to redefine the rule for this single attribute.
However, the initial rule for such an attribute often just is the value of another attribute.
At such a place the reader is left wondering.
This problem could be remedied by completely redefining larger program fragments.
This in turn decreases code reuse.
Reuse, that is, sharing of common code turned out to be beneficial for the development process as the
use of different contexts provides more opportunities to test for correctness.
No conclusion is attached to this observation, other than being another example of the tension between clarity
of explanation and the logistics of compiler code management.

\paragraph{Combining theory and practice.}
Others have described type systems in a practical setting as well.
For example, Jones \cite{jones00thih} describes the core of Haskell98 by a monadic style type inferencer.
Pierce \cite{typing:types-prog-lang:pierce} explains type theory and provides many small implementations performing
(mainly) type checking for the described type systems in his book.
On the other hand, only recently the static semantics of Haskell has been described formally \cite{faxen02semantics-haskell}.
Extensions to Haskell usually are formally described but once they find their way into a production compiler the interaction
with other parts of Haskell is left in the open or is at best described in the manual.

The conclusion of these observations might be that a combined description of a language, its semantics,
its formal analysis (like the type system),
and its implementation is not feasible.
Whatever the cause of this is, certainly one contributing factor is the sheer size of all these
aspects in combination.
We feel that our approach contributes towards a completer description of Haskell,
or any other language if described by the AG system.
Our angle of approach is to keep the implementation and its explanation consistent and understandable
at the same time.
However, this document clearly is not complete either.
Formal aspects are present, let alone a proof that the implementation is sound and complete
with respect to the formal semantics.
Of course one may wonder if this is at all possible; in that case our approach may well
be a feasible second best way of describing a compiler implementation.

\paragraph{EH vs Haskell.}
The claim of our title also is that we provide an implementation of Haskell,
thereby implying recent versions of Haskell, or at least Haskell98.
However, \thispaper\ does not include the description of (e.g.) a class system;
the full version of EH however does.

%endif %% inclConcl

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Acknowledgement
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclAck
\paragraph{Acknowledgements.}
We thank both (anonymous) reviewers for their extremely valuable and helpful comments.
%endif %% inclAck







%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% References
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if not asSlides
\AddContentsLine{References}
%if dist
%%1srcfile(afp04.bbl%%)%else
%if refToPDF
\bibliographystyle{uhcbook}
%else
\bibliographystyle{plain}
%endif
{\sloppy\raggedright
\bibliography{LitAdm}
}
%endif %% dist
%endif %% not asSlides

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Appendices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclApp

\appendix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AG Patterns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{AG pattern: global variable simulation}

%if forAfpHandout
This part is not included in this version of \thispaper.
%else
|Int| unique thread as an example
%endif

\section{AG pattern: unique value generation}
\label{app-ag-pattern-uid}
%if forAfpHandout
This part is not included in this version of \thispaper.
%else

UID
\chunkCmdUseMark{EHCommon.2.UID.Utils}
\chunkCmdUseMark{EHCommon.2.UID.mkNewLevUID}

Threading as global
\chunkCmdUseMark{EHUniq.2}

%endif


\section{AG pattern: gathering}
\label{app-agpattern-gathering}

%if forAfpHandout
This part is not included in this version of \thispaper.
%else

Via threading

Via collecting, via |USE|.

\section{AG pattern: mutual dependent info}

Gathering placeholders and later updating them

ty inference + pat name gathering

%endif

\section{AG pattern: gathering from within and usage higher up}

%if forAfpHandout
This part is not included in this version of \thispaper.
%else
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Additional checks
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH: Additional checks}
\chunkCmdUseMark{EHExtraChecks.1}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Glue
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH: Missing glue}

%if forAfpHandout
This part is not included in this version of \thispaper.
%else

Scanner config
%endif

\section{EH: Connecting with the outside world}
\label{app-outside-connect}

%if forAfpHandout
This part is not included in this version of \thispaper.
%else
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Omitted
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclOmitted
\section{Omitted}
\subsection{EH version 1}
\subsubsection{Pretty printing}
\chunkCmdUseMark{EHPretty.1.Base.Rest}
\chunkCmdUseMark{EHPretty.1.TyExpr.ConNm}
\chunkCmdUseMark{EHPretty.1.ParNeed.Rest}
\chunkCmdUseMark{EHPretty.1.PatExpr}
\chunkCmdUseMark{EHPretty.1.AGItf.topTyPP}

\subsubsection{Pretty printing |Gamma|}
\chunkCmdUseMark{EHGam.1.ppGam}

\subsubsection{Type pretty printing}
\chunkCmdUseMark{EHTyPretty.1.appFunPP}
\chunkCmdUseMark{EHTyPretty.1.ParNeed}

\subsubsection{Error pretty printing}
\chunkCmdUseMark{EHErrorPretty.1}

\subsubsection{Connecting to the outside world}
Haskell Main
\label{app-main-eh}
\chunkCmdUseMark{EHC.1.main}
\chunkCmdUseMark{EHC.1.doCompile}

Scanning
\chunkCmdUseMark{EHC.1.commonScannerConfig}
\chunkCmdUseMark{EHC.1.scannerConfig}
\chunkCmdUseMark{EHC.1.scanHandle}

Options to the compiler
\chunkCmdUseMark{EHCommon.1.Options}

\subsubsection{Misc}
PP utils
\chunkCmdUseMark{EHCommon.1.PP.Rest}

Misc
\chunkCmdUseMark{EHCommon.1.Misc}

AG main
\chunkCmdUseMark{EHMainAG.1}



\subsection{EH version 2}
Error gathering, additional errors, type common AG, pretty printing
\subsubsection{Error structure}
\chunkCmdUseMark{EHErrorAbsSyn.2}
\chunkCmdUseMark{EHError.2}
\chunkCmdUseMark{EHErrorPretty.2}

\subsubsection{Error gathering}
\chunkCmdUseMark{EHGatherError.2}

\subsubsection{Type common AG}
\chunkCmdUseMark{EHTyCommonAG.2}

\subsubsection{Pretty printing}
\chunkCmdUseMark{EHPretty.2}

\subsection{EH version 3}
Common, pretty printing, uniq
\subsubsection{Common}
\chunkCmdUseMark{EHCommon.3}

\subsubsection{Type pretty printing}
Pretty printing

This can also be used as example of unique threading.
\chunkCmdUseMark{EHTyPretty.3}
\chunkCmdUseMark{EHTyCommonAG.3}

\subsubsection{Pretty printing}
\chunkCmdUseMark{EHPretty.3}

\subsubsection{Unique id}
\chunkCmdUseMark{EHUniq.3}

\subsubsection{Gam}
\chunkCmdUseMark{EHGam.3}

\subsection{EH version 4}
Substitution, error gathering, pretty printing, uniq, common
\subsubsection{Substitution}
\chunkCmdUseMark{EHCnstr.4}

\subsubsection{Error gathering}
\chunkCmdUseMark{EHGatherError.4}

\subsubsection{Type pretty printing}
\chunkCmdUseMark{EHTyPretty.4}

\subsubsection{Pretty printing}
\chunkCmdUseMark{EHPretty.4}

\subsubsection{Unique id}
\chunkCmdUseMark{EHUniq.4}

\subsection{EH version 5}
Error gathering, pretty printing, uniq
\subsubsection{Error gathering}
\chunkCmdUseMark{EHGatherError.5}

\subsubsection{Pretty printing}
\chunkCmdUseMark{EHPretty.5}

\subsubsection{Unique id}
\chunkCmdUseMark{EHUniq.5}

\subsection{EH version 6}
Error gathering, pretty printing, uniq
\subsubsection{Error gathering}
\chunkCmdUseMark{EHGatherError.6}

\subsubsection{Pretty printing}
\chunkCmdUseMark{EHPretty.6}

\subsubsection{Unique id}
\chunkCmdUseMark{EHUniq.6}

%endif

%endif %% inclApp


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Index
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if inclInx

\AddContentsLine{Index}
{\small
\Input{\jobname.ind}
}

%endif %% inclInx


\end{document}

