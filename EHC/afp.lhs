% $Id: afp.lhs 199 2004-05-12 19:11:13Z andres $

%let a4       = True
%let noprelim = True
%let color    = False

\documentclass[%
%if yesBeamer
%if asSlides
               ignorenonframetext,
%else
%if asArticle
%if forAfpLLNCS
               class=llncs,
%else
               class=article,
%endif
%else
               class=book,
%endif
%endif
%endif
%if not asSlides
               a4paper,
%if veryWide
               11pt,
%else
               10pt,
%endif
               headinclude,% new for typearea
               footexclude,% new for typearea
               nopictures%
%endif
]
%if yesBeamer
               {beamer}
%else
%if asArticle
               {beamer}%{article}
%else
               {beamer}%{book}
%endif
%endif

%include lhs2TeX.fmt

%if style == poly

%include greek_indices.fmt
%include indices.fmt
%include more_indices.fmt
% include lang.fmt
% include types.fmt
% include judgments.fmt
%endif

%include afp.fmt

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
%if forAfpLLNCS
[noamsthm]
%endif
{beamerbasearticle}
%endif
%else
%\usepackage[absolute]{textpos}%
\usepackage{color}
\usepackage[fleqn]{amsmath}
\usepackage{mparhack}% to get marginpars to show up correctly
\usepackage{natbib}
\bibpunct{(}{)}{;}{a}{}{,}
%endif

%if not (asSlides || forAfpLLNCS)
% the following statement deviates from the default in that
% we use nothing (a space) to separate author from year in citations
%if wide
%if asArticle
\usepackage[left=2.75cm,right=2.75cm,top=3cm,bottom=3cm,nohead]{geometry}
%else
\usepackage[left=2.75cm,right=2.75cm,top=3cm,bottom=3cm,asymmetric]{geometry}
%endif
%elif veryWide
\usepackage[left=2.25cm,right=2.25cm,top=2.5cm,bottom=2.5cm,nohead]{geometry}
%endif
%endif

\usepackage{pgf}
\usepackage{pgfarrows}
\usepackage{pgfnodes}

%if not asSlides
\usepackage
%if fontsite
           [sc]%
%endif
           {mathpazo}% change main font
\linespread{1.05}% larger line spacing due to palatino
\renewcommand{\bfdefault}{b}% bx palatino isn't available anyway
%if not fontsite
\usepackage[scaled=.95]{helvet}% change sf font
%else
\usepackage{grotesk}
\let\originaltextsc=\textsc
\renewcommand{\textsc}[1]{%
  \begingroup
  \fontfamily{5plj}\selectfont
  \originaltextsc{#1}%
  \endgroup}
%endif
\usepackage{calc}
%endif

\usepackage{array}

% experimental \DeclareMathOperator redefinition to use sf font
%if forAfpLLNCS
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

\usepackage{hyperref}

%include lhs2TeX.sty

% Better formatting of code blocks ...

%if not asSlides
%include kscode.fmt
%endif

% All sorts of predefined formatting directives ...

%include spacing.fmt

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

%if asArticle
%else
\let\prevSection=\section
\let\prevSubsection=\subsection
\def\section{\chapter}
\def\subsection{\prevSection}
\def\subsubsection{\prevSubsection}
%endif

%if forAfpTRUU1
\usepackage{TRtitlepage}
%endif

%if forAfpLLNCS
% \usepackage{makeidx}
%endif

% Specific for UHC

%format (Lst(x))    = "[" x "]"

%format ~=          = "\sim"
%format +++         = "\cup "
%format `intersect`     = "\cap "
%format :=          = "\vDash "
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

%format coe         = "{\delta}"
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
%format coeWeave    = "weave_{" coe "}"
%format coeWipe     = "wipe_{" coe "}"
%format coeCode     = "code"
%format coeEval     = "eval_{" coe "}"

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

%if forAfpLLNCS
\setlength{\mathindent}{.03\textwidth}
%else
\setlength{\mathindent}{.05\textwidth}
%endif

%if forAfpTRUU1
\setlength{\marginparsep}{.015\textwidth}
%endif

% title
\title{Typing Haskell with an Attribute Grammar}
\author{Atze Dijkstra and Doaitse Swierstra}
\date{\today}
%if forAfpLLNCS
\institute{Institute of Information and Computing Sciences,\\
Utrecht University,\\
P.O.Box 80.089, \\
Padualaan 14, Utrecht, Netherlands,\\
\email{@atze@@cs.uu.nl@},
\email{@doaitse@@cs.uu.nl@},\\
WWW home page:
\texttt{http://www.cs.uu.nl}
}
%endif

%if not forAfpLLNCS
\mode<article>{\makeindex}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Import of all code chunks in lhs2tex'd format
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


\newcommand{\EHCTexVersion}{tmp-\jobname}
\newcommand{\inputEHCTex}[2]{\input #1/#2}
\inputEHCTex{\EHCTexVersion}{EHInfer.tex}
\inputEHCTex{\EHCTexVersion}{EHInferExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferPatExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferTyExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferKiExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHInferData.tex}
\inputEHCTex{\EHCTexVersion}{EHInferCaseExpr.tex}
\inputEHCTex{\EHCTexVersion}{EHGatherError.tex}
\inputEHCTex{\EHCTexVersion}{EHAbsSyn.tex}
\inputEHCTex{\EHCTexVersion}{EHTyAbsSyn.tex}
\inputEHCTex{\EHCTexVersion}{EHErrorAbsSyn.tex}
\inputEHCTex{\EHCTexVersion}{EHPretty.tex}
\inputEHCTex{\EHCTexVersion}{EHUniq.tex}
\inputEHCTex{\EHCTexVersion}{EHTyCommonAG.tex}
\inputEHCTex{\EHCTexVersion}{EHExtraChecks.tex}
\inputEHCTex{\EHCTexVersion}{EHMainAG.tex}
\inputEHCTex{\EHCTexVersion}{EHTy.tex}
\inputEHCTex{\EHCTexVersion}{EHError.tex}
\inputEHCTex{\EHCTexVersion}{EHErrorPretty.tex}
\inputEHCTex{\EHCTexVersion}{EHTyQuantify.tex}
\inputEHCTex{\EHCTexVersion}{EHTyPretty.tex}
\inputEHCTex{\EHCTexVersion}{EHTyInstantiate.tex}
\inputEHCTex{\EHCTexVersion}{EHC.tex}
\inputEHCTex{\EHCTexVersion}{EHParser.tex}
\inputEHCTex{\EHCTexVersion}{EHCommon.tex}
\inputEHCTex{\EHCTexVersion}{EHCnstr.tex}
\inputEHCTex{\EHCTexVersion}{EHTyFitsIn.tex}
\inputEHCTex{\EHCTexVersion}{EHGam.tex}
%\inputEHCTex{\EHCTexVersion}{EHCodeAbsSyn.tex}
%\inputEHCTex{\EHCTexVersion}{EHCode.tex}
%\inputEHCTex{\EHCTexVersion}{EHCodePretty.tex}
%\inputEHCTex{\EHCTexVersion}{EHCodeJava.tex}
%\inputEHCTex{\EHCTexVersion}{EHGenCode.tex}

% rules
\input rules.tex

%pgf picture
\input afp-pgf.tex

\gdef\SetFigFont#1#2#3#4#5{}

\begin{document}

%if forAfpLLNCS
\frontmatter
%endif

%if forAfpTRUU1
\TRtitlepage
{Typing Haskell with an Attribute Grammar (Part I)}
{Atze Dijkstra \\ Doaitse Swierstra}
{UU-CS-2004-037}
%else
\maketitle
%endif

\frame<presentation>{\titlepage}

% Avoid indentation
\setlength{\parindent}{0mm}
\addtolength{\parskip}{0.4\baselineskip}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Abstract
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if forAfpTRUU1
\section*{Preface}
%endif

%if asArticle
\begin{abstract}
%endif
%if forAfpTRUU1
\subsection*{Type systems for functional languages (abstract)}
%endif
Much has been written about type systems.
Much less has been written about implementing type systems.
Even less has been written about implementations of real compilers where
all aspects of a compiler come together.
This paper helps filling the final gap by describing the implementation of a compiler
for a simplified variant of Haskell.
By using an attribute grammar system, aspects of a compiler implementation
are described separately and
added in a sequence of steps,
thereby giving a series of increasingly complex (working) compilers.
Also, the source text of both paper and executable compilers come
from the same source files by an underlying minimal weaving system.
Therefore, sources and (this) explanation are kept consistent.
%if asArticle
\end{abstract}
%endif

%if forAfpTRUU1
\subsection*{Context of this paper}
A previous version of this paper has been presented and
distributed at the AFP2004 summerschool\footnote{The proceedings
for the AFP2004 summerschool have yet to appear.}.
This paper describes a part of a Haskell compiler under development \cite{dijkstra04ehc-web},
focussing on the type system of the langauge and its implementation.
Subsequent papers will describe the remaining parts of the implementation.

Not all parts of the implementation are explained in this paper.
In a subsequent paper (continuing with part II) for example data structures (|data| types, kind inference and records)
will be introduced and explained.
As a consequence at some points in this paper a forward reference will be made to material to be published
later.
This concerns mainly future design decisions which have an influence on design decisions made in this paper.
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Preamble
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if not forAfpLLNCS
{\setlength{\parskip}{0cm}
\tableofcontents
\listoffigures
}
%endif

\frame<presentation>{
%\setlength{\parskip}{.25ex}
\tableofcontents[hidesubsections]
}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Part I
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if forAfpTRUU1
\part{Type checking, inference and polymorphism}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Intro
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{Introduction and overview}

%if not onlyCurrentWork

Haskell98\cite{peytonjones03has98-rev-rep} is a complex language,
not to mention its more experimental incarnations.
Though also intended as a research platform, realistic compilers for Haskell \cite{www04ghc}
have grown over the years
and understanding of and experimentation with
those compilers is not an easy task.
Experimentation on a smaller scale usually is based upon relatively simple and restricted implementations
\cite{jones99thih}, often focussing only on a particular aspect of
the language and/or its implementation.
This paper aims at walking somewhere between this complexity and simplicity by
\begin{itemize}
\item
Describing the implementation of essential aspects of Haskell
(or any other (functional) programming language), hence the name Essential Haskell (EH) used
for simplified variants of
Haskell\footnote{The 'E' in EH might also be expanded to other aspects of the compiler, like being an \textbf{E}xample}
in this paper.
\item
Describing these aspects separately in order to provide a better understanding.
\item
Adding these aspects on top of each other, each addition gives rise to a complete compiler for
a Haskell subset, finally leading
to a compiler for full Haskell (and extensions).
\item
Using tools like the AG system, to allow for separate descriptions yet also to allow
the creation of working software based on these separate descriptions.
\end{itemize}
The following sections will expand on this by looking at
the intentions and purpose of this paper in more detail.
This is followed by a short description
of the final language for which we develop compilers throughout this paper.

\subsection{Purpose}

\frame<presentation>
{
\frametitle{What do we hope to have achieved by this talk?}
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
\item Practical type inference for arbitrary-rank types (Simon P Jones e.a.)
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

\frame<presentation>
{
\frametitle{What do we hope to achieve in the near future?}
\begin{itemize}
\item Extend the set of compilers up to a full Haskell++ compiler
\item Keep it understandable
\item So it can be used for both experimentation and education
\begin{itemize}
\item by anybody, by ourselves
\end{itemize}
\item Will grow over time
\end{itemize}
}

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
\item And most recent sources at @http://www.cs.uu.nl/groups/ST/Ehc/WebHome@
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

For whom is this paper intended?
\begin{itemize}
\item
For students who wish to learn more about the implementation of functional languages.
This paper also informally explains required theory, in particular theory about type systems.
\item
For researchers who want to build (e.g.) a prototype and to experiment
with extensions to the type system and need a non-trivial and realistic starting point.
This paper provides documentation, design rationales and an implementation for such a starting point.
\item
For those who wish to encounter a larger example of the tools used to build the compilers in this paper.
This paper uses the AG system \cite{baars04ag-www} to separately describe aspects of the language implementation,
and parser combinators \cite{swierstra00parser-toytool,swierstra99parser-tutor}
to compactly describe executable syntax.
Other tools for maintaining consistency between different versions of the resulting compilers
and the source code text included in this paper are also used but not discussed in here.
\end{itemize}

For this intended audience this paper provides
\begin{itemize}
\item
A description of the implementation of a type checker/inferencer for an
extended subset of Haskell.
Extended in the sense of allowing higher ranked polymorphism
\cite{peytonjones03pract-inf-rank,botlan03ml-power-f,odersky97putting-ann}
and existentials \cite{perry91phd,laufer94poly-absdata,mitchell88absty-exist}.
A subset in the sense of (e.g.) leaving out the class system.
However, it is the intention of the authors to gradually extend
the compiler towards the full functionality offered by Haskell.
\item
A description of the semantics of Haskell, lying between the
more formal
\cite{hall96type-class-haskell,faxen02semantics-haskell}
and more implementation oriented
\cite{jones00thih,ipt:impl-func-prog-lang} or similar to \cite{typing:types-prog-lang:pierce}.
\item
A gradual instead of a big bang explanation. It should be noted however that
gradual explanation is different from gradual evolution; the former being based
on a complete version, the latter following a path to an at the time unknown final version.
\item
A somewhat informal proof of the belief that the complexity of a compiler
can be managed by splitting the implementation of the compiler into separate aspects.
\item
A working combination of otherwise usually separately proven or implemented features.
\end{itemize}

However, this paper does \emph{not} provide
\begin{itemize}
\item
Type theory or parsing theory as a subject on its own.
This paper is intended to describe ``how to implement'' and
will use theory from that point of view.
Theoretical aspects are touched upon from a more intuitive point of view.
\end{itemize}

Although at occasions informally and concisely introduced where necessary,
familiarity with the following will make reading and understanding this paper easier
\begin{itemize}
\item
Functional programming, in particular using Haskell
\item
Compiler construction in general
\item
Parser combinator library and AG system \cite{baars04ag-www,uust04www}
\item
Type systems, |lambda|-calculus
\end{itemize}


It is our belief and hope that by following this fine line between theory and its implementation,
we serve both who want to learn and those who want to do research are served.
It is also our belief that by splitting the big problem into smaller aspects the combination can
be explained in an easier way.
Finally, we believe that this can only be accomplished if supported by proper tooling,
currently the AG system and a weaving system
\TBD{[cite...]},
and in the future perhaps by integrated environments
\cite{schrage04www-proxima}.
\TBD{[cite Proxima, Programmatica]}

In the following sections we give examples of the Haskell features
present in the
series of compilers described in
%if omitEH5Onwards
the following chapters.
%else
\chapterRef{ehc1} throughout
\chapterRef{ehc6}.
%endif
Only short examples are given, intended to get a feel for what is explained in more detail
and implemented in the relevant versions of the compiler.

\subsection{A short tour}

Though all compilers described in this paper deal with a different aspect,
they all have in common that they are based on the \IxAsIs{|lambda|-calculus},
most of the time using the syntax and semantics of Haskell.
The first version of our series of compilers therefore most closely resembles the
|lambda|-calculus, in particular typed |lambda|-calculus extended with |let| expressions
and some basic types like |Int|, |Char| and tuples.

\paragraph{EH version 1: |lambda|-calculus.}
An EH program is a single expression, contrary to a Haskell program which consists of a set of declarations forming a module.

\begin{code}
%%1srcfile<test/1-all-ok2.eh%%>
\end{code}

All variables need to be typed explicitly, absence of an explicit type is considered to be an error.
The corresponding compiler (EH version 1, \chapterRef{ehc1}) checks the explicit types against
actual types. For example:

\begin{code}
%%1srcfile<test/1-all-fail2.eh%%>
\end{code}

is not accepted.

Besides the basic types |Int| and |Char|, composite types can be formed by building tuples and defining functions:

\begin{code}
%%1srcfile<test/1-all-ok3.eh%%>
\end{code}

Functions accept one parameter only, which can be a pattern.
Functions are not polymorphic (yet).

\frame<presentation>
{
\frametitle{EH version 1: |lambda|-calculus}
\begin{itemize}
\item EH program is single expression
\SafeCode{%
\begin{code}
%%1srcfile<test/1-all-ok2.eh%%>
\end{code}
}
\item Types |Int|, |Char|, tuples and functions
\SafeCode{%
\begin{code}
%%1srcfile<test/1-all-ok3.eh%%>
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
%%1srcfile<test/1-all-fail2.eh%%>
\end{code}
}
gives rise to error annotated representation of program:
\begin{TT}
%%1ppfile<test/1-all-fail2.eh%%>
\end{TT}
\end{itemize}
}

\paragraph{EH version 2: Explicit/implicit typing.}
The next version
(EH version 2, \chapterRef{ehc2})
allows the explicitly specified types to be omitted.
The consequently missing type information has to be inferred by the compiler.

\begin{code}
%%2srcfile<test/1-sig-fail.eh%%>
\end{code}
will reconstruct the type specification |i :: %%2file<test/1-sig-fail.eh%%>|.

The reconstructed type information is monomorphic, for example for the identity function in
\begin{code}
%%2srcfile<test/2-id-int.eh%%>
\end{code}
the type |id :: %%2file<test/2-id-int.eh%%>|
will be reconstructed.

\frame<presentation>
{
\frametitle{EH version 2: Explicit/implicit typing}
\begin{itemize}
\item Type signature may be omitted
\SafeCode{%
\begin{code}
%%2srcfile<test/1-sig-fail.eh%%>
\end{code}
}
\item
Missing type is inferred: |i :: %%2file<test/1-sig-fail.eh%%>|
\item Inferred types are monomorphic
\SafeCode{%
\begin{code}
%%2srcfile<test/2-id-int.eh%%>
\end{code}
}
gives rise to type
|id :: %%2file<test/2-id-int.eh%%>|
\end{itemize}
}

\paragraph{EH version 3: Polymorphism.}
The third version
(EH version 3, \chapterRef{ehc3}) performs standard
Hindley-Milner type inference \cite{ipt:type-infer-milner,damas82principal-type}
which infers also parametric polymorphism.
For example,
\begin{code}
let  id = \x -> x
in   id 3
\end{code}
gives type |id :: %%3<let id = \x -> x in id%%>|.

A type for a value can also be specified explicitly
\begin{code}
let  id :: a -> a
     id = \x -> x
in   id 3
\end{code}
The type signature is checked against the inferred type.

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
gives type |id :: %%3<let id = \x -> x in id%%>|
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

\paragraph{EH version 4: Higher ranked types.}
Standard Hindley-Milner type inference cannot infer polymorphic parameters,
so-called rank-2 (or even higher ranked) polymorphism.
In general, this is a hard if not impossible thing to do
\cite{jim95rank,kfoury94direct,kfoury99rank2-decid,kfoury03rank2-princ},
so the fourth version (EH version 4, \chapterRef{ehc4}) does not infer this type information but
allows explicitly specified polymorphism for (e.g.) parameters.

For example, the following is allowed.
\begin{code}
let  f :: (forall a . a -> a) -> (Int,Char)
     f = \i -> (i 3, i 'x')
in   f
\end{code}
Note that the type signature cannot be omitted here.

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
infers |f :: %%4<let  f :: (a -> a) -> (Int,Char) in f%%>|

Specifying a complete type signature can be difficult for complicated types,
so it is permitted to leave argument and results of a function unspecified
using a \IxAsDef{partial type signature}.
\begin{code}
%%4srcfile<test/4-ty-wild1.eh%%>
\end{code}
Only the part which cannot be inferred is given in the signature.

Finally, type information can be hidden, or encapsulated,
by using existential quantification
\begin{code}
%%4srcfile<test/4-ex-extr.eh%%>
\end{code}
The value |xy| contains an |Int| and a function making an |Int| from the
value of which the type has been hidden.
The extraction is done by function |ixy|.
An attempt to construct |pq| fails.

When a value of an existentially quantified type is opened, that is,
identifiers are bound to its components,
the hidden type becomes visible in the form of a fresh type constant.
The explicit |exists| may also be omitted, for example
|xy :: (a, a->Int)| is interpreted as |xy :: exists a . (a, a->Int)|.


\frame<presentation>
{
\frametitle{EH version 4: Higher ranked types}
\begin{itemize}
\item<+->
Type signatures for quantifiers on argument (higher ranked) positions
\SafeCode{%
\begin{code}
let  f :: (forall a . a -> a) -> (Int,Char)
     f = \i -> (i 3, i 'x')
in   f
\end{code}
}
\item<+-> Notational sugaring allows omission of quantifier
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
%%4srcfile<test/4-ex-extr.eh%%>
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

%if omitEH5Onwards
%else
\paragraph{EH version 5: Data types.}
The fifth version (EH version 5, \chapterRef{ehc5})
adds |data| types and opening/unpacking/scrutinizing
a data type value by means of a |case| expression.
\begin{code}
%%5srcfile<test/5-list.eh%%>
\end{code}

\frame<presentation>
{
\frametitle{EH version 5: Data types}
\begin{itemize}
\item
User defined data types
\SafeCode{%
\begin{code}
%%5srcfile<test/5-list.eh%%>
\end{code}
}
\item Unpacking via case expression
\end{itemize}
}

\paragraph{EH version 6: Kinding.}
The previous version allows incorrect programs because
data types can be used incorrectly:
\begin{code}
%%6srcfile<test/5-list-wrong.eh%%>
\end{code}
The type of |v| is not a type of a value, and thus the type of |v|
itself is not well-typed.
The sixth version (EH version 6, \chapterRef{ehc6})
adds kind (that is, the type of a type) inferencing.
%if noAFP04
For example, the previous example gives
\begin{TT}
%%6ppfile<test/5-list-wrong.eh%%>
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
%%6ppinline<let data Eq a b = Eq (forall f . f a -> f b) in 3%%>
\end{TT}

\frame<presentation>
{
\frametitle{EH version 6: Kinds}
\begin{itemize}
\item
Type expressions can be incorrectly used
\SafeCode{%
\begin{code}
%%6srcfile<test/5-list-wrong.eh%%>
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

\frame<presentation>
{
\frametitle{EH version 6: Kind polymorphism}
\begin{itemize}
\item
Polymorphic kinds
\SafeCode{%
\begin{code}
%%6srcfile<test/5-all-ok2.eh%%>
\end{code}
}
infers kind @Eq :: Forall a . a -> a -> *@
\item
Kind signatures for types (similar to type signatures for values)

\SafeCode{%
\begin{code}
%%6srcfile<test/6-expl-ki.eh%%>
\end{code}
}
\end{itemize}
}

\frame<presentation>
{
\frametitle{EH version 7: non extensible records}
\begin{itemize}
\item Replacement for tuples
\SafeCode{%
\begin{code}
%%6srcfile<test/7-all-ok2.eh%%>
\end{code}
}
\end{itemize}
}

\frame<presentation>
{
\frametitle{EH version [8..]}
\begin{itemize}
\item 8: Code generation
\item{} [9..]: Extensible records, class system, modules, ...
\end{itemize}
}

%endif omitEH5Onwards

\subsection{Haskell language elements not described}

The last compiler in the series of compilers in this paper still lacks
many features available in Haskell.
Just to mention a few:
\begin{itemize}
\item
Binding group analysis
\item
Syntax directives like infix declarations
\item
Class system
\item
Modules
%if noAFP04
\cite{diatchki02hask-module,shields01first-class-mod}.
%endif
\item
Type synonyms
\item
Syntactic sugar for |if|, |do|, list notation and comprehension.
\item
Code generation
\end{itemize}

%if False
\subsubsection{Untackled newly arisen issues}
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

\subsection{About the presented code}

The fact that multiple versions of a compiler are described and
are the basis of the story around which the explanation has been woven brings
some consequences
\begin{itemize}
\item
Versions are built on top of each other.
However, in practice this meant that
the last version was built and subsequently simplified to
earlier versions. 
It is not the case that the versions represent a timeline,
a tale of how the different versions came into being.
It also means that all versions are dependent on each other and
are designed as a whole.
Any desired change in the last version may imply a change in the first version.
Metaphorically speaking, in order to change the grown-up compiler you may have to tweak
its childhood.

In an ideal situation this would not have been necessary;
but it would be unfair if we did not mention the work that went into getting
the stepwise built-up compilers to work as neatly as is does now.
\item
Since we do not only want to sketch the approach but want to present
a complete compiler we also have to deal with many non-interesting details.
The complete compiler text can be found on the website accompanying
this paper \cite{dijkstra04ehc-web}.
\end{itemize}

%if not omitLitDiscuss
\subsection<article>{Literature}

\TBD{}

here????
Or merged with previous.

Similar, implementation describing

Rules only

Haskell
%endif

%endif not onlyCurrentWork

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 1: typed |lambda|-calculus}
\label{ehc1}

%if not onlyCurrentWork

\frame<presentation>{\tableofcontents[current,hidesubsections]}

In this section we will build the first version of our series of compilers:
the typed |lambda|-calculus packaged in Haskell syntax in which
all values need explicitly be given a type.
The compiler checks if the specified types are in agreement with actual
value definitions.
For example

\begin{code}
%%1srcfile<test/1-all-ok2.eh%%>
\end{code}

is accepted, whereas

\begin{code}
%%1srcfile<test/1-all-fail2.eh%%>
\end{code}
produces a pretty printed version of the erroneous program,
annotated with errors.
\begin{TT}
%%1ppfile<test/1-all-fail2.eh%%>
\end{TT}

Although the implementation of a type system will be the main focus of this section,
any such implementation
lives in co-existence with the complete environment/framework needed to build a compiler.
Therefore, aspects conveniently omitted from subsequent sections, like parsing, connection with
the abstract syntax, the use of the AG system and error reporting will also be
touched upon.

First we will start with the EH language elements implemented and how they correspond to
abstract syntax, followed by the translation done by parsing from concrete syntax to abstract syntax.
Our first aspect described using the AG notation will be a pretty printed representation of the abstract syntax tree,
reflecting the original input as closely as possible.
The second aspect concerns type checking, which involves several attributes for computing a type 
associated with certain parts of the abstract syntax tree.

\subsection{Concrete and abstract syntax}
The \IxAsDef{concrete syntax} of a (programming) language describes the structure of acceptable
sentences for that language, or more down to earth, it describes what a compiler for that language
accepts with respect to the textual structure.
On the other hand, \IxAsDef{abstract syntax} describes the structure used by the compiler itself for
analysis and code generation.
Translation from the more user friendly concrete syntax to the machine friendly abstract syntax is done by a
parser; from the abstract to the concrete representation is done by a pretty printer.

Let us focus our attention on the abstract syntax for EH, in particular the part
defining the structure for expressions (the full syntax can be found in \figRef{abs-syn-eh1}).
A |DATA| definition is the analogue of a Haskell |data| definition,
used to define a piece of the abstract syntax.

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

The notation of the AG system is used to define an expression |Expr|
to be a range of alternatives (or productions);
for example a single variable |Var| represents the occurrence of an identifier
(referring to a value introduced by
a declaration),
or |App| represents the application of a function to an argument.
The EH fragment
\begin{code}
%%1srcfile<afp-eh/02.eh%%>
\end{code}
is represented by the following piece of abstract syntax tree:
\begin{TT}
%%1astfile<afp-eh/02.eh%%>
\end{TT}
Note that the program is incorrect for this version of EH because type
signatures are missing.

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
%%1srcfile<afp-eh/02.eh%%>
\end{code}
  }
 \end{column}
 \begin{column}{.5\textwidth}
\begin{TTtiny}
%%1astfile<afp-eh/02.eh%%>
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
\chunkCmdUse{EHAbsSyn.1.AGItf}
\chunkCmdUse{EHAbsSyn.1.Decl}
\chunkCmdUse{EHAbsSyn.1.PatExpr}
\chunkCmdUse{EHAbsSyn.1.AllPatExpr}
\chunkCmdUse{EHAbsSyn.1.TyExpr}
\chunkCmdUse{EHAbsSyn.1.AllTyExpr}
\chunkCmdUse{EHAbsSyn.1.AllExpr}
\chunkCmdUse{EHAbsSyn.1.AllNT}
\end{Figure}

Looking at this example and the rest of the abstract syntax in \figRef{abs-syn-eh1} we can make several
observations of what is allowed to notate in EH and what can be expected from the implementation.
\begin{itemize}
\item
Applications (|App|) and constants (|Con|) occur in expressions (|Expr|), patterns (|PatExpr|)
and type expressions (|TyExpr|).
This similarity is sometimes exploited to factor out common code, and, if
factoring out cannot be done, leads to similarities between pieces of code.
This is the case with pretty printing, which is quite similar for the different kinds of
constructs.
\item
In the abstract syntax an alternative belongs to a nonterminal (or |DATA|), for
example ``|App| of |Expr|''.
On request the AG system generates corresponding Haskell data types with
the same name as the |DATA| defined, alternatives are mapped to constructors
with the name of the |DATA| combined with the name of the alternative,
separated by |_|. For example: |Expr_App|.
If necessary, the same convention will be used when referring to an
alternative\footnote{In this way we overcome a problem in Haskell,
where it is required that the constructors for all data types are different.}.
\item
Type signatures (|Decl_TySig|) and value definitions (|Decl_Val|) may be mixed freely.
However, type signatures and value definitions for the same identifier are still related.
For this version of EH, each identifier introduced by means of a value definition must
have a corresponding type signature specification.
\item
Because of the textual decoupling of value definitions and type signatures,
a type signature might specify the type for an identifier occurring inside a pattern:
\begin{code}
let  a      ::  Int
     (a,b)  =   (3,4)
in   ...
\end{code}
Currently we do not allow for this, but the following is allowed:
\begin{code}
let  ab        ::  (Int,Int)
     ab@(a,b)  =   (3,4)
in   ...
\end{code}
because the specified type for |ab| corresponds to the top of a pattern of a value definition.
\item
Composite values are created by tupling, denoted by |(..,..)|.
The same notation is also used for patterns (for unpacking a composite value) and
types (describing the structure of the composite).
In all these cases the corresponding structure consists of a |Con| applied to the elements
of the tuple.
On the value level a |Con| stands for a value constructor, on the type level for
a type constructor.
For now there is only one constructor: for tuples.
\item
The constructor for tuples also is the one which needs special treatment because it
actually stands for a infinite family of constructors.
This can be seen in the encoding of the name of the constructor which is composed of
a |","| together with the arity of the constructor.
For example, the expression |(3,4)| is encoded as an application |App| of |Con ",2"|
to the two |Int| arguments: (,2 3 4).
In our examples we will follow the Haskell convention, in which we write (,) instead of 2,.

By using this encoding we get the unit type |()| for free as it is encoded
by the name |",0"|.
We also can encode the tuple with one element which is for tuples themselves quite useless
as tuples have at least two elements.
However, later on, in \chapterRef{ehc4}, this turns out to be convenient.
This and other naming conventions are available via the following definitions
for Haskell names |HsName|.

\chunkCmdUseMark{EHCommon.1.HsName.type}
\chunkCmdUseMark{EHCommon.1.HsName.Base}

\item
Each application is wrapped on top with an |AppTop|.
This has no meaning in itself but as we will see in section~\ref{sec-pretty1}
it simplifies the pretty printing of expressions.
\item
The location of parenthesis around an expression is remembered by a |Parens| alternative.
This is to avoid the effort of finding back appropriate places to insert parenthesis.
\item
|AGItf| is the top of a complete abstract syntax tree.
The top of an abstract syntax tree is the place where interfacing
(hence the convention |Itf|)
with the outside, that is, the Haskell world takes place.
At |AGItf| 
  \begin{itemize}
  \item Initialization of inherited attributes takes place.
  \item Synthesized attributes are routed back into the tree as inherited attributes.
  \item Synthesized attributes are passed to the outside, to the Haskell world.
  \end{itemize}
It is a convention in this paper to give all nonterminals in the abstract syntax a
name with |AGItf| in it,
if it plays a similar role.
\item
The remaining alternatives for the non-terminal |Expr|
stand for their EH counterparts, for example |IConst| for an |Int| constant,
and |Lam| for a |lambda|-expression.
\item
Groups of nonterminals may be given a name through the |SET| directive.
For example, |AllNT| (All NonTerminals) is the name for all nonterminals occurring in the abstract syntax.
This provides an indirection when referring to nonterminals in attribute definitions (|ATTR|).
Later compiler versions can change the definition for |AllNT| without the need to
also modify the list of nonterminals for which an attribute definition |ATTR| is
declared.
\end{itemize}

\subsection{Parsing: from concrete to abstract (syntax)}
An abstract syntax tree is obtained as the result of parsing.
The parser combinator library used (see \cite{uust04www} and \figRef{parser-combinators}) to specify the parser for EH allows
us to simultaneously define the syntactic structure and the connection to
the abstract syntax.
For example, the parser for the simplest of expressions

\chunkCmdUse{EHParser.1.pExprBase}

recognises variables via |pVarid|.
The parser |pVarid| is defined in a general purpose scanner library \cite{uust04www}
about which
we will say no more than that it provides basic parsers tailored towards the recognition
of Haskell lexical elements.
All parsers from this library return their result as a string,
which can conveniently be passed as an argument to the semantic function for a variable, |sem_Expr_Var|.
This latter function is generated by the AG system form the attribute grammar.
This is also the case for the recognition of integer and character constants with the help
appropriate helper functions that convert the string returned by the scanner parsers |pInteger| and |pChar|
respectively.

\chunkCmdUse{EHParser.1.scanWrappers}

The use of semantic functions deserves some closer inspection.
For each of the alternatives of a non-terminal the AG system generates a
\IxAsDef{semantic function},
that takes as argument the semantic functions corresponding to its right hand side,
and constructs a function mapping inherited to synthesized attributes.
The structure of such a function, that is, its type is similar to its related and also generated
datatype.
Similar in the sense that both take their components as their first arguments.
For example, both |Expr_Var| as a data constructor and |sem_Expr_Var| take
a string as their first argument.
It is this similarity we exploit because we shortcut the usual two-step process of
first creating the abstract syntax structure and then applying the semantic function.
Instead the semantic function is applied immediately,
without creating the intermediate
structure.
For all practical purposes, for now, this is all we need in order to be able to use
these functions
(see also \cite{swierstra99comb-lang}).

\mode<all>{%
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
}

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
\chunkCmdUse{EHParser.1.pExprBase}
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

\chunkCmdUseMark{EHParser.1.pExpr}

An application |pExprApp| is a juxtapositioning of a non empty series
of |pExprBase|'s.
A |pExprApp| itself can be prefixed with parsers making the expression
into a |let|- or |lambda|-expression.
The parser |pExprPrefix| which recognizes this prefix
and returns a function doing this prefixing in terms of the
abstract syntax.
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
Note that no |AppTop| is placed around a singleton list as this is not an application
and the function |mkApp| is not used on an empty list.

\chunkCmdUseMark{EHCommon.1.mkApp.Base}
\chunkCmdUseMark{EHCommon.1.mkApp.Rest}
\chunkCmdUseMark{EHCommon.1.mkApp.mkConApp}
\chunkCmdUseMark{EHCommon.1.mkApp.mkProdApp}

\frame<presentation>[plain]
{
\frametitle{Exploiting |App| similarities}
\begin{itemize}
\item Abstraction for parsing |App| structures
\chunkCmdFrameUse{EHParser.1.pApp}
\item |App| building functions:
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

type MkConAppAlg t = (String -> t,t -> t -> t,t -> t,Int -> t)

eCata  :: MkConAppAlg t -> Expr -> t
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

The algebra is also used in the parser |pParenProd| recognising
either a unit |()|, parentheses |(pE)| around a more elementary parser |pE|
or a tuple |(pE,pE,...)| of elements |pE|.

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

The definition for |pParenProd| quite literally follows this enumeration of alternatives,
where in case of parentheses around a single |pE| the parentheses 
do not carry any semantic meaning.
Note that the parser is in a left factorized form
in order to make parsing take linear time.

The parsers for patterns and type expressions also use these abstractions,
for example, the parser for type expressions

\chunkCmdUse{EHParser.1.pTyExpr}

\frame<presentation>[plain]
{
\frametitle{Parsing type/pattern expressions}
\begin{itemize}
\item Similar to normal expressions
\item Type expressions (for example)
\chunkCmdFrameUse{EHParser.1.pTyExpr}
\end{itemize}
}

defines a |tyExprAlg| to be used to recognise parenthesized and tupled type expressions.
The parser for |pTyExpr| uses |pChainr| to recognise a list
of more elementary types separated by |->|.

The parser for patterns is because of its similarity with the previous expression
parsers given without further explanation

\chunkCmdUse{EHParser.1.pPatExpr}
\chunkCmdUse{EHParser.1.pPatExprBase}

Finally, the parsers for the program itself and declarations should have few surprises by now,
except for the use of |pBlock| recognising a list of more elementary parsers
where the offside rule of
Haskell is used.
We will not look at this any further.

\chunkCmdUse{EHParser.1.pDecl}
\chunkCmdUse{EHParser.1.pAGItf}

Once all parsing results are passed to semantic functions we enter the world of attributes
as offered by the AG system.
The machinery for making the compiler actually producing some output is not explained here but
can be found in the sources.
From this point onwards we will forget about that and just look at computations performed on
the abstract syntax tree through the separate views as provided by the AG system.

\subsection{Pretty printing: from abstract to concrete}
\label{sec-pretty1}
The first aspect we define is |pp| that constructs the pretty printed representation
for the abstract syntax tree, starting with the definition for |Expr|

\savecolumns
\chunkCmdUseMark{EHPretty.1.Base.ExprSimple}
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
%%1srcfile<test/1-all-fail2.eh%%>
\end{code}
}
\end{itemize}
\end{column}
\begin{column}{.6\textwidth}
\begin{itemize}
\item Abstract syntax
\begin{TT}
%%1astfile<test/1-all-fail2.eh%%>
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
%%1ppfile<test/1-all-fail2.eh%%>
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
\item (and more, but left out)
\end{itemize}
}

The |pp| attribute is defined (via |ATTR|)
as a synthesized (because after the two | || |) attribute for all nonterminals of the abstract syntax.
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

The computation of the |pp| attribute is straightforward in the sense that
only a few basic combinators are needed
(see \cite{uust04www} and \figRef{pretty-printing-combinators}).
The additional complexity arises from
the encoding for applications.
The problem is that
straightforward printing prints as |(2,3)| as |,2 2 3|.
This is solved at the
|AppTop| alternative by gathering all arguments (here: |[2,3]|)
for a function (here: tuple constructor |,2|, or |(,)|) into |appArgPPL|:

\chunkCmdUseMark{EHPretty.1.Expr.ConNm}

The |appArgPPL| and the two other required attributes |appFunNm| for the name and
|appFunPP| for the pretty printing of the function which is applied
all are used to gather information from deeper within the syntax tree
for further processing higher up in the tree by |ppAppTop|

\chunkCmdUseMark{EHCommon.1.PP.ppAppTop}
\chunkCmdUseMark{EHCommon.1.PP.NeededByExpr}

Additionally, because the pretty
printing with non-optimally placed parentheses
resembles the structure of the abstract syntax tree more,
this variant still is computed for the |App| and |Con| alternative in a local |pp|
attribute.
This |pp| value is used for producing error messages
(see section~\ref{sec-error1}),
but not for the computation of |pp| at the |AppTop| alternative.
For the |App| alternative parentheses are placed without consideration
if this is necessary and the |Con| alternative prints (via |ppCpn|) the constructor
following the Haskell convention.

We will not look into the pretty printing for
patterns and type expressions because it is almost an exact replica of the code for expressions.
The remainder of the |pp| related attribute computations is also omitted as
they are rather straightforward uses of |>||<| and |>-<|.

\subsection{Types}

We will now turn our attention towards the way a type system is incorporated into EH.
As we are focussed more on implementation than theory, our point of view is a more pragmatic one in that
a type system is an aid or tool for program analysis and a way to ensure specific desirable properties of a program.

\TBD{better intro here}

\subsubsection{What is a type}

A \IxAsDef{type} is a description of the interpretation of a value
where value has to read here as a bitpattern.
This means in particular that machine operations such as integer addition,
are only applied to patterns that are to be interpreted as integers.
The flow of values, that is, the copying between memory locations,
through the execution of a program may
only be such that the a copy is allowed only if the types of
source and destination relate to each other in some proper fashion:
a value from a source must fit in a destination.
If we copy or move a bit patterns it keeps its interpretation.
This is to prevent unintended use of bitpatterns, which might
likely lead to the crash of a program.

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
sigma  =  Int | Char | -> | , | ,, | ...
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

In this section we start by introducing a type language: in a more formal setting for use
in typing rules, and in a more practical setting using the AG system, that gives us an implementation too.
In the following section we discuss the typing rules, the mechanism for enforcing the fitting of types and the checking itself.
Types will be introduced informally and from a practical point of view
instead of a more formal approach
\cite{fp:type-theory:func-prog,wadler89theorems-for-free,typing:types-prog-lang:pierce,ipt:theory-of-objects}.

Types are described using a type language.
Our initial type language, for this first version of EH,
allows some basic types and two forms of composite types, functions and tuples.
A type |sigma| can be
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
Actually, the following definition is close to the one used by the implementation

\EHCOneTyLangB

This definition also introduces the possibility of describing types like |Int Int|.
This being rather meaningless the first version is to be preferred
We nevertheless use the second one since it is 
used by the implementation of later versions
of EH.
Here we just have to make sure no types like |Int Int| are created.

The corresponding encoding using AG notation differs in the
presence of an |Any| type.
This type is also denoted by |ANY| to indicate that it takes
the role of |Bot| as well as |Top| usually present in type languages.

\chunkCmdUseMark{EHTyAbsSyn.1}

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

No |AppTop| alternative is present since we want to keep this definition as simple as possible.
Not only will we define attributions for |Ty|,
but we will also use it to define plain Haskell values of the corresponding Haskell data type.

No |Parens| alternative is present either, again to keep the type structure as simple as possible.
However, this makes pretty printing more complicated because the proper locations for parenthesis
have to be computed.

\subsubsection{Type pretty printing}
As before, |TyAGItf| is the place where interfacing with the Haskell world takes place.
The AG system also introduces proper data types to be used in the Haskell world, the following is
the data type as generated by the AG system:
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
in the AG world which is subsequently applied to types represented by Haskell data type values.
As an demonstration of how this is done, pretty printing is used once again.
Using the same design and tools as used for pretty printing |Expr| (and pattern/type expressions)
abstract syntax we can define pretty printing for a type:

\chunkCmdUseMark{EHTyPretty.1.pp}

Some additional attributes are necessary to determine whether an |App| is at the top of
the spine of |App|'s

\chunkCmdUseMark{EHTyCommonAG.1.ConNm}
\chunkCmdUseMark{EHTyCommonAG.1.SpinePos}

\paragraph{Parenthesis.}
Because no explicit |Parens| alternative is present in the type structure,
appropriate places to insert parenthesis have to be computed.
This is solved by passing the need for parentheses
as contextual information using the inherited attribute |parNeed|.
However, as this part is not necessary for understanding
the implementation of a type system it can safely be
skipped.

\chunkCmdUseMark{EHTyPretty.1.ParNeed}

And supporting Haskell definitions:

\chunkCmdUseMark{EHCommon.1.ParNeed}

The idea here is that a |Ty| and its context jointly determine whether parentheses are needed.
This need is encoded in |ParNeed|, the contextual need is passed to |ppParNeed| as the parameter |globNeed|
and the local need as |locNeed|.
In |ppParNeed| parentheses are added if the contextual need is higher than the local need.
For example, at |AppTop| the printing of a tuple always adds parentheses via function |ppAppTop|,
so no additional parentheses are needed.
This is reflected in the result of |parNeedApp| which tells us that for a product
locally never (encoded by |ParOverrideNeeded|, first element of result 3-tuple) additional parentheses are needed,
and for the function part of the application (which is useless here since it is meant for the product constructor) and its
arguments in principle not (encode by |ParNotNeeded|, second element of result tuple).
The list (third element of result tuple) is meant for the arguments of the application.

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
acoording to a fixed naming convention. An example of
such a fuction is |pp_Syn_TyAGItf|, which when applied to |t|
accesses the
synthesized |pp| attribute at an attributed non-terminal
|t| of type |TyAGItf|.



\subsection{Checking types}
The type system of a programming language is usually described by typing rules.
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
a more structured form called a \IxAsDef{judgement}:
\[
|context |\stackrel{|judgetype|}{|:-|}| construct : property ~> more ^^ results |
\]
This reads as
\begin{quote}
In the interpretation |judgetype| the |construct| has property |property| assuming
|context| and with optional additional |more ^^ results|.
\end{quote}
Although the rule formally are to be interpreted purely equationally it may help to realise
that from an implementors point of view this (more or less)
corresponds to an implementation template, either in the form of a function
\begin{code}
judgetype =  \construct ->
             \context -> ... (property,more_results)
\end{code}
or a piece of AG
\begin{code}
ATTR judgetype [  context: ... | |
                  property: ...  more_results: ... ]

SEM judgetype
  |  construct
       lhs.(property,more_results) = ... @lhs.context ...
\end{code}
Typing rules and these templates for implementation however
differ in that an implementation prescribes the order in which the computation for
a property takes place, whereas a typing rule simply postulates
relationships between parts of a rule without specifiying how
this should be enforced or computed.
If it benefits clarity of explanation, typing rules presented
throughout this paper will be more explicit in the flow of information
so as to be closer to the corresponding implementation.

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
The rules in \figRef{rules.expr1A} also refer to |Gamma|,
often called \IxAsDef{assumptions}, the \IxAsDef{environment} or
a \IxAsDef{context} because it provides information about what may
be assumed about, say, identifiers.
Identifiers are distinguished on the case of the first character,
capitalized |I|'s starting with an uppercase, uncapitalized |i|'s otherwise
\begin{code}
ident  =  identv
       |  identc
\end{code}
Type constants use capitalized identifiers |identc| for their names,
whereas for identifiers bound to an expression in a |let|-expression
we will use lower case identifiers such as |identv|.

An environment |Gamma| is a set of bindings notated as a list
\begin{code}
Gamma = [ident :-> sigma]
\end{code}
Because we assume the reader to be familiar with Haskell we will also use
Haskell operators like concatenation |++| on lists in our type rules.
We also make use of other behavior of lists, for example
when looking up something in a |Gamma|. If
two definitions for one identifier are present the
first one will be taken, therefore effectively shadowing the second.
Only when the list notation becomes cumbersome a vector notation |Vec| will be used instead.

A list structure is enough to encode the presence of an identifier in a |Gamma|, but it
cannot be used to detect multiple occurrences caused by duplicate introductions.
In the AG implementation a stack of lists is used instead

\chunkCmdUseMark{EHCommon.1.AssocL}
\chunkCmdUseMark{EHGam.1.Base.sigs}
\chunkCmdUseMark{EHGam.1.Base.funs}

Entering and leaving a scope is implemented by means of pushing and popping a |Gamma|.
Additions to a |Gamma| will take place on the top of the stack only.
A |gamUnit| used as an infix operator will print as |`gamUnit`|.

A specialization |ValGam| of |Gam| is used to store and lookup the type of value identifiers.

\chunkCmdUseMark{EHGam.1.ValGam.Base}
\chunkCmdUseMark{EHGam.1.valGamLookup}

Later on the variant |valGamLookup| will do some additional work, for now it
does not differ from |gamLookup|.

\subsubsection{Checking Expr}

The rules in \figRef{rules.expr1A} do not provide much information about how
the type |sigma| in the consequence of a rule
is to be computed; it is just stated that it should relate in some way
to other types.
However, type information can be made available to parts of the abstract syntax tree either
because the programmer has supplied it somewhere or because the compiler can reconstruct it.
For types given by a programmer the compiler has to check if such a type indeed correctly
describes the value for which the type is given.
This is called \IxAsDef{type checking}.
If no type information has been given for a value the compiler needs
to reconstruct or infer this type based on the structure of the abstract syntax
tree and the semantics of the language as defined by the typing rules.
This is called \IxAsDef{type inference}.
Here we deal with type checking, the next version of EH
(section~\ref{ehc2}) deals with type inference.

\rulerCmdUse{rules.fit1}
\rulerCmdUse{rules.expr1B}

We now can tailor the type rules in \figRef{rules.expr1A} towards an implementation
which performs type checking, in \figRef{rules.expr1B}.
The rules now take an additional context, the expected (or known) type |sigmak|
(attribute |knTy|) as specified by the programmer.
An additional type of judgement |fit| (\figRef{rules.fit1}) is needed to check an actual type against a known type.
For example, the \ruleRef{e-int1B} checks that its actual |Int| type matches the
known type |sigmak|.
Both the definition of |sigmak| in terms of AG

\chunkCmdUseMark{EHInferExpr.1.knTy}

initialized on top by

\chunkCmdUseMark{EHInferExpr.1.knTy.AGItf}

and the implementation of the type \ruleRef{e-int1B}

\chunkCmdUseMark{EHInferExpr.1.Const}

correspond one-to-one to their formal counterpart
together with definitions for type constants

\chunkCmdUseMark{EHTy.1.tyConsts}

The local attribute |fTy| (by convention) contains the type
as computed on the basis of the abstract syntax tree.
This type |fTy| is then compared to the expected type |lhs.knTy|
via the implementation |fitsIn| of the rules for |fit|/|<=|.
In infix notation |fitsIn| prints as |<=|.
|fitsIn| returns a |FIOut| (\textbf{f}itsIn \textbf{o}utput) datastructure from which the resulting
type can be retrieved by using the accessor function |foTy|.
The split into separate attributes has been made in this
way so later redefinitions of |fTy| can be made independently
of the definition for |fo|.

The Haskell counterpart of |jfit sigma1 <= sigma2 : sigma|
is implemented by |fitsIn|.
The function |fitsIn| checks if a value of type |sigma1| can flow (that is, stored) into a memory location
of type |sigma2|.
This is an asymmetric relation because ``a value flowing into a location'' does not imply that it can flow
the other way around, so |<=|, or |fitsIn| conceptually has a direction, even though the following
implementation in essence is a test on equality of types.

The \ruleRef{f-arrow1} in \figRef{rules.fit1} for comparing function types compares the
types for arguments in the other direction relative to the function type itself.
Only in \secPageRef{ehc4-fitsin} when |<=| really behaves asymmetrically we will discuss this aspect
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
\item Environment |Gamma|/|valGam| holds assumptions (types) about value identifiers
\item Expected/known/top-down type |sigmak|/|knTy| together with bottom-up type |sigma|/|ty| from child expression
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
\chunkCmdFrameUse{EHTyFitsIn.1.FIOut}
\begin{itemize}
\item a type to continue checking with
\item errors (if any) to incorporate in pretty printed program
\end{itemize}
\item Result |sigma|
\begin{itemize}
\item Is in principle unnecessary (we are just checking)
\item But need to know the `maximal' type for |sigmal <= ANY| to continue type checking with
\item Is already a bit of type inference
\end{itemize}
\end{itemize}
}

\chunkCmdUseMark{EHTyFitsIn.1.FIOut}
\chunkCmdUseMark{EHTyFitsIn.1.foHasErrs}
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
\chunkCmdFrameUse{EHTy.1.unMkTy.sigs}
\item In case of failure, return ``will do'' values:
\begin{itemize}
\item |Ty_Any| (for |Ty|)
\item |[]| (for |TyL|)
\end{itemize}
\end{itemize}
}


The function |fitsIn| checks if the |Ty_App| structure
and all type constants |Ty_Con| are equal.
If not, a non-empty list of errors will be returned as well as type |Ty_Any| (|Any|/|ANY|).

The constant |Ty_Any| denoted by |Any|/|ANY| plays two roles

\begin{itemize}
\item
|Ty_Any| plays the role of |Bot| when it
occurs at the lhs of |ANY <= sigma| because it is returned as the result of an erroneous condition signaled by 
a previous |fitsIn|.
It represents a value which never can or may be used.
In that case it does not matter how it will be used because an error already has occurred.
\item
|Ty_Any| plays the role of |Top| when it
occurs at the rhs of |sigma <= Bot|.
This happens when nothing is known about the type of the memory location a value will be put in.
In that case we conclude that that location has type |sigma|.
This is the case for \ruleRef{e-app1} where we cannot determine what the
expected type for the argument of the function in the application is.
\end{itemize}

|Ty_Any| represents ``don't care'' and ``don't know'' respectively.
Even if a correspondence with |Top| and |Bot| from type theory is outlined,
one should be aware that |Ty_Any| is used to smoothen the type checking,
and is not meant to be a (new) type theoretic value.

The type rules leave in the open how to handle a situation when a required
constraint is broken.
For a compiler this is not good enough, being the reason |fitsIn| gives a ``will-do'' type
|Any| back together with an error for later processing (in section~\ref{sec-error1}).
Errors themselves are also described via AG

\chunkCmdUseMark{EHErrorAbsSyn.1.Base}

The |Error| datatype is available as a datatype in the same way a |Ty| is.
The error datatype is also used for signalling undeclared identifiers: 

\chunkCmdUseMark{EHInferExpr.1.Var}

Again, the error condition is signalled by a non empty list of errors
if a lookup in the |Gamma| fails.


Typing \ruleRef{e-ident1B} uses the environment |Gamma| to retrieve the type
of an identifier.
This environment |valGam| for types of
identifiers simply is declared as an inherited attribute,
initialized at the top of the abstrcat syntax tree.
The update with bindings for identifiers will be dealt with later.
\chunkCmdUseMark{EHInfer.1.valGam}

One may wonder why the judgement |jfit sigma1 <= sigma2 : sigma|
and its implementation |fitsIn| do return a type at all.
For the idea of checking was to only pass explicit type information |sigmak| (or |knTy|)
from the top of the abstract syntax tree to the leaf nodes.
However, this idea breaks when we try to check the expression |id 3| in

\begin{code}
let  id :: Int -> Int
     id = \x -> x
 in  id 3
\end{code}

\paragraph{Application |App|.}
What is the |knTy| against which |3| will be checked?
It is the argument type of the type of |id|.
However, in \ruleRef{e-app1B} and its AG implementation,
the type of |id| is not the (top-to-bottom) |sigmak| (or |knTy|), but
will be the argument of the (bottom-to-top) resulting type of $e_1$ (or |@func.ty|)

\chunkCmdUseMark{EHInferExpr.1.App}

The idea here is to encode as |ANY -> sigmak| (passed to |func.knTy|) the partially
known function type and let
|fitsIn| fill in the missing details, that is find a type for |ANY|.
From that result the known/expected type of the argument can be extracted.
For this to work, some additional convenience functions for constructing a
type and deconstructing it are required.

\chunkCmdUseMark{EHTy.1.mkTy}
\chunkCmdUseMark{EHTy.1.mkTyProdApp}
\chunkCmdUseMark{EHTy.1.unMkTy.sigs}
\chunkCmdUseMark{EHTy.1.unMkTy.funs}
\chunkCmdUseMark{EHTy.1.tyProgArgs}

The algebra based |mkArrow| used for building abstract syntax trees
during parsing is now reused to build type structures.

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

Finally, for \ruleRef{e-lam1B} the check if |knTy| has the form |sigma -> sigma|
is done by letting |fitsIn| match the |knTy| with |ANY -> ANY|.
The result (forced to be a function type) is split up by
|tyArrowArgRes|.

\chunkCmdUseMark{EHInferExpr.1.Lam}

\subsubsection{Checking PatExpr}

Before we can look into more detail at the way new identifiers are
introduced in |let|- and |lambda|-expressions
we have to look at patterns.
The \ruleRef{e-let1B} is too restrictive for the actual language construct supported by EH.
The following program allows inspection of parts of a composite value by
naming its components through pattern matching:

\begin{code}
let  p :: (Int,Int)
     p@(a,b) = (3,4)
in   a
\end{code}

The \ruleRef{e-let1C} from \figRef{rules.expr1B.C}
together with the rules for patterns from \figRef{rules.pat1}
reflect the desired behaviour.
These rules differ from those in \figRef{rules.expr1B}
in that a pattern instead of a single identifier is allowed in a
value definition and argument of a |lambda|-expression.

\rulerCmdUse{rules.expr1B.C}
\rulerCmdUse{rules.pat1}

Again the idea is to distribute a known type over the pattern
and dissect it into its constituents.
However, patterns do not return a type but
type bindings for the identifiers inside a pattern instead.
The bindings are subsequently used in
|let|- and |lambda|-expressions.

A tuple pattern with \ruleRef{p-prod1} is encoded in the same way
as tuple expressions, that is, pattern |(a,b)| is encoded as
an application |(,) a b| with an |AppTop| on top of it.

\chunkCmdUseMark{EHInferPatExpr.1.knTy}
\chunkCmdUseMark{EHInferPatExpr.1.knTy.Init}

For this version of EH
the only pattern is a tuple pattern.
Therefore, we can use
this (current) limitation to unpack a known product type |knTy| into its elements
distributed over its arguments via |knTyL|. The additional complexity arises
from repair actions in case the arity of the pattern and its known type do not match.
In that case the subpatterns are given as many |ANY|'s as known type as necessary,
determined by the actual arity of the application.

\chunkCmdUseMark{EHInferPatExpr.1.arity}

As a result of this unpacking, at a
|Var| alternative |knTy| holds the type of the variable name introduced.
The type is added to the |valGam| threaded through the pattern, gathering
all introduced bindings.

\chunkCmdUseMark{EHInferPatExpr.1.valGam}

\subsubsection{Checking declarations}

In a |let|-expression type signatures, patterns and expressions do meet.
\RuleRef{e-let1C} from \figRef{rules.expr1B.C} shows that the idea is straightforward:
take the type signature, distribute it over a pattern to extract bindings
for identifiers and pass both type signature (as |knTy|) and bindings (as |valGam|)
to the expression.
This works fine for single combinations of type signature and the corresponding value definition for
a pattern.
It does not work
\begin{itemize}
\item
For mutually recursive value definitions.
\begin{code}
let  f :: ...
     f = \x -> ... g ...
     g :: ...
     g = \x -> ... f ...
in   ...
\end{code}
In the body of |f| the type |g| must be known and vice-versa.
There is no ordering of what can be defined and checked first.
In Haskell this would make |f| and |g| together a binding group.
\item
For textually separated signatures and value definitions.
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
However, due to the presence of the type signatures it is possible to gather all signatures first
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

\chunkCmdUseMark{EHInfer.1.tySigGam.TysigLet}
\chunkCmdUseMark{EHInfer.1.tySigGam.Val}
\chunkCmdUseMark{EHInfer.1.TySig}

Attribute |gathTySigGam| is used to gather type signatures.
Attribute |tySigGam| is used to distribute the gathered type signatures over the declarations.
In a value declaration we check if a pattern has a type signature
which is to be used as the known type of the pattern and the expression.

\chunkCmdUseMark{EHInfer.1.tyInstKnown}

If the pattern has an identifier at the top level (pattern |(a,b)| has not, |ab@(a,b)| has) and
a signature for this top level identifier everything is in order, indicated by a |True| value in |hasTySig|.

\chunkCmdUseMark{EHInfer.1.mbTopNm}

The value of |hasTySig| is also used to decide to
what the top level identifier of a pattern should be bound,
via |inclVarBind| used at \pageRef{EHInferPatExpr.1.valGam}.

\chunkCmdUseMark{EHInfer.1.inclVarBind}

If a type signature for an identifier is already defined there is no need
to rebind the identifier via an addition of a binding to |valGam| (see ).

New bindings are not immediately added to |valGam| but are first gathered in a
separately threaded attribute |patValGam|, much in the same way
as |gathTySigGam| is used.

\chunkCmdUseMark{EHInfer.1.patValGam}
\chunkCmdUseMark{EHInfer.1.Let}

Newly gathered bindings are stacked on top of the inherited |valGam| before passing them
on to both declarations and body.
Note that this implicitly makes this a three-pass algorithm over declarations.

Some additional functionality for pushing and popping the stack |valGam| is also needed

\chunkCmdUseMark{EHGam.1.Rest.sigs}
\chunkCmdUseMark{EHGam.1.Rest.funs}

Extracting the top of the stack |patValGam| gives all the locally introduced
bindings in |lValGam|.
An additional error message is produced (later on, section~\ref{sec-error1})
if any duplicate bindings are present in |lValGam|.

\subsubsection{Checking TyExpr}

All that is left to do now is to use the type expressions to extract type signatures.
This is straightforward; since type expressions (abstract syntax for what the programmer specified)
and types (as internally used by the compiler) have almost the same structure.

\chunkCmdUseMark{EHInferTyExpr.1}

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
\item |sigmak|/|knTy| is decomposed via |AGPat(DeCompose)| pattern
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

\subsection{Reporting program errors}
\label{sec-error1}

Errors are defined by AG too (see also \pageRef{EHErrorAbsSyn.1.Base}).

\chunkCmdUseMark{EHErrorAbsSyn.1.Rest}

Errors are gathered and at convenient places grouped made part of the
pretty printing |pp|.

\chunkCmdUseMark{EHGatherError.1.GatherExpr}
\chunkCmdUseMark{EHGatherError.1.GatherRest}
\chunkCmdUseMark{EHError.1}

The pretty print of collected errors is made available for
inclusion in the pretty printing of the abstract syntax tree (see \pageRef{EHPretty.1.Base.ExprSimple})
We have chosen to only report the collected errors at
the end of a group of declaration or at a specific
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
\chunkCmdFrameUse{EHErrorAbsSyn.1.Base}
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

\subsection{Tying it all together}

Finally, all needs to be tied together to obtain a working program.
This involves a bit of glue code to (for example) combine scanner (not discussed here),
parser, semantics, passing compiler options and the generation of output.
Though not always trivial at times it mainly is non-trivial because glueing
the pieces can mean that many missing details have to be specified.
For brevity, these details are omitted.

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

%endif not onlyCurrentWork


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 2
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 2: monomorphic type inference}
\label{ehc2}

%if not onlyCurrentWork

\frame<presentation>{\tableofcontents[current,hidesubsections]}

The next version of EH drops the requirement that all value definitions
need to be accompanied by an explicit type signature.
For example, repeating the example from the introduction

\begin{code}
%%2srcfile<test/1-sig-fail.eh%%>
\end{code}
gives
\begin{TT}
%%2ppfile<test/1-sig-fail.eh%%>
\end{TT}

\frame<presentation>[containsverbatim]
{
\frametitle{EH 2: monomorphic type inference}
\begin{itemize}
\item Type signature may be omitted
\item For example
\SafeCode{%
\begin{code}
%%2srcfile<test/1-sig-fail.eh%%>
\end{code}
} 
\item gives
\begin{TT}
%%2ppfile<test/1-sig-fail.eh%%>
\end{TT}
\item Type is inferred/reconstructed
\end{itemize}
}

The idea is that the type system implementation has an internal representation
for ``knowing it is a type, but not yet which one'' which can be replaced
by a more specific type if that becomes known.
The internal representation for a yet unknown type
is called a type variable, similar to mutable variables
for values.

The implementation attempts to gather as much information as possible
from a program
to reconstruct (or infer) types for type variables.
However, the types it can reconstruct are limited to those allowed by
the used type language, that is, basic types, tuples and functions.

So
\begin{code}
%%2srcfile<test/2-id-int.eh%%>
\end{code}
will give
\begin{TT}
%%2ppfile<test/2-id-int.eh%%>
\end{TT}

\frame<presentation>[containsverbatim]
{
\frametitle{Monomorphism}
\begin{itemize}
\item Functions can only be used for one type, not many (polymorphism)
\item For example
\SafeCode{%
\begin{code}
%%2srcfile<test/2-id-int.eh%%>
\end{code}
}
\item infers
\begin{TT}
%%2ppfile<test/2-id-int.eh%%>
\end{TT}
\end{itemize}
}

If the use of |id| to define |v| is omitted less information (namely the argument of |id| is an int) to infer a type for |id| is available
\begin{TT}
%%2ppinline<let id = \x -> x in id%%>
\end{TT}

On the other hand, if contradictory information is found we will have
\begin{TT}
%%2ppfile<test/2-id-intchar.eh%%>
\end{TT}
This may look a bit strange for a Haskell programmer, but we will concern ourselves 
with polymorphism no sooner then with the next version of EH (\chapterRef{ehc3}).

\frame<presentation>[containsverbatim]
{
\frametitle{Monomorphism}
\begin{itemize}
\item Polymorphic use leads to errors
\item For example
\SafeCode{%
\begin{code}
%%2srcfile<test/2-id-intchar.eh%%>
\end{code}
}
\item gives
\begin{TT}
%%2ppfile<test/2-id-intchar.eh%%>
\end{TT}
\end{itemize}
}

Partial type signatures are also allowed.
A partial type signature specifies a type only for a part, allowing
a cooperation between the programmer who specifies what is (e.g.) already
known about a type signature and the type inferencer filling in the unspecified details.
For example:

\begin{code}
%%2srcfile<test/2-ty-wild.eh%%>
\end{code}

The type inferencer pretty prints the inferred type instead of the explicity type signature:

\begin{code}
%%2srcfile<test/2-ty-wild.eh%%>
\end{code}

The discussion of the implementation of this feature is postponed until
\secRef{ehc2partial-sig} in order to demonstrate the effects of an additional feature
on the compiler implementation.

\frame<presentation>
{
\frametitle{Partial type signature}
\begin{itemize}
\item Type need not be specified completely
\SafeCode{%
\begin{code}
%%2srcfile<test/2-ty-wild.eh%%>
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
%%2ppfile<test/2-ty-wild.eh%%>
\end{TT}
\item Shows inferred type only if partially specified
\end{itemize}
}

\subsection{Type variables}

In order to be able to represent yet unknown types the type language needs
\IxAsDef{type variable}s to represent this.

\begin{code}
sigma  =  Int | Char
       |  (sigma,...,sigma)
       |  sigma -> sigma
       |  tvar
\end{code}

The type structure |Ty| also needs to be extended with an alternative for a variable

\chunkCmdUseMark{EHTyAbsSyn.2}

The AG system allows us to separately describe the extension with a new variant as well
as describe separately the additionaly required attribution,
for example the pretty printing of the type

\chunkCmdUseMark{EHTyPretty.2}

A type variable is identified by a unique identifier, a |UID|.

\chunkCmdUseMark{EHCommon.2.UID.Base}
\chunkCmdUseMark{EHCommon.2.UID.Rest}
\chunkCmdUseMark{EHTy.2.TyVarId.Base}
\chunkCmdUseMark{EHTy.2.TyVarId.Rest}

Generation of unique identifiers is not explained here, it can be found in the source code itself.
For now we will ignore this aspect and just assume a unique |UID| can be obtained.

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
       |  tvar
\end{code}
}
\item Type variable |tvar| represents yet unknown type
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
For example, if the check |tvar <= sigma| is encountered
making the previously unknown type |tvar| equal to |sigma|
is the easiest way to make |tvar <= sigma| true.
An alternative way to look at this is that |tvar <= sigma| is true under the
constraint that |tvar| equals |sigma|.

\subsubsection{Remembering and applying constraints}

Next we can observe that once a certain type |tvar| is declared to be
equal to a type |sigma| this fact has to be remembered.

\begin{code}
Cnstr                       =  [tvar :-> sigma]
\end{code}

A set of \IxAsDef{constraint}s |Cnstr| is a set of bindings for type variables,
represented as

\chunkCmdUseMark{EHCnstr.2.Cnstr.Base}
\chunkCmdUseMark{EHCnstr.2.Cnstr.Rest}

If |cnstrUnit| is used as an infix operator it is printed as |`cnstrUnit`| in
the same way as used in type rules.

Different strategies can be used to cope with constraints
\cite{heeren02hm-constr,sulzmann97constrained-type}.
Here
constraints |Cnstr| are used to replace all other
references to |tvar| by |sigma|,
for this reason often named a \IxAsDef{substitution}.
Mostly this will be done immediately after constraints are obtained as
to avoid finding a new and probably conflicting constraint for
a type variable.
Applying constraints means substituting type
variables with the bindings in
the constraints, hence the class
|Substitutable|
for those structures which have references to type
variables hidden inside and can replace, or substitute those type variables

\chunkCmdUseMark{EHCnstr.2.Substitutable}

The operator | ||=>| applies constraints |Cnstr| to a
|Substitutable|.
Function |ftv| extracts the free type variable references as a set of
|TVarId|'s.

A |Cnstr| can be applied to a type

\chunkCmdUseMark{EHCnstr.2.SubstitutableTy}

and is lifted straightforwardly to lists

\chunkCmdUseMark{EHCnstr.2.SubstitutableList}

A |Cnstr| can also be applied to another |Cnstr|

\chunkCmdUseMark{EHCnstr.2.SubstitutableCnstr}

but one must be aware that | ||=>| is non-commutative as constraints |s1| in |s1 ||=> s2| take precedence
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
Cnstr                       =  [tvar :-> sigma]
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
\chunkCmdFrameUse{EHCnstr.2.Substitutable}
\end{itemize}
}

\frame<presentation>[plain]
{
\frametitle{Type substitution}
\begin{itemize}
\item Replacing type variables in a |Ty|:
\chunkCmdFrameUse{EHCnstr.2.SubstitutableTy}
\item Other datatypes (like |Cnstr|) are instances of |Substitutable| too
\end{itemize}
}

\subsubsection{Computing constraints}

The only source of constraints is the check |fitsIn| which finds
out if a type can flow into another.
The previous version of EH had one option in case a type could not fit
in another: report an error.
Now,
if one of the types is unknown, that is, a type variable we have the additional option to
return a constraint on that type variable.
The implementation |fitsIn| of |<=| has to be rewritten to include additional cases
for type variables and the return of constraints

\savecolumns
\chunkCmdUseMark{EHTyFitsIn.2.FIOut}
\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.2.FIOut.empty}
\savecolumns
\chunkCmdUseMark{EHTyFitsIn.2.fitsIn.Base}
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
it differs in the following aspects

\begin{itemize}
\item
The datatype |FIOut| returned by has an additional field |foCnstr| holding found constraints.
\item
The function |bind| creates a binding for a type variable to a type.
The use of |bind| is shielded by |occurBind| which checks if the type variable for
which a binding is created does not occur free in the type bound to.
This is to prevent (e.g.) |a <= a -> a| to succeed.
This is because it is not clear if |a :-> a -> a| should be the resulting constraint
or |a :-> (a -> a) -> (a -> a)| or one of infinitely many other possible solutions.
A so called \IxAsDef{infinite type}
like this is inhibited by the so called \IxAsDef{occur check}.
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
\chunkCmdFrameUse{EHTyFitsIn.2.FIOut}
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

Constraints are used to make found knowledge about previously unknown
types explicit.
The typing rules in \figRef{rules.expr1A} (and \figRef{rules.expr1B}, \figRef{rules.expr1B.C})
in principle need to be changed.
The only reason to adapt some of the rules to the variant in
\figRef{rules.expr2}
is to clarify the way constraints are used.

\rulerCmdUse{rules.expr2}

The type rules in \figRef{rules.expr2} enforce an order in which
checking and inferring types has to be done.

\TBD{...}

Actually, the rules in \figRef{rules.expr2} are more
cluttered with constraints flowing around if we want to approximate
the corresponding AG description, for example for expression application

\chunkCmdUsePrevLit{EHInferExpr.1.App}
\chunkCmdUseMark{EHInferExpr.2.tyCnstr}
\chunkCmdUseMark{EHInferExpr.2.App}

This definition builds on top of the previous version by
redefining some attributes (indicated by |:=| instead of |=|),
the appearance on screen or paper should have a different color or
shade of gray to indicate it is a repetition from a previous appearance elsewhere
in the text.
%if expandPrevRef
%else
We will not continue doing this because it consumes additional space but the
added AG text should be read in conjunction with the original version
because that is also the way the source text is treated by the AG system.
Instead we will refer to the page of the previous code in front of the AG code,
the code for the |Var| alternative shows an example.
%endif

To correspond better with the related AG code the \ruleRef{e-app2} should be

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
Used in tandem they strengthen eachother.

An implementation by necessity imposes additional choices to make a typing
rule into an algorithmic solution.
For example, our description preserves the invariant
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
this will also be omitted for later typing rules.
However, the constraint applications are shown by the AG code for
the |App| alternative and the following |Var| alternative

\chunkCmdUseOnPrev{EHInferExpr.1.Var}{EHInferExpr.2.Var}

The rules for constants all resemble the one for |Int|, \ruleRef{e-int2}.
Their implementation additionaly takes care of constraint handling

\chunkCmdUseOnPrev{EHInferExpr.1.Const}{EHInferExpr.2.Const}

The handling of products does not differ much from the previous
implementation.
A \ruleRef{e-con2} has been included in the typing rules,
as a replacement for \ruleRef{e-prod1B} (\figRef{rules.expr1B}) better
resembling its implementation.
Again the idea is to exploit that in this version of EH tupling is the
only way to construct an aggregrate value.
A proper structure for its type is (again) forced by |fitsIn|.

\chunkCmdUseOnPrev{EHInferExpr.1.Con}{EHInferExpr.2.Con}

Finally, 

\chunkCmdUseOnPrev{EHInferExpr.1.Lam}{EHInferExpr.2.Lam}

which uses some additional functions for creating type variables

\chunkCmdUseMark{EHTy.2.NewTyVar}
\chunkCmdUseMark{EHTy.2.NewTyVarL}

Some observations are in place
\begin{itemize}
\item
The main difference with the previous implementation is the use
of type variables to represent unknown knowledge.
Previously |ANY| was used for that purpose, for example,
the \ruleRef{e-lam2} and its implementation show that fresh
type variables |tvari| in |tvar1 -> tvar2| are used instead
of |ANY -> ANY| to enforce a |.. -> ..| structure.
If |ANY| still would be used, for
\begin{code}
let  id = \x -> x
in   id 3
\end{code}
the conclusion would be drawn that |id :: ANY -> ANY|,
whereas |id :: tvar -> tvar| would later on have bound |tvar :-> Int| (at the application |id 3|).
So, |ANY| represents ``unknown knowledge'',
a type variable |tvar| represents ``not yet known knowledge''
to which the inferencing process later has to refer to make it ``known knowledge''.
\item
Type variables are introduced under the condition that they are
\Ix{fresh type variable}``fresh''.
For a typing rule this means that these type
variables are not in use elsewhere,
often more concretely specified with a condition |tvar `notElem` ftv(Gamma)|.
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
\includegraphics[height=25cm]{figs/cycle-case1.pdf}
\onslide<2>
\includegraphics[height=25cm]{figs/cycle-case2.pdf}
\onslide<3>
\includegraphics[height=25cm]{figs/cycle-case3.pdf}
\onslide<4>
\includegraphics[height=25cm]{figs/cycle-case4.pdf}
\onslide<5>
\includegraphics[height=25cm]{figs/cycle-case4b.pdf}
\onslide<6>
\includegraphics[height=25cm]{figs/cycle-case5.pdf}
\onslide<7>
\includegraphics[height=25cm]{figs/cycle-case6.pdf}
\onslide<8>
\includegraphics[height=25cm]{figs/cycle-case7.pdf}
\end{overprint}
}

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
by means of |tyEnsureNonBotTop|

\chunkCmdUseMark{EHInferPatExpr.2.Var}
\chunkCmdUseMark{EHTy.2.tyEnsureNonBotTop}

For tuples we again make use of the fact that the |Con| alternative will always
represent a tuple.
%if omitEH5Onwards
When datatypes are introduced (not part of this paper) this will no longer be the case.
%else
From \chapterRef{ehc5} when datatypes are introduced
and onwards this will no longer be the case.
%endif
Here, we already make the required \ruleRef{p-con2} more general
than is required here in that it will reflect the idea of a pattern.
A pattern (in essence) can be represented by a function taking a value of some type and
dissecting it into a tuple containing all its constituents.
For now, because we have only tuples to dissect, the
function returned by the |Con| alternative is just the identity
on tuples of the correct size.
The application \ruleRef{p-app2} consumes an element of this tuple representing
the dissected value and uses it for checking and inferring the constituent.

The implementation of this representation convention returns the dissecting function type
in |patFunTy|

\chunkCmdUseMark{EHInferPatExpr.2.patFunTy}

which is at the top distributed as described for the previous version.

\chunkCmdUseOnPrev{EHInferPatExpr.1.knTy}{EHInferPatExpr.2.knTy}

Finally, the type itself and additional constraints are returned

\chunkCmdUseMark{EHInferPatExpr.2.Rest}

The careful reader may have observed that the direction of |<=|
for fitting actual (synthesized, bottom-up) and known type (inherited, top-down)
is the opposite of the direction used for expressions.
This is a result of a difference in the meaning of an expression and a pattern.
An expression builds a value from bottom to top as seen in the context of an abstract syntax
tree.
A pattern dissects a value from top to bottom.
The flow of data is opposite, hence the direction of |<=| also.

\subsection{Declarations}

Again, at the level of declarations all is tied together.
Because we first gather information about patterns and then about expressions
two separate threads for gathering constraints are used, |patTyCnstr|
and |tyCnstr| respectively.

\chunkCmdUseMark{EHInfer.2.Let}
\chunkCmdUseMark{EHInfer.2.Rest}

If a type signature has been given it is used as the known type for both
expression and pattern. If not, the type of a pattern is used as such.

\chunkCmdUseMark{EHInfer.2.tyInstKnown}

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
\item Parser for type expressions
\chunkCmdFrameUse{EHParser.2.pTyExpr}
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
In other words, an impact analysis.

First, both abstract syntax and the parser contain an additional alternative for parsing the "@...@" notation
chosen for unspecified type information designated by |Wild| for wildcard:

\chunkCmdUseMark{EHAbsSyn.2.TyExpr}
\chunkCmdUseMark{EHParser.2.pTyExpr}

A wildcard type is treated in the same way as a type variable as it also represents unknown 
type information:

\chunkCmdUseMark{EHInferTyExpr.2.tyVarGather}
\chunkCmdUseMark{EHInferTyExpr.2.ty}

Changes also have to be made to omitted parts of the implementation, in particular the pretty printing
and generation of unique identifiers.
We mention the necessity of this but omit the relevant code.

Finally, in order to decide which type to pretty print all `wild' type variables are gathered
so we can check if any wild type variables were introduced.

\chunkCmdUseMark{EHInferTyExpr.2.tyVarWildL}

Pretty printing then uses the final type which has all found constraints incorporated.

\chunkCmdUseMark{EHInfer.2.finValGam}

%endif not onlyCurrentWork

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 3
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 3: polymorphic type inference}
\label{ehc3}

%if not onlyCurrentWork

\frame<presentation>{\tableofcontents[current,hidesubsections]}

The third version of EH adds polymorphism, in particular so-called parametric polymorphism
which allows functions to be used on arguments of differing types.
For example
\begin{code}
%%srcfile<afp-eh/04.eh%%>
\end{code}
gives |v :: %%3file<afp-eh/04.eh%%>|
and |id :: %%3<let id = \x -> x in id%%>|.
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
%%srcfile<afp-eh/04.eh%%>
\end{code}
}
\item |a -> a| means |forall a . a -> a|
\end{itemize}
}

The type signature may be omitted, in that case the same type will still be inferred.
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
%%3srcfile<test/3-mono-arg.eh%%>
\end{code}

It will give the following output:

\begin{TT}
%%3ppfile<test/3-mono-arg.eh%%>
\end{TT}

The problem here is that the polymorphism of |f| in |a| means that the caller
of |f| can freely choose what this |a| is for a particular call.
However, from the viewpoint
of the body of |f| this limits the choice of |a| to no choice at all.
If the caller has all the freedom to make the choice, the callee has none.
This is encoded as a type constant @c_@ chosen for |a| during type checking
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
%%3srcfile<test/3-mono-arg.eh%%>
\end{code}
}
\item Incorrect:
\SafeCode{%
\begin{code}
%%3srcfile<test/3-mono-arg3.eh%%>
\end{code}
}
\item Only identifiers bound by |let|
\end{itemize}
}

Another example of the limitations of polymorphism in this version of EH is the following
variation
\begin{code}
%%3srcfile<test/3-mono-arg2.eh%%>
\end{code}
for which the compiler will infer types
\begin{TT}
%%3ppfile<test/3-mono-arg2.eh%%>
\end{TT}

\frame<presentation>
{
\frametitle{Polymorphism and inferencing}
\begin{itemize}
\item<+-> Type inferencer deduces polymorphism at |let| bindings only
\item Bindings can only be polymorphically used in body of |let|:
\SafeCode{%
\begin{code}
%%3srcfile<test/3-mono-arg2.eh%%>
\end{code}
}
\item Type of \alert<+>{|f :: %%3file<test/3-mono-arg2.eh%%>|}
\onslide<+->
\item If used in same group of bindings:
\SafeCode{%
\begin{code}
%%3srcfile<test/3-mono-arg4.eh%%>
\end{code}
}
\item Type of \alert<+>{|f :: %%3file<test/3-mono-arg4.eh%%>|}
\item<+-> Analysis per \emph{binding group}
\end{itemize}
}

EH version 3 allows parametric polymorphism but not polymorphic parameters.
The parameter |i| has a monomorphic type, which is made even more clear when
we make an attempt to use this |i| polymorphically in
\begin{code}
%%3srcfile<test/3-mono-arg3.eh%%>
\end{code}
for which the compiler will infer types
\begin{TT}
%%3ppfile<test/3-mono-arg3.eh%%>
\end{TT}

Because |i| is not allowed to be polymorphic it can either be used on |Int| or |Char|, but
not both.

However, the next version (\chapterRef{ehc4}) does permit polymorphic parameters.

The ``monomorphic parameter'' problem could have been solved by allowing a programmer to explicitly specify a
|forall| for the type of the parameter |i| of |f|.
The type of |f| would then be |(forall a . a -> a) -> (Int,Char)| instead of
|forall a . (a -> a) -> (Int,Char)|.
In this version of EH (resembling Haskell98) it
is not permitted to specify polymorphism explicitly,
but the next version of EH does permit this.

The reason not to allow explicit types is that Haskell98 and this version of EH have as a design principle
that all explicitly specified types in a program are redundant.
That is, after removal of explicit type signatures,
the type inferencer can still reconstruct all types.
It is guaranteed that all reconstructed
types are the same as the removed signatures or more general, that is,
the type signatures are a special case of the inferred types.
This guarantee is called the principal type property
\cite{damas82principal-type,milner78type-poly,hindley69princ-type}.

However, type inferencing also has its limits\TBD{[cite...]}.
In fact, the richer a type system becomes, the more difficulty a type inferencing algorithm
has in making the right choice for a type.
In the next version it is allowed to specify types which otherwise could not have
been found by a type inferencing algorithm.

\subsection{Type language}

The type language for this version of EH adds quantification with the universal quantifier |forall|
\begin{code}
sigma  =  Int | Char
       |  (sigma,...,sigma)
       |  sigma -> sigma
       |  tvar | tvarf
       |  forall alpha . sigma
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
       |  tvar | tvarf
       |  forall alpha . sigma
\end{code}
}
\onslide
\item AG:
\chunkCmdFrameUse{EHTyAbsSyn.3}
\item (more about |tvarf|/|Fixed| later)
\end{itemize}
}

A |tvarf| stands for
a fixed type variable,
a type variable which may not be constrained but still stands for
an unknown type.
A series of consecutive quantifiers in |forall alpha1 . forall alpha2 . ... sigma|
is abbreviated to |forall ^ Vec(alpha) . sigma|.

The corresponding abstract syntax for type expressions
also need an additional alternative

\chunkCmdUseMark{EHTyAbsSyn.3}

together with a convenience function for making such a quantified type

\chunkCmdUseMark{EHTy.3}

The discussion of type variable categories is postponed until \secRef{ehc3instantiation}.

However, the syntax of this version of EH only allows type variables to be specified as part
of a type signatures.
The quantifier |forall| cannot be explicitly denoted.

\chunkCmdUseMark{EHAbsSyn.3}

As a consequence the parser for type expressions has to include an alternative
in |pTyExprBase| to parse type variables.

\chunkCmdUseMark{EHParser.3}

The type language suggests that a quantifier may occur anywhere in a type.
This is not the case, quantifiers may only be on the top of a type.
A second restriction is that quantified types
are present only in a |Gamma| whereas no |forall|'s are
present in types used throughout type inferencing expressions and patterns.
This is to guarantee the principle type property \TBD{more refs [..]}.

\subsection{Type inference}

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
\item Standard Hindley/Milner type inference
\item (other rules and code omitted)
\end{itemize}
}

\rulerCmdUse{rules.expr2.3}

A quantified type, or also often named \IxAsDef{type scheme},
is introduced in \ruleRef{e-let3} and \ruleRef{e-let-tysig3} and instantiated
in \ruleRef{e-ident3}, see \figRef{rules.expr2.3}.
We will first look at the \IxAsIs{instantiation}.

\subsubsection{Instantiation}
\label{ehc3instantiation}

\chunkCmdUseMark{EHInferExpr.3.Var}

A quantified type is used whenever a value identifier having that type is referred to
in an expression.
We may freely decide what type the quantified type variables may have as long
as each type variable stands for a monomorphic type.
However, at this point it is not known which type a type variable
stands for so fresh type variables are
used instead.
This is called \IxAsDef{instantiation}, or \IxAsIs{specialization}.
The resulting instantiated type partakes in the inference process as usual.

The removal of the quantifier and replacement with all quantified type variables
is done by |tyInst|:

\chunkCmdUseMark{EHTyInstantiate.3.tyInst}

Function |tyInst| strips all quantifiers and substitutes the quantified type variables with fresh ones.

\subsubsection{Quantification}

The other way around, quantifying a type, happens when a type is
bound to a value identifier and added to a |Gamma|.
The way this is done varies with the presence of a type signature.
\RuleRef{e-let3} and \ruleRef{e-let-tysig3} (\figRef{rules.expr2.3})
differ only in the use of a type signature, if present.

\chunkCmdUseOnPrev{EHInfer.1.TySig}{EHInfer.3.TySig}

A type signature simply is quantified over all free type variables in the type using

\chunkCmdUseMark{EHTyQuantify.3.tyQuantify}
\chunkCmdUseMark{EHTyQuantify.3.tyQuantifyClosed}

Type variables introduced by a wildcard may not be quantified over.

We now run into a problem which will be solved no sooner than the next version of EH.
The type signature acts as a known type |knTy| against which checking takes place.
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
we end up with fitting |tvar1 -> Int <= a -> a|.
This can be done via constraints |[tvar1 :-> Int, a :-> Int]|.
However, |a| was supposed to be chosen by the caller of |id|
while now it is constrained by the body of |id| to be an |Int|.
Somehow constraining |a| whilst being used as part of a known type for the body of
|id| must be inhibited.
\item
Alternatively, |sigTy| may be used.
However, the inferencing process and the fitting done by |fitsIn| cannot (yet) handle
types with quantifiers.
\end{itemize}

For now, this can be solved by replacing all quantified type variables of a known type
with type constants

\chunkCmdUseOnPrev{EHInfer.1.tyInstKnown}{EHInfer.3.tyInstKnown}

by using a variant of |tyInst|

\chunkCmdUseMark{EHTyInstantiate.3.tyInstKnown}

This changes the category of a type variable to `fixed'.
A \IxAsDef{fixed type variable} is like a plain type variable but may not be constrained,
that is, bound to another type.
This means that |fitsIn| has to be adapted to prevent that from happening.
The difference with the previous version only lies in the handling of type variables.
Type variables now may be bound if not fixed and are equal only if their categories match too.
For brevity the new version of |fitsIn| is omitted.


\subsubsection{Generalization/quantification of inferred types}

How do we determine if a type for some expression is polymorphic?
If a type signature is given, the signature itself describes the
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
body expression bound to |id|, is via the identifier |id|.
So, if the inferred type |tvar1 -> tvar1| for the expression |\x -> x| has free type variables
(here: |[tvar1]|)
and these type variables are not used in the types of other bindings, in particular those
in the global |Gamma|,
we know that the expression |\x -> x| nor any other type will constrain those free type variables.
The type for such a type variable apparently can be freely chosen by
the expression using |id|, which is exactly the meaning
of the universal quantifier.
These free type variables are the candidate type variables over which quantification can take place,
as described by the typing rules for |let|-expressions in \figRef{rules.expr2.3} and its implementation

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
with the exception of those available more globally, the |gTyTvL|.

\chunkCmdUseMark{EHGam.3.valGamQuantify}
\chunkCmdUseMark{EHGam.3.gamMap}

The condition that quantification only may be done for type variables not occurring in
the global |Gamma| is a necessary one.
Take for example,
\begin{code}
%%3srcfile<test/3-mono-glob.eh%%>
\end{code}

If the type |g :: a -> (a,a)| would be concluded, |g| can be used with |y| an |Int| parameter, as
in the example. Function |f| can then be used with |x| a |Char| parameter.
This would go wrong because |h| assumes the types of its parameters |x| and |y| are equal.
So, this justifies the error given by the compiler for this version of EH:

\begin{TT}
%%3ppfile<test/3-mono-glob.eh%%>
\end{TT}

All declarations in a |let|-expression together form what in Haskell is called a binding group.
Inference for these declarations is done together and all the types of all identifiers
are quantified together. The consequence is that a declaration that on its own would be polymorphic,
may no be so in conjunction with an additional declaration which uses the previous declaration

\begin{code}
%%3srcfile<test/3-mono-bind.eh%%>
\end{code}

The types of the function |id1| and value |v1| are inferred in the same binding group.
However, in this binding group the type for |id1| is |tvar1 -> tvar1| for some type variable |tvar1|,
without any quantifier around the type.
The application |id1 3| therefore infers an additional constraint |tvar1 :-> Int|, resulting
in type |Int -> Int| for |id1|

\begin{TT}
%%3ppfile<test/3-mono-bind.eh%%>
\end{TT}

On the other hand, |id2| is used after quantification, outside the binding group, with
type |forall a . a -> a|.
The application |id2 3| will not constrain |id2|.

In Haskell binding group analysis will find groups of mutually dependent definitions,
each of these called a binding group. These groups are then ordered
according to ``define before use'' order.
Here, for EH, all declarations in a |let|-expression
automatically form a binding group, the ordering of to binding groups |d1| and |d2| has
to be done explicitly by |let d1 in let d2 in ...|.

Being together in a binding group can create a problem for inferencing mutually recursive definitions,
for example:

\begin{code}
%%3srcfile<test/3-mut-rec.eh%%>
\end{code}

This results in

\begin{TT}
%%3ppfile<test/3-mut-rec.eh%%>
\end{TT}

For |f1| it is only known that its type is |tvar1 -> tvar2|.
Similarly |g1| has a type |tvar3 -> tvar4|.
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

\chunkCmdUseMark{EHGam.1.TyGamInfo}
\chunkCmdUseMark{EHGam.1.TyGam}
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

%endif not onlyCurrentWork





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 4
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 4: |forall| and |exists| everywhere}
\label{ehc4}

%if not onlyCurrentWork

\frame<presentation>{\tableofcontents[current,hidesubsections]}

This version of EH adds explicit types with quantifiers at all positions in a type,
existential quantification and an interpretation of unquantified types to
quantified types.

\paragraph{Higher ranked explicit polymorphism.}
For example in
\begin{code}
%%4srcfile<test/3-poly-rank.eh%%>
\end{code}
|f| has type |f :: %%4file<test/3-poly-rank.eh%%>|,
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
%%4srcfile<test/3-poly-rank.eh%%>
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
\item No type inference to infer higher ranked types, only ``not forgetting'' quantifier information
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
%%4srcfile<test/4-ex-extr.eh%%>
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
%%4ppfile<test/4-ex-extr.eh%%>
\end{TT}

\frame<presentation>
{
\frametitle{Existential types}
\begin{itemize}
\item ``It exists, but I've forgotten what it was''
\SafeCode{%
\begin{code}
%%4srcfile<test/4-ex-extr.eh%%>
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
%%4ppfile<test/4-ex-extr.eh%%>
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
%%4srcfile<test/4-ex-extr3.eh%%>
\end{code}

gives

\begin{TT}
%%4ppfile<test/4-ex-extr3.eh%%>
\end{TT}

\frame<presentation>[containsverbatim]
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
%%4srcfile<test/4-ex-extr3.eh%%>
\end{code}
}
\item
\begin{TT}
%%4ppfile<test/4-ex-extr3.eh%%>
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
%%4srcfile<test/4-ex-extr4.eh%%>
\end{code}

gives

\begin{TT}
%%4ppfile<test/4-ex-extr4.eh%%>
\end{TT}

\frame<presentation>[containsverbatim]
{
\frametitle{Opening an existential}
\begin{itemize}
\item Nested existentials are retained
\SafeCode{%
\begin{code}
%%4srcfile<test/4-ex-extr4.eh%%>
\end{code}
}
\item
\begin{TT}
%%4ppfile<test/4-ex-extr4.eh%%>
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
%%4srcfile<test/4-ex-extr2.eh%%>
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
%%4ppfile<test/4-ex-extr2.eh%%>
\end{TT}

\frame<presentation>
{
\frametitle{Quantifier location}
\begin{itemize}
\item Notational sugar/convention
\SafeCode{%
\begin{code}
%%4srcfile<test/4-ex-extr2.eh%%>
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
%%4ppfile<test/4-ex-extr2.eh%%>
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
       |  tvar | tvarf
       |  Qu alpha . sigma, Qu `elem` {forall, exists}
\end{code}
We also need an infinite supply of type constants |tcon|.
Quantifiers |Qu| may now appear anywhere in a type.

The |Quant| alternative of the type structure has to made more general
to be able to encode |exists| too.

\chunkCmdUseMark{EHTyAbsSyn.4}

Some additional functions on quantifiers |TyQu| are defined here too.
These may seem unnecessary but the extra layer of abstraction
is convenient when the range of quantifiers is extended
%if omitEH5Onwards
later on (not included in this paper):
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
%%srcfile<test/4-exists-sub.eh%%>
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
%%srcfile<test/4-forall-sub.eh%%>
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

Let us look at some other examples to get a better idea of what |fitsIn|/|<=|
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
\item |fitsIn|/|<=| compares types with quantifiers
\end{itemize}
\end{itemize}
}

\frame<presentation>
{
\frametitle{Issue: asymmetry |fitsIn|/|<=|}
\begin{itemize}
\item Forgetting type information is ok, but irreversible
\SafeCode{%
\begin{code}
%%srcfile<test/4-exists-sub.eh%%>
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
\frametitle{Issue: asymmetry |fitsIn|/|<=|}
\begin{itemize}
\item Instantiation polymorphism is ok,
but the reverse only if it can be guaranteed no additional
constraints will be found for the quantified type variable
\SafeCode{%
\begin{code}
%%srcfile<test/4-forall-sub.eh%%>
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
\frametitle{Issue: asymmetry |fitsIn|/|<=|}
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
%%srcfile<test/4-choose.eh%%>
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
So, |id| is instantiated to |tvar1 -> tvar1|, giving |choose id :: (tvar1 -> tvar1) -> tvar1 -> tvar1|
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
using |fitsIn|/|<=| to allow richer types still to match properly.

\frame<presentation>
{
\frametitle{Impredicativeness}
\begin{itemize}
\item Quantified types throughout type inference can/cannot
be bound to type variables?
\SafeCode{%
\begin{code}
%%srcfile<test/4-choose.eh%%>
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
in particular
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
is to instantiate |sigma1| before binding it to a type variable |tvar|
in the case |sigma2 == tvar|.
This information is passed as an additional parameter to |fitsIn|, notated
by |instLFIOpts|, named an \IxAsDef{instantiating context}.
\item
For checking an expression |e :: sigma2| of a declaration |v :: sigma1; v = e| to its known type
|sigma1|
we check |sigma2 <= sigma1|.
In this case we want to avoid instantiating |sigma2|.
The necessity of avoiding this becomes clear if we look at a situation where
|sigma2 == (exists a . a, ...)| and |sigma1 == (tvar,...)|,
coinciding with a situation where an explicit type signature is absent.
Now, if |exists a . a| is instantiated with type constants before it is bound to |tvar|
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
%if omitEH5Onwards
(not included in this paper).
%else
(\chapterRef{ehc5}).
%endif
\end{itemize}

\label{ehc4-fitsIn-strength}
|fitsIn|/|<=| therefore needs an option |fiopt| to describe these variations
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
A |True| value for the flag |fioBindRFirst| states that in case of |sigma <= tvar|
a constraint |(tvar :-> sigma)| will result,
otherwise first |sigma| will be instantiated and |tvar| be bound to
the instantiated |sigma|.
Similary we have |fioBindLFirst| for |tvar <= sigma|.
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

\chunkCmdUseMark{EHTyFitsIn.4.FIOpts}

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
%%srcfile<test/4-forall-sub.eh%%>
\end{code}
to the check |Int -> Int <= forall a . a -> a| which has to be done for |id = ii|.
If |forall a . a -> a| is instantiated with type variables |tvar|,
the check would succeed with a constraint |(tvar :-> Int)|.
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
This is simulated by instantiating with fresh constrainable type variables |tvar|,
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

\chunkCmdUseMark{EHTyFitsIn.4.CoCo}
\chunkCmdUseMark{EHTyFitsIn.4.cocoGam}

It also shows that only for function and tuple types we know what to do in such a situation.
Complications in this area will arise with the introduction of datatypes
%if omitEH5Onwards
later on (not included in this paper).
%else
in \chapterRef{ehc5}.
%endif

\paragraph{|fitsIn| parameters and result.}
So, let us know finally look at the implementation of |<=|,
and redo the implementation of |fitsIn|.
First, |fitsIn| needs to pass information both up and downwards.
Upwards was already implemented via

\chunkCmdUseMark{EHTyFitsIn.4.FIOut}

which is extended with |CoContraVariant| information and threads a UID value
needed for instantiating types together with the downward information stored in

\chunkCmdUseMark{EHTyFitsIn.4.FIIn}

The parameter |fiCoContra| is used to indicate if the comparison |<=| is flipped.

\paragraph{The fitting.}
The preliminaries of |fitsIn| have not changed much compared to
the previous version (\pageRef{EHTyFitsIn.2.fitsIn.Base}).
All internally defined functions now take an additional top-down |fi :: FIIn|
parameter and some work has to be done for extracting and passing variance information
(in function |res|).

\savecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.Prelim}

The fitting of quantified types uses |unquant| which removes all top level quantifiers.

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.unquant}

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

\restorecolumns
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.Base}

The order in which all these case are listed is now important
as the cases for |fioBindLFirstN| and |fioBindRFirstN| will
be executed only after types are stripped of their top level quantifiers.

Compared to the rules in \figRef{rules.fit4.quant}
an additional case has been included for an exact match of
two quantified types when we want |t1 <= t2| and |t2 <= t1| both
to hold. We will postpone discussion until
%if omitEH5Onwards
later (not included in this paper).
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
\chunkCmdUseMark{EHTyFitsIn.4.fitsIn.Rest}

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
%if omitEH5Onwards
later on (not included in this paper).
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

\TBD{previous should be redone.}


\TBD{until here}


\chunkCmdUseMark{EHInferExpr.4.Rest}

\chunkCmdUseMark{EHInferPatExpr.4.Rest}






%if inclOmitted
\subsection{Omitted, more of the same}
Substitution, error gathering, pretty printing, uniq, common
%endif

%if not omitLitDiscuss
\subsection<article>{Literature}

\TBD{}

Higher ranked types, \cite{peytonjones03pract-inf-rank,botlan03ml-power-f}

Cannot do inference for rank3, \cite{jim95rank,kfoury94direct,kfoury99rank2-decid,kfoury03rank2-princ}

Existentials, via universal \cite{laufer96class-existential}

%endif




%endif not onlyCurrentWork




%if omitEH5Onwards
%else

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 5
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 5: data types}
\label{ehc5}

%if forAfpHandout
This part is not included in this version of this paper.
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
        |  cocovar       -- variant variable
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






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 6
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 6: kind inference}
\label{ehc6}

%if forAfpHandout
This part is not included in this version of this paper.
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 7
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 7: non extensible records}
\label{ehc7}

\frame<presentation>
{
\frametitle{Records}
\begin{itemize}
\item
\SafeCode{%
\begin{code}
%%7srcfile<test/7-all-ok.eh%%>
\end{code}
}
\end{itemize}
}

\subsection{Current implementation examples}

\begin{code}
%%7srcfile<test/7-all-ok.eh%%>
\end{code}

with output

\begin{TT}
%%7ppfile<test/7-all-ok.eh%%>
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
%%7srcfile<test/7-tup-sel.eh%%>
\end{code}
\end{itemize}

Rows themselves are delimited by |(||...||)|, variants by |(<...>)|.

A row/record is always based on (or an extension of) an empty row/record.
For a following version allowing extensible records this restriction (obviously) is removed.

\paragraph{Case expression, punning.}
Pattern matching a record is done via case:

\begin{code}
%%7srcfile<test/7-pun.eh%%>
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
%%7ppfile<test/7-pun.eh%%>
\end{TT}

\paragraph{Relation with other proposals.}
For the rest the proposal by Jones \cite{jones99lightweight-ext-rec} is followed.

This proposal also deviates from TRex (by extending from left to right) and is incompatible
with Haskell because of the use of @'.'@ as the builtin selection operator.

\subsection{Issues, partly resolved or previous}

%if inclFuture

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


\subsection<article>{Literature}

%endif inclFuture




%if inclFuture

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 8
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 8: code generation}
\label{ehc8}

Coercion (of records), related to subsumption

\subsection<article>{Literature}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 9
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 9: Explicit Implicitness}
\label{ehc9}

%{
%format pred        = "\mathbf{pred}"
%format rule        = "\mathbf{rule}"
%format pia         = pi "^a"
%format piasigma    = pia "_{" sigma "}"

Extensible records as case study

Subsumption and proof of impl params

Prolog alike proof system/rules combined with coercion terms ????

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
let  f  ::  (# r lacks l #) ->  (r | l :: a)  -> a
     f  =                       \r            -> r.l
     g  ::  (# Eq a #) ->  [a] ->  [a] -> Bool
     g  =   \(# eq #)      \x      \y  -> (==) (#eq#) ... && (==) ...
\end{code}

The latter approach has a couple of advantages.
\begin{itemize}
\item
LL parsing is made easier.
\item
The same notation can be used for expressions (value terms) and patterns.
\end{itemize}

The obvious syntactic sugar can be added, e.g. |(# p, q #) ->| for |(# p #) -> (# q #) ->|

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
\rulerCmdUse{rules.expr9.e-app9}
\]

This involves some additional notation:
\begin{itemize}
\item
In this rule we assume that each value term is translated to a form
where implicit parameters are explicit. This translation is denoted by |Transl|.
\item
We also assume that |Gamma| holds values from the implicit world
by means of bindings of the form |[pi ~> e]|, associating predicates |pi| to implicit values |e|.
\item
The type language has an additional alternative:
\begin{code}
sigma  =  ..
       |  (# pi #)
pi     =  r lacks \
       |  C ^^ Vec(sigma)
       |  v = sigma
\end{code}
The alternatives for |pi| respectively denote the lacking constraint for extensible records, class constraint and equality constraint
(for use by generalized data types).
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
pred  Eq a ~> ( eq :: a -> a -> Bool )
pred  Eq a ~> x => Ord a ~> ( lt :: a -> a -> Bool, Eq :: (# Eq a #) ) = ( r | Eq = x )
\end{code}
This should be read as:
\begin{itemize}
\item
A predicate |Eq a| has as its proof/evidence/witness object a record with a function |eq|.
\item
A predicate |Ord a| has as its proof a record with a function |lt| and another record |Eq| (for the superclass).
The proof object for |Eq| value is declared implicit.
However, additional evidence can be specified, here |Eq a ~> x|.
Additionally it can be used to partially specify its initialization.
In this way also default fields should be specifiable.
\\
Issues: translation to function taking |x| as well as |r| as offset params as parameter? Can this work, is it a good idea at all?
Or just only use the notation but under the hood use record structure as being known anyway.
\end{itemize}

Rules, corresponding to instance declaratations, populate the world of predicates:
\begin{code}
rule Eq Int = ( eq = primEqInt )
rule Eq a ~> e => Eq [a] ~> l =  ( eq = \a -> \b ->  eq (# e #) (head a) (head b) &&
                                                     eq (# l #) (tail a) (tail b)
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
rule Ord a ~> o => Eq a ~> e = o.Eq
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
rule eqList1 :: Eq a ~> e => Eq [a] ~> l =  ( eq = \a -> \b ->  eq (# e #) (head a) (head b) &&
                                                                eq (# l #) (tail a) (tail b)
                                            )

... eq (# eqList1 eqInt1 #) ...
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

%}

\subsection<article>{Literature}

Named instances \cite{kahl01named-instance}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 10
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 10: type synonyms}
\label{ehc10}

with explicit kinding, plus checking thereof

as lambda's on type level, effect on fitsIn (where expansion takes place).

\begin{code}
L :: * -> *
data L a = N | C a (L a)
\end{code}

\subsection<article>{Literature}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 11
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 11: classes}
\label{ehc11}

built on implicit params

\subsection<article>{Literature}

\cite{laufer96class-existential}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 12
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 12: modules}
\label{ehc12}

built on records

\subsection<article>{Literature}





%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC 13
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH 13: syntax macro's}
\label{ehc13}

or other rewritings in the form a pre parser ??

do notation

list comprehension

if expression

case expression pattern reordering/compiler

infix declarations

binding group analysis, dependency analysis

\subsection<article>{Literature}

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC ??
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH ??: genericity, |deriving|}

|deriving| for data structures



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC XX-1
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH XX-1: data structure optimizing}
\label{ehcXX1}

Converting O(n) structures to faster (i.e., [] -> Set, assoc list -> Map)

\subsection<article>{Literature}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC XX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{EH XX: Haskell front end}
\label{ehcXX}

UHC ??

error messaging, line/col position, comment ????

\subsection<article>{Literature}


%endif %%inclFuture

%endif omitEH5Onwards






%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% References
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if not asSlides
\AddContentsLine{References}
%if refToPDF
\bibliographystyle{uhcbook}
%else
\bibliographystyle{plain}
%endif
\bibliography{LitAdm}
%endif

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Appendices
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if not omitAppendix && not onlyCurrentWork

\appendix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AG Patterns
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

\section{AG pattern: global variable simulation}

%if forAfpHandout
This part is not included in this version of this paper.
%else
|Int| unique thread as an example
%endif

\section{AG pattern: unique value generation}
\label{app-ag-pattern-uid}
%if forAfpHandout
This part is not included in this version of this paper.
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
This part is not included in this version of this paper.
%else

Via threading

Via collecting, via |USE|.

\section{AG pattern: mutual dependent info}

Gathering placeholders and later updating them

ty inference + pat name gathering

%endif

\section{AG pattern: gathering from within and usage higher up}

%if forAfpHandout
This part is not included in this version of this paper.
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
This part is not included in this version of this paper.
%else

Scanner config
%endif

\section{EH: Connecting with the outside world}
\label{app-outside-connect}

%if forAfpHandout
This part is not included in this version of this paper.
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

%endif omitAppendix

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Index
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%if not asSlides && not onlyCurrentWork
\AddContentsLine{Index}
{\small
\Input{\jobname.ind}
}
%endif

\end{document}

