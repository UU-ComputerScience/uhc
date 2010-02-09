%%
%% This is file `st-thesis.cls`
%% $Id: st-thesis.cls 2286 2004-06-15 13:01:19Z adam $
%%
%% An all-inclusive documentclass for (Master-) theses
%% of ST students.
%%  - We aim at making sans-serif more accepted.
%%  - B5paper is a nice format, even though we're not PhD yet.
%%  - Autoloads the stratego package for source listings, etc.
%%
%% Initial setup by Arthur van Dam <adam@cs.uu.nl>
%% Feel free to contribute!
%%

\ProvidesClass{st-thesis}[2004/05/10]

\makeatletter

% PDF detection
\newif\ifpdf
\newif\ifpdftex
\ifx\pdfoutput\undefined
  \pdffalse
  \pdftexfalse
\else
  \pdftrue
  \pdftextrue
  \AtBeginDocument{%
    \pdfpageheight = \paperheight
    \pdfpagewidth = \paperwidth
  }
\fi

%\ProcessOptions

\RequirePackage[unknownkeysallowed]{keyval}[1997/11/10]
\def\Str@Warning#1{\ClassWarning{st-thesis}{#1}}
\def\Str@WarningNoLine#1{\ClassWarningNoLine{st-thesis}{#1}}
\def\Str@Info#1{\ClassInfo{st-thesis}{#1}}
\def\Str@WarnOptionValue#1#2#3{%
  \Str@Warning{%
    Unexpected value `#1'\MessageBreak
    of option `#2' instead of\MessageBreak
    #3%
  }
}

\def\Str@true{true}
\def\Str@false{false}

\def\Str@boolkey{\@dblarg\Str@@boolkey}
\def\Str@@boolkey[#1]#2#3{%
  \lowercase{\def\Str@tempa{#3}}%
  \ifx\Str@tempa\@empty
    \let\Str@tempa\Str@true
  \fi
  \ifx\Str@tempa\Str@true
  \else
    \ifx\Str@tempa\Str@false
    \else
      \let\Str@tempa\relax
    \fi
  \fi
  \ifx\Str@tempa\relax
    \Str@WarnOptionValue{#3}{#1}{`true' or 'false'}%
  \else
    \Str@Info{Option `#1' set `\Str@tempa'}%
    \csname Str@#2\Str@tempa\endcsname
  \fi
}

\def\Str@fontsize#1{%
  \def\Str@normalfontsize{#1}
}

%% Ouch! this is ugly, but there's no other way...
\def\Str@afourpaper{a4paper}
\def\Str@bfivepaper{b5paper}
\let\Str@papersize\undefined

\newif\ifStr@parskip
%\Str@parskipfalse
\define@key{Str}{parskip}[true]{%
  \Str@boolkey{parskip}{#1}}

\newif\ifStr@marginnotes
%\Str@marginnotesfalse
\define@key{Str}{marginnotes}[true]{%
  \Str@boolkey{marginnotes}{#1}}

\newif\ifStr@sffont
%\Str@sffontfalse
\define@key{Str}{sffont}[true]{%
  \Str@boolkey{sffont}{#1}}

\newif\ifStr@twoside
%\Str@twosidefalse
\define@key{Str}{twoside}[true]{%
  \Str@boolkey{twoside}{#1}}

\define@key{Str}{b5paper}[true]{\let\Str@papersize\Str@bfivepaper}

\define@key{Str}{a4paper}[true]{\let\Str@papersize\Str@afourpaper}

\newif\ifStr@widepage
%\Str@widepagefalse
\define@key{Str}{widepage}[true]{%
  \Str@boolkey{widepage}{#1}}

\newif\ifStr@fancyhdr
%\Str@fancyhdrfalse
\define@key{Str}{fancyhdr}[true]{%
  \Str@boolkey{fancyhdr}{#1}}

\newif\ifStr@hyperref
%\Str@hyperreffalse
\define@key{Str}{hyperref}[true]{%
  \Str@boolkey{hyperref}{#1}}

\newif\ifStr@openright
%\Str@openrightfalse
\define@key{Str}{openright}[true]{%
  \Str@boolkey{openright}{#1}}

\newif\ifStr@color
\define@key{Str}{color}[true]{%
  \Str@boolkey{color}{#1}}

\define@key{Str}{10pt}[]{%
  \Str@fontsize{10pt}}

\define@key{Str}{11pt}[]{%
  \Str@fontsize{11pt}}

\define@key{Str}{12pt}[]{%
  \Str@fontsize{12pt}}

\define@key{Str}{reportopt}[]{
  \PassOptionsToClass{#1}{report}}

\setkeys{Str}{sffont,color,b5paper,twoside,widepage,fancyhdr,parskip,10pt,openright,hyperref}

%%%%%%%%
% Alternative ProcessOptions
\def\Str@setkey{\setkeys{Str}}
\let\Str@origExecuteOptions\ExecuteOptions
\let\ExecuteOptions\Str@setkey
\let\ExecuteOptions\Str@origExecuteOptions
\relax
\def\ProcessOptionsWithKV#1 { %
  \let\@tempa\@empty
  \@for\CurrentOption:=\@classoptionslist\do { %
    \@ifundefined{KV@#1@\CurrentOption}{\Str@Warning{undefined: \CurrentOption}} { %
      \edef\@tempa{\@tempa,\CurrentOption,}}}
%      \typeout{uiteindelijke setkeys: \@tempa\@ptionlist{\@currname.\@currext}}
  \edef\@tempa { %
    \noexpand\setkeys{#1}{\@tempa\@ptionlist{\@currname.\@currext}}} %
    
  \@tempa
  \let\@classoptionslist\relax
  \AtEndOfClass{\let\@unprocessedoptions\relax}}

\ProcessOptionsWithKV{Str}

\ifStr@sffont
  \renewcommand{\familydefault}{cmss}
\fi %%@sffont

%%%% Class Body %%%%

%% First determine which options have to be passed along:
\@ifundefined{Str@normalfontsize}{}{%
  \PassOptionsToClass{\Str@normalfontsize}{report}
}

%\ifStr@bfivepaper
\ifx\Str@papersize\undefined\relax\else
  \PassOptionsToClass{\Str@papersize}{report}
\fi

\ifStr@twoside
  \PassOptionsToClass{twoside}{report}
\fi
\ifStr@openright
  \PassOptionsToClass{openright}{report}
\fi

%% This all is in fact an extension of the `report` class
\LoadClass[]{report}
\typeout{---------------------------}
\RequirePackage{geometry}
\RequirePackage{calc}
\RequirePackage{ifthen}

\ifStr@fancyhdr
  \Str@Info{Using fancy headers/footers by fancyhdr.}
  \RequirePackage{fancyhdr}
\fi %%@fancyhdr

\ifStr@parskip
  \RequirePackage{parskip}
\fi %%@parskip

\ifStr@widepage
  \geometry{hmargin=0.85in, vmargin=0.9in, headsep=20pt}
\fi

\ifStr@twoside
  \geometry{twoside}
\fi %%@twoside

\ifStr@hyperref
  \ifStr@color
  \RequirePackage{color}
  \definecolor{darkblue}{rgb}{0,0,0.7}
  \definecolor{darkgreen}{rgb}{0,0.7,0}
  \definecolor{darkred}{rgb}{0.7,0,0}
  \ifpdf
  \PassOptionsToPackage{pdftex,plainpages=false,pdfpagelabels,bookmarksopen}{hyperref}
  \else
  \PassOptionsToPackage{dvips}{hyperref}
  \fi
  \RequirePackage[linkcolor=darkblue,pagecolor=darkblue,menucolor=darkblue,%
                  citecolor=darkred,urlcolor=blue,filecolor=blue,%
                  linktocpage,colorlinks]{hyperref}
  \else
  \RequirePackage[linkcolor=black,pagecolor=black,menucolor=black,%
                  citecolor=black,urlcolor=black,filecolor=black,%
                  linktocpage,colorlinks=false]{hyperref}
  \fi
\fi
%%% General.
\newcommand{\clearplaindoublepage}{\newpage{\pagestyle{plain}\cleardoublepage}}
\newcommand{\clearemptydoublepage}{\newpage{\pagestyle{empty}\cleardoublepage}}



%%% Some redefinitions of chapter and page-markup.
%%% In case of b5paper (instead of a4paper) some size redefs are performed later.
  \renewcommand\part{%
    \if@openright
      \clearemptydoublepage
    \else
      \clearpage
    \fi
    \thispagestyle{empty}%
    \if@twocolumn
      \onecolumn
      \@tempswatrue
    \else
      \@tempswafalse
    \fi
    \null\vfil
    \secdef\@part\@spart}

  \def\@endpart{\vfil\newpage
                \if@twoside
                 \if@openright
                  \null
                  \thispagestyle{empty}%
                  \newpage
                 \fi
                \fi
                \if@tempswa
                  \twocolumn
                \fi}

  \renewcommand\chapter{\if@openright\clearemptydoublepage\else\clearpage\fi
                      \thispagestyle{plain}%
                      \global\@topnum\z@
                      \@afterindentfalse
                      \secdef\@chapter\@schapter}

\newcommand{\chapnonr}[2]{%
\chapter*{#2}
\phantomsection
\addcontentsline{toc}{chapter}{#1}
}

\newcommand{\preface}{%
\chapnonr{Preface}{Preface}
}

\newcommand{\introduction}{%
\chapnonr{Introduction}{Introduction}
}

%% Lightweight variants of the \*matter commands from book class
\newcommand\frontmatter{\pagenumbering{roman}}
\newcommand\mainmatter{\pagenumbering{arabic}}

\renewcommand\tableofcontents{%
    \if@twocolumn
      \@restonecoltrue\onecolumn
    \else
      \@restonecolfalse
    \fi
    \chapnonr{\contentsname}{\contentsname
        \@mkboth{%
           \MakeUppercase\contentsname}{\MakeUppercase\contentsname}}%
    \@starttoc{toc}%
    \if@restonecol\twocolumn\fi
    }

\renewenvironment{thebibliography}[1]
     {\chapnonr{\bibname}{\bibname\@mkboth{\MakeUppercase\bibname}{\MakeUppercase\bibname}}%
      \list{\@biblabel{\@arabic\c@enumiv}}%
           {\settowidth\labelwidth{\@biblabel{#1}}%
            \leftmargin\labelwidth
            \advance\leftmargin\labelsep
            \@openbib@code
            \usecounter{enumiv}%
            \let\p@enumiv\@empty
            \renewcommand\theenumiv{\@arabic\c@enumiv}}%
      \sloppy
      \clubpenalty4000
      \@clubpenalty \clubpenalty
      \widowpenalty4000%
      \sfcode`\.\@m}
     {\def\@noitemerr
       {\@latex@warning{Empty `thebibliography' environment}}%
      \endlist}

\renewenvironment{theindex}
               {\chapnonr{\indexname}{\indexname\@mkboth{\MakeUppercase\indexname}{\MakeUppercase\indexname}}
               \if@twocolumn
                  \@restonecolfalse
                \else
                  \@restonecoltrue
                \fi
                \columnseprule \z@
                \columnsep 35\p@
                \twocolumn[\@makeschapterhead{\indexname}]%
                \@mkboth{\MakeUppercase\indexname}%
                        {\MakeUppercase\indexname}%
                \thispagestyle{plain}\parindent\z@
                \parskip\z@ \@plus .3\p@\relax
                \let\item\@idxitem}
               {\if@restonecol\onecolumn\else\clearpage\fi}

%%% If papersize is B5, change page-layout *and* top-/bottomskips for headings
\ifx\Str@papersize\Str@bfivepaper
  \geometry{b5paper}
%% part markup

  \def\@part[#1]#2{%
      \ifnum \c@secnumdepth >-2\relax
        \refstepcounter{part}%
        \addcontentsline{toc}{part}{\thepart\hspace{1em}#1}%
      \else
        \addcontentsline{toc}{part}{#1}%
      \fi
      \markboth{}{}%
      {\centering
       \interlinepenalty \@M
       \normalfont
       \ifnum \c@secnumdepth >-2\relax
         \huge\bfseries \partname\nobreakspace\thepart
         \par
         \vskip 20\p@
       \fi
       \Huge \bfseries #2\par}%
      \@endpart}
  \def\@spart#1{%
      {\centering
       \interlinepenalty \@M
       \normalfont
       \Huge \bfseries #1\par}%
      \@endpart}

%% chapter markup
  % See above for \renewcommand{\chapter}
  \def\@chapter[#1]#2{\ifnum \c@secnumdepth >\m@ne
                           \refstepcounter{chapter}%
                           \typeout{\@chapapp\space\thechapter.}%
                           \addcontentsline{toc}{chapter}%
                                     {\protect\numberline{\thechapter}#1}%
                      \else
                        \addcontentsline{toc}{chapter}{#1}%
                      \fi
                      \chaptermark{#1}%
                      \addtocontents{lof}{\protect\addvspace{10\p@}}%
                      \addtocontents{lot}{\protect\addvspace{10\p@}}%
                      \if@twocolumn
                        \@topnewpage[\@makechapterhead{#2}]%
                      \else
                        \@makechapterhead{#2}%
                        \@afterheading
                      \fi}
  \def\@makechapterhead#1{%
    \vspace*{50\p@}%
    {\parindent \z@ \raggedright \normalfont
      \ifnum \c@secnumdepth >\m@ne
%        \huge\bfseries \@chapapp\space \thechapter
          \Large\bfseries \@chapapp\space \thechapter
          \par\nobreak
          \vskip 20\p@
      \fi
      \interlinepenalty\@M
%      \Huge \bfseries #1\par\nobreak
      \huge \bfseries #1\par\nobreak
%      \vskip 40\p@
      \vskip 30\p@
    }}
  \def\@schapter#1{\if@twocolumn
                     \@topnewpage[\@makeschapterhead{#1}]%
                   \else
                     \@makeschapterhead{#1}%
                     \@afterheading
                   \fi}
  \def\@makeschapterhead#1{%
    \vspace*{50\p@}%
    {\parindent \z@ \raggedright
      \normalfont
      \interlinepenalty\@M
%    \Huge \bfseries  #1\par\nobreak
      \huge \bfseries  #1\par\nobreak
      \vskip 40\p@
    }}

%% section markup and sub-variants
  \renewcommand\section{\@startsection {section}{1}{\z@}%
%                                       {-3.5ex \@plus -1ex \@minus -.2ex}%
%                                       {2.3ex \@plus.2ex}%
%                                   {\normalfont\Large\bfseries}}
                                       {-2.4ex \@plus -1ex \@minus -.2ex}%
                                       {1.5ex \@plus.2ex}%
                                       {\normalfont\large\bfseries}}
  \renewcommand\subsection{\@startsection{subsection}{2}{\z@}%
%                                         {-3.25ex\@plus -1ex \@minus -.2ex}%
%                                         {1.5ex \@plus .2ex}%
%                                   {\normalfont\large\bfseries}}
                                         {-2.2ex\@plus -1ex \@minus -.2ex}%
                                         {1ex \@plus .2ex}%
                                         {\normalfont\normalsize\bfseries}}
  \renewcommand\subsubsection{\@startsection{subsubsection}{3}{\z@}%
%                                         {-3.25ex\@plus -1ex \@minus -.2ex}%
%                                         {1.5ex \@plus .2ex}%
%                                   {\normalfont\normalsize\bfseries}}
                                         {-2.2ex\@plus -1ex \@minus -.2ex}%
                                         {1ex \@plus .2ex}%
                                         {\normalfont\normalsize\bfseries}}
  \renewcommand\paragraph{\@startsection{paragraph}{4}{\z@}%
%                                        {3.25ex \@plus1ex \@minus.2ex}%
%                                        {-1em}%
%                                   {\normalfont\normalsize\bfseries}}
                                        {2.2ex \@plus1ex \@minus.2ex}%
                                        {-1em}%
                                        {\normalfont\normalsize\bfseries}}
  \renewcommand\subparagraph{\@startsection{subparagraph}{5}{\parindent}%
%                                           {3.25ex \@plus1ex \@minus .2ex}%
%                                           {-1em}%
%                                   {\normalfont\normalsize\bfseries}}
                                           {2.2ex \@plus1ex \@minus .2ex}%
                                           {-1em}%
                                           {\normalfont\normalsize\mdseries}}
\fi %%@b5paper


\ifStr@marginnotes
  \geometry{includemp=true}
  \ifStr@fancyhdr
    \setlength{\headwidth}{\textwidth}
    \addtolength{\headwidth}{\marginparsep}
    \addtolength{\headwidth}{\marginparwidth}
  \fi
\else
  \geometry{includemp=false}
  \ifStr@fancyhdr
    \setlength{\headwidth}{\textwidth}
  \fi
\fi %%@marginnotes

%% Fancy headings
\ifStr@fancyhdr
  \AtBeginDocument{\pagestyle{fancy}}

  \pagestyle{fancy}

  \newcommand{\@stylehf}[1]{\normalfont\small#1}


%% Creative use of left and rightmarks:
%% \leftmark contains chapter title
%% \rightmark contains chapter title (if available)
%% The marks are *not* placed left or right according to their name:
%% ___________________________________________
%% | sec.no.  sec.title | sec.title  sec.no. |
%% |--------------------|--------------------|
%%    ...even page...       ...odd page...
%% |--------------------|--------------------|
%% | page.no.  ch.title | ch.title  page.no. |
%% |____________________|____________________|

  \renewcommand{\chaptermark}[1]{\markboth{\@stylehf{#1}}{}}
  \renewcommand{\sectionmark}[1]{\markright{\@stylehf{#1}}}

  \newcommand{\@itenosec}[2]{%
    \ifthenelse{\equal{\leftmark}{\rightmark} \or \equal{\rightmark}{}}{#1}{#2}%
  }

  \fancyhf{}

  \renewcommand{\headrulewidth}{0.4pt}
  \renewcommand{\footrulewidth}{0.4pt}

  \fancyhead[LE,RO]{\@itenosec{}{\@stylehf{\thesection}}}
  \fancyhead[RE,LO]{\@itenosec{}{\nouppercase{\rightmark}}}
  \fancyfoot[LE,RO]{\@stylehf{\thepage}}
  \fancyfoot[RE,LO]{\nouppercase{\leftmark}}

  \fancypagestyle{plain}{
    \fancyhf{}

    \renewcommand{\headrulewidth}{0pt}
    \renewcommand{\footrulewidth}{0.4pt}

    \fancyfoot[LE,RO]{\@stylehf{\thepage}}
  }
\fi %%@fancyhdr

%% End of class specification
%%
