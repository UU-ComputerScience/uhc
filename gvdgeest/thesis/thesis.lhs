\documentclass[sffont=false,color=true,a4paper,openright]{st-thesis}

\usepackage{epsfig}
\usepackage{helperf}
\usepackage{natbib}
\usepackage[english]{babel}
\usepackage{amsmath, amsthm, amssymb}


%include lhs2TeX.fmt
%include lhs2TeX.sty

%\setlength{\blanklineskip}{\parskip}
\newtheoremstyle{grt}% name
  {10pt} % space above
  {10pt} % space below
  {\rm} % bofy font
  {\parindent} % ident - empty=no indent, \parindent= paragraph indent
  {\bf} % thm head font
  {. } % punctuation after thm head
  { } % space after thm head: `` ``=normal \newline=linebreak
  {} % thm head specification

\theoremstyle{grt}
\newtheorem{df}{Definition}[chapter]

\newenvironment{myitemize}{\begin{itemize} \setlength{\parskip}{0pt}}{\end{itemize}}

\newenvironment{Figure}[2]{%
  \def\FCap{#1}%
  \def\FLab{#2}%
  \begin{figure}%
  \rule{\textwidth}{0.2pt}%
}{%
  \rule{\textwidth}{0.2pt}%
  \caption{\FCap} \label{\FLab}%
  \end{figure}%
}

%format . = "."
%format forall = "\forall"
%format forallword = "forall"
%format exists = "\exists"

%format s1 = "s_1"
%format s4 = "s_4"
%format sn = "s_n"
%format t1 = "t_1"
%format tn = "t_n"
%format ti = "t_i"
%format c0= "c_0"
%format c1= "c_1"
%format c2= "c_2"
%format c3= "c_3"
%format c4= "c_4"
%format c5= "c_5"
%format v0= "v_0"
%format v1= "v_1"
%format v2= "v_2"
%format v3= "v_3"
%format v4= "v_4"
%format v5= "v_5"
%format v6= "v_6"
%format v7= "v_7"
%format v8= "v_8"
%format v10= "v_{10}"
%format v11= "v_{11}"
%format vn= "v_n"
%format vbar= "\bar{v}"
%format dbar= "\bar{d}"
%format abar= "\bar{a}"
%format xbar= "\bar{x}"
%format ybar= "\bar{y}"
%format zbar= "\bar{z}"
%format over = "\mathbf{over}"
%format inst = "\mathbf{inst}"
%format never = "\mathbf{never}"
%format close = "\mathbf{close}"
%format disjoint = "\mathbf{disjoint}"
%format alpha = "\alpha"
%format alphan = "\alpha_n"
%format alpha1 = "\alpha_1"
%format beta  = "\beta"
%format tau   = "\tau"
%format tau1  = "\tau_1"
%format tau2  = "\tau_2"
%format taun  = "\tau_n"
%format taui  = "\tau_i"
%format taubar = "\overline{\tau}"
%format pi   = "\pi"
%format pig   = "\pi_g"
%format pi1   = "\pi_1"
%format pi2   = "\pi_2"
%format pii   = "\pi_i"
%format pin   = "\pi_n"
%format pibar   = "\bar{\pi}"
%format PiAssume = "\Pi_{assume}"
%format PiProve  = "\Pi_{prove}"
%format PiGen    = "\Pi_{gen}"
%format Pi1    = "\Pi_{1}"
%format Pi2    = "\Pi_{2}"
%format Pi       = "\Pi"
%format PiTheta  = "\Pi_{\Theta}"
%format gamma = "\gamma"
%format Gamma = "\Gamma"
%format rho = "\rho"
%format rho1 = "\rho_1"
%format varsigma = "\varsigma"
%format sigma = "\sigma"
%format Sigma = "\Sigma"
%format sigmak = "\sigma^k"
%format sigmav = "\sigma_v"
%format sigmavbar = "\overline{\sigma_v}"
%format sigma1 = "\sigma_1"
%format sigma2 = "\sigma_2"
%format sigman = "\sigma_n"
%format sigmabar = "\overline{\sigma}"
%format sigmabar' = "\overline{\sigma}''"
%format varpi  = "\varpi"
%format varpi1  = "\varpi_1"
%format varpi2  = "\varpi_2"
%format varpi3  = "\varpi_3"
%format varpi4  = "\varpi_4"
%format varpi5  = "\varpi_5"
%format varpi6  = "\varpi_6"
%format <=>  = "\Longleftrightarrow "
%format  ==>  = "\Longrightarrow "
%format G1    = "G_1"
%format Gj    = "G_j"
%format H1    = "H_1"
%format Hi    = "H_i"
%format B1    = "B_1"
%format Bk    = "B_k"
%format intersect = "\cap"
%format union = "\cup"
%format mapsto = "\mapsto"
%format bar = "|"
%format emptyset = "\emptyset"
%format bindbar = "\overline{bind}"
%format vartheta = "\vartheta"
%format Theta = "\Theta"
%format ThetaQ = "\Theta(\pi)"
%format ThetaR = "\Theta(r)"
%format insign  = "\in"
%format notinsign  = "\notin"
%format ^^  = "\ "
%format CalM = "\cal{M}"
%format CalC = "\cal{C}"
%format CalA = "{\cal{A}}"
%format CalR = "\cal{R}"
%format CalQ = "q"
%format infty = "\infty"
%format (Sup (y) x) = y "^{" x "}"
%format (Sub (y) x) = y "_{" x "}"
%format (Sub2 (y) (x)) = y "_{" x "}"
%format supseteq = "\supseteq"
%format entails = "\Vdash_{e}"
%format entailsA = "\Vdash_{e}^{\cal{A}}"
%format solves  = "\vdash_{s}"
%format isdef  = "=_{\textit{def}}"
%format <->    = "\leftrightarrow"
%format deriv(x) = "\longrightarrow_{" x "}"
%format |> = "\triangleright"
%format not = "not"
%format (satisf x y) = "\lfloor" x "\rfloor_{" y "}"

%  ~\\
%  ~\\
%  ~\\
%  ~\\
%  ~\\

\begin{document}
% % % % % % % % % % % % %
%     Front Matter      %
% % % % % % % % % % % % %
\frontmatter

% -- Title page
\begin{titlepage}
\begin{center}
~\\
~\\
~\\
\large{Thesis for the degree of Master of Science}\\
~\\
~\\
~\\
\Huge{Constraints for Type Class Extensions}\\
\large
~\\
~\\
~\\
\LARGE{Gerrit van den Geest}\\
\large
~\\
~\\
~\\
Supervisor: prof.~dr.~S.\,D.~Swierstra\\
Daily supervisors: dr.~B.\,J.~Heeren and dr.~A.~Dijkstra\\
Utrecht, February 2007\\
INF/SCR-06-36\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
~\\
Department of Information and Computing Sciences \\
Universiteit Utrecht\\
P.O. Box 80.089\\
3508 TB Utrecht\\
The Netherlands\\
~\\
\includegraphics{graphs/uu.pdf}
\end{center}
\end{titlepage}


%\maketitle
%\clearemptydoublepage  % voor lege pagina

% -- Abstract
%include 00-abstract.lhs

% -- Table of Contents
\tableofcontents

% \listoffigures

% \listoflistings

% % % % % % % % % % % % %
%     Main Matter       %
% % % % % % % % % % % % %

\clearemptydoublepage  % voor lege pagina
\mainmatter

% -- Contents
%include 01-introduction.lhs
%include 02-preliminaries.lhs
%include 03-literature.lhs
%include 05-helium.lhs
%include 06-ehc.lhs
%include 07-constraints.lhs
%include 08-evidence.lhs
%include 09-localinstances.lhs
%include 10-improvement.lhs
%include 11-conclusion.lhs

% % % % % % % % % % % % %
%      End Matter       %
% % % % % % % % % % % % %

% -- Bibliography
\bibliography{references}  %{alpha}
\bibliographystyle{plainnat}  %{plain} %{papers}

% -- Appendixes
%  \input{00-appendix.tex}

\end{document}
