\documentclass[handout,xcolor=dvipsnames,sans,mathserif]{beamer}

\usepackage[english]{babel}
\usepackage{mathpazo}

%include polycode.fmt

\setbeamertemplate{navigation symbols}{}
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

\title{Running Haskell on the CLR}
\subtitle{``but does it run on Windows?''}
\author{Jeroen Leeuwestein, Tom Lokhorst\\\small\texttt{jleeuwes@@cs.uu.nl, tom@@lokhorst.eu}} 
\date{\today} 

\frame{\titlepage} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Don't get your hopes up!}

\pause

%format module  = "\stress{\textbf{module}}"
%format where   = "\stress{\textbf{where}}"
%format foreign = "\stress{\textbf{foreign}}"
%format import  = "\stress{\textbf{import}}"
%format data    = "\stress{\textbf{data}}"
\vspace*{1cm}

> module Main where
  
> foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
  
> inc :: Int -> Int
> inc x = x + 1
  
> data List = Nil | Cons Int List
  
> length :: List -> Int
> length Nil         = 0
> length (Cons x xs) = inc (length xs)
  
> five :: List
> five = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil))))
  
> main = length five

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{Why target the CLR?}

  \only<2|| handout:1>
  {
    A lot of presence.
    \begin{itemize}
      \item \stress{Multiple versions} of Windows desktops.
      \item \stress{OS X} and \stress{Linux} desktops, through Mono.
      \item Web browsers, through \stress{Silverlight} and \stress{Moonlight}.
      \item Mobile devices:
        \begin{itemize}
          \item Windows \stress{Mobile}.
          \item Mono on the \stress{iPhone} and \stress{Android}.
        \end{itemize}
      \item In the cloud!
        \begin{itemize}
          \item Windows \stress{Azure}: Distributed computation environment.
        \end{itemize}
    \end{itemize}
  }

  \only<3|| handout:2>
  {
    Rich environment.
    \begin{itemize}
      \item \stress{Interop} with other languages.
      \item Access a huge set of \stress{libraries}.
      \item Provide libraries developed in \stress{Haskell}.
    \end{itemize}
  }

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

  \frametitle{What is the CLR?}

  \only<1|| handout:1>
  {
  Common Language Runtime / Mono Project

  \begin{itemize}
    \item \stress{Stack-based} virtual machine.
    \item First-class support for \stress{classes} with \stress{methods}.
    \item Basic operations for \stress{reference types} and \stress{value types}.
    \item \stress{Type safe}: operations must match the exact type.
    \item Dynamic \stress{casting} is allowed.
    \item Executes \stress{Common Intermediate Language} (CIL).
    \item CIL has a concrete syntax.
      \begin{itemize}
        \item ilasm
        \item ildasm / monodis
      \end{itemize}
  \end{itemize}
  }
  \only<2|| handout:2>
  {

%format pclass  = "\stress{\textbf{.class}}"
%format method  = "\stress{\textbf{.method}}"
%format entrypoint  = "\stress{\textbf{.entrypoint}}"
%format locals  = "\stress{\textbf{.locals}}"
%format init  = "\stress{\textbf{init}}"
\vspace*{1cm}
> pclass private Test extends [mscorlib]System.Object
>  {
>    method private static void Main () cil managed 
>    {
>	entrypoint
>	locals init (int32  x)
>	ldc_i4 2 
>	stloc 0 
>	ldc_i4 3 
>	ldloc 0 
>	add 
>	call void class [mscorlib]System.Console::WriteLine(int32)
>	ret 
>    }
> }

  }


\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Architecture of .NET backend}

\$ bin/8/ehc \stress{-ccil} Test.hs\\
\pause
\$ ls\\
Test.hs   \stress{Test.il}\\
\pause
\$ \stress{ilasm} Test.il\\
\pause
\$ ls\\
\stress{Test.exe}  Test.hs   Test.il\\
\pause
\$ \stress{mono} Test.exe\\
42

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Architecture of .NET backend}

\includegraphics[scale=0.50]{ehc-dataflow2.png}


\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Architecture of .NET backend}

Haskell package \stress{language-cil}.

Abstract syntax for the Common Intermediate Language.

With build functions and pretty printer for concrete syntax.

\pause

Future:
\begin{itemize}
  \item Support all CIL constructs
  \item Parser for concrete syntax
  \item Analysis functions
  \item Release on Hackage
\end{itemize}

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Philosophy on the Runtime System}

How to treat the RTS?
\begin{itemize}
  \item As an \stress{abstract machine}?
    \uncover<2->
    {
    \begin{itemize}
      \item simulate virtual memory
      \item simulate registers
      \item simulate functions and function calls
    \end{itemize}
    }
  \item Use it for what it was \stress{designed}
    \uncover<3->
    {
    \begin{itemize}
      \item build strongly typed objects
      \item use inheritance
      \item use method calling conventions
      \item interop with other languages
    \end{itemize}
    }
\end{itemize}

\uncover<4->
{
Look at the what other languages do (\stress{F\#}).
}

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}

\frametitle{Philosophy on the Runtime System}
\framesubtitle{Some questions}

%format data  = "\stress{\textbf{data}}"

> data List = Nil | Cons Int List

\begin{minipage}[c][5cm][c]{\textwidth}
\only<2|| handout:1>
{
What is the \stress{type} of List?
\\\\
What are the \stress{types} of Nil and Cons?
\\\\
How do we handle do \stress{thunks} and \stress{partial applications}?
\\\\
And what about \stress{updates}?
}
\only<3|| handout:2>
{
\includegraphics[scale=0.50]{list_diagram1.png}
}
\only<4|| handout:3>
{
> Cons 1 (xs `append` ys)
}
\only<5|| handout:4>
{
\includegraphics[scale=0.50]{list_diagram2.png}
}
\end{minipage}

\end{frame} 

% -----------------------------------------------------------------------------

\begin{frame}
\begin{center}
\Huge{xs = |[1,2]|}
\end{center}
\end{frame}

\begin{frame}
\includegraphics[scale=0.40]{listof12.png}
\end{frame}

% -----------------------------------------------------------------------------


\begin{frame}
\frametitle{Code generation}

\begin{itemize}
  \item Generate code from GRIN
  \item Direct translation of GRIN constructs

% >global_x_3_10_0_25 <- store (Ffun_x_15_10_0_25)
% >global_x_5_15_23_0 <- store (CInt 1)
% >
% >fun_x_15_10_0_25 = unit ()
% >
% >main  
% >  = fetchfield global_x_3_10_0_25 0 ; \x ->
% >    case x of
% >     CJust
% >       -> fetchfield global_x_3_10_0_25 1 ; \y ->
% >          unit (CJust y)
% >     Ffun_x_15_10_0_25
% >       -> updateunit (CJust global_x_5_15_23_0) global_x_3_10_0_25

\end{itemize}

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Sequence}

%format (stress (e)) = "\stress{" e "}"

Evaluate \stress{|expr|} and bind the result to \stress{|x|}.

\only<1>{
>expr ; \x -> ... length x ...
}
\only<2|| handout:0>{
>stress expr ; \x -> ... length x ...
}
\only<3|| handout:0>{
>expr ; stress (\x ->) ... length x ...
}
\only<4|| handout:0>{
>expr ; \x -> stress (... length x ...)
}

\pause
expr\\
\pause
STLOC x\\
\pause
...\\
LDLOC x\\
CALL length(object)\\
...\\

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Case}

Match a \stress{tag} variable against different alternatives.

\only<1>{
>case tag of
>  CNil  -> ...
>  CCons -> ...
}
\only<2|| handout:0>{
>stress(case tag) of
>  CNil  -> ...
>  CCons -> ...
}
\only<3|| handout:0>{
>case tag of
>  stress(CNil)  -> stress(...)
>  CCons -> ...
}

\pause
tag\\
\pause
L1:\\
~~DUP\\
~~ISINST CNil\\
~~BRFALSE L2\\
~~POP\\
~~...\\
L2:\\

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Store}

Store a \stress{value} on the heap and return a \stress{pointer} to it.

>store val

\pause
val\\
NEWOBJ RefObj::.ctor(object)\\

\pause

All our values are already stored on the heap, so we only have to create a pointer.

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Update}

Update the value pointed to by \stress{pointer |x|} with \stress{|val|}.

>update x val

LDLOC x\\
val\\
STFLD RefObj::Value\\

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Fetch 0}

Fetch the \stress{tag} of a node, following \stress{pointer |x|}.

>fetch x [0]

\pause

LDLOC x\\
LDFLD RefObj::Value\\

\medskip
\pause
We have no representation for \stress{stand-alone tags}. We use the \stress{complete node}.

\end{frame}
\begin{frame}
\frametitle{Code generation}
\framesubtitle{Fetch n}

Fetch the \stress{first field} of a node, following \stress{pointer |x|}.

>fetch x [1]

\pause
LDLOC x\\   % or global, or parameter
LDFLD RefObj::Value\\
\only<2>{LDFLD Int/Int::Value\\}
\only<3>{LDFLD \stress{Int/Int}::Value\\}

\medskip
\pause
Uh oh! We have to know the \stress{class}.

\end{frame}



\begin{frame}
\frametitle{Code generation}
\framesubtitle{Fetch n -- Class information}

Fortunately, GRIN stores this information for us:

>GrExpr_FetchField x 1 (Just (GrTag_Con {1,1} 0 Int))

Phew.

\end{frame}


\begin{frame}
\frametitle{Code generation}
\framesubtitle{Binding multiple variables}

However:

>... ; \x ->
>inc x ; \((stress (y z))) ->
>...

\pause
\begin{itemize}
\item We have to extract the first field to bind to |z|.
\pause
\item We need the \stress{class} information for this.
      
      \only<3>{LDFLD ?/?::Value\\}
      \only<4-|| handout:0>{LDFLD ?/?::?\\}

\pause\pause
\item But we don't know what |y| is!
\end{itemize}

\end{frame}


% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{Code generation}
\framesubtitle{Types!}

We need the possible \stress{tags} of every \stress{variable}, so we can figure out which \stress{class} to use.

Basically type (tag) inferencing. A lot of work!

\pause

Fortunately, the \stress{heap points-to analysis} does this already.

\end{frame}


\begin{frame}
\frametitle{Heap points-to analysis}

The analysis gives us, for each \stress{variable}, what kind of \stress{values} it can contain.

Example:

>fetch T 1      ; x ->
>inc x          ; \(y z) ->
>update T (y z)

|T| is a thunk here.

\end{frame}


\begin{frame}
\frametitle{Heap points-to analysis}

>fetch T 1      ; x ->
>inc x          ; \(y z) ->
>update T (y z)

Variables:

\begin{tabular}{lll}
T   & Pointer & [13,14] \\
inc & Node    & [(CInt, [Basic])] \\
x   & Pointer & [13,14] \\
y   & Tag     & CInt \\
z   & Basic \\
\end{tabular}

Heap:

\begin{tabular}{lll}
13 & Node & [(CInt, [Basic])] \\
14 & Node & [(CInt, [Basic]),(Finc, [Pointer [13,14]])] \\
\end{tabular}

\end{frame}



\begin{frame}
\frametitle{Future work}
\framesubtitle{Obvious enhancements}

\begin{itemize}
  \item stloc x, ldloc x
  \item more stack focussed code
    \begin{itemize}
      \item Silly-like
      \item tail calls!
    \end{itemize}
  \item remove RefObj indirection
  \item use value types
  \item more polymorphic code
    \begin{itemize}
      \item inline unboxed values
    \end{itemize}
\end{itemize}

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{Future work}
\framesubtitle{More `out there' stuff}

Simon Peyton Jones on Haskell for CLR:
\begin{itemize}
  \item Generate IL
    \begin{itemize}
      \item Runtime representation for \stress{thunks}
    \end{itemize}
  \item \stress{Interop} with .NET libraries
    \begin{itemize}
      \item No \stress{foreign import ...} for everything
    \end{itemize}
  \item Other GHC primitives:
    \begin{itemize}
      \item the I/O monad
      \item arbitrary precision arithmetic
      \item concurrency
      \item exceptions
      \item finalisers
      \item stable pointers
      \item Software transactional memory 
    \end{itemize}
  \item Existing libraries
\end{itemize}

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\frametitle{In conclusion}

We think our \stress{runtime representation} is workable.

We have an interesting \stress{prototype} that shows this.

There's much work still to be done...

\end{frame}

% -----------------------------------------------------------------------------

\begin{frame}
\begin{center}
\Huge{EOF}
\end{center}
\end{frame}

% -----------------------------------------------------------------------------

\end{document}

