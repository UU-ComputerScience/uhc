\documentclass{beamer}

%include lhs2TeX.fmt
%include lhs2TeX.sty
%include polycode.fmt

%format . = "."
%format forall = "\forall"
%format forallword = "forall"
%format exists = "\exists"

%format s1 = "s_1"
%format s4 = "s_4"
%format sn = "s_n"
%format t1 = "t_1"
%format tn = "t_n"
%format c0= "c_0"
%format c1= "c_1"
%format c2= "c_2"
%format c3= "c_3"
%format c4= "c_4"
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

%format (red (x)) = "\textcolor{red}{" x "}"
%format emptyline = "~"

\blanklineskip=2mm
\mathindent=3mm

\newcommand{\comment}[1]{}

\usepackage[english]{babel}
\usepackage{graphicx}
\usepackage{stmaryrd}
\usepackage{textpos}
\usepackage{color}

\setbeamertemplate{blocks}[rounded][shadow=true]
\beamerboxesdeclarecolorscheme{goodpopup}{red}{uuyellow} %brown!40!yellow}
\beamerboxesdeclarecolorscheme{normal}{yellow!50!black}{white!90!yellow}

\usetheme[style=fancy]{uu}
%\usetheme[]{plain}
%\usetheme[style=fancy,sidebar=false,showpagenr=false, includehead=false]{uu}
%\usetheme{Berlin}

\title{Constraints for Type Class Extensions}
\subtitle{Master's Thesis Defense}
\author{Gerrit van den Geest}
\institute[Center for Software Technology]{
  Supervisor: prof.~dr.~S.\,D.~Swierstra\\
  Daily supervisors: dr.~B.\,J.~Heeren and dr.~A.~Dijkstra\\
  ~\\
  ~\\
  Center for Software Technology\\
  Universiteit Utrecht}
\date{February 22, 2007}

\begin{document}

\begin{frame}
  \titlepage
%\includegraphics[scale=0.20]{WithClass.png}
\end{frame}

%\begin{frame}\frametitle{Overview}
%  \setcounter{tocdepth}{1}
%  \tableofcontents
%\end{frame}

% Who is my primary audience?
%  ST-Staff, ST-master students (so knowledge of type classes and some extensions)
%
% What should the audience remember from my talk?
%  How easy one can formulate and experiment with type class extensions in our framework. 
%
% Assignment:
%  Formulate the extensions to the class and instance mechanism of Haskell as described by Atze Dijkstra et al.
%  within the Top library. Review the existing type class directives and find new ones to cope with the newly derived power.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 1 Preliminaries: Type classes
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%Introduction to type classes (class/instance decls, overloading, polymorphism, predicate, 
%                              superclass, dict translation, evidence)
\section{Introduction}


% 1  * overloaded functions can be declared in a type class.
%    * class decl introduces the predicate Eq.
%
% 2  * functions are overloaded in the type variable a and have the type Eq a => ....
% 3  * overloading != polymorphism
%    * polymorphism -> same impl for diff types
%    * overloading  -> uses different impl for each type
% 
% 4  * Convenient : show, +, ==, 
%    * Extensible : inst decls
%    * instance decl:  add a new type + cor impl
%    * instance trasn: creates an instance using another inst
%
% 5  * SET: infinitely large (eq [Int], [[Int]], [[[Int]]], [[[[Int]]]].
%    * Function defined in terms of overloaded are also overloaded!
%
% Volgende -> Not only type checking -> translation
\begin{frame}\frametitle{Type classes}
\begin{block}{}
\begin{code}
 class Eq a where
   eq :: a -> a -> Bool
 
 instance Eq  Char where
   eq = primEqChar
 
 instance Eq a => Eq [a] where
   eq []      []      = True
   eq (x:xs)  (y:ys)  = eq x y && eq xs ys
   eq _       _       = False

 elem :: Eq a => a -> [a] -> Bool
 elem x []      = False
 elem x (y:ys)  = eq x y || elem x ys

 main = elem "Hello" ["Hello", "World"]
\end{code}
\end{block}
\only<2-> 
{
\begin{textblock}{6}(8, -12)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
eq :: Eq a => a -> a -> Bool
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\only<3> 
{
\begin{textblock}{6}(8, -9)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
overloading |/=| polymorphism
\begin{code}
length :: [a] -> Int
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\only<4> 
{
\begin{textblock}{8}(7, -9)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
Eq = {Char, [Char], [[Char]], ...}
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}


% * Class decl is translated into a datatype
% * Instance decls in values of that datatype
% * Ground instances such as eqInt
% * Transformers such as eqList construct dictionaries from other dictionaries. 
% * Note that the dictionary for Eq is used on the elements and the tail of the list is compared using a recursive call.
\begin{frame}\frametitle{Translation}
\begin{block}{}
\begin{code}
 (red (data)) Eq a =                 Eq {  (red(eq)) :: a -> a -> Bool  }
  
 (red (eqChar)) ::                   Eq Char
 (red (eqChar)) =                    Eq {  (red(eq)) =  primEqChar      }

 (red (eqList)) :: Eq a  (red (->))  Eq [a]
 (red (eqList d))        =           Eq {  (red(eq)) = f                }
   where  f []      []      = True
          f (x:xs)  (y:ys)  = (red (eq d)) x y && (red (eq (eqList d))) xs ys
          f _       _       = False

 elem :: Eq a => a -> [a] -> Bool
 elem x []      = False
 elem x (y:ys)  = eq x y || elem x ys

 main = elem "Hello" ["Hello", "World"]
\end{code}
\end{block}
\end{frame}


% * Also the predicate of |elem| is translated 
% * In main the parameter is given
\begin{frame}\frametitle{Translation}
\begin{block}{}
\begin{code}
 (red (data)) Eq a =                Eq {  (red(eq)) :: a -> a -> Bool  }
  
 (red (eqChar)) ::                  Eq Char
 (red (eqChar)) =                   Eq {  (red(eq)) =  primEqChar      }

 (red (eqList)) :: Eq a  (red(->))  Eq [a]
 (red (eqList d))        =          Eq {  (red(eq)) = f                }
   where  f []      []      = True
          f (x:xs)  (y:ys)  = (red (eq d)) x y && (red (eq (eqList d))) xs ys
          f _       _       = False

 elem :: Eq a (red(->)) a -> [a] -> Bool
 elem (red (d)) x []      = False
 elem (red (d)) x (y:ys)  = (red (eq d)) x y || elem (red (d)) x ys

 main = elem (red ((eqList eqChar))) "Hello" ["Hello", "World"]
\end{code}
\end{block}
\end{frame}


% * Classes can be arranged in a hierarchy: |Eq| is a superclass of |Ord|.
% * Arrow is not an implication: an instance from Ord cannot be constructed from an instance for Eq.
%   The meaning is that the instances in Ord are a subset of those in Eq.
% * In the translation an additional field is added to the introduced datatype for each superclass.
% * This means that for constructing the dictionary for Ord Int, we must also have the dict for Eq Int
%
% Probeer credits te geven... Jones, 
\begin{frame}\frametitle{Superclasses}
\begin{block}{}
\begin{code}
 class Eq a => Ord a where
   cmp :: a -> a -> Ordering

 instance Ord Char where
   cmp = cmpChar
\end{code}
\end{block}
\pause
\only<2-> 
{
\begin{textblock}{6}(8, -5)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
Eq a => Ord a
 means
Eq supseteq Ord
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\pause
\begin{block}{}
\begin{code}
 (red(data)) Ord a =  Ord   {  (red(cmp))    :: a -> a -> Bool
                            ,  (red(eqOrd))  :: (red(Eq a))    }

 (red(ordChar))  ::   Ord Char
 (red(ordChar))  =    Ord   {  (red(cmp))    = cmpChar
                            ,  (red(eqOrd))  = (red(eqChar))   }
\end{code}
\end{block}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 2: Problem statement, objectives, and approach
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% Problem statement (implementation, extensions, evidence, type-errors)
\section{Problem statement and approach}


% * The original design of type classes in 1989 was conservative. 
% * Since then many extensions are proposed, and those extensions are still being proposed.
% * Latest FP trend: encode all kind of problems using type class extensions. 
% * Given all these extensions: is there something wrong in the design of type classes? Is there a better solution?
%   Waarom zijn type classes op zich niet genoeg VOORBEELD??.
% * Difficult type-error messages, or no type errors at all.
\begin{frame}\frametitle{Problem statement: HUGE design space}
\pause
\begin{textblock}{9}(0, -5)
\begin{block}{Constructor classes}
\begin{code}
 class Monad m where 
   return  :: a -> m a
   (>>=)   :: m a -> (a -> m b) -> m b

 instance Monad [] 
 instance Monad Maybe 
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{10}(0, -4)
\begin{block}{Overlapping instances}
\begin{code}
 instance Show a => Show [a] 

 instance Show [Char] 

 instance Show a => Show [[a]] 
 emptyline
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{11}(0, -3)
\begin{block}{Multi-parameter type classes}
\begin{code}
 class Coll c e where
   empty   :: c
   insert  :: e -> c -> c
 
 instance Coll [a] a 

 test c = insert 'c' (insert True c)
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{12}(0, -2)
\begin{block}{Functional dependencies}
\begin{code}
 class Coll c e | c -> e where
   empty   :: c
   insert  :: e -> c -> c

 instance Coll [a] a 

 test c = insert 'c' (insert True c)
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{13}(0, -1)
\begin{block}{Local instances}
\begin{code}
 emptyline
 f g =  let  instance Eq Int where 
               x == y = primEqInt (x `mod` 360) (y `mod` 360) 
        in   ...
 emptyline
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{14}(0, 0)
\begin{block}{Different context-reduction strategies}
\begin{code}
 f (x:xs) (y:xs) = x > y && xs == ys
 -- Possible types for |f|
 f :: Ord a            => [a] -> [a] -> Bool
 f :: (Ord a, Eq a)    => [a] -> [a] -> Bool
 f :: (Ord a, Eq [a])  => [a] -> [a] -> Bool
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{15}(0, 1)
\begin{block}{Type class directives}
\begin{code}
 never       Eq (a -> b)          : "functions cannot be tested for equality"
 never       Num Bool             : "arithmetic on booleans isn't supported"
 close       Integral             : "the only Integral instances are Int and Integer"
 disjoint    Integral Fractional  : "something which is fractional can never be integral"
 emptyline
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{16}(0, 2)
\begin{block}{Other extensions encoded using predicates}
\begin{code}
 f :: (r lacks l)  => ...

 g :: (?width)     => ...

 h :: (a <= b)     => ...
\end{code}
\end{block}
\end{textblock}
\pause
\begin{textblock}{16}(0, 3)
\begin{block}{Associated type synonyms}
\begin{code}
 class Collects c where
   type Elem c
   empty   :: c
   insert  :: Elem c -> c -> c
\end{code}
\end{block}
\end{textblock}
\end{frame}


% Approach ala Top 
\begin{frame}\frametitle{Problem statement and approach}
\begin{block}{Problem statement}
\begin{itemize}
  \item No uniform approach to formulate type class extensions.
  \item Not easy to experiment with design decisions and extensions.
  \item Type error messages are difficult to understand.
\end{itemize}
\end{block}
\pause
\begin{block}{Approach}
\begin{itemize}
  \item Formulate overloading into constraints.
  \item Let CHRs generate every (type) correct reduction alternative.
  \item Represent reduction alternatives in a graph.
  \item Use heuristics to find a solution in the graph.
\end{itemize}
\end{block}
\end{frame}




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 3: Constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Formulate overloading into constraints}


% 1.  We formulate overloading into constraints:
%     - Prove constraints are generated at places where overloaded functions are used (instantiation)
%     - Assume constraints are generated at places where overloaded functions are defined  (skolemization, generalization)
%     We leave |pi| abstract, we make no assumptions over the predicate language used.
%
% 2.  For, example, consider the use of the equality function on [Char]
%     - The type scheme of eq is instantiated with the type of the arguments ([Char])
%
% 3.  This results in the proof obligations: Prove (Eq [Char])
%     Intuitively, we can see that we this constraint can be satisfied using the instances for list and char
%     In this example we used type class predicates, however to support local instances we need another predicate language
\begin{frame}\frametitle{Formulating overloading into constraints}
\begin{block}{Constraint language:}
\begin{code}
 data Constraint pi =  Prove   pi   |   Assume  pi  
\end{code}
\end{block}
\pause
\begin{block}{Example 1:}
\begin{code}
 -- |eq :: Eq a => a -> a -> Bool|

 test = eq "" "Hello"
\end{code}
\end{block}

\only<3> 
{
\begin{textblock}{6}(8, -2)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
Prove (Eq [Char])
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}


% 1  Consider the following function for testing insertion into an ordered list
%    - an element is inserted into an ordered list
%    - then, the result is compared with the sorted result.
%    - note the scoped instance.
% 2  In this example we can identify two scopes: a local and a global scope
%    - A scope is identified by a list of integers
%    - [] is the global scope
%    - lists with the same length are sibling scopes
% 3  Predicates:
%    - To solve local instances, we need to keep track of in which scope a constraint occurs
%    - Therefore the predicate is now a tuple consisting of a type class pred and a scope
%    - notice the predicate in the explicit type signature of testInsert
%    - This type signature is skolemized with the type constant c1, resulting in the Assume constraint
%    - The usages of insert and sort result in the Prove constraint
%    - The usage of eq result in the other Prove constraint
%    - In some way we have to proof that we can derive the proves from the assumption
\begin{frame}\frametitle{Annotating predicates with scope}
\begin{block}{Example 2: Local instances}
\begin{code}
 -- |insert  :: Ord a => a -> [a] -> [a]|
 -- |sort    :: Ord a => [a] -> [a]|

 testInsert :: Ord a => a -> [a] -> Bool
 testInsert x xs =  let  instance Eq a => Eq [a] where
                           eq = ...
                         ys = insert x (sort xs)
                    in   eq ys (sort ys)
\end{code}
\end{block}
\pause
\begin{block}{Identification of scopes}
\begin{itemize}
  \item A scope is identified by a list of integers |([Int])|,
  \item |[]| is the global scope, and
  \item |[1,1], [1,2]| are sibling scopes.
\end{itemize}
\end{block}
\only<3> 
{
\begin{textblock}{6}(8, -6)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
  {  Assume  (Ord c1,   []   )
  ,  Prove   (Ord c1,   [1]  ) 
  ,  Prove   (Eq [c1],  [1]  ) }
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}


% 1  By abstracting over predicates we get the following for free:
%    - Constructor class: because it only allows higher kinded types in predicates
%    - Multi-parameter classes: because is only allows for predicates over more then one type.
%    - Furthermore it allows us to annotate predicates with scope
% 2  Also it allows to encode other predicate languages, such as:
%    - lacks, has
%    - implicit parameters
\begin{frame}\frametitle{Advantages}
\begin{block}{Constraints allows use to encode:}
\begin{itemize}
  \item Constructor classes 
  \item Multi-parameter type classes
  \item Predicates annotated with a scope
\end{itemize}
\end{block}
\pause
\begin{block}{Also other predicate extensions can be encoded:}
\begin{itemize}
  \item |lack| and |has| predicates for extensible records
  \item |?| and |%| predicates for implicit parameters
  \item ....
\end{itemize}
\end{block}
\end{frame}



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 4: Constraint solving using Constraint Handling Rules
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Constraint Solving using Constraint Handling Rules}
% What are Constraint Handling Rules?
% What is context reduction?
% How instance and class declarations are translated to CHRs?
% How do we encode scoped instances?
% To satisfy proof constraint we have to do two things 
% - context-reduce proofs using instance and class declarations
% - check is the reduced proofs are entailed by the assumptions


% 1  * Encode context-reduction using CHRs
%    * Generate context reduction alternatives using CHRs
%    * Represent these alternatives in a graph
%    * Use a heuristic to choose a solution from the graph 
%
% 2  * Reduction constraints are generated by CHRs
%    * A reduction constraint represents a reduction alternative
%    * The second component is meta information
\begin{frame}\frametitle{Solving constraints}
\begin{block}{To solve constraints we}
\begin{itemize}
  \item Let CHRs generate |Reduction| constraints.
  \item Represent |Reduction| constraints in a graph.
  \item Use heuristics to find a solution in the graph.
\end{itemize}
\end{block}
\pause
\begin{block}{Extend the constraint language}
\begin{code}
 data Constraint pi info  =   ...
                          |   Reduction pi info [pi]
\end{code}
\end{block}
\only<3> 
{
\begin{textblock}{11}(3, -5)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
  {  Reduction (Eq Char)      "eqChar"   []
  ,  Reduction (Eq (v1, v2))  "eqTuple"  [Eq v1, Eq v2] }
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}


% * CHRs: head, guard, body
% * Simplification: replace head -> body
% * Propagation: add body if head 
% * To avoid non-termination a prop is applied only one time on each constraint
% \item A propagation CHR adds the |body| if the |head| matches.
% \item A propagation CHR is applied |only| once to each constraint.
% \item A simplification CHR replaces the |head| by the |body|.
\begin{frame}\frametitle{Constraint Handling Rules}
\begin{block}{CHR syntax}
\begin{code}
  C   ==>  G ^^ | ^^ D      ^^  (propagation)
  C   <=>  G ^^ | ^^ D      ^^  (simplification)
\end{code}
\end{block}
\pause
\begin{block}{Constraint Handling Rules}
\begin{itemize}
\item Language for writing constraint solvers created by Thom Fr\"{u}hwirth.
\item Idea of using CHRs for type classes proposed by Sulzmann et al.
\item Understanding functional dependencies via CHRs.
\end{itemize}
\end{block}
\end{frame}


\begin{frame}\frametitle{Context reduction}
\begin{block}{Context reduction using instance declarations}
\begin{code}
 instance Eq Char where ...

 Prove(Eq Char, s)     ==>  [] `visibleIn` s
                       |    Reduction (Eq Char, s) "eqChar" []
\end{code}
\pause
\begin{code}
 instance Eq a => Eq [a] where ...

 Prove(Eq [a]     , s)  ==>  [] `visibleIn` s
                        |    Prove(Eq   a, s)
                        ,    Reduction (Eq [a], s) "eqList" [(Eq a, s)]
\end{code}
\end{block}
\end{frame}


\begin{frame}\frametitle{Context reduction}
\begin{block}{Context reduction using the class hierarchy}
\begin{code}
 class Eq a where ...
 class Eq a => Ord a where ...
\end{code}
\pause
\begin{code}
 -- reducing using the class hierarchy
 Prove(Eq a, s), Prove(Ord a, s)
    ==>  Reduction (Eq a, s) "eqOrd" [(Ord a, s)]
\end{code}
\pause
\only<3-> 
{
\begin{textblock}{6}(8, -6)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
  {  Assume  (Ord  c1, [])
  ,  Prove   (Eq   c1, []) }
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\pause
\begin{code}
 -- propagating the class hierarchy
 Assume(Ord a, s)  
    ==>  Assume(Eq a, s), Reduction (Eq a, s) "eqOrd" [(Ord a, s)]
\end{code}
\end{block}
\end{frame}


% Algemene regel!! Voor ieder predicate p
\begin{frame}\frametitle{Simplification of predicates annotated with scope}
\begin{block}{How can the following predicates be simplified?}
\begin{code}
  {  Assume  (Ord  c1, [])
  ,  Prove   (Ord  c1, [1]) 
  ,  Prove   (Eq   c1, [1]) }
\end{code}
\end{block}
\pause
\begin{block}{Lifting a predicate to the parent scope}
\begin{code}
  Prove(p, s)  ==>  not (toplevel s)
               |    Prove (p, parent s)
               ,    Reduction (p, s) "scope" [(p, parent s)]
\end{code}
\end{block}
\only<3> 
{
\begin{textblock}{11}(2, -5)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
  {  Reduction (Ord  c1, [1])   "scope"  [(Ord  c1, [])] 
  ,  Reduction (Eq   c1, [1])   "eqOrd"  [(Ord  c1, [1])]  
  ,  Reduction (Eq   c1, [1])   "scope"  [(Eq   c1, [])] 
  ,  Reduction (Eq   c1, [])    "eqOrd"  [(Ord  c1, [])]  
  }
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% PART 5: Simplification graphs and Heuristics
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
\section{Simplification graphs and heuristics}


% - Add assumptions to the graph
% - Add reductions to the graph
%     - two type of nodes
%     - node for preds + and node
% - Advantages
%     - every (type) correct alternative is present 
%     - allows for easy debugging (vis. representation of the problem)
%     - info. can also be used for generating type-error messages
%     - allows for encoding od different design decisions using heuristics
% Voorbeeld van een graafje 
\begin{frame}\frametitle{Simplification graph for local instances}
\only<1> 
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr1.pdf}
\end{center}
\begin{block}{}
\begin{code}
 Prove  (Eq   c1, [1])
 Prove  (Ord  c1, [1])
\end{code}
\end{block}
}
\only<2> 
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr2.pdf}
\end{center}
\begin{block}{}
\begin{code}
 Assume  (Ord  c1, []) 
 emptyline
\end{code}
\end{block}
}
\only<3> 
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr3.pdf}
\end{center}
\begin{block}{}
\begin{code}
 Reduction (Eq   c1, [1])   "scope"  [(Eq   c1, [])] 
 Reduction (Ord  c1, [1])   "scope"  [(Ord  c1, [])] 
\end{code}
\end{block}
}
\only<4> 
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr4.pdf}
\end{center}
\begin{block}{}
\begin{code}
 Reduction (Eq   c1, [1])    "eqOrd"  [(Ord  c1, [1])]  
 Reduction (Eq   c1, [])     "eqOrd"  [(Ord  c1, [])]  
\end{code}
\end{block}
}
\end{frame}


% Using the graph and heuristics we are able to emulate different context reduction strategies
\begin{frame}\frametitle{Advantages of simplification graphs}
\begin{block}{Graphs make experimenting with type classes easy!}
\begin{itemize}
  \item Visualization of the problem!
  \item Lot of information present for type error messages.
  \item Every (type) correct reduction alternative is present.
  \item Specify alternative reduction strategies in heuristics.
\end{itemize}
\end{block}
\pause
\begin{block}{Examples of heuristics:}
\begin{itemize}
  \item Heuristic emulating Haskell 98 or GHC context reduction.
  \item Cost-path heuristic.
  \item Heuristic that utilizes backtracking to find a solution.
  \item Or a nice combination of the above heuristics.
\end{itemize}
\end{block}
\end{frame}


% Present an example to shoe how many design decisions there are an how 
% these can be represented in a simplification graph.
%
% For show in the function ppTable we have to prove |Show [[[v1]]]|
%
% (Show [[[v1]]]) ->2d (Show[v1]) ->list (Show v1)
\begin{frame}\frametitle{Example: local and overlapping instance declarations}
\begin{block}{}
\begin{code}
class Show a where
  show :: a -> [Char]

instance Show Char              -- showChar
instance Show a =>  Show [a]    -- show[]
instance Show [Char]            -- show[Char]
emptyline
emptyline
ppTable hdr tbl 
  =  let  instance Show a =>  Show [[a]]  -- show[[]]
     in   ... show (hdr : tbl) ...
emptyline
emptyline
main = ppTable ["Name", "DOB"]  [["G", "19830511"]
                                ,["A", "19830208"]]
\end{code}
\end{block}
\only<2-> 
{
\begin{textblock}{11}(1, -5)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
  Prove   (Show [[[v1]]])

  ppTable :: Show [[[a]]]  => [[a]] -> [[[a]]] -> [Char]
  ppTable :: Show [a]      => [[a]] -> [[[a]]] -> [Char]
  ppTable :: Show a        => [[a]] -> [[[a]]] -> [Char]
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}


\begin{frame}\frametitle{Simplification graph: first attempt}
\only<1,2>
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr5.pdf}
\end{center}
}
\only<3->
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr6.pdf}
\end{center}
}
\only<2-> 
{
\begin{textblock}{5}(0, -13)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
Heuristic:
\begin{itemize}
  \item Eager reduction
  \item Prefer local above global
\end{itemize}
\end{beamerboxesrounded}
\end{textblock}
}
\only<4-> 
{
\begin{textblock}{12}(0, -10)
\begin{block}{Inferred type for the function |ppTable|}
\begin{code}
  (red (instance Show [Char]))            -- show[Char]

  ppTable :: (red(Show a)) =>  [[a]] -> [[[a]]] -> [Char]

  main = (red(ppTable)) ["Name", "DOB"]  [["G", "19830511"]
                                         ,["A", "19830208"]]
\end{code}
\end{block}
\end{textblock}
}
\only<5-> 
{
\begin{textblock}{5}(2, -4)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
Prove (Show Char)
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}


\begin{frame}\frametitle{Simplification graph: second attempt}
\only<1,2>
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr5.pdf}
\end{center}
}
\only<3->
{
\begin{center}
\includegraphics[scale=0.70]{graphs/rgr7.pdf}
\end{center}
}
\only<2-> 
{
\begin{textblock}{5}(0, -13)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
Heuristic II:
\begin{itemize}
  \item Eager reduction for local instances.
  \item Otherwise stop.
\end{itemize}
\end{beamerboxesrounded}
\end{textblock}
}
\only<4-> 
{
\begin{textblock}{12}(0, -10)
\begin{block}{Inferred type for the function |ppTable|}
\begin{code}
  (red (instance Show [Char]))            -- show[Char]

  ppTable :: (red(Show [a])) =>  [[a]] -> [[[a]]] -> [Char]

  main = (red(ppTable)) ["Name", "DOB"]  [["G", "19830511"]
                                         ,["A", "19830208"]]
\end{code}
\end{block}
\end{textblock}
}
\only<5-> 
{
\begin{textblock}{5}(2, -4)
\begin{beamerboxesrounded}[shadow=true,scheme=goodpopup]{}
\begin{code}
Prove (Show [Char])
\end{code}
\end{beamerboxesrounded}
\end{textblock}
}
\end{frame}

\section{Conclusion}
\begin{frame}\frametitle{Conclusion}
\begin{block}{Contributions}
\begin{itemize}
  \item First in using graphs and heuristics to solve and experiment with type classes.
  \item First in using CHRs with explicit |Prove| and |Assume| constraints.
  \item Implementation of this framework and a basic CHR solver.
\end{itemize}
\end{block}
\pause
\begin{block}{Conclusion}
\begin{itemize}
  \item Uniform encoding of type class extensions.
  \item Graphs and heuristics make it easy to experiment with extensions.
  \item Every well known type class extension can be encode using this framework.
  \item Framework can be used for both Helium/Top and EHC.
\end{itemize}
\end{block}
\end{frame}
\end{document}
