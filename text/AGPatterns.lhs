%include lhs2TeX.fmt
%include forSubText.fmt

\label{sl_sec_ag_typical_usage}
An attribute can be inherited, synthesized, or both.
These alternatives correspond to the way monads are categorized in reader, writer and state monads, respectively.
An inherited attribute like @tpGam@ in the preceding section contains information which is distributed
and meant as data which is used, not created and returned as a result like a synthesized attribute \verb'exprTp'. 

\textbf{Threading and copy rules}.
Attributes which are both inherited and synthesized are often used to thread information
through an AST. As such they serve as global state or as an accumulator of information.
An example of global state is a counter providing a source of unique values, required for
creating fresh type variables later on (see figure~\ref{sl_fig_gam_expr_var}):

\begin{code}
  ATTR Expr
    [ | exprUniq: {Int} | ]
  
  SEM
    | Add loc . u         = @lhs.exprUniq + 1
                            -- and do something with
                            -- lhs.exprUniq
          e1  . exprUniq  = @u
          e2  . exprUniq  = @e1.exprUniq
          lhs . exprUniq  = @e2.exprUniq
\end{code}

The attribute @exprUniq@ runs through the AST like a thread,
that is, its value conceptually runs from top to bottom, left to right
and finally bottom to top through the abstract syntax tree.
Using such a threaded attribute involves quite a bit of attribute definitions.
Luckily, this trivial copying work is the burden of the AG system which silently
adds definitions for copying attributes having the same name (see \cite{uu:ag} for more details).
In the preceding example, the definitions for  @ @@e1.exprUniq@ and @ @@lhs.exprUniq@ are
thus automatically generated.

Some care has to be taken when a counter like this is used. It is all too easy
to use this counter in building a data structure which is then used earlier (w.r.t. the ordering imposed
by the value of the counter). This generally creates a cyclic dependency that in the current
implementation of AG is not noticed earlier than at runtime by, for example, a black hole a 
stack overflow.

\textbf{Accumulating}.
Another example of a threaded attribute, used as an accumulator, can be found in section~\ref{sl_sec_expression_infer}.
It involves attribute @tpSubst@ used to gather type variable subsitutions.

\textbf{Lists}.
List structures are used often to encode sequences. An example of a list occurs in
section~\ref{sl_sec_pattern_infer}
where @PatExprs@, a sequence of @PatExpr@'s is defined:

\begin{code}
  DATA PatExprs
    | Nil
    | Cons          hd         : PatExpr
                    tl         : PatExprs
\end{code}

List definitions occur so often that these can be abbreviated by

\begin{code}
  TYPE PatExprs     = [PatExpr]
\end{code}


As a final remark, even though attribute usage corresponds to monad usage, monads are difficult to
use when it comes to combining them. This is not the case with the AG system because combining attributes
simply boils down to generating code where all attribute computation is expressed in one large catamorphism.

