%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% VarMp utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 module {%{EH}VarMp.Utils}
%%]

%%[99 import({%{EH}Base.Common})
%%]
%%[99 import({%{EH}VarMp},{%{EH}Substitutable},{%{EH}Ty})
%%]

%%[99 import(qualified Data.Set as Set, qualified Data.Map as Map)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Various VarMp utilities:
\begin{itemize}
\item Graphlike traversal
\end{itemize}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Filter by transitive closure from rootset
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(varmpGraphVisit)
-- | Filter the base level by traversing as a graph
varmpGraphVisit
  :: TyVarIdS
     -> VarMp
     -> VarMp
varmpGraphVisit start (VarMp l (m:ms))
  = VarMp l (m':ms)
  where m'
          = graphVisit (\newm m tvar
                         -> case Map.lookup tvar m of
                              Just i -> (Map.insert tvar i newm, varFreeSet i)
                              _      -> (newm,Set.empty)
                       )
                       Set.union
                       Map.empty
                       start m
%%]
graphVisit visit unionUnvisited thr start graph
(thr -> graph -> node -> (thr,Set.Set node)) 
