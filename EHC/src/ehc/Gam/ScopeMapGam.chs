%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment, Map based, providing levels for scoping
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Environment/Gamma where the lexical level + scoping is used to provide nesting behavior.
Both a SGam and its entries know at which scope they are.

Insertion is efficient, lookup also, because a single Map is used.

The Map holds multiple entries, each with its own scope identifier.
An SGam holds
\begin{itemize}
\item a stack of scopes, encoding the nesting, where
\item each scope holds mappings for MetaLev's
\end{itemize}
Results are filtered out w.r.t. this stack, i.e. are checked to be in scope.
In principle this can be done eagerly, that is, immediately after a change in scope, in particular in sgamPop.
After some experimentation it did turn out that doing this lazily is overall faster, that is, when the SGam is consulted (lookup, conversion to association list, etc).
Conceptually thus the invariant is that no entry is in the map which is not in scope. Guaranteeing this invariant is thus not done by the one function breaking it (sgamPop).
%%]

%%[8 module {%{EH}Gam.ScopeMapGam}
%%]

%%[8 import(qualified Data.Set as Set,qualified Data.Map as Map,Data.Maybe,Data.List)
%%]
%%[8 import(UHC.Util.ScopeMapGam) export(module UHC.Util.ScopeMapGam)
%%]

%%[50 import({%{EH}VarMp})
%%]

%%[50 hs import(Data.Typeable(Typeable), Data.Generics(Data), {%{EH}Base.Serialize})
%%]
%%[50 import(Control.Monad, {%{EH}Base.Binary})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance (Serialize v) => Serialize (SGamElt v) where
  sput (SGamElt a b) = sput a >> sput b
  sget = liftM2 SGamElt sget sget
%%]

%%[50
instance (Ord k, Serialize k, Serialize v) => Serialize (SGam k v) where
  sput (SGam a b c) = sput a >> sput b >> sput c
  sget = liftM3 SGam sget sget sget
%%]

%%[50
-- instance (Binary v) => Serialize (SGamElt v)
-- instance (Ord k, Binary k, Binary v) => Serialize (SGam k v)
%%]

