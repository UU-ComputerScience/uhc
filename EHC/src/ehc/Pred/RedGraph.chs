%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Reduction graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest.

%%[9 module {%{EH}Pred.RedGraph} import({%{EH}Base.Common},{%{EH}Ty},{%{EH}VarMp})
%%]

%%[9 import({%{EH}CHR.Constraint},{%{EH}CHR.Constraint})
%%]

%%[9 import({%{EH}Pred.Heuristics})
%%]

%%[9 import(qualified Data.Map as Map)
%%]

%%[9 import(EH.Util.AGraph,EH.Util.Pretty) export(module EH.Util.AGraph)
%%]

%%[9 import(Data.Graph.Inductive.Graph)
%%]

-- debug
%%[9 import(EH.Util.Utils)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Graph node
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(RedNode(..))
data RedNode p
  =  Red_Pred { rednodePred 	::  !p	 	}
  |  Red_And  { rednodePreds 	::  ![p] 	}
  deriving (Eq, Ord)

true  ::  RedNode p
true  =   Red_And []
%%]

%%[9 export(RedGraph,emptyRedGraph)
type RedGraph p info = AGraph (RedNode p) info

emptyRedGraph :: Ord p => RedGraph p i
emptyRedGraph = emptyAGraph
%%]

%%[9 export(mkRedGraphFromReductions,addToRedGraphFromReductions)
addToRedGraphFromReductions :: Ord p => [Constraint p info] -> RedGraph p info -> RedGraph p info
addToRedGraphFromReductions cs g = foldr addReduction g cs

mkRedGraphFromReductions :: Ord p => [Constraint p info] -> RedGraph p info
mkRedGraphFromReductions cs = addToRedGraphFromReductions cs emptyRedGraph
%%]

%%[9 export(addToRedGraphFromAssumes,mkRedGraphFromAssumes)
addToRedGraphFromAssumes :: Ord p => ConstraintToInfoMap p info -> RedGraph p info -> RedGraph p info
addToRedGraphFromAssumes cm g = Map.foldWithKey addAssumption g cm

mkRedGraphFromAssumes :: Ord p => ConstraintToInfoMap p info -> RedGraph p info
mkRedGraphFromAssumes cm = addToRedGraphFromAssumes cm emptyRedGraph
%%]

%%[9
instance PP p => Show (RedNode p) where
  show = showPP . pp

instance PP p => PP (RedNode p) where
  pp (Red_Pred p)    = pp p
  pp (Red_And [])    = pp "True"
  pp (Red_And _ )    = pp "And"
%%]

%%[9 export(ppRedGraph)
ppRedGraph :: (PP p, PP i) => RedGraph p i -> PP_Doc
ppRedGraph ag = "RedGraph" >-< indent 2 ((ppBracketsCommasV $ nodes g) >-< (ppBracketsCommasV $ edges g) >-< pp (show g))
  where g = agraphGraph ag
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Constructing a graph from Reduction constraints
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(addAssumption)
addAssumption :: Ord p => Constraint p info -> [info] -> RedGraph p info -> RedGraph p info
addAssumption (Assume  p)  is  = insertEdges (zip3 (repeat (Red_Pred p)) (repeat true) is) 
addAssumption _            _   = id
%%]

%%[9 export(addReduction)
addReduction :: Ord p => Constraint p info -> RedGraph p info -> RedGraph p info
addReduction (Reduction p i [q])  =  insertEdge (Red_Pred p, Red_Pred q  , i)
addReduction (Reduction p i ps)   =  let  andNd  = Red_And ps
                                          edges  = map (\q -> (andNd, Red_Pred q, i)) ps
                                     in   insertEdges ((Red_Pred p, andNd, i) : edges)
addReduction _                    =  id
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Generating alternatives from a reduction graph
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(redAlternatives)
redAlternatives :: (Ord p {-, PP p, PP info debug -}) => RedGraph p info -> p -> HeurAlts p info
redAlternatives gr = recOr
  where  recOr   p       = HeurAlts  p  (map recAnd  (successors gr (Red_Pred p))) 
         recAnd  (i, n)  = HeurRed   i  (map recOr   (preds n))
         preds  n  = case n of
                       Red_Pred  q   -> [q]
                       Red_And   qs  -> qs
%%]
  where  recOr   p       = HeurAlts  p  (map recAnd  ((\v -> trp "AA" (ppRedGraph gr >-< p >#< v) v) $ successors gr (Red_Pred p))) 

