module EH101.Pred.RedGraph
( module EH.Util.AGraph
, RedNode (..)
, RedGraph, emptyRedGraph
, mkRedGraphFromReductions, addToRedGraphFromReductions
, addToRedGraphFromAssumes, mkRedGraphFromAssumes
, ppRedGraph
, addAssumption
, addReduction
, redPruneReductionsUntil
, redAlternatives )
where
import EH101.Base.Common
import EH101.Ty
import EH101.VarMp
import Data.Maybe
import EH101.CHR.Constraint
import EH101.CHR.Constraint
import EH101.Pred.Heuristics
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH.Util.AGraph
import EH.Util.Pretty
import Data.Graph.Inductive.Graph
import EH.Util.Utils

{-# LINE 34 "src/ehc/Pred/RedGraph.chs" #-}
data RedNode p
  =  Red_Pred { rednodePred 	::  !p	 	}
  |  Red_And  { rednodePreds 	::  ![p] 	}
  deriving (Eq, Ord)

mkRedNode :: [p] -> RedNode p
mkRedNode [p] = Red_Pred p
mkRedNode ps  = Red_And ps

redNodePreds :: RedNode p -> [p]
redNodePreds (Red_Pred  q) = [q]
redNodePreds (Red_And  qs) = qs

true  ::  RedNode p
true  =   mkRedNode []

{-# LINE 52 "src/ehc/Pred/RedGraph.chs" #-}
type RedGraph p info = AGraph (RedNode p) info

emptyRedGraph :: Ord p => RedGraph p i
emptyRedGraph = emptyAGraph

{-# LINE 59 "src/ehc/Pred/RedGraph.chs" #-}
addToRedGraphFromReductions :: Ord p => [Constraint p info] -> RedGraph p info -> RedGraph p info
addToRedGraphFromReductions cs g = foldr addReduction g cs

mkRedGraphFromReductions :: Ord p => [Constraint p info] -> RedGraph p info
mkRedGraphFromReductions cs = addToRedGraphFromReductions cs emptyRedGraph

{-# LINE 67 "src/ehc/Pred/RedGraph.chs" #-}
addToRedGraphFromAssumes :: Ord p => ConstraintToInfoMap p info -> RedGraph p info -> RedGraph p info
addToRedGraphFromAssumes cm g = Map.foldrWithKey addAssumption g cm

mkRedGraphFromAssumes :: Ord p => ConstraintToInfoMap p info -> RedGraph p info
mkRedGraphFromAssumes cm = addToRedGraphFromAssumes cm emptyRedGraph

{-# LINE 75 "src/ehc/Pred/RedGraph.chs" #-}
instance PP p => Show (RedNode p) where
  show = showPP . pp

instance PP p => PP (RedNode p) where
  pp (Red_Pred p)    = pp p
  pp (Red_And [])    = pp "True"
  pp (Red_And _ )    = pp "And"

{-# LINE 85 "src/ehc/Pred/RedGraph.chs" #-}
ppRedGraph :: (PP p, PP i) => RedGraph p i -> PP_Doc
ppRedGraph ag = "RedGraph" >-< indent 2 ((ppBracketsCommasV $ nodes g) >-< (ppBracketsCommasV $ edges g) >-< pp (show g))
  where g = agraphGraph ag

{-# LINE 95 "src/ehc/Pred/RedGraph.chs" #-}
addAssumption :: Ord p => Constraint p info -> [info] -> RedGraph p info -> RedGraph p info
addAssumption (Assume  p)  is  = insertEdges (zip3 (repeat (Red_Pred p)) (repeat true) is)
addAssumption _            _   = id

{-# LINE 101 "src/ehc/Pred/RedGraph.chs" #-}
addReduction :: Ord p => Constraint p info -> RedGraph p info -> RedGraph p info
addReduction (Reduction {cnstrPred=p, cnstrInfo=i, cnstrFromPreds=[q]})
                                    =  insertEdge (Red_Pred p, Red_Pred q  , i)
addReduction (Reduction {cnstrPred=p, cnstrInfo=i, cnstrFromPreds=ps})
                                    =  let  andNd  = Red_And ps
                                            edges  = map (\q -> (andNd, Red_Pred q, i)) ps
                                       in   insertEdges ((Red_Pred p, andNd, i) : edges)
addReduction _                      =  id

{-# LINE 119 "src/ehc/Pred/RedGraph.chs" #-}
redPruneReductionsUntil :: (Ord p) => [p] -> (p -> Bool) -> RedGraph p info -> RedGraph p info
redPruneReductionsUntil leaves stop gr
  = dels (map Red_Pred leaves) gr
  where dels leaves g = foldr del g leaves
        del  leaf   g | all (not . stop)
                        $ redNodePreds leaf = dels (map snd pres)
                                              $ deleteNode leaf
                                              $ foldr (\(_,from) g -> deleteEdge (from,leaf) g) g pres
                      | otherwise           = g
          where pres = predecessors g leaf

{-# LINE 136 "src/ehc/Pred/RedGraph.chs" #-}
redAlternatives :: (Ord p {-, PP p, PP info debug -}) => RedGraph p info -> p -> HeurAlts p info
redAlternatives gr
  = recOr Set.empty
  where  recOr visited p = HeurAlts p (map (recAnd visited') (successors gr (Red_Pred p)))
           where visited' = Set.insert p visited

         recAnd visited (i, n)
           = HeurRed i (map mk qs)
           where qs = redNodePreds n
                 mk q | Set.member q visited = HeurAlts q [HeurRed_Rec q]
                      | otherwise            = recOr visited q

