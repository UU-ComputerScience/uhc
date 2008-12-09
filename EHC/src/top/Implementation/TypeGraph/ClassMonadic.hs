{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.TypeGraph.ClassMonadic where

import Top.Interface.Basic
import Top.Interface.TypeInference
import Top.Interface.Qualification
import qualified Top.Implementation.TypeGraph.Class as TG
import Top.Implementation.TypeGraph.Basics
import Top.Types
import Top.Solver
import qualified Data.Map as M
import qualified Data.Set as S

class (HasBasic m info, HasTI m info, HasQual m info, HasTG m info, MonadWriter LogEntries m, Show info) => HasTypeGraph m info | m -> info

instance (HasBasic m info, HasTI m info, HasQual m info, HasTG m info, MonadWriter LogEntries m, Show info) => HasTypeGraph m info

class Monad m => HasTG m info | m -> info where
   withTypeGraph :: (forall graph . TG.TypeGraph graph info => graph -> (a, graph)) -> m a

useTypeGraph :: HasTG m info => (forall graph . TG.TypeGraph graph info => graph -> a) -> m a
useTypeGraph f = withTypeGraph (\g -> (f g, g))

changeTypeGraph :: HasTG m info => (forall graph . TG.TypeGraph graph info => graph -> graph ) -> m ()
changeTypeGraph f = withTypeGraph (\g -> ((), f g))

-- construct a type graph

addTermGraph :: HasTypeGraph m info => Tp -> m VertexId
addTermGraph tp =
   do unique   <- getUnique
      synonyms <- getTypeSynonyms
      (newUnique, vid) <- withTypeGraph
         (\graph -> let (u, v, g) = TG.addTermGraph synonyms unique tp graph
                    in ((u, v), g))
      setUnique newUnique
      return vid
      
addVertex :: HasTypeGraph m info => VertexId -> VertexInfo -> m ()
addVertex vid info = changeTypeGraph (TG.addVertex vid info)

addEdge :: HasTypeGraph m info => EdgeId -> info -> m ()
addEdge edgeId info = changeTypeGraph (TG.addEdge edgeId info)

addNewEdge :: HasTypeGraph m info => (VertexId, VertexId) -> info -> m ()
addNewEdge pair info = changeTypeGraph (TG.addNewEdge pair info)

-- deconstruct a type graph

deleteEdge :: HasTypeGraph m info => EdgeId -> m ()
deleteEdge edgeId = changeTypeGraph (TG.deleteEdge edgeId)

-- inspect an equivalence group in a type graph

verticesInGroupOf :: HasTypeGraph m info => VertexId -> m [(VertexId, VertexInfo)]
verticesInGroupOf vid = useTypeGraph (TG.verticesInGroupOf vid)

childrenInGroupOf :: HasTypeGraph m info => VertexId -> m ([ParentChild], [ParentChild])
childrenInGroupOf vid = useTypeGraph (TG.childrenInGroupOf vid)

constantsInGroupOf :: HasTypeGraph m info => VertexId -> m [String]
constantsInGroupOf vid = useTypeGraph (TG.constantsInGroupOf vid)

representativeInGroupOf :: HasTypeGraph m info => VertexId -> m VertexId
representativeInGroupOf vid = useTypeGraph (TG.representativeInGroupOf vid)

edgesFrom :: HasTypeGraph m info => VertexId -> m [(EdgeId, info)]
edgesFrom vid = useTypeGraph (TG.edgesFrom vid)

-- query a path in an equivalence group
allPaths :: HasTypeGraph m info => VertexId -> VertexId -> m (TypeGraphPath info)
allPaths v1 v2 = useTypeGraph (TG.allPaths v1 v2)

allPathsList :: HasTypeGraph m info => VertexId -> [VertexId] -> m (TypeGraphPath info)
allPathsList v1 vs = useTypeGraph (TG.allPathsList v1 vs)

allPathsListWithout :: HasTypeGraph m info => S.Set VertexId -> VertexId -> [VertexId] -> m (TypeGraphPath info)
allPathsListWithout set v1 vs = useTypeGraph (TG.allPathsListWithout set v1 vs)

-- substitution and term graph
substituteVariable :: HasTypeGraph m info => Int -> m Tp
substituteVariable i =
   do synonyms <- getTypeSynonyms
      useTypeGraph (TG.substituteVariable synonyms i)

substituteType :: HasTypeGraph m info => Tp -> m Tp
substituteType tp =
   do synonyms <- getTypeSynonyms
      useTypeGraph (TG.substituteType synonyms tp)
      
substituteTypeSafe :: HasTypeGraph m info => Tp -> m (Maybe Tp)
substituteTypeSafe tp =
   do synonyms <- getTypeSynonyms
      useTypeGraph (TG.substituteTypeSafe synonyms tp)
      
makeSubstitution   :: HasTypeGraph m info => m [(VertexId, Tp)]
makeSubstitution =
   do synonyms <- getTypeSynonyms
      useTypeGraph (TG.makeSubstitution synonyms)

typeFromTermGraph :: HasTypeGraph m info => VertexId -> m Tp
typeFromTermGraph vid = useTypeGraph (TG.typeFromTermGraph vid)
   
-- Extra administration
markAsPossibleError :: HasTypeGraph m info => VertexId -> m ()
markAsPossibleError vid = changeTypeGraph (TG.markAsPossibleError vid)

getMarkedPossibleErrors :: HasTypeGraph m info => m [VertexId]
getMarkedPossibleErrors = useTypeGraph TG.getMarkedPossibleErrors

unmarkPossibleErrors :: HasTypeGraph m info => m ()
unmarkPossibleErrors = changeTypeGraph TG.unmarkPossibleErrors
   
---------------------
------ EXTRA
   
theUnifyTerms :: HasTypeGraph m info => info -> Tp -> Tp -> m ()
theUnifyTerms info t1 t2 =
   do v1  <- addTermGraph t1
      v2  <- addTermGraph t2        
      addNewEdge (v1, v2) info
 
makeFixpointSubst :: HasTypeGraph m info => m FixpointSubstitution
makeFixpointSubst = 
   do xs <- makeSubstitution
      let list = [ (i, tp) | (VertexId i, tp) <- xs ]
      return (FixpointSubstitution (M.fromList list))
     