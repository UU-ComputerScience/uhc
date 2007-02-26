{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.TypeGraph.Heuristic where

import Top.Implementation.TypeGraph.ClassMonadic
import Top.Implementation.TypeGraph.Basics
import Top.Implementation.TypeGraph.Path
import Top.Types
import Top.Solver
import Utils (internalError)

-----------------------------------------------------------------------------

type PathHeuristics info = Path (EdgeId, info) -> [Heuristic info]

newtype Heuristic  info = Heuristic (forall m . HasTypeGraph m info => HComponent m info)
data HasTypeGraph m info => Selector m info 
   = Selector       (String, (EdgeId, info) -> m (Maybe (Int, String, [EdgeId], info)))
   | SelectorList   (String, [(EdgeId, info)] -> m (Maybe (Int, String, [EdgeId], info)))

data HComponent m info 
     = Filter    String ([(EdgeId, info)] -> m [(EdgeId, info)])
     | Voting   [Selector m info]

getSelectorName :: (MonadWriter LogEntries m, HasTypeGraph m info) => Selector m info -> String
getSelectorName (Selector     (name,_)) = name
getSelectorName (SelectorList (name,_)) = name

resultsEdgeFilter :: (Eq a, Monad m) => ([a] -> a) -> String -> ((EdgeId,info) -> m a) -> HComponent m info
resultsEdgeFilter selector description function =
   Filter description $ \es -> 
   do tupledList <- let f tuple = 
                           do result <- function tuple
                              return (result, tuple)
                    in mapM f es
      let maximumResult 
            | null tupledList = internalError "Top.TypeGraph.Heuristics" "resultsEdgeFilter" "unexpected empty list" 
            | otherwise       = selector (map fst tupledList)
      return (map snd (filter ((maximumResult ==) . fst) tupledList))

maximalEdgeFilter :: (Ord a, Monad m) => String -> ((EdgeId,info) -> m a) -> HComponent m info
maximalEdgeFilter = resultsEdgeFilter maximum

minimalEdgeFilter :: (Ord a, Monad m) => String -> ((EdgeId,info) -> m a) -> HComponent m info
minimalEdgeFilter = resultsEdgeFilter minimum

edgeFilter :: Monad m => String -> ((EdgeId, info) -> m Bool) -> HComponent m info
edgeFilter description function = 
   Filter description $ \es -> 
      do xs <- filterM function es
         return (if (null xs) then es else xs)


-----------------------------------------------------------------------------

doWithoutEdges :: HasTypeGraph m info => [(EdgeId, info)] -> m result -> m result
doWithoutEdges xs computation = 
   case xs of 
      []   -> computation
      [e]  -> doWithoutEdge e computation
      e:es -> doWithoutEdge e (doWithoutEdges es computation)

doWithoutEdge :: HasTypeGraph m info => (EdgeId, info) -> m result -> m result
doWithoutEdge (edge, info) computation =
   do -- copy1 <- mapM showGroupOf [0..100]
      deleteEdge edge       
      result <- computation           
      addEdge edge info
      -- copy2 <- mapM showGroupOf [0..100]
      -- if copy1 /= copy2 then 
      --   error ("SAFETY check failed\n\n" ++ head [ x1++x2 | (x1, x2) <- zip copy1 copy2, x1 /= x2]) else
      return result

eqInfo2 :: (EdgeId, info) -> (EdgeId, info) -> Bool
eqInfo2 (EdgeId _ _ b1, _) (EdgeId _ _ b2, _) = b1 == b2

info2ToEdgeNr :: (EdgeId, info) -> EdgeNr
info2ToEdgeNr (EdgeId _ _ i, _) = i

-----------------------------------------------------------------------------

class HasTwoTypes a where
   getTwoTypes :: a -> (Tp, Tp)

getSubstitutedTypes :: (HasTypeGraph m info, HasTwoTypes info) => info -> m (Maybe Tp, Maybe Tp)
getSubstitutedTypes info = 
   do let (t1,t2) = getTwoTypes info
      mt1 <- substituteTypeSafe t1
      mt2 <- substituteTypeSafe t2
      return (mt1, mt2)
