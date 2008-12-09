-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Solver.PartitionCombinator where

import Top.Types
import Top.Solver
import Top.Ordering.Tree
import qualified Data.Map as M

type Chunks constraint = [Chunk constraint]
type Chunk  constraint = (ChunkID, Tree constraint)
type ChunkID           = Int

solveChunkConstraints ::
   (M.Map Int (Scheme Predicates) -> constraint -> constraint) -> -- function to update the type scheme variables
   ConstraintSolver constraint info ->                                -- constraint solver to solve the constraints in a chunk
   (Tree constraint -> [constraint]) ->                               -- function to flatten the constraint tree
   Chunks constraint -> ConstraintSolver constraint info
   
solveChunkConstraints update (ConstraintSolver f) flattening chunks =
   ConstraintSolver (\os _ -> 
      let rec options [] = (emptyResult (uniqueCounter options), noLogEntries)
          rec options ((_, tree) : rest) =
             let constraintList = flattening tree
                 (result, entries)
                    | null constraintList = 
                         (emptyResult (uniqueCounter options), noLogEntries)
                    | otherwise = 
                         f options constraintList
                 newOption = options { uniqueCounter = uniqueFromResult result }
                 schemeMap = typeschemesFromResult result
                 newRest   = [ (chunkID, fmap (update schemeMap) t) | (chunkID, t) <- rest ]
                 (resultRec, entriesRec) = rec newOption newRest
             in (result `combineResults` resultRec, entries `mappend` entriesRec)
      in rec os chunks)