-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Solver.SwitchCombinator where

import Top.Interface.Basic
import Top.Solver

-- |The first solver is used to solve the constraint set. If this fails (at least one 
-- error is returned), then the second solver takes over.     
(|>>|) :: ConstraintSolver constraint info -> ConstraintSolver constraint info -> ConstraintSolver constraint info
ConstraintSolver f |>>| ConstraintSolver g = ConstraintSolver $ \options constraints ->
   let (result1, logs1) = f options constraints
       (result2, logs2) = g options constraints
       p (_, ErrorLabel s) = s /= "ambiguous predicate" -- temporary*
       p _                 = True
       switchLog = singleEntry 5 "CombinationSolver: Switching to second solver"
   in if null (filter p $ errorsFromResult result1)
         then (result1, logs1)
         else (result2, logs1 `mappend` switchLog `mappend` logs2) 

-- * For now, ignore the ambiguous predicate messages that are returned. They are not shown anyway.
-- These error messages are returned because of the mismatch between the constraints that are generated
-- by the Helium compiler, and the constraints as they are in the Top constraint solver.