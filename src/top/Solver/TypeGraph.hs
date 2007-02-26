{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Solver.TypeGraph where

import Top.Solver
import Top.Constraint
import Top.Constraint.Information
import Top.Implementation.General
import Top.Implementation.Basic
import Top.Implementation.Overloading
import Top.Implementation.TypeInference
import Top.Implementation.TypeGraph.Standard
import Top.Implementation.TypeGraphSubstitution
import Top.Implementation.TypeGraph.Heuristic
import Top.Monad.Select
-- for testing only
--import Top.Types
--import Top.Constraint.Equality

type TG  info = BasicMonad (TGS info)
type TGS info = And ( Fix (BasicState info) ) 
                        ( And ( Simple (TIState info) ) 
                              ( And ( Simple (TypeGraphState info) ) 
                                    ( Simple (OverloadingState info) )
                              )
                        )

solveTypeGraph :: (Solvable constraint (TG info), TypeConstraintInfo info) 
                     => TG info () -> SolveOptions -> [constraint] -> TG info (SolveResult info)
solveTypeGraph m options cs =
   do initialize cs options >> m
      onlySolveConstraints cs
      solveResult

typegraphConstraintSolver :: (TypeConstraintInfo info, Solvable constraint (TG info)) 
                                => PathHeuristics info -> ConstraintSolver constraint info
typegraphConstraintSolver hs = 
   let setHeuristics = deselect (modify (\tgs -> tgs { heuristics = hs }))
   in makeConstraintSolver (solveTypeGraph setHeuristics)

typegraphConstraintSolverDefault :: (TypeConstraintInfo info, Solvable constraint (TG info)) 
                                       => ConstraintSolver constraint info
typegraphConstraintSolverDefault = 
   makeConstraintSolver (solveTypeGraph (return ()))

---
{-
cs = [ TVar 0 .==. (TVar 1 .->. TVar 1) $ "a" 
     , TVar 0 .==. (TVar 2 .->. TVar 3) $ "b"
     , TVar 2 .==. intType $ "c" 
     , TVar 3 .==. boolType $ "d" 
     ]
     
test = let (a, b) = solve (solveOptions {uniqueCounter = 4}) cs typegraphConstraintSolverDefault
       in (b, errorsFromResult a) -}