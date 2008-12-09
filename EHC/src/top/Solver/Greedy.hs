{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances  #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Solver.Greedy where

import Top.Implementation.General
import Top.Implementation.Basic
import Top.Implementation.TypeInference
import Top.Implementation.FastSubstitution
import Top.Implementation.SimpleSubstitution
import Top.Implementation.Overloading
import Top.Solver
import Top.Constraint
import Top.Constraint.Information
-- for testing only
-- import Top.Types
-- import Top.Constraint.Equality

type Greedy  info = BasicMonad (GreedyS info)
type GreedyS info = And ( Fix (BasicState info) ) 
                        ( And ( Simple (TIState info) ) 
                              ( And ( Simple (GreedyState info) ) 
                                    ( Simple (OverloadingState info) )
                              )
                        )

solveGreedy :: (Solvable constraint (Greedy info), TypeConstraintInfo info) =>
               SolveOptions -> [constraint] -> Greedy info (SolveResult info)
solveGreedy = solveConstraints

greedyConstraintSolver :: (TypeConstraintInfo info, Solvable constraint (Greedy info)) => ConstraintSolver constraint info
greedyConstraintSolver = makeConstraintSolver solveGreedy

--------------------------------

type GreedySimple  info = BasicMonad (GreedySimpleS info)
type GreedySimpleS info = And ( Fix (BasicState info) ) 
                              ( And ( Simple (TIState info) ) 
                                    ( And ( Simple (SimpleState info) ) 
                                          ( Simple (OverloadingState info) )
                                    )
                              )

solveSimple :: (Solvable constraint (GreedySimple info), TypeConstraintInfo info) =>
               SolveOptions -> [constraint] -> GreedySimple info (SolveResult info)
solveSimple = solveConstraints

greedySimpleConstraintSolver :: (TypeConstraintInfo info, Solvable constraint (GreedySimple info)) => ConstraintSolver constraint info
greedySimpleConstraintSolver = makeConstraintSolver solveSimple

--------------------------------
{-
cs :: [EqualityConstraint String]
cs = [ TVar 0 .==. (TVar 1 .->. TVar 1) $ "a" 
     , TVar 0 .==. (TVar 2 .->. TVar 3) $ "b" 
     , TVar 2 .==. intType $ "c" 
     , TVar 3 .==. boolType $ "d" 
     ]

test = let (a, b) = solve (solveOptions {uniqueCounter = 4}) cs greedyConstraintSolver
       in (b, errorsFromResult a)

test2 = let (a, b) = solve (solveOptions {uniqueCounter = 4}) cs greedySimpleConstraintSolver
        in (b, errorsFromResult a) -}