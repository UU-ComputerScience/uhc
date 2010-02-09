{-| Module      :  ListOfHeuristics
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    A list of all type graph heuristics that is used.
-}

module Helium.StaticAnalysis.Heuristics.ListOfHeuristics (listOfHeuristics) where

import Helium.Compiler.Args (Option(..))
import Helium.StaticAnalysis.Miscellaneous.ConstraintInfo
import Helium.StaticAnalysis.Heuristics.HeuristicsInfo () -- instances
import Top.Types
import Top.Implementation.TypeGraph.Heuristic
import Top.Implementation.TypeGraph.DefaultHeuristics
-- import Helium.StaticAnalysis.Heuristics.RepairSystem (repairSystem)
import Top.Implementation.TypeGraph.ClassMonadic
import Helium.StaticAnalysis.Heuristics.RepairHeuristics
import Helium.StaticAnalysis.Heuristics.UnifierHeuristics
import Helium.StaticAnalysis.Heuristics.OnlyResultHeuristics
import Helium.StaticAnalysis.Heuristics.TieBreakerHeuristics

-- temporary
import Top.Implementation.TypeGraph.Path
import Data.Maybe
import Top.Implementation.TypeGraph.Basics

listOfHeuristics :: [Option] -> Siblings -> Path (EdgeId, ConstraintInfo) -> [Heuristic ConstraintInfo]
listOfHeuristics options siblings path =
   let is = [ makeEdgeNr i | SelectConstraintNumber i <- options ]
   in [ selectConstraintNumbers is | not (null is) ]
   ++
   [ avoidForbiddenConstraints       -- remove constraints that should NEVER be reported
   , highParticipation 0.95 path
   , phaseFilter                     -- phasing from the type inference directives
   ] ++
   -- Repair system is disabled
   -- [ repairSystem | NoRepairHeuristics `notElem` options
   -- ] ++
   [ Heuristic (Voting (
        [ siblingFunctions siblings
        , siblingLiterals
        , applicationHeuristic 
        , variableFunction         -- Similar to applicationHeuristic, works in absence of application node
        , tupleHeuristic                -- Similar to applicationHeuristic, but for tuples
        , fbHasTooManyArguments
        , constraintFromUser path  -- From .type files
        , unaryMinus (Overloading `elem` options)
        ] ++
        [ similarNegation | Overloading `notElem` options ] ++   -- Avoid mix-up of -. and - if non-overloaded
        [ unifierVertex   | UnifierHeuristics `elem` options ]))
   | NoRepairHeuristics `notElem` options   -- All selectors are turned off when NoRepairHeuristics is on.
   ] ++
   [ inPredicatePath | Overloading `elem` options ] ++
   [ avoidApplicationConstraints
   , avoidNegationConstraints
   -- , typeVariableInvolved {- I am not convinced yet. Bastiaan -}
   , avoidTrustedConstraints
   , avoidFolkloreConstraints
   , firstComeFirstBlamed
   ]

-- Never report a constraint which is highly trusted
-- (even if this means that you have to report multiple errors)
-- This should be the first heuristic that is applied 
avoidForbiddenConstraints :: Heuristic ConstraintInfo
avoidForbiddenConstraints = Heuristic (
   let f (_, info) = return (not (isHighlyTrusted info))
   in edgeFilter "Avoid forbidden constraints" f)

-- two more heuristics for the Type Inference Directives
-- (move to another module?)
phaseFilter :: Heuristic ConstraintInfo
phaseFilter = Heuristic (
   let f (_, info) = return (phaseOfConstraint info)
   in maximalEdgeFilter "Highest phase number" f)

constraintFromUser :: HasTypeGraph m ConstraintInfo => Path (EdgeId, ConstraintInfo) -> Selector m ConstraintInfo
constraintFromUser path = 
   SelectorList ("Constraints from .type file", helper path)

 where
   helper path edges = 
      let
          bestEdge = rec path
          edgeNrs  = [ i | (EdgeId _ _ i, _) <- edges ]
 
          rec path =
             case path of
                x :|: y -> f min (rec x) (rec y)
                x :+: y -> f max (rec x) (rec y)
                Step (EdgeId _ _ cNR, info) |  isJust (maybeUserConstraint info) && cNR `elem` edgeNrs 
                        -> Just cNR
                _       -> Nothing
	 
          f :: (a -> a -> a) -> Maybe a -> Maybe a -> Maybe a	    
          f g ma mb = 
             case (ma, mb) of
                (Just a, Just b) -> Just (g a b)
                (Nothing, _    ) -> mb
                _                -> ma
      in 
         case [ tuple | tuple@(EdgeId _ _ cNR, _) <- edges, Just cNR == bestEdge ] of
            [] -> return Nothing
            (edgeID, info):_ -> 
	       let (groupID, number) = maybe (0, 0) id (maybeUserConstraint info)
	           otherEdges = let p info =
		                       case maybeUserConstraint info of
				          Just (a, b) -> a == groupID && b > number
					  Nothing     -> False
		                in [ e | (e, i) <- edges, p i ] -- perhaps over all edges!
	       in return . Just $
	             (8, "constraints from .type file", edgeID:otherEdges, info)