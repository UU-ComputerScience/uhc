{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.TypeGraphSubstitution where

import Top.Implementation.TypeGraph.Class (TypeGraph)
import Top.Implementation.TypeGraph.ClassMonadic
import Top.Implementation.TypeGraph.Standard
import Top.Implementation.TypeGraph.Heuristic
import Top.Implementation.TypeGraph.Path
import Top.Implementation.TypeGraph.Basics
import Top.Interface.Substitution
import Top.Interface.Basic
import Top.Interface.TypeInference
import Top.Interface.Qualification
import Top.Implementation.TypeGraph.DefaultHeuristics
import Top.Implementation.TypeGraph.ApplyHeuristics
import Top.Monad.Select
import Top.Monad.StateFix
import Top.Solver
import Top.Implementation.General
import Top.Util.Embedding

------------------------------------------------------------------------
-- (I)  Algebraic data type

data TypeGraphState info = TypeGraphState 
   { typegraph  :: StandardTypeGraph info
   , heuristics :: PathHeuristics info
   }

------------------------------------------------------------------------
-- (II)  Instance of SolveState (Empty, Show)

instance Show info => SolveState (TypeGraphState info) where
   stateName _ = "Typegraph substitution state"
  
instance Show info => Show (TypeGraphState info) where
   show = show . typegraph

instance Show info => Empty (TypeGraphState info) where
   empty = TypeGraphState empty defaultHeuristics

------------------------------------------------------------------------
-- (III)  Embeddings

instance Embedded ClassSubst (TypeGraphState info) (TypeGraphState info)              where embedding = idE
instance Embedded ClassSubst (Simple (TypeGraphState info) x m) (TypeGraphState info) where embedding = fromFstSimpleE embedding

------------------------------------------------------------------------
-- (IV)  Instance declaration

instance ( Monad m
         , Embedded ClassSubst (s (StateFixT s m)) t
         , HasTG (Select t (StateFixT s m)) info
         ) => 
           HasTG (StateFixT s m) info where 

   withTypeGraph f = deSubst (withTypeGraph f)
         
instance ( MonadState s m
         , Embedded ClassSubst s (TypeGraphState info)
         ) => 
           HasTG (Select (TypeGraphState info) m) info where
           
   withTypeGraph f =
    do (a, new) <- gets (f . typegraph)
       modify (\tgs -> tgs { typegraph = new })
       return a  

instance ( HasBasic m info
         , HasTI m info
         , HasQual m info
         , HasTG m info
         , MonadWriter LogEntries m
         , Show info
         , MonadState s m
         , Embedded ClassSubst s (TypeGraphState info)
         ) => 
           HasSubst (Select (TypeGraphState info) m) info where

   makeSubstConsistent = 
      do hs <- gets heuristics
         select (removeInconsistencies hs)
      
   unifyTerms a b c  = select (theUnifyTerms a b c)
   findSubstForVar a = select (substituteVariable a)
   fixpointSubst     = select  makeFixpointSubst

removeInconsistencies :: HasTypeGraph m info => PathHeuristics info -> m ()
removeInconsistencies hs =
   do errs <- applyHeuristics hs
      mapM_ deleteEdge (concatMap fst errs)
      mapM_ (addLabeledError unificationErrorLabel . snd) errs
      if null errs
	    then -- everything is okay: no errors were found.
	       unmarkPossibleErrors
        else -- Bug patch 3 february 2004
	         -- safety first: check whether *everything* is really removed. 
	      removeInconsistencies hs