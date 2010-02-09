{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Constraint.Equality where

import Top.Types
import Top.Constraint
import Top.Constraint.Information
import Top.Interface.Substitution
import Top.Interface.TypeInference
import Data.List (union)

data EqualityConstraint info
   = Equality Tp Tp info
   
-- |The constructor of an equality constraint.
(.==.) :: Tp -> Tp -> info -> EqualityConstraint info
(.==.) = Equality

instance Show info => Show (EqualityConstraint info) where
   show (Equality t1 t2 info) = 
      let showInfo = "   : {" ++ show info ++ "}"
      in show t1 ++ " == " ++ show t2 ++ showInfo
      
instance Functor EqualityConstraint where
   fmap f (Equality t1 t2 info) =
      Equality t1 t2 (f info)
      
instance Substitutable (EqualityConstraint info) where
   sub |-> (Equality t1 t2 info) = Equality (sub |-> t1) (sub |-> t2) info
   ftv (Equality t1 t2 _)        = ftv t1 `union` ftv t2
   
instance ( TypeConstraintInfo info
         , HasSubst m info
         , HasTI m info
         ) => 
           Solvable (EqualityConstraint info) m
   where
      solveConstraint (Equality t1 t2 info) =
         unifyTerms (equalityTypePair (t1, t2) info) t1 t2
         
      checkCondition (Equality t1 t2 _) =
         do t1' <- applySubst t1
            t2' <- applySubst t2 
            (_ ,syns) <- getTypeSynonyms         
            return (expandType syns t1' == expandType syns t2')