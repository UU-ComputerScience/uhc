{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Constraint.Information where

import Top.Types

instance TypeConstraintInfo ()
instance PolyTypeConstraintInfo ()

instance TypeConstraintInfo String
instance PolyTypeConstraintInfo String

class Show info => TypeConstraintInfo info where
   equalityTypePair     :: (Tp, Tp)  -> info -> info
   ambiguousPredicate   :: Predicate -> info -> info
   unresolvedPredicate  :: Predicate -> info -> info
   predicateArisingFrom :: (Predicate, info) -> info -> info
   parentPredicate      :: Predicate -> info -> info
   escapedSkolems       :: [Int]     -> info -> info
   neverDirective       :: (Predicate, info) -> info -> info
   closeDirective       :: (String, info)    -> info -> info
   disjointDirective    :: (String, info) -> (String, info) -> info -> info
   
   -- default definitions
   equalityTypePair _     = id
   ambiguousPredicate _   = id
   unresolvedPredicate _  = id
   predicateArisingFrom _ = id
   parentPredicate _      = id
   escapedSkolems _       = id
   neverDirective _       = id
   closeDirective _       = id
   disjointDirective _ _  = id
   
class TypeConstraintInfo info => PolyTypeConstraintInfo info where
   instantiatedTypeScheme :: Forall (Qualification Predicates Tp) -> info -> info
   skolemizedTypeScheme   :: (Tps, Forall (Qualification Predicates Tp)) -> info -> info

   -- default definition
   instantiatedTypeScheme _  = id
   skolemizedTypeScheme _    = id