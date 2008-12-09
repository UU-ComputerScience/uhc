{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
--
-- A data type to represent constraints in general, and a type class for
-- constraints that are solvable.
--
-----------------------------------------------------------------------------

module Top.Constraint where

import Top.Types (Substitutable(..))

type Constraints m = [Constraint m]
data Constraint  m = 
   forall c . (Show c, Substitutable c) => Constraint c (c -> m ()) (c -> m Bool)

-- |A constraint is solvable if it knows how it can be solved in a certain
-- state (a monadic operation), if it can check afterwards whether the final
-- state satisfies it, and when it can be shown.
class (Show c, Substitutable c, Monad m) => Solvable c m where 
   solveConstraint :: c -> m ()
   checkCondition  :: c -> m Bool
   
   -- default definition
   checkCondition _ = return True

instance Show (Constraint m) where 
   show (Constraint c _ _) = show c

instance Substitutable (Constraint m) where
   ftv (Constraint c _ _)     = ftv c
   sub |-> (Constraint c f g) = Constraint (sub |-> c) f g

instance Monad m => Solvable (Constraint m) m where
   solveConstraint (Constraint c f _) = f c
   checkCondition  (Constraint c _ g) = g c

-- |Lifting a constraint to the Constraint data type. Every instance of
-- the Solvable type class can be lifted.
liftConstraint :: Solvable c m => c -> Constraint m
liftConstraint c = Constraint c solveConstraint checkCondition

liftConstraints :: Solvable c m => [c] -> Constraints m
liftConstraints = map liftConstraint

mapConstraint :: (forall a . m1 a -> m2 a) -> Constraint m1 -> Constraint m2
mapConstraint t (Constraint c f g) = Constraint c (t . f) (t . g)

newtype Operation m = Op_ String

operation :: Monad m => String -> m () -> Constraint m
operation s m = Constraint (Op_ s) (const m) (const (return True))

instance Show (Operation m) where
   show (Op_ s) = "<" ++ s ++ ">"

instance Substitutable (Operation m) where
   ftv _    = []
   _ |-> op = op

-- |If both constraints of type 'a' and 'b' can be solved in a Monad 'm', then
-- 'Either a b' constraints can also be solved in this monad.
instance (Solvable a m, Solvable b m) => Solvable (Either a b) m where
   solveConstraint = either solveConstraint solveConstraint
   checkCondition  = either checkCondition  checkCondition

-- |The data type ConstraintSum is similar to the (standard) Either data type.    
-- However, its Show instance is slightly different as the name of the constructor
-- is not shown.
data ConstraintSum f g info 
   = SumLeft  (f info) 
   | SumRight (g info)

instance (Show (f info), Show (g info)) => Show (ConstraintSum f g info) where
   show = constraintSum show show

instance (Functor f, Functor g) => Functor (ConstraintSum f g) where
   fmap f = constraintSum (SumLeft . fmap f) (SumRight . fmap f)

instance (Substitutable (f info), Substitutable (g info)) => Substitutable (ConstraintSum f g info) where
   (|->) sub = constraintSum (SumLeft . (sub |->)) (SumRight . (sub |->))
   ftv       = constraintSum ftv ftv

instance (Solvable (f info) m, Solvable (g info) m) => Solvable (ConstraintSum f g info) m where
   solveConstraint = constraintSum solveConstraint solveConstraint
   checkCondition  = constraintSum checkCondition  checkCondition

-- |Similar to the 'either' function.
constraintSum :: (f info -> c) -> (g info -> c) -> ConstraintSum f g info -> c
constraintSum f _ (SumLeft a)  = f a
constraintSum _ f (SumRight b) = f b