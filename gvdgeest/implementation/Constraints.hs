{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances  #-}
module Constraints
     ( Constraint     (..)
     , Substitutable  (..)
     , Rule
     )
where

import Data.Monoid

import CHRSolver

class (Eq v, Eq s, Monoid s) => Substitutable a v s | a -> v, a -> s where
  ftv         :: a -> [v]
  substitute  :: s -> a -> a

type Rule p s info = CHR (Constraint p info) s

-- The constraint language:
data Constraint p info  =  Prove      p
                        |  Assume     p
                        |  Reduction  p info [p]
                        deriving (Eq, Ord)

instance (Show p, Show info) => Show (Constraint p info) where
  show (Prove     p)       = "Prove(" ++ show p ++ ")"
  show (Assume    p)       = "Assume(" ++ show p ++ ")"
  show (Reduction p i ps)  = "Red(" ++ show p ++ ", " ++ show i ++ ", " ++ show ps ++ ")"
  
instance Substitutable p v s => Substitutable (Constraint p info) v s where
  ftv (Prove  p)          = ftv p
  ftv (Assume p)          = ftv p
  ftv (Reduction p _ ps)  = ftv p ++ concatMap ftv ps

  substitute s (Prove  p)          = Prove   (substitute s p)
  substitute s (Assume p)          = Assume  (substitute s p)  
  substitute s (Reduction p i ps)  = Reduction  (substitute s p) i (map (substitute s) ps)

instance (Matchable p s, Ord info) => Matchable (Constraint p info) s where
  match (Prove     p) (Prove     q) = match p q
  match (Assume    p) (Assume    q) = match p q
  match _           _               = Nothing

  subst s     (Prove  p)         = Prove  (subst s p)
  subst s     (Assume p)         = Assume (subst s p)
  subst s     (Reduction p i ps) = Reduction  (subst s p) i (map (subst s) ps)
