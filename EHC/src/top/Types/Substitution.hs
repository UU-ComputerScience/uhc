{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
--
-- This module contains a data type to represent (plain) types, some basic 
-- functionality for types, and an instance for Show.
--
-----------------------------------------------------------------------------

module Top.Types.Substitution where

import Top.Types.Primitive
import Data.List (union, (\\), nub)
import qualified Data.Map as M
import qualified Data.Set as S
import Top.Util.IntErr (internalError)

----------------------------------------------------------------------
-- * Substitutions and substitutables

infix 4 |->

class Substitution s where
   lookupInt   :: Int -> s -> Tp         -- lookup the type of a type variable in a substitution   
   removeDom   :: [Int] -> s -> s        -- remove from the domain of the substitution
   restrictDom :: [Int] -> s -> s        -- restrict the domain of the substitution
   dom         :: s -> [Int]             -- domain of substitution
   cod         :: s -> Tps               -- co-domain of substitution
   
class Substitutable a where
   (|->)       :: Substitution s => s -> a -> a   -- apply substitution
   ftv         :: a -> [Int]                      -- free type variables

-- |The next type variable that is not free (default is zero)
nextFTV :: Substitutable a => a -> Int
nextFTV a = case ftv a of
               [] -> 0
               is -> maximum is + 1

----------------------------------------------------------------------
-- * Substitution instances 

-- |A substitution represented by a finite map.
type MapSubstitution = M.Map Int Tp

instance Substitution MapSubstitution where

   lookupInt i    = M.findWithDefault (TVar i) i
   removeDom      = flip (foldr M.delete)
   restrictDom is = let set = S.fromList is 
                    in M.filterWithKey (\i _ -> S.member i set)
   
   dom = M.keys
   cod = M.elems 

emptySubst :: MapSubstitution
emptySubst = M.empty

-- |Compose two finite map substitutions: safe.
-- Note for 'M.union': bindings in right argument shadow those in the left
(@@) :: MapSubstitution -> MapSubstitution -> MapSubstitution
fm1 @@ fm2 = fm1 `M.union` M.map (\t -> fm1 |-> t) fm2  

-- |Compose two finite map substitutions: quick and dirty!
(@@@) :: MapSubstitution -> MapSubstitution -> MapSubstitution
(@@@) = M.union 

singleSubstitution :: Int -> Tp -> MapSubstitution
singleSubstitution = M.singleton

listToSubstitution :: [(Int,Tp)] -> MapSubstitution
listToSubstitution = M.fromList

-- |A fixpoint is computed when looking up the target of a type variable in this substitution. 
-- Combining two substitutions is cheap, whereas a lookup is more expensive than the 
-- normal finite map substitution.
newtype FixpointSubstitution = FixpointSubstitution (M.Map Int Tp)

instance Substitution FixpointSubstitution where
   lookupInt i original@(FixpointSubstitution fm) = 
      case M.lookup i fm of
         Just tp | tp == TVar i -> TVar i
                 | otherwise    -> original |-> tp
         Nothing                -> TVar i
   removeDom   is (FixpointSubstitution fm) = FixpointSubstitution (M.filterWithKey (\i _ -> i `notElem` is) fm)
   restrictDom is (FixpointSubstitution fm) = let js = M.keys fm \\ is
                                              in FixpointSubstitution (M.filterWithKey (\i _ -> i `notElem` js) fm)
   dom (FixpointSubstitution fm) = M.keys fm
   cod (FixpointSubstitution fm) = M.elems fm

-- |The empty fixpoint substitution 
emptyFPS :: FixpointSubstitution
emptyFPS = FixpointSubstitution M.empty
 
-- |Combine two fixpoint substitutions that are disjoint
disjointFPS :: FixpointSubstitution -> FixpointSubstitution -> FixpointSubstitution
disjointFPS (FixpointSubstitution fm1) (FixpointSubstitution fm2) = 
   let notDisjoint = internalError "Substitution" "disjointFPS" "the two fixpoint substitutions are not disjoint"
   in FixpointSubstitution (M.unionWith notDisjoint fm1 fm2)   
 
----------------------------------------------------------------------
-- * Wrapper for substitutions

wrapSubstitution :: Substitution substitution => substitution -> WrappedSubstitution                                     
wrapSubstitution substitution = 
   WrappedSubstitution substitution 
      ( lookupInt
      , removeDom
      , restrictDom
      , dom
      , cod
      )

data WrappedSubstitution = 
   forall a . Substitution a => 
      WrappedSubstitution a 
         ( Int -> a -> Tp   
         , [Int] -> a -> a
         , [Int] -> a -> a
         , a -> [Int]
         , a -> Tps
         )

instance Substitution WrappedSubstitution where
   lookupInt   i  (WrappedSubstitution x (f,_,_,_,_)) = f i x
   removeDom   is (WrappedSubstitution x (_,f,_,_,_)) = wrapSubstitution (f is x)
   restrictDom is (WrappedSubstitution x (_,_,f,_,_)) = wrapSubstitution (f is x)
   dom            (WrappedSubstitution x (_,_,_,f,_)) = f x
   cod            (WrappedSubstitution x (_,_,_,_,f)) = f x

----------------------------------------------------------------------
-- * Substitutables instances
   
instance Substitutable Tp where
   sub |-> tp = 
      case tp of 
         TVar i     -> lookupInt i sub
         TCon _     -> tp
         TApp t1 t2 -> TApp (sub |-> t1) (sub |-> t2) 
   ftv tp = 
      case tp of
         TVar i     -> [i]
         TCon _     -> []
         TApp t1 t2 -> ftv t1 `union` ftv t2

instance Substitutable a => Substitutable [a] where
   sub |-> as = map (sub |->) as
   ftv as     = foldr union [] (map ftv as)

instance (Substitutable a, Substitutable b) => Substitutable (a, b) where
   sub |-> (a, b) = (sub |-> a, sub |-> b)
   ftv (a, b)     = ftv a `union` ftv b

instance Substitutable a => Substitutable (Maybe a) where
   sub |-> ma  = maybe Nothing (Just . (sub |->)) ma
   ftv         = maybe [] ftv

instance (Substitutable a, Substitutable b) => Substitutable (Either a b) where
   sub |-> x = either (Left . (sub |->)) (Right . (sub |->)) x
   ftv       = either ftv ftv

freezeFTV :: Substitutable a => a -> a
freezeFTV a =
   let sub = listToSubstitution [ (i, TCon ('_':show i)) | i <- ftv a ]
   in sub |-> a 
   
allTypeVariables :: HasTypes a => a -> [Int]
allTypeVariables = ftv . getTypes

allTypeConstants :: HasTypes a => a -> [String]
allTypeConstants = 
   let f (TVar _)   = []
       f (TCon s)   = [s]
       f (TApp l r) = f l ++ f r
   in nub . concatMap f . getTypes