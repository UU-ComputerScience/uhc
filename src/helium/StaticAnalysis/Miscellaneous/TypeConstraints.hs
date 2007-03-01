{-# OPTIONS -fallow-undecidable-instances #-}
{-| Module      :  TypeConstraints
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
    
    The type constraints used by the Helium compiler (all derived from the
	basic constraints that are supplied by the Top framework). Some constraints
	are lifted to work on finite maps as well.
-}

module Helium.StaticAnalysis.Miscellaneous.TypeConstraints where

import Top.Constraint
import Top.Constraint.Equality hiding ((.==.))
import Top.Constraint.Qualifier
import Top.Constraint.Polymorphism hiding ((.::.))
import Top.Constraint.Information
import Top.Interface.Basic
import Top.Interface.Substitution
import Top.Interface.TypeInference
import Top.Interface.Qualification
import Top.Types
import qualified Data.Map as M

type TypeConstraints info = [TypeConstraint info]
data TypeConstraint  info
   = TC1 (EqualityConstraint info)
   | TC2 (ExtraConstraint info)
   | TC3 (PolymorphismConstraint info)
   | TCOper String (forall m . HasSubst m info => m ())

instance (HasBasic m info, HasTI m info, HasSubst m info, HasQual m info, PolyTypeConstraintInfo info) 
            => Solvable (TypeConstraint info) m where 
   solveConstraint (TC1 c)      = solveConstraint c
   solveConstraint (TC2 c)      = solveConstraint c
   solveConstraint (TC3 c)      = solveConstraint c
   solveConstraint (TCOper _ f) = f
   checkCondition  (TC1 c)      = checkCondition c
   checkCondition  (TC2 c)      = checkCondition c
   checkCondition  (TC3 c)      = checkCondition c
   checkCondition  (TCOper _ _) = return True

instance Show info => Show (TypeConstraint info) where
   show (TC1 c)      = show c
   show (TC2 c)      = show c
   show (TC3 c)      = show c
   show (TCOper s _) = s

instance Substitutable (TypeConstraint info) where
   sub |-> (TC1 c) = TC1 (sub |-> c)
   sub |-> (TC2 c) = TC2 (sub |-> c)
   sub |-> (TC3 c) = TC3 (sub |-> c)
   sub |-> tc     = tc
   ftv (TC1 c)    = ftv c
   ftv (TC2 c)    = ftv c
   ftv (TC3 c)    = ftv c
   ftv _          = []

------------

polySubst :: M.Map Int (Scheme Predicates) -> TypeConstraint info -> TypeConstraint info
polySubst schemeMap tc =
   case tc of
      TC3 (Instantiate tp sigma info)        -> TC3 (Instantiate tp (f sigma) info)
      TC3 (Skolemize tp (monos, sigma) info) -> TC3 (Skolemize tp (monos, f sigma) info)
      _                                      -> tc
      
 where
   f :: Sigma Predicates -> Sigma Predicates
   f sigma = 
      case sigma of 
         SigmaVar i -> maybe sigma SigmaScheme (M.lookup i schemeMap)
         _          -> sigma
             
spreadFunction :: TypeConstraint info -> Maybe Int
spreadFunction tc =
   case tc of
      TC1 (Equality t1 t2 info)         -> spreadFromType t2
      TC3 (Instantiate tp ts info)      -> spreadFromType tp
      TC3 (Skolemize tp ts info)        -> spreadFromType tp
      TC3 (Implicit t1 (ms, t2) info)   -> spreadFromType t1
      _                                 -> Nothing

spreadFromType :: Tp -> Maybe Int
spreadFromType (TVar i) = Just i
spreadFromType _        = Nothing

------------------------------------------------------------------------------
-- Lifted constructors

infix 3 .==., .===., .::., .:::., !::!, !:::!, .<=., .<==., !<=!, !<==!

lift combinator = 
    \as bs cf -> 
       let constraints = concat (M.elems (M.intersectionWith f as bs))
           rest        = bs M.\\ as
           f a list    = [ (a `combinator` b) (cf name) | (name,b) <- list ]
       in (constraints, rest)
      
(.==.) :: Show info => Tp -> Tp -> info -> TypeConstraint info
(t1 .==. t2) info = TC1 (Equality t1 t2 info)
    
(.===.) :: (Show info, Ord key) => M.Map key Tp -> M.Map key [(key,Tp)] -> (key -> info) -> ([TypeConstraint info], M.Map key [(key,Tp)])
(.===.) = lift (.==.)

(.::.) :: Show info => Tp -> TpScheme -> info -> TypeConstraint info
tp .::. ts = tp .<=. SigmaScheme ts

(.:::.) :: (Show info, Ord key) => M.Map key TpScheme -> M.Map key [(key,Tp)] -> (key -> info) -> ([TypeConstraint info], M.Map key [(key,Tp)])  
(.:::.) = lift (flip (.::.))

(!::!) :: Tp -> TpScheme -> Tps -> info -> TypeConstraint info
(tp !::! ts) monos info = TC3 (Skolemize tp (monos, SigmaScheme ts) info)

(!:::!) :: (Show info, Ord key) => M.Map key TpScheme -> M.Map key Tp -> Tps -> (Tps -> key -> key -> info) -> ([TypeConstraint info], M.Map key Tp)  
(as !:::! bs) monos info =
   let op key tp (cs, fm) =
          case M.lookup key as of
             Just ts -> 
                let -- the key of the type scheme (equal, but may have a different range). 
                    key' = head (filter (==key) (M.keys as)) {- this is the other name -}
                in ((tp !::! ts) monos (info monos key key') : cs, fm)
             Nothing -> (cs, M.insert key tp fm)
   in M.foldWithKey op ([], M.empty) bs

(.<=.) :: Show info => Tp -> Sigma Predicates -> info -> TypeConstraint info
(tp .<=. ts) info = TC3 (Instantiate tp ts info)

(.<==.) :: (Show info, Ord key) => M.Map key (Sigma Predicates) -> M.Map key [(key,Tp)] -> (key -> info) -> ([TypeConstraint info], M.Map key [(key,Tp)])  
(.<==.) = lift (flip (.<=.))
     
-- the old implicit instance constraint
(!<=!) :: Show info => Tps -> Tp -> Tp -> info -> TypeConstraint info
(!<=!) ms t1 t2 info = TC3 (Implicit t1 (ms, t2) info)

(!<==!) :: (Show info, Ord key) => Tps -> M.Map key Tp -> M.Map key [(key,Tp)] -> (key -> info) -> ([TypeConstraint info], M.Map key [(key,Tp)])
(!<==!) ms = lift (flip ((!<=!) ms)) 

genConstraints :: Tps -> (key -> info) -> [(Int, (key, Tp))] -> TypeConstraints info
genConstraints monos infoF =
   let f (sv, (key, tp)) = TC3 (Generalize sv (monos, tp) (infoF key)) 
   in map f

predicate :: Predicate -> info -> TypeConstraint info
predicate p cinfo = TC2 (Prove p cinfo)