{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Constraint.Polymorphism where

import Top.Types hiding (contextReduction)
import Top.Constraint
import Top.Constraint.Equality ( (.==.) )
import Top.Interface.Basic
import Top.Interface.TypeInference
import Top.Interface.Substitution
import Top.Interface.Qualification
import Top.Constraint.Information
import Data.List (union, intersperse)

data PolymorphismConstraint info
   = Generalize   Int (Tps, Tp) info
   | Instantiate  Tp (Sigma Predicates) info   -- or: explicit instance constraint
   | Skolemize    Tp (Tps, Sigma Predicates) info
   | Implicit     Tp (Tps, Tp) info
   
-- |The constructor of an instantiate (explicit instance) constraint.
(.::.) :: Tp -> Scheme Predicates -> info -> PolymorphismConstraint info
tp .::. s = Instantiate tp (SigmaScheme s)

instance Show info => Show (PolymorphismConstraint info) where
   show constraint = 
      case constraint of
         Generalize sv (monos, tp) info ->
            "s" ++ show sv ++ " := Generalize" ++ commaList [show (map TVar (ftv monos)), show tp] ++ showInfo info
         Instantiate tp sigma info ->
            show tp ++ " := Instantiate" ++ commaList [showQuantors sigma] ++ showInfo info            
         Skolemize tp (monos, sigma) info ->
            show tp ++ " := Skolemize" ++ commaList [show (map TVar (ftv monos)), showQuantors sigma] ++ showInfo info 
         Implicit t1 (monos, t2) info ->
            show t1 ++ " := Implicit" ++ commaList [show (map TVar (ftv monos)), show t2] ++ showInfo info
            
    where showInfo info = "   : {" ++ show info ++ "}"
          commaList = par . concat . intersperse ", "
          par s = "(" ++ s ++ ")"

instance Functor PolymorphismConstraint where
   fmap f constraint =
      case constraint of
         Generalize sv pair info      -> Generalize sv pair (f info)
         Instantiate tp sigma info    -> Instantiate tp sigma (f info)          
         Skolemize tp pair info       -> Skolemize tp pair (f info)
         Implicit t1 (monos, t2) info -> Implicit t1 (monos, t2) (f info)
         
instance Substitutable (PolymorphismConstraint info) where
   sub |-> typeConstraint =
      case typeConstraint of
         Generalize sv (monos, tp) info -> Generalize sv (sub |-> monos, sub |-> tp) info
         Instantiate tp sigma info      -> Instantiate (sub |-> tp) (sub |-> sigma) info         
         Skolemize tp pair info         -> Skolemize (sub |-> tp) (sub |-> pair) info
         Implicit t1 (monos, t2) info   -> Implicit (sub |-> t1) (sub |-> monos, sub |-> t2) info
         
   ftv typeConstraint =
      case typeConstraint of
         Generalize _ (monos, tp) _ -> ftv monos `union` ftv tp
         Instantiate tp sigma _     -> ftv tp `union` ftv sigma         
         Skolemize tp pair _        -> ftv tp `union` ftv pair
         Implicit t1 (monos, t2) _ -> ftv t1 `union` ftv monos `union` ftv t2
         
instance ( HasBasic m info 
         , HasTI m info
         , HasSubst m info
         , HasQual m info
         , PolyTypeConstraintInfo info
         ) => 
           Solvable (PolymorphismConstraint info) m where
   solveConstraint constraint =
      case constraint of

         Generalize var (m, tp) _ ->
            do -- makeConsistent -- done by contextReduction
               contextReduction
               m'     <- applySubst m
               tp'    <- applySubst tp
               changeQualifiers applySubst
               scheme <- generalizeWithQualifiers m' tp'
               storeTypeScheme var scheme
                     
         Instantiate tp sigma info ->
            do scheme <- findScheme sigma
               let newInfo = instantiatedTypeScheme scheme info
               qtp    <- instantiateM scheme
               let (ps, itp) = split qtp
               proveQualifiers (equalityTypePair (itp, tp) newInfo) ps
               pushConstraint $ liftConstraint
                  (itp .==. tp $ newInfo)

         Skolemize tp (monos, sigma) info -> 
            do scheme <- findScheme sigma
               let newInfo = skolemizedTypeScheme (monos, scheme) info
               qtp <- skolemizeFaked (equalityTypePair (tp, tp) $ newInfo) monos scheme
               let (ps, stp) = split qtp
               assumeQualifiers (equalityTypePair (tp, tp) newInfo) ps
               pushConstraint $ liftConstraint
                  (tp .==. stp $ newInfo) 
                  
         Implicit t1 (monos, t2) info ->
            do sv <- getUnique
               pushConstraints $ liftConstraints
                  [ Generalize sv (monos, t2) info
                  , Instantiate t1 (SigmaVar sv) info
                  ]