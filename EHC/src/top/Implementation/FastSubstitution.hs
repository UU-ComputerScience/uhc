{-# OPTIONS -fglasgow-exts -fallow-overlapping-instances -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.FastSubstitution where 

import Top.Types
import Top.Implementation.General
import Top.Util.Embedding
import Top.Monad.Select
import Top.Interface.TypeInference
import Top.Interface.Basic
import Top.Interface.Substitution
import qualified Data.Map as M
import Utils (internalError)

------------------------------------------------------------------------
-- (I)  Algebraic data type

newtype GreedyState info = GreedyState { unGS :: FixpointSubstitution }

------------------------------------------------------------------------
-- (II)  Instance of SolveState (Empty, Show)

instance SolveState (GreedyState info) where
   stateName _ = "Greedy Substitution State"
  
instance Show (GreedyState info) where
   show gs = let FixpointSubstitution hs = unGS gs in show hs -- _ = "<Greedy Substitution>"

instance Empty (GreedyState info) where
   empty = GreedyState (FixpointSubstitution M.empty)

------------------------------------------------------------------------
-- (III)  Embeddings

instance Embedded ClassSubst (GreedyState info) (GreedyState info)                  where embedding = idE
instance Embedded ClassSubst (Simple (GreedyState info) m b) (GreedyState info) where embedding = fromFstSimpleE embedding

------------------------------------------------------------------------
-- (IV)  Instance declaration

instance ( MonadState s m
         , HasBasic m info
         , HasTI m info
         , Embedded ClassSubst s (GreedyState info)
         ) => 
           HasSubst (Select (GreedyState info) m) info where

   makeSubstConsistent = return ()
   findSubstForVar i   = gets (lookupInt i . unGS)
   fixpointSubst       = gets unGS

   unifyTerms info t1 t2 =
      do t1'      <- applySubst t1
         t2'      <- applySubst t2
         synonyms <- select getTypeSynonyms

         case mguWithTypeSynonyms synonyms t1' t2' of        
            Left _           -> select (addLabeledError unificationErrorLabel info)
            Right (used,sub) -> 
               let mutp = equalUnderTypeSynonyms synonyms (sub |-> t1') (sub |-> t2') 
                   utp = maybe err id mutp
                   err = internalError "Top.Solvers.GreedySubst" "greedyState" "types not unifiable"
                   f (FixpointSubstitution fm) =
                         FixpointSubstitution (M.union (M.fromList [ (i, lookupInt i sub) | i <- dom sub ]) fm)
                   g = writeExpandedType synonyms t2 utp 
                     . writeExpandedType synonyms t1 utp 
                   h = if used then g . f else f
               in modify (GreedyState . h . unGS)

-- The key idea is as follows:
-- try to minimize the number of expansions by type synonyms.
-- If a type is expanded, then this should be recorded in the substitution. 
-- Invariant of this function should be that "atp" (the first type) can be
-- made equal to "utp" (the second type) with a number of type synonym expansions             
writeExpandedType :: OrderedTypeSynonyms -> Tp -> Tp -> FixpointSubstitution ->  FixpointSubstitution
writeExpandedType synonyms = writeTypeType where

   writeTypeType :: Tp -> Tp -> FixpointSubstitution -> FixpointSubstitution
   writeTypeType atp utp original = 
      case (leftSpine atp,leftSpine utp) of        
         ((TVar i,[]),_) -> 
            writeIntType i utp original
         
         ((TCon s,as),(TCon t,bs)) 
            | s == t && not (isPhantomTypeSynonym synonyms s) -> 
                 foldr (uncurry writeTypeType) original (zip as bs)                   
         
         ((TCon _, _),_) -> 
            case expandTypeConstructorOneStep (snd synonyms) atp of
               Just atp' -> writeTypeType atp' utp original
               Nothing   -> internalError "Top.Solvers.GreedySubst" "writeTypeType" ("inconsistent types(1)" ++ show (atp, utp))      
    
         _ -> internalError "Top.Solvers.GreedySubst" "writeTypeType" ("inconsistent types(2)" ++ show (atp, utp))  
      
   writeIntType :: Int -> Tp -> FixpointSubstitution -> FixpointSubstitution     
   writeIntType i utp original@(FixpointSubstitution fm) = 
      case M.lookup i fm of 
         
         Nothing  -> 
            case utp of
               TVar j | i == j -> original
               _               -> FixpointSubstitution (M.insert i utp fm)
               
         Just atp ->
            case (leftSpine atp,leftSpine utp) of
               ((TVar j,[]),_) -> writeIntType j utp original
               ((TCon s,as),(TCon t,bs)) | s == t -> foldr (uncurry writeTypeType) original (zip as bs)
               ((TCon _, _), _) -> case expandTypeConstructorOneStep (snd synonyms) atp of
                                      Just atp' -> writeIntType i utp (FixpointSubstitution (M.insert i atp' fm))
                                      Nothing   -> -- FIX!!!   HERSCHRIJVEN! 
                                                   -- de volgende situatie trad op:
                                                   --    utp=Categorie, atp = [Char]
                                                   --  met type Categorie = String
                                         case expandTypeConstructorOneStep (snd synonyms) utp of
                                           Just utp' -> 
                                              writeIntType i atp (FixpointSubstitution (M.insert i utp' fm))
                                           Nothing ->
                                               internalError "Top.Solvers.GreedySubst" "writeIntType" ("inconsistent types(1)" ++ show (i, utp, atp))
               _                -> internalError "Top.Solvers.GreedySubst" "writeIntType" "inconsistent types(2)"