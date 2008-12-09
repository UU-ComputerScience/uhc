{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Implementation.SimpleSubstitution where 

import Top.Types
import Top.Implementation.General
import Top.Interface.Substitution
import Top.Interface.TypeInference
import Top.Interface.Basic
import Top.Monad.Select
import Top.Util.Embedding
import Top.Util.Empty

------------------------------------------------------------------------
-- (I)  Algebraic data type

newtype SimpleState info = SimpleState { unSS :: MapSubstitution }

------------------------------------------------------------------------
-- (II)  Instance of SolveState (Empty, Show)

instance SolveState (SimpleState info) where 
   stateName _ = "Simple Substitution State"

instance Show (SimpleState info) where
   show _ = "<Simple Substitution>"

instance Empty (SimpleState info) where
   empty = SimpleState emptySubst

------------------------------------------------------------------------
-- (III)  Embeddings

instance Embedded ClassSubst (SimpleState info) (SimpleState info)              where embedding = idE
instance Embedded ClassSubst (Simple (SimpleState info) x m) (SimpleState info) where embedding = fromFstSimpleE embedding

------------------------------------------------------------------------
-- (IV)  Instance declaration

instance ( MonadState s m
         , HasBasic m info
         , HasTI m info
         , Embedded ClassSubst s (SimpleState info)
         ) => 
           HasSubst (Select (SimpleState info) m) info where
 
    makeSubstConsistent = 
        return ()

    unifyTerms info t1 t2 =
        do synonyms <- select getTypeSynonyms
           t1'      <- applySubst t1
           t2'      <- applySubst t2
           case mguWithTypeSynonyms synonyms t1' t2' of
              Right (_, sub) -> 
                 modify (SimpleState . (sub @@) . unSS)
              Left _ -> select (addLabeledError unificationErrorLabel info)

    findSubstForVar i =   
       gets (lookupInt i . unSS)

    fixpointSubst = 
        gets (FixpointSubstitution . unSS)