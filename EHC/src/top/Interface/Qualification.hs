{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Interface.Qualification where

import Top.Monad.Select
import Top.Monad.StateFix
import Top.Types hiding (contextReduction)
import Top.Interface.Substitution

------------------------------------------------------------------------
-- (I)  Class name and (dedicated) deselect function

data ClassQual = ClassQual

deQual :: (Embedded ClassQual (s (StateFixT s m)) t, Monad m) => Select t (StateFixT s m) a -> StateFixT s m a
deQual = deselectFor ClassQual

------------------------------------------------------------------------
-- (II)  Type class declaration


class Monad m => HasQual m info | m -> info where  

   -- general
   proveQualifier           :: info -> Predicate -> m ()
   assumeQualifier          :: info -> Predicate -> m ()
   changeQualifiers         :: (Predicate -> m Predicate) -> m ()
   
   allQualifiers            :: m [Predicate]
   generalizeWithQualifiers :: Tps -> Tp -> m (Scheme [Predicate])
   
   improveQualifiers        :: Bool -> m [(info, Tp, Tp)]
   improveQualifiersNormal  :: m [(info, Tp, Tp)]
   improveQualifiersFinal   :: m [(info, Tp, Tp)]
   simplifyQualifiers       :: m ()
   ambiguousQualifiers      :: m ()
   
    -- class environment
   setClassEnvironment :: ClassEnvironment -> m ()
   getClassEnvironment :: m ClassEnvironment
   
   -- default definitions   
   generalizeWithQualifiers monos = 
      return . generalize monos . ([] .=>.)
         
   improveQualifiers normal =
      if normal then improveQualifiersNormal else improveQualifiersFinal
     
   improveQualifiersNormal = 
      return []
      
   improveQualifiersFinal =
      return []
   
   simplifyQualifiers =
      return ()
   
   ambiguousQualifiers =
      return ()
         
------------------------------------------------------------------------
-- (III)  Instance for solver monad

instance ( Monad m
         , Embedded ClassQual (s (StateFixT s m)) t
         , HasQual (Select t (StateFixT s m)) info
         ) => 
           HasQual (StateFixT s m) info where

   proveQualifier  info p   = deQual (proveQualifier info p)
   assumeQualifier info p   = deQual (assumeQualifier info p)
   changeQualifiers f       = deQual (changeQualifiers (select . f))
   
   allQualifiers = deQual $ allQualifiers
   generalizeWithQualifiers monos tp = 
      deQual (generalizeWithQualifiers monos tp)
      
   improveQualifiers       = deQual . improveQualifiers
   improveQualifiersNormal = deQual $ improveQualifiersNormal
   improveQualifiersFinal  = deQual $ improveQualifiersFinal
   simplifyQualifiers      = deQual $ simplifyQualifiers
   ambiguousQualifiers     = deQual $ ambiguousQualifiers
   
   setClassEnvironment      = deQual . setClassEnvironment
   getClassEnvironment      = deQual $ getClassEnvironment
      
------------------------------------------------------------------------
-- (IV)  Additional functions

proveQualifiers :: HasQual m info => info -> Predicates -> m ()
proveQualifiers info = mapM_ (proveQualifier info)

assumeQualifiers :: HasQual m info => info -> Predicates -> m ()
assumeQualifiers info = mapM_ (assumeQualifier info)

contextReduction :: (HasSubst m info, HasQual m info) => m ()
contextReduction = 
   do makeSubstConsistent 
      changeQualifiers applySubst
      improveQualifiersFix True
      simplifyQualifiers
      
ambiguities :: (HasSubst m info, HasQual m info) => m ()
ambiguities = 
   do contextReduction
      improveQualifiersFix False
      ambiguousQualifiers
      
improveQualifiersFix :: (HasSubst m info, HasQual m info) => Bool -> m ()
improveQualifiersFix normal =
   do improvements <- improveQualifiers normal
      case improvements of
         [] -> return ()
         _  -> do mapM_ (\(info, t1, t2) -> unifyTerms info t1 t2) improvements
                  makeSubstConsistent
                  improveQualifiersFix normal