{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
--
-- Additional state information that should be stored in order to perform
-- type inference.
--
-----------------------------------------------------------------------------

module Top.Implementation.TypeInference where

import Top.Types
import Top.Implementation.General
import Top.Interface.TypeInference
import Data.List
import qualified Data.Map as M
import Top.Util.Empty
import Top.Monad.Select
import Control.Monad.State
import Utils (internalError)

------------------------------------------------------------------------
-- (I)  Algebraic data type

data TIState info = TIState
   { counter             :: Int                         -- ^ A counter for fresh type variables
   , synonyms            :: OrderedTypeSynonyms         -- ^ All known type synonyms
   , skolems             :: [([Int], info, Tps)]        -- ^ List of skolem constants
   , schemeMap           :: M.Map Int (Scheme Predicates)  -- ^ Type scheme map
   }

------------------------------------------------------------------------
-- (II)  Instance of SolveState (Empty, Show)

instance Show info => SolveState (TIState info) where 
   stateName _ = "Type Inference State"
   
-- |An empty type inference state.
instance Show info => Empty (TIState info) where
   empty = TIState
      { counter             = 0
      , synonyms            = noOrderedTypeSynonyms
      , skolems             = []
      , schemeMap           = M.empty
      }

instance Show info => Show (TIState info) where
   show s = unlines [ "counter: " ++ show (counter s)
                    , "skolem constants: " ++ show (skolems s)
                    , "synonyms: " ++ concat [ t++"; " | t <- M.keys (fst (synonyms s)) ]
                    , let f (i, x) = "   s"++show i++" = "++show x
                      in unlines $ map f (M.toList $ schemeMap s)
                    ] 

------------------------------------------------------------------------
-- (III)  Embeddings

instance Embedded ClassTI (Simple (TIState info) x m) (TIState info)  where embedding = fstSimpleE

------------------------------------------------------------------------
-- (IV)  Instance declaration

instance ( MonadState s m
         , Embedded ClassTI s (TIState info)
         ) =>
           HasTI (Select (TIState info) m) info where
           
   getUnique   = gets counter
   setUnique i = modify (\x -> x { counter = i })

   getTypeSynonyms    = gets synonyms
   setTypeSynonyms xs = modify (\x -> x { synonyms = xs })

   getSkolems    = gets skolems
   setSkolems sk = modify (\x -> x { skolems = sk })

   allTypeSchemes = 
      gets schemeMap
      
   getTypeScheme i =  
      let err = internalError "Top.States.QualifierState" "getTypeScheme" "sigma var not found in map"
      in gets (M.findWithDefault err i . schemeMap)

   storeTypeScheme sv scheme = 
      let f s = s { schemeMap = M.insert sv scheme (schemeMap s) }
      in modify f