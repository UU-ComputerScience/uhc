{-| Module      :  OnlyResultHeuristics
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable

	Two (filter) heuristics that prevent an application or a negation to be 
	reported as incorrect if only the result type is reponsible for non-unifiability.
-}

module Helium.StaticAnalysis.Heuristics.OnlyResultHeuristics where

import Top.Implementation.TypeGraph.Heuristic
import Top.Interface.TypeInference
import Top.Types
import Helium.Utils.OneLiner (OneLineTree)
import Helium.Syntax.UHA (Range)
import Helium.StaticAnalysis.Miscellaneous.UHA_Source
      
-----------------------------------------------------------------------------

class MaybeApplication a where
   maybeNumberOfArguments :: a -> Maybe Int
   maybeApplicationEdge   :: a -> Maybe (Bool, [(UHA_Source, Tp)])

class IsPattern a where
   isPattern :: a -> Bool
   
avoidApplicationConstraints :: (HasTwoTypes info, MaybeApplication info) => Heuristic info
avoidApplicationConstraints = 
   Heuristic (edgeFilter "Avoid application constraints" f) where
   
  f pair@(edge, info) = 
   case maybeNumberOfArguments info of
      Nothing -> return True
      Just nrArgs ->
       doWithoutEdge pair $

          do synonyms <- getTypeSynonyms                 
             (maybeFunctionType, maybeExpectedType) <- getSubstitutedTypes info  
             case (maybeFunctionType,maybeExpectedType) of    
                (Just functionType,Just expectedType) -> return (not onlyResult)               
                   
                  where 
                    onlyResult = length xs == nrArgs &&
                                 length ys == nrArgs &&           
                                 unifiable synonyms (tupleType xs) (tupleType ys)                    
                    xs         = fst (functionSpineOfLength nrArgs functionType)
                    ys         = fst (functionSpineOfLength nrArgs expectedType)  
                _ -> return True  


-----------------------------------------------------------------------------

class MaybeNegation a where
   maybeNegation :: a -> Maybe Bool

avoidNegationConstraints :: (HasTwoTypes info, MaybeNegation info) => Heuristic info
avoidNegationConstraints = 
   Heuristic (edgeFilter "Avoid negation constraints" f) where
  
  f pair@(edge, info) =
   case maybeNegation info of
      Nothing -> return True
      Just isIntNegation -> doWithoutEdge pair $  
            do synonyms <- getTypeSynonyms
               (_, mtp) <- getSubstitutedTypes info
               case mtp of                   
                  Just tp -> 
                     let newtvar = TVar (nextFTV tp)
                         testtp = (if isIntNegation then intType else floatType) .->. newtvar
                     in return (not (unifiable synonyms tp testtp))
                  _ -> return True                          

-----------------------------------------------------------------------------
