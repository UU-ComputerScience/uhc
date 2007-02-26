-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
--
-- This module contains type synonyms to represent type synonyms. A collection
-- of type synonyms can always be ordered, since (mutually) recursive type
-- synonyms are not permitted. The ordering of type synonyms must be determined
-- to find a minimal number of unfold steps to make two types syntactically 
-- equivalent.
--
-----------------------------------------------------------------------------

module Top.Types.Synonym where

import Top.Types.Primitive
import Top.Types.Substitution
import Utils (internalError)
import Data.Graph (scc, buildG)
import Data.Tree (flatten)
import qualified Data.Map as M

----------------------------------------------------------------------
-- * Type synonyms

-- |A (unordered) collection of type synonyms is represented by a finite map of
-- strings (the name of the type synonym) to pairs that have an int
-- (the number of arguments of the type synonym) and a function.
type TypeSynonyms        = M.Map String (Int, Tps -> Tp)
-- |An ordering of type synonyms maps a name of a type synonym to 
-- a position in the ordering.
type TypeSynonymOrdering = M.Map String Int
-- |An (unordered) collection of type synonyms, together with an ordering.
type OrderedTypeSynonyms = (TypeSynonymOrdering, TypeSynonyms)

----------------------------------------------------------------------
-- * Utility functions

-- |An empty collection of ordered type synonyms.
noOrderedTypeSynonyms :: OrderedTypeSynonyms
noOrderedTypeSynonyms = (M.empty, M.empty)

-- |A string is a list of characters
stringAsTypeSynonym :: OrderedTypeSynonyms
stringAsTypeSynonym = (M.singleton "String" 0, M.singleton "String" (0, \_ -> listType charType))

-- |Order a collection of type synonyms, and return this ordering paired with
-- sets of mutually recursive type synonyms that are detected.
getTypeSynonymOrdering :: TypeSynonyms -> (TypeSynonymOrdering, [[String]])
getTypeSynonymOrdering synonyms =
   let
       (nameTable, intTable) = let keys = M.keys synonyms
                               in ( M.fromList (zip keys [0..])
                                  , M.fromList (zip [0..] keys)
                                  )

       err          = internalError "Top.Types.Synonyms" "getTypeSynonymOrdering" "error in lookup table"
       lookupName n = maybe err id (M.lookup n nameTable)
       lookupInt  i = maybe err id (M.lookup i intTable)

       edges = let op s1 (arity, function) es =
                      let i1 = lookupName s1
                          cs = constantsInType (function (map TVar [0 .. arity - 1]))
                          add s2 = case M.lookup s2 nameTable of
                                      Just i2 -> (:) (i2,i1)
                                      Nothing -> id
                      in foldr add es cs
               in M.foldWithKey op [] synonyms
       
       graph = buildG (0, (M.size synonyms - 1)) edges
       list  = map flatten (scc graph)

       (ordering, recursive, _) =
          let op ints (os, rs, counter) =
                 case ints of
                    [int] | (int, int) `notElem` edges     -- correct type synonym
                      -> (M.insert (lookupInt int) counter os, rs, counter + 1)
                    _ -> (os, map lookupInt ints : rs, counter)
          in foldr op (M.empty, [], 0) list
   in
      (ordering, recursive)

isPhantomTypeSynonym :: OrderedTypeSynonyms -> String -> Bool
isPhantomTypeSynonym (_, xs) s = 
   case M.lookup s xs of 
      Nothing     -> False
      Just (i, f) -> 
         let is   = take i [0..]
             tp   = f (map TVar is)
             free = ftv tp
         in any (`notElem` free) is

----------------------------------------------------------------------
-- * Expansion of a type

-- |Fully expand a type in a recursive way.
expandType :: TypeSynonyms -> Tp -> Tp
expandType synonyms tp =
   let (x,xs) = leftSpine (expandTypeConstructor synonyms tp)
   in foldl TApp x (map (expandType synonyms) xs)

-- |Fully expand the top-level type constructor.
expandTypeConstructor :: TypeSynonyms -> Tp -> Tp
expandTypeConstructor synonyms tp =
   maybe tp (expandTypeConstructor synonyms) (expandTypeConstructorOneStep synonyms tp)

-- |Fully expand the top-level type constructor.
expandToplevelTC :: OrderedTypeSynonyms -> Tp -> Maybe Tp
expandToplevelTC (_, synonyms) = 
   fmap (expandTypeConstructor synonyms) . expandTypeConstructorOneStep synonyms

-- |Try to expand the top-level type constructor one step.
expandTypeConstructorOneStep :: TypeSynonyms -> Tp -> Maybe Tp
expandTypeConstructorOneStep synonyms tp =
   case leftSpine tp of
      (TCon s, tps) -> case M.lookup s synonyms of
                          Just (i, f) | i == length tps -> Just (f tps)
                                      | otherwise       -> internalError "Top.Types.Synonyms"
                                                                         "expandTypeConstructorOneStep"
                                                                         "invalid arity of type synonym"
                          Nothing     -> Nothing
      _             -> Nothing

-- |Try to expand the top-level type constructor of one of the two paired Top.Types. If both
-- top-level type constructors can be expanded, then the type synonym thast appears first
-- in the ordering is expanded.
expandOneStepOrdered :: OrderedTypeSynonyms -> (Tp, Tp) -> Maybe (Tp, Tp)
expandOneStepOrdered (ordering, synonyms) (t1,t2) =
   let f tp = case fst (leftSpine tp) of
                 TCon s -> M.lookup s ordering
                 _      -> Nothing
       expand tp = case expandTypeConstructorOneStep synonyms tp of
                      Just x  -> x
                      Nothing -> internalError "Top.Types.Synonyms" "expandOneStep" "invalid set of OrderedTypeSynonyms"
   in case (f t1, f t2) of
         (Just i1, Just i2) | i1 <= i2  -> Just (expand t1, t2)
                            | otherwise -> Just (t1, expand t2)
         (Just _ , Nothing) -> Just (expand t1, t2)
         (Nothing, Just _ ) -> Just (t1, expand t2)
         _                  -> Nothing
