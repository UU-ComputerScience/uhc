-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
--
-- A unification algorithm for types, which can take a list of (ordered) 
-- type synonyms into account.
--
-----------------------------------------------------------------------------

module Top.Types.Unification where

import Top.Types.Substitution
import Top.Types.Primitive
import Top.Types.Synonym
import qualified Data.Map as M
import Top.Util.IntErr (internalError)

-- |There are two reasons why two types cannot be unified: either two (different) type constants clash (they
-- should be the same), or a type variable should be unified with a composed type that contains this same
-- type variable.
data UnificationError 
   = ConstantClash String String
   | InfiniteType Int
 deriving (Show,Eq)       

-- |The most general unification (substitution) of two types.
mgu :: Tp -> Tp -> Either UnificationError MapSubstitution 
mgu t1 t2 = 
   case mguWithTypeSynonyms noOrderedTypeSynonyms t1 t2 of
      Left uError  -> Left uError
      Right (_, s) -> Right s

-- Expand type synonyms as lazy as possible
-- example: 
--   if String => [Char]
--   then   v11 -> [v11]  `mgu`  String -> [[v14]]
--   should be:
--      [ v11 := [Char] , v14 := Char ]
--
-- Note: the boolean indicates whether exansions were necessary       
mguWithTypeSynonyms :: OrderedTypeSynonyms -> Tp -> Tp -> Either UnificationError (Bool, MapSubstitution)
mguWithTypeSynonyms typesynonyms = rec emptySubst

 where
   rec sub t1 t2 =
     case (leftSpine t1, leftSpine t2) of
        ((TVar i,[]), _) -> recVar sub i t2               
        (_, (TVar i,[])) -> recVar sub i t1
        ((TCon s, ss), (TCon t, tt)) 
           | s == t && not (isPhantomTypeSynonym typesynonyms s) -> 
                recList sub ss tt
           | otherwise -> 
                case expandOneStepOrdered typesynonyms (t1, t2) of 
                   Just (t1', t2') -> 
                      case rec sub t1' t2' of
                         Left  uError    -> Left uError
                         Right (_, sub') -> Right (True, sub') 
                   Nothing -> Left (ConstantClash s t)
                                                                          
        _ -> case (t1, t2) of
                (TApp l1 r1, TApp l2 r2) -> recList sub [l1, r1] [l2, r2]
                _ ->  internalError "Top.Types.Unification" "mguWithTypeSynonyms" "illegal type"

   recVar sub i tp = 
      case M.lookup i sub of
         Just t2 -> 
            case rec sub tp t2 of
               Right (True,sub') -> 
                  let mtp = equalUnderTypeSynonyms typesynonyms (sub' |-> tp) (sub' |-> t2)
                  in case mtp of 
                        Just newTP -> Right (True,singleSubstitution i newTP @@ removeDom [i] sub')
                        Nothing -> internalError "Top.Types.Unification" "mguWithTypeSynonyms" "illegal types" 
               answer -> answer
         Nothing -> 
            case sub |-> tp of 
               TVar j | i == j           -> Right (False, sub)
               tp'    | i `elem` ftv tp' -> Left (InfiniteType i)
                      | otherwise        -> Right (False, singleSubstitution i tp' @@ sub)                                     
            
   recList sub [] [] = Right (False,sub)
   recList sub (s:ss) (t:tt) = 
      case rec sub s t of
         Left uError -> Left uError
         Right (b,sub') -> 
            case recList sub' ss tt of
               Left uError      -> Left uError
               Right (b',sub'') -> Right (b || b', sub'')
   recList _ _ _ = 
      internalError "Top.Types.Unification" "mguWithTypeSynonyms" "kinds do not match"

-- |Find the most general type for two types that are equal under type synonyms
-- (i.e., the least number of expansions)
equalUnderTypeSynonyms :: OrderedTypeSynonyms -> Tp -> Tp -> Maybe Tp
equalUnderTypeSynonyms typesynonyms t1 t2 = 
   case (leftSpine t1,leftSpine t2) of 
      ((TVar i,[]),(TVar _,[])) -> Just (TVar i) 
      ((TCon s,ss),(TCon t,tt)) 
         | s == t && not (isPhantomTypeSynonym typesynonyms s) -> 
              do let f = uncurry (equalUnderTypeSynonyms typesynonyms)
                 xs <- mapM f (zip ss tt)
                 Just (foldl TApp (TCon s) xs)
         | otherwise -> 
              do (t1', t2') <- expandOneStepOrdered typesynonyms (t1, t2)
                 equalUnderTypeSynonyms typesynonyms t1' t2'

      _ -> Nothing

-- |Given a set of (ordered) type synonyms, can two types be unified?                              
unifiable :: OrderedTypeSynonyms -> Tp -> Tp -> Bool
unifiable typesynonyms t1 t2 =
   case mguWithTypeSynonyms typesynonyms t1 t2 of
      Left  _ -> False
      Right _ -> True
      
-- |Same as unifiable, but takes as input a list of types
unifiableList :: OrderedTypeSynonyms -> Tps -> Bool
unifiableList typesynonyms (t1:t2:ts) = 
   case mguWithTypeSynonyms typesynonyms t1 t2 of
      Left _         -> False
      Right (_, sub) -> unifiableList typesynonyms (sub |-> (t2:ts))
unifiableList _ _ = True
