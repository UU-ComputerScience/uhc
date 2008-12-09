{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
--
-- Universal and existential quantification of types
--
-----------------------------------------------------------------------------

module Top.Types.Quantification where

import Top.Types.Primitive
import Top.Types.Substitution
import Data.List
import Data.Maybe
import Top.Util.IntErr (internalError)

-----------------------------------------------------------------------------
-- * Quantification

newtype Quantification q a = Quantification ([Int], QuantorMap, a)

type QuantorMap = [(Int, String)]

withoutQuantors :: Quantification q a -> Bool
withoutQuantors (Quantification (is, _, _)) = null is

showQuantor :: Show q => Quantification q a -> String
showQuantor = show . f where
   f :: Quantification q a -> q
   f = internalError "Top.Types.Quantification" "showQuantor" "quantor unknown"

noQuantifiers :: a -> Quantification q a
noQuantifiers a = Quantification ([], [], a)

quantifiers :: Quantification q a -> [Int]
quantifiers (Quantification (is, _, _)) = is

unquantify :: Quantification q a -> a
unquantify (Quantification (_, _, a)) = a

instance Substitutable a => Substitutable (Quantification q a) where
   sub |-> (Quantification (is, qmap, a)) = Quantification (is, qmap, removeDom is sub |-> a)
   ftv     (Quantification (is, _   , a)) = ftv a \\ is

instance HasTypes a => HasTypes (Quantification q a) where
   getTypes      (Quantification (_, _, a))     = getTypes a
   changeTypes f (Quantification (is, qmap, a)) = Quantification (is, qmap, changeTypes f a)

introduceTypeVariables :: Substitutable a => Int -> Quantification q a -> (Int, a)
introduceTypeVariables i (Quantification (qs, _, a)) = 
   let sub = listToSubstitution (zip qs (map TVar [i..]))
   in (i + length qs, sub |-> a)

introduceSkolemConstants :: Substitutable a => Int -> Quantification q a -> (Int, a)
introduceSkolemConstants i (Quantification (qs, _, a)) = 
   let sub = listToSubstitution (zip qs (map makeSkolemConstant [i..]))
   in (i + length qs, sub |-> a)

bindTypeVariables :: Substitutable a => [Int] -> a -> Quantification q a
bindTypeVariables is a = Quantification (is `intersect` ftv a, [], a)

bindSkolemConstants :: HasSkolems a => [Int] -> a -> Quantification q a
bindSkolemConstants scs a = 
   let scs'  = scs `union` allSkolems a       
       skMap = [ (i, TVar i) | i <- scs' ] 
   in Quantification (scs', [], changeSkolems skMap a)

getQuantorMap :: Quantification q a -> QuantorMap
getQuantorMap (Quantification (_, qm, _)) = qm

-----------------------------------------------------------------------------
-- * Universal quantification

data Universal
type Forall = Quantification Universal

instance Show Universal where
   show = const "forall"
   
instantiate, skolemize :: Substitutable a => Int -> Forall a -> (Int, a)
instantiate = introduceTypeVariables
skolemize   = introduceSkolemConstants

generalize :: (Substitutable context, Substitutable a) => context -> a -> Forall a
generalize context a = 
   quantify (ftv a \\ ftv context) a

generalizeAll :: Substitutable a => a -> Forall a
generalizeAll a = quantify (ftv a) a
   
quantify :: Substitutable a => [Int] -> a -> Forall a
quantify = bindTypeVariables

unskolemize :: HasSkolems a => [Int] -> a -> Forall a
unskolemize = bindSkolemConstants
     
-----------------------------------------------------------------------------
-- * Existential quantification

data Existential
type Exists = Quantification Existential

instance Show Existential where
   show = const "exists"

open, reveal :: Substitutable a => Int -> Exists a -> (Int, a)
open   = introduceSkolemConstants
reveal = introduceTypeVariables

close :: HasSkolems a => [Int] -> a -> Exists a
close = bindSkolemConstants

unreveal :: Substitutable a => [Int] -> a -> Exists a
unreveal = bindTypeVariables

-----------------------------------------------------------------------------
-- * Skolemization

skolemPrefix :: String
skolemPrefix = "_"

makeSkolemConstant :: Int -> Tp
makeSkolemConstant = TCon . (skolemPrefix++) . show 

fromSkolemString :: String -> Maybe Int
fromSkolemString s
   | skolemPrefix `isPrefixOf` s = 
        Just (read (drop (length skolemPrefix) s))
   | otherwise = Nothing

skolemizeFTV :: Substitutable a => a -> a
skolemizeFTV a = 
   let sub = listToSubstitution [ (i, makeSkolemConstant i) | i <- ftv a ]
   in sub |-> a   
   
class HasSkolems a where
   allSkolems    :: a -> [Int]
   changeSkolems :: [(Int, Tp)] -> a -> a
   
instance HasSkolems Tp where
   allSkolems (TVar _)   = []
   allSkolems (TCon s)   = case fromSkolemString s of
                              Just i  -> [i]
                              Nothing -> []
   allSkolems (TApp l r) = allSkolems l `union` allSkolems r   
   
   changeSkolems skMap = rec where
      rec tp@(TVar _) = tp
      rec tp@(TCon s) = case fromSkolemString s of
                              Just i  -> maybe tp id (lookup i skMap)
                              Nothing -> tp
      rec (TApp l r)  = TApp (rec l) (rec r)
      
instance HasSkolems a => HasSkolems [a] where
   allSkolems = foldr union [] . map allSkolems
   changeSkolems skMap = map (changeSkolems skMap) 
   
-----------------------------------------------------------------------------
-- * Pretty printing

data ShowQuantorOptions = ShowQuantorOptions
   { showTopLevelQuantors :: Bool
   , dontUseIdentifiers   :: [String]
   , variablePrefix       :: String
   , showAllTheSame       :: Bool
   , useTheNameMap        :: Bool
   }

defaultOptions :: ShowQuantorOptions
defaultOptions = ShowQuantorOptions 
   { showTopLevelQuantors = False
   , dontUseIdentifiers   = []
   , variablePrefix       = "v"
   , showAllTheSame       = False
   , useTheNameMap        = True
   }

showQuantors :: ShowQuantors a => a -> String
showQuantors = showQuantorsWithout (defaultOptions { showTopLevelQuantors = True }) 

-- |This class can deal with the pretty printing of (possibly nested) quantifiers.
class Show a => ShowQuantors a where
   showQuantorsWithout :: ShowQuantorOptions -> a -> String   
   
   -- default definition
   showQuantorsWithout = const show

instance ShowQuantors Tp

instance (Substitutable a, ShowQuantors a, Show q) => Show (Quantification q a) where 
   show = showQuantorsWithout defaultOptions
   
instance (Substitutable a, ShowQuantors a, Show q) => ShowQuantors (Quantification q a) where
   showQuantorsWithout options q@(Quantification (is, qmap, a)) = 
      let 
          qs          = is `intersect` ftv a
          quantorText | null qs || not (showTopLevelQuantors options) = ""
                      | otherwise = unwords (showQuantor q : (map (\i -> show (sub |-> TVar i)) qs) ++ [". "])
          dontUse     = dontUseIdentifiers options
          -- find an appropriate name for bound type variables that are in the name map
          qmap1       | not (useTheNameMap options) || showAllTheSame options = []
                      | otherwise = 
                           let op (rest, donts) (i,n)
                                  | i `elem` qs = let ints = [1..] :: [Int]
                                                      s = head [ n ++ extra 
                                                               | extra <- "" : map show ints
                                                               , n ++ extra `notElem` donts 
                                                               ]
                                                  in ((i,s):rest, s:donts)
                                  | otherwise   = (rest, donts)
                           in fst (foldl op ([], dontUse) qmap)
          dontUse1    = map snd qmap1 ++ dontUse                 
          -- find a name for the other bound type variables
          qmap2       | showAllTheSame options = []
                      | otherwise = zip (filter (`notElem` map fst qmap1) qs) (variableList \\ dontUse1)
          dontUse2    = map snd qmap2 ++ dontUse1
          frees       = ftv a \\ (map fst (qmap1 ++ qmap2))
          sub         = listToSubstitution $  [ (i, TCon s) | (i,s) <- qmap1 ++ qmap2 ]
                                           ++ [ (i, TCon (variablePrefix options ++ show i)) | i <- frees ]
          newOptions  = options { dontUseIdentifiers   = dontUse2
                                , showTopLevelQuantors = True 
                                }
      in 
          quantorText ++ showQuantorsWithout newOptions (sub |-> a)
         
-- |List of unique identifiers.(a, b, .., z, a1, b1 .., z1, a2, ..)
variableList :: [String]
variableList =  [ [x]        | x <- ['a'..'z'] ]
             ++ [ (x:show i) | i <- [1 :: Int ..], x <- ['a'..'z'] ]            
