%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CHR solver
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Derived from work by Gerrit vd Geest, but greatly adapted to use more efficient searching.

%%[9 module {%{EH}CHR.Solve} import({%{EH}CHR},{%{EH}CHR.Constraint},{%{EH}CHR.Key})
%%]

%%[9 import({%{EH}Base.Trie} as Trie)
%%]

%%[9 import(UU.Pretty,EH.Util.PPUtils)
%%]

%%[9 export(CHRStore,emptyCHRStore)
type TrieStore x = Trie.Trie (Trie.TrieKey Key) x
type CHRStore cnstr guard subst = TrieStore (CHR cnstr guard subst)
type CHRWorkList cnstr = Trie.Trie (Trie.TrieKey Key) cnstr

emptyCHRStore :: TrieStore x
emptyCHRStore = Trie.empty
%%]

%%[9 export(chrStoreFromElems,chrStoreUnion,chrStoreUnions,chrStoreSingletonElem)
chrStoreFromElems :: Keyable x => [x] -> TrieStore x
chrStoreFromElems xs = Trie.fromList [ (toKey x,x) | x <- xs ]

chrStoreSingletonElem :: Keyable x => x -> TrieStore x
chrStoreSingletonElem = Trie.singletonKeyable

chrStoreUnion :: TrieStore x -> TrieStore x -> TrieStore x
chrStoreUnion = Trie.union

chrStoreUnions :: [TrieStore x] -> TrieStore x
chrStoreUnions = Trie.unions
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pretty printing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(ppCHRStore)
ppCHRStore :: PP c => CHRStore c g s -> PP_Doc
ppCHRStore = ppCurlysCommasBlock . map (\(k,v) -> k >-< indent 2 (":" >#< v)) . Trie.toList
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Solving
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[9 export(chrSolve')
chrSolve' :: CHRStore c g s -> [c] -> [c]
chrSolve' = undefined
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Gerrit's stuff:
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

{-# OPTIONS -fglasgow-exts #-}
module CHRSolver
 ( (<=>)
 , (==>)
 , (|>)
 , CHR (..)
 , Matchable (..)
 , chrSolve'
 , chrSolve
 , chrSolveWl
 , Subst
 , Pattern (..)
 , propMatch
 )
where

-- 
-- TODO:: - Solver can be made faster in the case: ftv constraint == []
--        - Use of monoid (`mappend`) for substitutions is wrong, substitutions must agree

import Data.List ((\\))
import qualified Data.Set as Set
import Data.Maybe
import Data.Monoid
import Test.QuickCheck (property, Property)

infix   1 <=>, ==>
infixr  0 |>

class (Ord c, Monoid s) => Matchable c s | c -> s where
  match :: c -> c -> Maybe s
  subst :: s -> c -> c

  -- default case: syntactic equality
  match x y | x == y    = Just mempty
            | otherwise = Nothing
  subst = flip const

data CHR c s = CHR { head    :: [c]
                   , guard   :: (s -> Maybe s)
                   , body    :: [c]
                   }

emptyGuard :: (Monoid a) => t -> Maybe a
emptyGuard = const $ Just mempty

instance (Show c, Eq c) => Show (CHR c s) where
  show (CHR  hs _ cs) = if all (`elem` cs) hs
                        then  sepWithComma hs ++ " ==> " ++ isEmpty (sepWithComma (cs \\ hs))
                        else  sepWithComma hs ++ " <=> " ++ isEmpty (sepWithComma cs)

(|>) :: CHR p s -> (s -> Maybe s) -> CHR p s
(|>) (CHR hs _ bs) g = CHR hs g bs

(<=>), (==>) :: Monoid s => [p] -> [p] -> CHR p s
hs <=>  bs = CHR hs emptyGuard bs
hs ==>  bs = hs <=> (hs ++ bs)

type Constraints c = Set.Set c

chrSolve' :: Matchable c s => [CHR c s] -> [c] -> [c]
chrSolve' chrs cs = Set.toList $ chrSolveWl chrs (Set.fromList cs) cs

chrSolve :: Matchable c s => [CHR c s] ->  Constraints c -> Constraints c
chrSolve chrs cs = chrSolveWl chrs cs (Set.toList cs)

chrSolveWl :: Matchable c s => [CHR c s] -> Constraints c -> [c] -> Constraints c
chrSolveWl chrs = foldr (solveConstraint chrs)

solveConstraint :: Matchable c s => [CHR c s] -> c -> Constraints c -> Constraints c
solveConstraint chrs c cs = foldr (applyCHR chrs [] mempty c) cs chrs

applyCHR :: Matchable c s => [CHR c s] -> [c] -> s -> c -> CHR c s -> Constraints c -> Constraints c
applyCHR chrs ms s' c (CHR hs g bs) constraints
  = foldr f constraints (matches c hs)
  where f (s, m, mhs) cs  = let sbs = map (subst s) bs                           
                                sms = map (subst s) (m:ms)
                                shs = map (subst s) mhs
                                s'' = s `mappend` s'  
                                (cs', wl) = simplify (g s'') sms sbs cs
                            in if null mhs
                               then chrSolveWl chrs cs' wl
                               else Set.fold (\d-> applyCHR chrs sms s'' d (CHR shs g sbs)) 
                                             cs 
                                             (Set.delete c cs) 

matches :: Matchable a s => a -> [a] -> [(s, a, [a])]
matches h' = rec []
  where rec _  []     = []
        rec rs (h:hs) = case match h' h of
 	                 Nothing -> rec (h:rs) hs
 	                 Just s  -> (s, h, rs ++ hs) : rec (h:rs) hs

simplify :: Matchable c s => Maybe s -> [c] -> [c] -> Constraints c -> (Constraints c, [c])
simplify Nothing   _   _    st = (st, [])
simplify (Just s)  hd  bd   st
 =  if all (`Set.member` st) hd
    then (foldr Set.insert st' sbd, new)
    else (st, [])
    where new = filter (\c -> not $ Set.member c st) sbd
          st' = foldr Set.delete st hd
          sbd = map (subst s) bd
     
-- Substitution + variable + instance Matchable
data Pattern a = Var Int
               | Val a
               deriving (Eq, Ord)

type Subst a = [(Int, a)]

instance Matchable a s => Matchable (Pattern a) (Subst a, s) where
  match (Val x) (Var i) = return ([(i, x)], mempty)
  match (Val x) (Val y) = do s <- match x y 
                             return ([], s)
  match _       _       = Nothing 

  subst (_, s)   (Val x) = Val (subst s x)
  subst (s, _) v@(Var i) = case lookup i s of
                                Nothing -> v
                                Just x  -> Val x
                                
instance Functor Pattern where
  fmap f (Val x) = Val (f x)
  fmap _ (Var i) = Var i

instance Show a => Show (Pattern a) where
  show (Val a) = show a
  show (Var i) = "v" ++ show i

--- Tests
propMatch :: Pattern Int -> Pattern Int -> Property
propMatch c h =
  case match c h of 
    Just s   -> property (c == subst s h)
    Nothing  -> property ()

instance Matchable Int ()

-- Some helper function for pp:
sepWithComma :: Show a => [a] -> String
sepWithComma = sepWith ", "

sepWith :: Show a => String -> [a] -> String
sepWith _   [] = ""
sepWith _   [x]    = show x
sepWith sep (x:xs) = show x ++ sep ++ sepWith sep xs

isEmpty :: String -> String
isEmpty "" = "true"
isEmpty s  = s
