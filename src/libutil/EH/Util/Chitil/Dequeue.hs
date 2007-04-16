-- Double-ended queue based on Okasaki's book
-- Olaf Chitil
-- 12/4/2007

-- 20070416, Atze Dijkstra, adapted for use in EH.Util library

module EH.Util.Chitil.Dequeue where

import Prelude hiding (last,init)
import qualified List (head,tail,reverse)

data Seq a = S !Int [a] !Int [a]

-- front seq = if isEmpty seq then Nothing else Just (headD seq,tailD seq)
front :: Seq a -> Maybe (a,Seq a)
front (S _ [] _ []) = Nothing
front (S _ [] _ [x]) = Just (x,empty)
front (S lenf (x:xs) lenr r) = Just (x,reverseD (check lenr r (lenf-1) xs))

-- back seq = if isEmpty seq then Nothing else Just (last seq,init seq)
back :: Seq a -> Maybe (a,Seq a)
back (S _ [] _ []) = Nothing
back (S _ [x] _ []) = Just (x,empty)
back (S lenf f lenr (x:xs)) = Just (x,check lenf f (lenr-1) xs)

empty :: Seq a
empty = S 0 [] 0 []

isEmpty :: Seq a -> Bool
isEmpty (S lenf _ lenr _) = (lenf + lenr == 0)

headD :: Seq a -> a
headD (S lenf f lenr r) = List.head (if lenf==0 then r else f)

last :: Seq a -> a
last (S lenf f lenr r) = List.head (if lenr==0 then f else r)

cons :: a -> Seq a -> Seq a
cons x (S lenf f lenr r) = check (lenf+1) (x:f) lenr r

tailD :: Seq a -> Seq a 
tailD (S _ [] _ _) = empty
tailD (S lenf f lenr r) = reverseD (check lenr r (lenf-1) (List.tail f))

snoc :: a -> Seq a -> Seq a
snoc x (S lenf f lenr r) = reverseD (check (lenr+1) (x:r) lenf f)

init :: Seq a -> Seq a
init (S _ _ _ []) = empty
init (S lenf f lenr r) = check lenf f (lenr-1) (List.tail r)

reverseD :: Seq a -> Seq a 
reverseD (S lenf f lenr r) = S lenr r lenf f 

-- Keep lists in balance: rebalance if front list too long
-- preconditions: lenf = length f, lenr = length r, lenr <= 3 * lenf + 1
check :: Int -> [a] -> Int -> [a] -> Seq a
check lenf f lenr r = if lenf <= 3 * lenr + 1 
                        then S lenf f lenr r
                        else S lenf' f' lenr' r'
  where
  len = lenf + lenr 
  lenf' = len `div` 2 
  lenr' = len - lenf' 
  (f',rf') = splitAt lenf' f 
  r' = r ++ List.reverse rf'
