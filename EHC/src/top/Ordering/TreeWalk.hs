{-# OPTIONS -fglasgow-exts #-}
-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  non-portable (requires extensions)
-----------------------------------------------------------------------------

module Top.Ordering.TreeWalk where

newtype TreeWalk = TreeWalk (forall a . List a -> [(List a, List a)] -> List a)

topDownTreeWalk :: TreeWalk
topDownTreeWalk = TreeWalk (\top cs -> top . children (unzip cs))
   where children (fs,gs) = concatList gs . concatList fs

bottomUpTreeWalk :: TreeWalk
bottomUpTreeWalk = TreeWalk (\top cs -> children (unzip cs) . top)
   where children (fs,gs) = concatList fs . concatList gs

inorderTopFirstPreTreeWalk :: TreeWalk
inorderTopFirstPreTreeWalk = TreeWalk (\top cs -> top . children cs)
   where children = concatList . map (\(f,g) -> g . f)

inorderTopLastPreTreeWalk :: TreeWalk
inorderTopLastPreTreeWalk = TreeWalk (\top cs -> children cs . top)
   where children = concatList . map (\(f,g) -> g . f)

inorderTopFirstPostTreeWalk :: TreeWalk
inorderTopFirstPostTreeWalk = TreeWalk (\top cs -> top . children cs)
   where children = concatList . map (\(f,g) -> f . g)

inorderTopLastPostTreeWalk :: TreeWalk
inorderTopLastPostTreeWalk = TreeWalk (\top cs -> children cs . top)
   where children = concatList . map (\(f,g) -> f . g)

reverseTreeWalk :: TreeWalk -> TreeWalk
reverseTreeWalk (TreeWalk f) = TreeWalk (\top cs -> f top (reverse cs))

-------------------------------------------------------------------

type List a = [a] -> [a]

concatList :: [List a] -> List a
concatList = foldr (.) id
