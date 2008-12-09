-----------------------------------------------------------------------------
-- | License      :  GPL
-- 
--   Maintainer   :  bastiaan@cs.uu.nl
--   Stability    :  provisional
--   Portability  :  portable
-----------------------------------------------------------------------------

module Top.Ordering.Tree where

import Top.Ordering.TreeWalk 
import Data.List (partition, intersperse)
import qualified Data.Map as M
import qualified Data.Set as S

type Trees a = [Tree a]
data Tree  a = Node (Trees a)             
             | AddList Direction [a] (Tree a)
             | StrictOrder (Tree a) (Tree a)
             | Spread Direction [a] (Tree a)
             | Receive Int
             | Phase Int [a]         
             | Chunk Int (Tree a)
   deriving Show             
                                                                    
emptyTree ::                     Tree a
unitTree  :: a ->                Tree a 
listTree  :: [a] ->              Tree a
binTree   :: Tree a -> Tree a -> Tree a

emptyTree   = Node [] 
unitTree c  = listTree [c]
listTree cs = cs .>. emptyTree
binTree a b = Node [a, b]

infixr 8 .>. , .>>. , .<. , .<<.

(.>.), (.>>.), (.<.), (.<<.) :: [a] -> Tree a -> Tree a
(.>.)  = makeTreeHelper AddList Down
(.>>.) = makeTreeHelper Spread Down
(.<.)  = makeTreeHelper AddList Up
(.<<.) = makeTreeHelper Spread Up

-- prevents adding an empty list
makeTreeHelper constructor direction xs tree
   | null xs   = tree 
   | otherwise = constructor direction xs tree
          
------------------------------------------------------------------------

data Direction   = Up | Down deriving (Eq, Show)
type Spreaded a  = M.Map Int [a]
type Phased a    = M.Map Int (List a)

flattenTree :: TreeWalk -> Tree a -> [a]
flattenTree (TreeWalk treewalk) theTree = 
   strictRec theTree []
    
    where    
     rec :: List a ->             -- downward constraints
            Tree a ->             -- the tree to flatten
            ( List a              -- the result
            , List a              -- upward constraints
            )
     rec down tree = 
        case tree of
        
           Node trees ->
              let tuples = map (rec id) trees
              in (treewalk down tuples, id)
           
           Chunk _ t -> 
              rec down t
                 
           AddList Up as t ->
              let (result, up) = rec down t
              in (result, (as++) . up)

           AddList Down as t ->
              rec ((as++) . down) t
              
           StrictOrder left right ->
              let left_result  = strictRec left
                  right_result = strictRec right
              in (treewalk down [(left_result . right_result, id)], id) 
              
           Spread direction as t -> 
              rec down (AddList direction as t)
              
           Receive _ -> 
              rec down emptyTree
              
           Phase _ as ->
              rec down (listTree as)                  

     strictRec :: Tree a ->             -- the tree to flatten
                  List a                -- the result
     strictRec tree = 
        let (result, up) = rec id tree
        in treewalk id [(result, up)]

spreadTree :: (a -> Maybe Int) -> Tree a -> Tree a
spreadTree spreadFunction = fst . rec M.empty
   where
    rec fm tree = 
       case tree of   

          Node trees -> 
             let (trees', sets) = unzip (map (rec fm) trees)
             in (Node trees', S.unions sets)
          
          Chunk cnr t -> 
             let (tree', set) = rec fm t
             in (Chunk cnr tree', set)
          
          AddList direction as t -> 
             let (tree', set) = rec fm t
             in (AddList direction as tree', set)

          StrictOrder left right -> 
             let (left' , set1) = rec fm left
                 (right', set2) = rec fm right
             in (StrictOrder left' right', S.union set1 set2)
          
          Spread direction as t -> 
             let (tree', set) = rec fmNew t
                 fmNew = M.unionWith (++) fm (M.fromList [ (i, [x]) | x <- doSpread, let Just i = spreadFunction x ])
                 (doSpread, noSpread) = 
                    partition (maybe False (`S.member` set) . spreadFunction) as
             in (Spread direction noSpread tree', set)
          
          Receive i -> 
             let t = maybe emptyTree listTree (M.lookup i fm)
             in (t, S.singleton i)
             
          Phase _ _ ->
             (tree, S.empty)

phaseTree :: a -> Tree a -> Tree a
phaseTree a = strictRec
   
   where
    rec tree = 
       case tree of
       
          Node trees -> 
             let (trees', phasesList) = unzip (map rec trees)
                 phases = foldr (M.unionWith (.)) M.empty phasesList
             in (Node trees', phases)
             
          Chunk cnr t ->
             let (tree', phases) = rec t
             in (Chunk cnr tree', phases)
             
          AddList dir as t ->
             let (tree', phases) = rec t
             in (AddList dir as tree', phases)
             
          StrictOrder left right -> 
             let left'  = strictRec left
                 right' = strictRec right
             in (StrictOrder left' right', M.empty)     
             
          Spread dir as t -> 
             let (tree', phases) = rec t
             in (Spread dir as tree', phases)
             
          Receive _  -> 
             (tree, M.empty)
             
          Phase i as ->
             (emptyTree, M.singleton i (as++))
          
    strictRec tree = 
       let (tree', phases) = rec tree
           f list = listTree (list [])
       in foldr1 StrictOrder (intersperse (unitTree a) (M.elems (M.insertWith binTree 5 tree' (M.map f phases))))
        
chunkTree :: Tree a -> [(Int, Tree a)]
chunkTree theTree = 
   let (ts, chunks) = rec theTree 
   in ((-1), ts) : chunks
  
  where   
   rec tree =
     case tree of
   
        Node trees -> 
           let (ts, chunks) = unzip (map rec trees)
           in (Node ts, concat chunks)
           
        -- This chunk should be solved later then the inner chunks.
        -- Therefore, the new chunk is appended
        Chunk cnr t ->
           let (ts, chunks) = rec t
           in (emptyTree, chunks ++ [(cnr, ts)]) 
          
        AddList direction as t ->
           let (ts, chunks) = rec t
           in (AddList direction as ts, chunks)

        StrictOrder left right -> 
           let (ts1, chunks1) = rec left
               (ts2, chunks2) = rec right
           in (StrictOrder ts1 ts2, chunks1 ++ chunks2)

        Spread direction as t ->
           let (ts, chunks) = rec t
           in (Spread direction as ts, chunks)

        _ -> (tree, [])

instance Functor Tree where
   fmap f tree =
      case tree of
         Node ts           -> Node (map (fmap f) ts)
         AddList d as t    -> AddList d (map f as) (fmap f t)
         StrictOrder t1 t2 -> StrictOrder (fmap f t1) (fmap f t2)
         Spread d as t     -> Spread d (map f as) (fmap f t)
         Receive i         -> Receive i
         Phase i as        -> Phase i (map f as)
         Chunk i t         -> Chunk i (fmap f t)
