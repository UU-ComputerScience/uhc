

-- UUAGC 0.9.6 (repmin)
module Repmin where

-- import System.Environment


repmin :: Tree -> Tree
repmin t = sem_Root (Root_Root t)

naive :: Tree -> Tree
naive t = replace t (tmax t + tmin t)
tmin (Tree_Leaf i) = i
tmin (Tree_Bin l r) = tmin l `min` tmin r
tmax (Tree_Leaf i) = i
tmax (Tree_Bin l r) = tmax l `max` tmax r

replace (Tree_Leaf i) n = Tree_Leaf n
replace (Tree_Bin l r) n = Tree_Bin (replace l n) (replace r n)


test :: (Tree -> Tree) -> Int -> Tree
test f n = f (tree n)

tree :: Int -> Tree
tree 1 = Tree_Leaf 1
tree n = Tree_Bin (tree (n-1)) (tree (n-1))

tsum (Tree_Leaf i)  = i
tsum (Tree_Bin l r) = tsum l + tsum r

main :: IO ()
main = do -- (n:_) <- getArgs
          --let tree2 = test naive 19
          let tree1 = test repmin 19
          print (tsum tree1)
          --print (tsum tree2)
-- Root --------------------------------------------------------
{-
   visit 0:
      synthesized attribute:
         tree                 : Tree 
   alternatives:
      alternative Root:
         child tree           : Tree 
-}
data Root  = Root_Root (Tree) 
-- cata
sem_Root :: Root  ->
            T_Root 
sem_Root (Root_Root _tree )  =
    (sem_Root_Root (sem_Tree _tree ) )
-- semantic domain
type T_Root  = ( Tree)
sem_Root_Root :: T_Tree  ->
                 T_Root 
sem_Root_Root tree_  =
    (let 
         -- "repmin.ag"(line 25, column 12)
         _treeOrmin =
             _treeImin + _treeImax
         -- copy rule (up)
         _lhsOtree =
             _treeItree
         ( _treeImax,_treeImin,_treeItree) =
             (tree_ _treeOrmin )
     in  ( _lhsOtree))
-- Tree --------------------------------------------------------
{-
   visit 0:
      inherited attribute:
         rmin                 : Int
      synthesized attributes:
         max                  : Int
         min                  : Int
         tree                 : Tree 
   alternatives:
      alternative Bin:
         child lt             : Tree 
         child rt             : Tree 
      alternative Leaf:
         child int            : {Int}
-}
data Tree  = Tree_Bin (Tree) (Tree) 
           | Tree_Leaf (Int) 
-- cata
sem_Tree :: Tree  ->
            T_Tree 
sem_Tree (Tree_Bin _lt _rt )  =
    (sem_Tree_Bin (sem_Tree _lt ) (sem_Tree _rt ) )
sem_Tree (Tree_Leaf _int )  =
    (sem_Tree_Leaf _int )
-- semantic domain
type T_Tree  = Int ->
               ( Int,Int,Tree)
sem_Tree_Bin :: T_Tree  ->
                T_Tree  ->
                T_Tree 
sem_Tree_Bin lt_ rt_  =
    (\ _lhsIrmin ->
         (let 
              -- "repmin.ag"(line 16, column 12)
              _lhsOmin =
                  _ltImin `min` _rtImin
              -- "repmin.ag"(line 17, column 12)
              _lhsOmax =
                  _ltImax `max` _rtImax
              -- "repmin.ag"(line 31, column 12)
              _lhsOtree =
                  Tree_Bin _ltItree _rtItree
              -- copy rule (down)
              _ltOrmin =
                  _lhsIrmin
              -- copy rule (down)
              _rtOrmin =
                  _lhsIrmin
              ( _ltImax,_ltImin,_ltItree) =
                  (lt_ _ltOrmin )
              ( _rtImax,_rtImin,_rtItree) =
                  (rt_ _rtOrmin )
          in  ( _lhsOmax,_lhsOmin,_lhsOtree)))
sem_Tree_Leaf :: Int ->
                 T_Tree 
sem_Tree_Leaf int_  =
    (\ _lhsIrmin ->
         (let 
              -- "repmin.ag"(line 14, column 12)
              _lhsOmin =
                  int_
              -- "repmin.ag"(line 15, column 12)
              _lhsOmax =
                  int_
              -- "repmin.ag"(line 30, column 12)
              _lhsOtree =
                  Tree_Leaf _lhsIrmin
          in  ( _lhsOmax,_lhsOmin,_lhsOtree)))
