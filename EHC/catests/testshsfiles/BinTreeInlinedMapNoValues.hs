fact :: Int -> Int 
fact n = if n <= 1 then 1 else n * fact (n - 1)

data BinTree = Leaf | BinTree BinTree BinTree

genBalancedBinTree :: Int -> BinTree
genBalancedBinTree n | n < 1 = Leaf
genBalancedBinTree n = BinTree sub sub
  where sub = genBalancedBinTree (n - 1)

mirror :: BinTree -> BinTree
mirror (BinTree l r) = BinTree (mirror r) (mirror l)
mirror x = x

countLeafs :: BinTree -> Int
countLeafs Leaf = 1
countLeafs (BinTree l r) = countLeafs l + 1 + countLeafs r

flatten :: BinTree -> [BinTree]
flatten (BinTree l r) = flatten l ++ flatten r
flatten Leaf = [Leaf]

testTreeInlinedMap :: BinTree
testTreeInlinedMap = mirror (genBalancedBinTree 20)

testBinTreeInlinedMap :: Int
testBinTreeInlinedMap = countLeafs testTreeInlinedMap + length (flatten testTreeInlinedMap)

main = print testBinTreeInlinedMap