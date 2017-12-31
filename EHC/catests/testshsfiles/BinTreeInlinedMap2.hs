fact :: Int -> Int 
fact n = if n <= 1 then 1 else n * fact (n - 1)

data BinTree a = Leaf a | BinTree (BinTree a) a (BinTree a)

mapTreeUnitToFac :: BinTree () -> BinTree Int
mapTreeUnitToFac (Leaf _) = Leaf (fact 12)
mapTreeUnitToFac (BinTree l _ r) = BinTree (mapTreeUnitToFac l) (fact 12) (mapTreeUnitToFac r)

genBalancedBinTree :: Int -> BinTree ()
genBalancedBinTree n | n < 1 = Leaf ()
genBalancedBinTree n = BinTree sub () sub
  where sub = genBalancedBinTree (n - 1)

mirror :: BinTree a -> BinTree a
mirror (BinTree l a r) = BinTree (mirror r) a (mirror l)
mirror x = x

sumTree :: BinTree Int -> Int
sumTree (Leaf x) = x
sumTree (BinTree l x r) = sumTree l + x + sumTree r

countLeafs :: BinTree a -> Int
countLeafs (Leaf _) = 1
countLeafs (BinTree l _ r) = countLeafs l + 1 + countLeafs r

flatten :: BinTree a -> [a]
flatten (BinTree l a r) = flatten l ++ a:flatten r
flatten (Leaf a) = [a]

testTreeInlinedMap :: BinTree Int
testTreeInlinedMap = mirror (mapTreeUnitToFac (genBalancedBinTree 20))

testBinTreeInlinedMap :: Int
testBinTreeInlinedMap = countLeafs testTreeInlinedMap + length (flatten testTreeInlinedMap) + sumTree testTreeInlinedMap

main = print testBinTreeInlinedMap