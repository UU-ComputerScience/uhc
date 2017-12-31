fact :: Int -> Int 
fact n = if n <= 1 then 1 else n * fact (n - 1)

data BinTree a = Leaf a | BinTree (BinTree a) a (BinTree a)

mapTreeUnitToFac :: BinTree () -> BinTree Int
mapTreeUnitToFac (Leaf _) = Leaf (fact 12)
mapTreeUnitToFac (BinTree l _ r) = BinTree l' (fact 12) r'
  where l' = mapTreeUnitToFac l
        r' = mapTreeUnitToFac r

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

testTree :: BinTree Int
testTree = mirror (mapTreeUnitToFac (genBalancedBinTree 20))

testBinTreeInternals :: Int
testBinTreeInternals = sumTree testTree

main = print testBinTreeInternals