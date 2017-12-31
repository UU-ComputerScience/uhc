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

testTree :: BinTree Int
testTree = mirror (mapTreeUnitToFac (genBalancedBinTree 20))

testBinTreeNoInternalsViaList :: Int
testBinTreeNoInternalsViaList = length (flatten testTree)

flatten :: BinTree a -> [a]
flatten (BinTree l a r) = flatten l ++ a:flatten r
flatten (Leaf a) = [a]

main = print testBinTreeNoInternalsViaList