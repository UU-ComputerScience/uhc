fact :: Int -> Int 
fact n = if n <= 1 then 1 else n * fact (n - 1)

data BinTree a = Leaf a | BinTree (BinTree a) a (BinTree a)

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

mapTree :: (a -> b) -> BinTree a -> BinTree b
mapTree f (Leaf a) = Leaf (f a)
mapTree f (BinTree l a r) = BinTree (mapTree f l) (f a) (mapTree f r)

testTreeHigherOrderMap :: BinTree Int
testTreeHigherOrderMap = mirror (mapTree (const (fact 12)) (genBalancedBinTree 20))

testBinTreeHigherOrderMap :: Int
testBinTreeHigherOrderMap = countLeafs testTreeHigherOrderMap + length (flatten testTreeHigherOrderMap) + sumTree testTreeHigherOrderMap

main = print testBinTreeHigherOrderMap