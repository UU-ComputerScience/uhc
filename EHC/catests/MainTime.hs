fact :: Int -> Int 
fact n = if n <= 1 then 1 else n * fact (n - 1)

data List a = Nil | Cons a (List a)

mapTreeUnitToFac :: List () -> List Int
mapTreeUnitToFac Nil = Nil
mapTreeUnitToFac (Cons _ r) = Cons (fact 12) r'
  where r' = mapTreeUnitToFac r

genBalancedList :: Int -> List ()
genBalancedList n | n < 1 = Nil
genBalancedList n = Cons () sub
  where sub = genBalancedList (n - 1)

sumTree :: List Int -> Int
sumTree Nil = 0
sumTree (Cons x r) = x + sumTree r

countNodes :: List a -> Int
countNodes Nil = 1
countNodes (Cons _ r) = 1 + countNodes r

flatten :: List a -> [a]
flatten (Cons a r) = a:flatten r
flatten Nil = []

testTreeInlinedMap :: List Int
testTreeInlinedMap = mapTreeUnitToFac (genBalancedList 20)

testListInlinedMap :: Int
testListInlinedMap = countNodes testTreeInlinedMap + length (flatten testTreeInlinedMap) + sumTree testTreeInlinedMap

main = print testListInlinedMap