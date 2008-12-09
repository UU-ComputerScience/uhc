-- 'l' is not accepted as param to 'f' because co/contra variance is not propagated
-- through the list type constructor

f :: ([Int->Int]) -> Int
l :: [forall a . a -> a]

v = f l

main = putStr ""
