
main :: [Char]
main = merge ['a', 'b', 'c'] ['a', 'c', 'd']

merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x == y = merge (x:xs) ys
                    | x < y  = x : merge xs (y:ys)
                    | x > y  = y : merge (x:xs) ys
