f :: a -> a
f xs = xs
g :: [a] -> Int
g xs = length (f xs)
h :: [Int] -> Int
h xs = sum (f xs)