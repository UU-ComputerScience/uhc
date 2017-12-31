mapm :: (a -> b) -> [a] -> [b]
mapm _ [] = []
mapm f (x:xs) = f x : map f xs

main = print (sum (mapm succ [1..1000000 :: Int]))