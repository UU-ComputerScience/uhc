
main = x [1,2,3,4]

x (x:xs) = (x, xs) > (x, xs) && xs == xs

z :: Ord a => a -> Bool
z y =  y == y && let 
                     f x = x > x && y == y
                 in f y 

