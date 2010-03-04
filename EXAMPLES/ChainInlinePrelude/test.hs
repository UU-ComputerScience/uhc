
fA :: Int -> Int
fA i = fB 1

fB :: Int -> Int
fB i = fC 1 

fC :: Int -> Int
fC i = 1

main = putStrLn (show (fA 1))