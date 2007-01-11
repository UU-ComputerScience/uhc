notMultiple :: Int -> Int -> Bool
notMultiple x y = not ((y `div` x) * x == y)

sieve :: [Int] -> [Int]
sieve (h:t) = h : sieve (filter (notMultiple h) t)

test1 = length (take 1500 (sieve [2..]))
main = putStrLn (show test1)
