notMultiple :: Int -> Int -> Bool
notMultiple x y = not ((y `div` x) * x == y)

sieve :: [Int] -> [Int]
sieve (h:t) = h : sieve (filter (notMultiple h) t)
sieve []    = []

test1 :: Int
test1 = sum (take 1500 (sieve [2..]))

main :: IO ()
main = putStrLn (show test1)
