fib :: Int -> Int
fib n   | n == 0  = 0
        | n == 1  = 1
        | True    = fib (n-1) + fib (n-2)

main = fib 33
