fact :: Int -> Int 
fact n = if n <= 1 then 1 else n * fact (n - 1)

main = print (sum (replicate 10000000 (fact 12)))