fibt :: Int -> Int
fibt x | x < 1 = 0
fibt 1 = 1
fibt n = fibt (n-1) + fibt (n-2)

main = print (fibt 30)