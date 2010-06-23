data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primGtInt"  (>) :: Int -> Int -> Bool

upto m n | m > n = []
         | True  = m : upto (m + 1) n

sum []     = 0
sum (n:ns) = n + sum ns

main = sum (upto 1 10)
