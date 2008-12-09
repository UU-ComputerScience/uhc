data [] a = ''[]'' | a : [a]
data Bool = False | True

foreign import ccall "primAddInt" (+) :: Int -> Int -> Int
foreign import ccall "primDivInt" (/) :: Int -> Int -> Int
foreign import ccall "primMulInt" (*) :: Int -> Int -> Int
foreign import ccall "primSubInt" (-) :: Int -> Int -> Int
foreign import ccall "primGtInt" (>) :: Int -> Int -> Bool
foreign import ccall "primEqInt" (==) :: Int -> Int -> Bool

filter p (h:t) = if p h then h : filter p t else filter p t
filter _ []    = []

take n (h:t) | n>0 = h : take (n-1) t
             | True = []
take _ []    = []

sum (h:t) = h + sum t
sum []    = 0

not x = if x then False else True

notMultiple x y = not ((y / x) * x == y)

enumFrom s = s : enumFrom (s+1)

sieve (h:t) = h : sieve (filter (notMultiple h) t)
sieve [] = []

main = sum (take 1500 (sieve (enumFrom 2)))
