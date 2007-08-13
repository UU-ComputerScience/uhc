data Bool = False | True
data ''[]'' a = a : [a] | ''[]''

{-
error :: forall a . [Char] -> a
error _ = undefined

undefined :: forall a . a
undefined = error "undefined"
-}

foreign import jazy "primAddInt" (+) :: Int -> Int -> Int
foreign import jazy "primGtInt"  (>) :: Int -> Int -> Bool

upto m n | m > n = []
         | True  = m : upto (m + 1) n

sum []     = 0
sum (n:ns) = n + sum ns

main = sum (upto 1 10)
