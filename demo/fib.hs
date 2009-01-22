-- -- data T a = T a a
data List a = Nil | Cons a (List a)
data Bool = True | False
-- -- 
-- TODO: should be jvmcall
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int
foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primMulInt" (*)  :: Int -> Int -> Int
foreign import ccall "primDivInt" (/)  :: Int -> Int -> Int
foreign import ccall "primEqInt"  (==) :: Int -> Int -> Bool
-- 
-- mylist = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Nil)))))))))
-- 
sum Nil         = 0
sum (Cons x xs) = x + sum xs

fibs  = Cons 1 (Cons 1 (zipWith (+) fibs (tail fibs)))

-- replicate 0 x = Nil
-- replicate n x = Cons x (replicate (n - 1) x)

replicate n x = if n == 0 then Nil else Cons x (replicate (n - 1) x)

zipWith f (Cons x xs) (Cons y ys) = Cons (f x y) (zipWith f xs ys)
zipWith _ _ _ = Nil

-- 
-- last Nil          = 99
-- last (Cons x Nil) = x
-- last (Cons x xs)  = last xs
-- 
-- head (Cons x _) = x
-- head Nil        = 13

take n Nil = Nil
take n (Cons x xs) = if n == 0 then Nil else Cons x (take (n-1) xs)
-- 
tail Nil = Nil
tail (Cons _ xs) = xs

-- 
-- test = \(T a b) -> b

-- This program fails: in one branch local variable 2 is assigned an unboxed int, in local variable 2 it is assigned an object.
main = sum (take 7500 fibs)
