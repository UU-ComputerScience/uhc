-- data T a = T a a
data List a = Nil | Cons a (List a)
-- 
-- TODO: should be jvmcall
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int

mylist = Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 (Cons 6 (Cons 7 (Cons 8 (Cons 9 (Cons 10 Nil)))))))))

sum Nil         = 0
sum (Cons x xs) = x + sum xs

-- 
-- last Nil          = 99
-- last (Cons x Nil) = x
-- last (Cons x xs)  = last xs
-- 
-- head (Cons x _) = x
-- head Nil        = 13
-- 
-- tail Nil = Nil
-- tail (Cons _ xs) = xs
-- 
-- test = \(T a b) -> b

-- This program fails: in one branch local variable 2 is assigned an unboxed int, in local variable 2 it is assigned an object.
main = 45 + (sum mylist)
