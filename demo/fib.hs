data T a = T a a
data List a = Nil | Cons a (List a)

-- TODO: should be jvmcall
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int

mylist = Cons 83 (Cons 2 (Cons 3 (Cons 4 Nil)))

sum Nil         = 0
sum (Cons x xs) = x + sum xs

last Nil          = 99
last (Cons x Nil) = x
last (Cons x xs)  = last xs

head (Cons x _) = x
head Nil        = 13

tail Nil = Nil
tail (Cons _ xs) = xs

test = \(T a b) -> b

main = 17 + head mylist
