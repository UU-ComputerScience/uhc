data T a = T a a
data List a = Nil | Cons a (List a)

mylist = Cons 1 (Cons 2 Nil)

last Nil          = 99
last (Cons x Nil) = x
last (Cons x xs)  = last xs

head (Cons x _) = x
head Nil        = 13

test = \(T a b) -> a

main = last mylist
  -- test (T (head mylist) (head Nil))
