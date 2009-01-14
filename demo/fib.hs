data List = Nil | Cons Int List

myList = 3 `Cons` (4 `Cons` Nil)

head Nil         = 9999
head (Cons x xs) = x

main = head myList
