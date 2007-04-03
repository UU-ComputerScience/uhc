


data List a = Cons a (List a) | Nil


class Test a where
  eq :: a -> a -> Bool


instance Test (List a) where
  eq Nil Nil = True
  eq (Cons _ xs) (Cons _ ys) = eq xs ys
  eq _ _ = False
