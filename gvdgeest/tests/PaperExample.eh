let data List a = Cons a (List a) | Nil

    class Eq a where
      eq :: a -> a -> Int
   {-- 
    instance Eq Int where
      eq = \x -> \y -> 0

    instance Eq a => Eq (List a) where
      eq = \x -> \y -> 0 --}

    class Eq a => Ord a where
      cmp :: a -> a -> Int 
{--
    instance Ord Int where
      cmp = \x -> \y -> 0

    instance Ord a => Ord (List a) where
      cmp = \x -> \y -> 0 --}

in let test = \(Cons x xs) -> \(Cons y ys) -> (cmp x y, eq x y, cmp xs ys)
   in test
