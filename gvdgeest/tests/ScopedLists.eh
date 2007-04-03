let    class Eq a where
          eq :: a -> a -> Int

       data List a = Cons a (List a)
                   | Nil
      
       testeq = \(Cons x xs) -> \(Cons y ys) -> (eq xs ys, eq x y)

in testeq
