let  class Eq a where
       eq :: a -> a -> Int

     instance Eq Int where
       eq = \x -> \y -> 0

in let class Eq a where
         iets :: a -> a -> Int
         tst :: a -> Int

       instance Eq Int where
         iets = \x -> \y -> 2
         tst = \x -> 3

       test = \x -> tst (eq x x)
   in test 12345
