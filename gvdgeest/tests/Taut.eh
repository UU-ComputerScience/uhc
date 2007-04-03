let data List a = Cons a (List a) | Nil
    data List1 a = Cons1 a (List1 a) | Nil1
    
    class Eq a where
      eq :: a -> a -> Int

    instance Eq Int where
      eq = \x -> \y -> 1

    instance (Eq a) => Eq (List1 a) where
      eq = \x -> \y -> 0

    test :: exists a . (Eq (List1 a)) => Int ->  a 
    test = \x -> eq (Cons1 1 Nil1) (Cons1 1 Nil1) 
   

    class HasDefault a where
      default :: a

    instance HasDefault (forall a . List a) where
      default = Nil

    test2 ::  (HasDefault (forall a . List a)) => List a 
    test2 = default

    test4 :: exists a . (HasDefault (List a)) => List a 
    test4 = Cons 5 Nil
in let test3 = \a ->  Cons a test2 
   in test4
