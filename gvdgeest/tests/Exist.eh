let data C_0_73_1_0 = C_0_73_1_0
    data Bool = True | False
    data List a = Cons a (List a)
                | Nil

    class Eq a where
      eq :: a -> a -> Bool

    instance Eq C_0_73_1_0 where
      eq = \x -> \y -> True

in 
 let   f :: exists a . Eq a => Bool -> a
       f = \x -> case x of
                   True  -> 1
                   False -> 0

       g ::  Bool -> exists a . Eq a => a
       g = \x -> case x of
                   True  -> True
                   False -> 0

       t =    (Cons (f True) (Cons (f False) Nil)
              ,Cons (g True) (Cons (g False) Nil)
              )

       x = Cons (g True) Nil
   
 in f True
