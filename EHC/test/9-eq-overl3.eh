let  data Bool = False | True
in   let  class Eq a where
            eq :: a -> a -> Bool
          instance dEqInt1 <: Eq Int where
            eq = \_ _ -> True
     in   let  f :: (! Eq a !) -> a -> a -> (! Eq b !) -> b -> b -> (Bool,Bool)
               f = \p q r s -> (eq p q, eq r s)
          in   let  v1 = f 3 4 5 6
                    v2 =  let  instance dEqInt2 <: Eq Int where
                                 eq = \_ _ -> False
                               v3 = f 3 4 5 6
                          in   f 3 4 5 6
               in   f 3 4 5 6
