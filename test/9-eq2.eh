let  data Bool = False | True
in   let  class Eq a where
            eq :: a -> a -> Bool
     in   let  f :: (# Eq a #) -> a -> a -> (# Eq b #) -> b -> b -> (Bool,Bool)
               f = \p q r s -> (eq p q, eq r s)
          in   let  v = f  (# Eq Int :> (eq = \_ _ -> False  ) #) 3 4
                           (# Eq Int :> (eq = \_ _ -> True   ) #) 5 6
               in   v
