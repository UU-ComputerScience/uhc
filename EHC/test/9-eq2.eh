let  data Bool = False | True
in   let  class Eq a where
            eq :: a -> a -> Bool
     in   let  f :: Eq a => a -> a -> Eq b => b -> b -> (Bool,Bool)
               f = \p q r s -> (eq p q, eq r s)
          in   let  v = f  (# (eq = \_ _ -> False  ) <: Eq Int #) 3 4
                           (# (eq = \_ _ -> True   ) <: Eq Int #) 5 6
               in   v
