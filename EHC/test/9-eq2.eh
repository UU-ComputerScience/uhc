let  data Bool = False | True
     class Eq a where
       eq :: a -> a -> Bool
in   let  f :: Eq a => a -> a -> Eq b => b -> b -> (Bool,Bool)
          f = \p q r s -> (eq p q, eq r s)
in   let  v = f  (! (eq = ...  ) <: Eq Int !) 3 4
                 (! (eq = ...  ) <: Eq Int !) 5 6
     in   v
