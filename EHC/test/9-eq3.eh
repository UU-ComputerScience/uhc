let  data Bool = False | True
in   let  class Eq a where
            eq :: a -> a -> Bool
          instance dEqInt1 <: Eq Int where
            eq = ...
          instance dEqInt2 <: Eq Int where
            eq = ...
in   let  f :: Eq a => a -> a -> Eq b => b -> b -> (Bool,Bool)
          f = \p q r s -> (eq p q, eq r s)
in   let  v = f  (# dEqInt1 <: Eq Int #) 3 4
                 (# dEqInt2 <: Eq Int #) 5 6
     in   v
