let  data Bool = False | True
     class Eq a where
       eq :: a -> a -> Bool
     instance Eq Int where
       eq = \_ _ -> True
in
let  f = \{! d <: Eq a !} a b -> (3::a)
     g = \{! d <: Eq Int !} a b -> 3
in   f 3 4
