let  data Bool = False | True
in
let  class Eq a where
       eq :: a -> a -> Bool
     instance Eq Int where
       eq = ...
in
let  f :: a -> a -> Eq a => Bool
     f = \x y -> eq x y
     -- v :: Bool
     v = f 3 4
in   v
