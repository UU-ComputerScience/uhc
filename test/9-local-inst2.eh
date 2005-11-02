let  data Bool = False | True
     class Eq a where
       eq :: a -> a -> Bool
in
let  f =  let  instance d <: Eq Int where eq = ...
               instance Eq Char where eq = ...
          in   eq (! d <: Eq Int !)
in
let  v =  f 3 4
in   v
