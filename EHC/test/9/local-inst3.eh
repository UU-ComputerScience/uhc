let  data Bool = False | True
     class Eq a where
       eq :: a -> a -> Bool
in
let  f =  \x -> let  instance Eq Int   where eq = ...
                     instance Eq Char  where eq = ...
                in   eq x x
in
let  v =  f 3
in   v
