let  data Bool = False | True
     class Eq a where
       eq :: a -> a -> Bool
     instance Eq Int where
       eq = \_ _ -> True
in   let  f = \p q r s -> (eq p q, eq r s)
in   let  v = f 3 4 5 6
     in   v
