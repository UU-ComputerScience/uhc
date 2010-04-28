let data Bool = True | False
in
let class Eq a where
      eq :: a -> a -> Bool
    instance Eq Int where
    instance Eq Char where
in
let v = eq
in  v

