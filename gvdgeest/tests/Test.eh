let data Bool = True | False 

    class Eq a where
      eq :: a -> a -> Int

    class Eq a => Ord a where
      cmp :: a -> a -> Int
 in
let instance Eq Int where
      eq = \x -> \y -> 0

    instance Ord Int where
      cmp = \x -> \y -> 0

    instance Eq Bool where
      eq = \x -> \y -> 0

    instance Ord Bool where
      cmp = \x -> \y -> 0
 in 
let 
    g = \x -> cmp x x

 in g 1
