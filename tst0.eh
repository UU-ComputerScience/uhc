let data Bool = False | True
in
let class Eq a where
      eq :: a -> a -> Bool
      ne :: a -> a -> Bool
    class Eq a => Ord a where
      lt :: a -> a -> Bool
      gt :: a -> a -> Bool
in  3
