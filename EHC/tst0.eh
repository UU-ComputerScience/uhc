let data Bool = False | True
    data List a = Nil | Cons a (List a)
in
let class Eq a where
      eq :: a -> a -> Bool
      ne :: a -> a -> Bool
    class Eq a => Ord a where
      lt :: a -> a -> Bool
      gt :: a -> a -> Bool
    instance dEqInt = Eq Int where
      lt = \_ _ -> True
      gt = \_ _ -> True
    instance dEqList = Eq a => Eq (List a) where
      lt = \_ _ -> True
      gt = \_ _ -> True
in  3
