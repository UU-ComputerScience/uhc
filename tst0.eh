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
      eq = \_ _ -> True
      ne = \_ _ -> True
    instance dEqList = Eq a => Eq (List a) where
      eq = \_ _ -> True
      ne = \_ _ -> True
    instance dOrdInt = Ord Int where
      lt = \_ _ -> True
      gt = \_ _ -> True
    instance dOrdList = Ord a => Ord (List a) where
      lt = \_ _ -> True
      gt = \_ _ -> True
in
let -- f = eq (Cons 3 Nil) (Cons 4 Nil)
    -- f = eq 3 4
    -- f = Cons (eq 3 4) (Cons (lt 5 6) Nil)
    f = \x -> Cons (eq x x) (Cons (lt x x) Nil)
    -- f = \x -> eq x x
in  3
