let data Ordering = LT | EQ | GT
    data Bool = False | True
    data L a = N | C a (L a)
in
let foreign import jazy "primCmpInt" compare :: Int -> Int -> Ordering
    boolToInt = \x -> if x then 1 else 0
in
let and = \x y -> case x of
                    True -> y
                    _    -> False
in
let class Eq a where
      eq :: a -> a -> Bool
    instance dEqInt <: Eq Int where
      eq = \x y ->
             case compare x y of
                EQ -> True
                _  -> False
    instance dEqList <: Eq a => Eq (L a) where
      eq = \l1 l2 -> case l1 of
                       C l ls -> case l2 of
                                   C r rs -> and (eq l r) (eq ls rs)
                                   _      -> False
                       N      -> case l2 of
                                   N -> True
                                   _ -> False
in
let v1 = boolToInt (eq (C 3 N) (C 3 N))
in  v1
