let data Bool = True | False
    data List a = Cons a (List a)
                | Nil
in
let class Eq a where
      eq :: a -> a -> Bool
in
let instance Eq Int where
      eq = \x -> \y -> True
   
    map :: (a -> b) -> List a -> List b
    map = \f -> \xs -> case xs of
                         Nil -> Nil
                         Cons x xs -> Cons (f x) (map f xs)

    --xx :: exists a . Eq a => (a, a)
    xx :: forall b . (forall a . Eq a => (a, a) -> b) -> b
    xx = \f -> f (1, 2)

    -- xs :: [exists a . Eq a -> (a, a)]   
    xs :: List (forall b . ( forall a . Eq a => (a, a) -> b) -> b)   
    xs = (Cons (\f -> f (1, 2)) Nil) 
in
let
    -- a = (\(x, y) -> eq x y) xx
    a = xx (\(x, y) -> eq x y)

    -- b = map (\(x, y) -> eq x y) xs
    b :: List Bool
    b = map (\f -> let g = f in g (\(x, y) -> eq x y)) xs

in b
