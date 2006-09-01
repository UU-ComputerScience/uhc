let Eq :: k -> l -> *
    data Eq a b = Eq (forall f . f a -> f b)
 in 3
