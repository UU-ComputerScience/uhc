let -- Eq :: (* -> *) -> (* -> *) -> *
    -- Eq :: * -> * -> *
    data Eq a b = Eq (forall f . f a -> f b)
    data L a = N | C a (L a)
    undefined :: forall a . a
 in let weird :: (a -> Eq a a) -> Eq L L
        weird = \g -> g undefined
     in 3
