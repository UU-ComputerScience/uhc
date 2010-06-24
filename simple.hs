foo :: forall a. a -> a
foo y = let f = (\x->x)
        in f y
