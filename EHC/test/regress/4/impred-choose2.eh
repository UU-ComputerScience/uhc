let  choose  ::  a -> a -> a
     id      ::  a -> a
     v3      =   (choose :: (forall a . a -> a) -> (forall b . b -> b) -> (forall c . c -> c)) id
     v4      ::  (forall a . a -> a) -> (forall b . b -> b)
     v4      =   choose id
in   v3
