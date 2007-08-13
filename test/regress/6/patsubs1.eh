let data L a = N | C a (L a)
    f :: (forall a . L a) -> L b
    f = \N -> N
in  f N
