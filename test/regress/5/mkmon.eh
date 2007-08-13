let data L a = N | C a (L a)
in  let data Mon m = MkMon (forall a . a -> m a)
        map :: (a -> b) -> L a -> L b
        map = \f -> \l ->
                 case l of
                    N -> N
                    C e l -> C (f e) (map f l)
    in  let mapm = map MkMon
        in  mapm
