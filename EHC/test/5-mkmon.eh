let data L a = N | C a (L a)
in  let data Mon m = MkMon (a -> m a)
        map :: (a -> b) -> L a -> L b
    in  let mapm = map MkMon
        in  mapm
