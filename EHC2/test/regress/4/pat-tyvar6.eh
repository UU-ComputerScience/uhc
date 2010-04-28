let v1 = f2 (\(x::Int) -> x)
    f2 = \i -> let v1 = f1 i in i 3
    f1 :: (a->a) -> Int
in  v1

