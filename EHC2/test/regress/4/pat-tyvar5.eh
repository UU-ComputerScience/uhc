let -- f1 :: (a->a) -> Int
    f2 = \i -> i 3
    v1 = f2 (\(x::Int) -> x) 
in  v1

