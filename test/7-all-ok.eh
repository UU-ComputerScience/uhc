let id :: a -> a
    id = \x -> x
    v = (b = 'x', c = id,a = id 4)
    v :: ((a :: Int, b :: Char)| c :: a -> a)
    v2 = (3,4)
    v3 :: (Int,Char)
    v3' = case v3 of
            (a,b) -> (b,a)
    v4 :: (a::a,f::a->Int)
    v4 = (a=3,f=id)
    v4fa = v4.f v4.a
    v5 :: Rec (|c::Int|)
    v6 :: (<c::Int>)
    v6c = v6.c
    vs = v.c v.a
    vc = case v of
           (a = aa, b = bb,c) -> (c aa,c bb)
in  vs
