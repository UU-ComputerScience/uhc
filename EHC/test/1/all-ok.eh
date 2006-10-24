let i :: Int
    i = 4
    f :: Int -> Int
    f = \x -> x
    ab :: (Int,Char)
    ab@(a,b) = (3,'x')
    v :: Int
    v = (\x -> f (f ((\x -> x) x))) 3
    fst :: (Int,Int) -> Int
    fst = \(a,b) -> a
    unit :: ()
    unit = ()
 in v
