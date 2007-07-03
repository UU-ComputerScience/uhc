-- all ok

data Bool = False | True

r1 :: (c :: Bool, a :: Int, b :: Char)
r1 = (a = 3, c = False, b = 'x')

r2 = (r1 | d = 3)
r3 = (r1 | c := 3)

r4 :: (a :: Int, c :: Bool)
r4 = r1

r5 :: (a :: Int, c :: Int)
r5 = (r1 | c := 3)

