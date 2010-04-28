-- all error

data Bool = False | True

r1 :: (c :: Bool, a :: Int, b :: Char)
r1 = (a = 3, c = False, b = 'x')

r2 = (r1 | c = 3)	-- r1 should not have a label c
r3 = (r1 | d := 3)	-- r1 must have label d

r6 :: (a :: Int, c :: Int)
r6 = (r1 | c = 3)	-- r1 already has label c
