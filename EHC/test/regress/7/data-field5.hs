-- data fields construction: duplicate field
-- %% inline test (prefix1) --

data X
  = X { b :: Int, a, c :: Char }

x1 = X { c = 'y', a = 'x', b = 4, b = 5 }

main = c x1
