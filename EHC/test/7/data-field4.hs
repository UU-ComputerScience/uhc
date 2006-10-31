-- data fields construction: missing field
-- %% inline test (prefix1) --

data X
  = X { b :: Int, a, c :: Char }

x1 = X { c = 'y', a = 'x' }

main = c x1
