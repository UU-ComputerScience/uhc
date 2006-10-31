-- data fields update: ok, type changing
-- %% inline test (prefix1) --

data X a
  = X { b :: Int, a, c :: a }

x1 = X { c = 'y', a = 'x', b = 4 }
x2 = x1 { a = 5, c = 5 }

main = b x2
