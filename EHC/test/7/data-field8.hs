-- data fields update: fail, type changing but not for all interdependend fields
-- %% inline test (prefix1) --

data X a
  = X { b :: Int, a, c :: a }

x1 = X { c = 'y', a = 'x', b = 4 }
x2 = x1 { a = 5 }

main = b x2
