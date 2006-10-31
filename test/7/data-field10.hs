-- data fields update: fail, existent fields, but not occurring all in a data constructor
-- %% inline test (prefix1) --

data X a
  = X { b :: Int, a, c :: a }
  | Y { b :: Int, d :: a }

x1 = X { c = 'y', a = 'x', b = 4 }
x2 = x1 { a = 5, d = 6 }

main = b x2
