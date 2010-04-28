-- data fields update: fail, multiple constructors, a constructor without field
-- %% inline test (prefix1) --

data X a
  = X { b :: Int, a, c :: a }
  | Y { b :: Int }
  | Z { d :: Int }

x1 = Z 4
x2 = x1 { b = 5 }

main = b x2
