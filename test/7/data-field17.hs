-- data field pattern match: fail, pun field name incorrect
-- %% inline test (prefix1) --

data X a
  = X { b :: Int, a, c :: a }
  | Y { b :: Int }
  | Z { d :: Int }

x1 = X 1 'a' 'b'
x2 = case x1 of
       X { c = cc, b=bb } -> (bb,cc)

main = x2 # 1
