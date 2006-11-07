-- data field pattern match: ok, matchorder same as def order, pun
-- %% inline test (prefix1) --

data X a
  = X { b :: Int, a, c :: a }
  | Y { b :: Int }
  | Z { d :: Int }

x1 = X 1 'a' 'b'
x2 = case x1 of
       X { b, c = cc } -> (b,cc)

main = x2 # 1
