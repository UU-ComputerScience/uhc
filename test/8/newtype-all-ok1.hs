-- newtype use: all ok
-- %% inline test (prefix1) --

newtype N x = N { xx :: x }

x1 = N [4]
x2 = x1 {xx = 5}
x3 = xx x2
x4 = case x2 of
       N z -> z
x5 = case x1 of
       N [z] -> z

main = x3 + x4 + x5

