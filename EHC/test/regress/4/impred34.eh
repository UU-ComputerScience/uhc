let  f1 :: (a->a) -> Int
     f3 = \j -> let  w1 = j 3
                     w2 = j 'x'
                     w3 = f2 j
                in   w1
     f2 = \h -> let  v1 = f1 h
                     v4 = f3 h
                     v2 = h 3
                     v3 = h 'x'
                in   v2
in   3
