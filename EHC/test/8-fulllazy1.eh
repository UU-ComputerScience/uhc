let c :: Int -> Int -> Int
    c = \_ _ -> 3
    f = \x -> let g = \y -> c (c x x) y
              in  c (g 3) (g 4)
in  f 6
