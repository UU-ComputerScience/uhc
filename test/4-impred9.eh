let g1 :: (exists a . (a,a->Int)) -> Int
    g2 :: (exists a . ((a,a),(a,a)->Int)) -> Int
    id :: a -> a
    f = \x -> let y1 = g1 x
                  y2 = g2 x
              in  y1
in  6
