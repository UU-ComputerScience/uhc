let g :: (exists a . (a,a->Int)) -> Int
    id :: a -> a
    f = \x -> let y1 = g x
                  y2 = g x
              in  y1
    v = f (3,id)
in  6
