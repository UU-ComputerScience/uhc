let  f :: (a -> a) -> Int
     f = \i -> i 3
     id :: a -> a
in   f id
