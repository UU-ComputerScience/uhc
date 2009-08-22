let  f :: (a -> a) -> Int
     f = \i -> i 3
     id :: a -> a
     id = \x -> x
in   f id
