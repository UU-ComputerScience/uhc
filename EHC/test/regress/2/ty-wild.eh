let  id  ::  ... -> ...
     id  =   \x -> x
in   let  f   ::  (Int -> Int) -> ...
          f   =   \i -> \v -> i v
          v   =   f id 3
     in   let  v = f id 3
          in   v
