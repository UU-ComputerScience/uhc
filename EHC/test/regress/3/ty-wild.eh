let  id  ::  %a -> %a
     id  =   \x -> x
in   let  f   ::  (Int -> Int) -> %a
          f   =   \i -> \v -> i v
          v   =   f id 3
     in   let  v = f id 3
          in   v
