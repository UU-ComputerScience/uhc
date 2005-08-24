let  id :: a -> a
     id = \x -> x
in
let  f = \i -> (i 3,i 'x')
     v = f ~id
in   v
