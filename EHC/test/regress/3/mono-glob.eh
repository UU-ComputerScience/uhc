let  h :: a -> a -> a
     f = \x ->  let  g = \y -> (h x y, y)
                in   g 3
in   f 'x'
