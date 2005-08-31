let  data K a = K (a -> b) (b -> Int)
     data Bool = True | False
     (K f g) = K (\x -> x) (\x -> 3)
in
let  v1 = g (f 3)
     v2 = g (f True)
in   3
