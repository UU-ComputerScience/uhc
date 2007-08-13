let  f = \(x::forall a . a -> a) -> \(y::a) -> x y
in
let  v = f (\(x::Int) -> x) 4
in   v
