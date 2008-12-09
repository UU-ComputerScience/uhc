-- automatic removal of field 'a' in v1 to arg of f coercion.

let  f :: (x::Int) -> Int
     f = \r -> r#x
in let
     v1 = (a=3,x=4)
in let
     v2 = f v1
in   v2

