let  data List a = Nil | Cons a (List a)
in   let  v =  case Cons 3 Nil of
                 Nil       -> 5
                 Cons x y  -> x
     in   v
