let  Eq           ::  k -> k -> *
     data Eq a b  =   Eq (forall f . f a -> f b)
     id           =   \x -> x
in   let  v =  case Eq id of
                 (Eq f) -> f
     in   v
