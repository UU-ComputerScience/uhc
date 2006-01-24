let  data Tup = Tup Int Int
     snd  = \(Tup x y) -> y
     app  = \f x -> f x
in   snd (app (Tup 1) 42)
