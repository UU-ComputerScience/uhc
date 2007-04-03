let data List a = Cons a (List a)
                | Nil
in let

    een :: List Int
    een = Nil

    twee :: forall a . List a
    twee = Nil
in (een, twee)

    
