let  data Term a
       = LitI a, a = Int
       | LitC a, a = Char
in
let  eval = \x -> case x of
                    LitI i -> i
                    LitC j -> j
in   3

