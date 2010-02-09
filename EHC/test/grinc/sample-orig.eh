let data List a = Nil | Cons a (List a)
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
in
let foldl = \f b l ->
        case l of
            Nil       -> b
            Cons n ns -> f (foldl f b ns) n
in foldl add 0 (Cons 1 (Cons 2 (Cons 3 (Cons 4 (Cons 5 Nil)))))
