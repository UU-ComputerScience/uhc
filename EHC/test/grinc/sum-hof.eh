let data Bool = False | True
    data List a = Nil | Cons a (List a)
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
    foreign import jazy "primGtInt" gt :: Int -> Int -> Bool
in
let upto = \m n ->
        if gt m n then
            Nil
        else
            Cons m (upto (add m 1) n)
    foldl = \f b l ->
        case l of
            Nil       -> b
            Cons n ns -> f (foldl f b ns) n
    sum = foldl add 0
in sum (upto 0 9)
