let data Bool = False | True
    data List a = Nil | Cons a (List a)
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
    foreign import jazy "primGtInt"  gt :: Int -> Int -> Bool
in
let upto = \m n ->
        if gt m n then
            Nil
        else
            Cons m (upto (add m 1) n)
    sum = \l ->
        case l of
            Nil     -> 0
            Cons n ns -> add n (sum ns)
in sum (upto 1 10)
