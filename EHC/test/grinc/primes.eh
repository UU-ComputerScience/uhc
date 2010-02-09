let data Bool = False | True
    data List a = Cons a (List a) | Nil
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
    foreign import jazy "primSubInt" sub :: Int -> Int -> Int
    foreign import jazy "primModInt" mod :: Int -> Int -> Int
    foreign import jazy "primEqInt"  eq  :: Int -> Int -> Bool
in
let not      = \x -> if x then False else True
    iterate  = \f x -> Cons x (iterate f (f x))
    filter   = \f l -> case l of
                           Cons h t  ->  let tl = filter f t
                                         in if f h then Cons h tl else tl
                           Nil       ->  Nil
    map      = \f l -> case l of
                           Cons h t  ->  Cons (f h) (map f t)
                           Nil       ->  Nil
    head     = \l   -> case l of
                           Cons h t  ->  h
    index    = \l i -> case l of
                           Cons h t  -> if eq i 0 then h else index t (sub i 1)


in
let isdivs      = \n x -> not (eq 0 (mod x n))
    thefilter   = \l -> case l of
                    Cons n ns -> filter (isdivs n) ns
    primes      = map head (iterate thefilter (iterate (add 1) 2))
in  index primes 10
