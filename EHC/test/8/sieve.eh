let data Ordering = LT | EQ | GT
    data Bool = False | True
    data List a = Nil | Cons a (List a)
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
    foreign import jazy "primDivInt" div :: Int -> Int -> Int
    foreign import jazy "primMulInt" mul :: Int -> Int -> Int
    foreign import jazy "primSubInt" sub :: Int -> Int -> Int
    foreign import jazy "primCmpInt" compare :: Int -> Int -> Ordering
    filter
        = \p -> \l ->
            case l of
                Cons h t -> case p h of
                                True -> Cons h (filter p t)
                                False -> filter p t
    take
        = \n -> \l ->
            case l of
                Cons h t -> case compare n 0 of
                                GT -> Cons h (take (sub n 1) t)
                                _  -> Nil
                _        -> Nil
    length
        = \l ->
            case l of
                Cons h t -> add 1 (length t)
                _        -> 0
    eq  = \x -> \y ->
            case compare x y of
                EQ -> True
                _  -> False
    not = \x ->
            case x of
                True -> False
                False -> True
    ne  = \x -> \y ->
            not (eq x y)
in
let notMultiple
        = \x -> \y ->
            not (eq (mul (div y x) x) y)
    sieve
        = \(Cons h t) ->
            Cons h (sieve (filter (notMultiple h) t))
    from
        = \s ->
            Cons s (from (add s 1))
in  length (take 50 (sieve (from 2)))
