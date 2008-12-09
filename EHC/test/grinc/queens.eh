let data List a = Nil | Cons a (List a)
    data Bool = False | True
in
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
    foreign import jazy "primSubInt" substract :: Int -> Int -> Int
    foreign import jazy "primGtInt"  gt :: Int -> Int -> Bool
    foreign import jazy "primEqInt"  eq :: Int -> Int -> Bool
in
let not = \b -> case b of
                    True -> False
                    False -> True
    neq = \a b -> not (eq a b)
    and = \a b -> case a of 
	  	     False -> False
                     True  -> b
in
let length = \l -> case l of
                       Nil      -> 0
                       Cons h t -> add 1 (length t)
    append = \l ys -> case l of
                          Nil      -> ys
                          Cons h t -> Cons h (append t ys)
    upto = \m n -> case gt m n of
                       True -> Nil
                       False -> Cons m (upto (add m 1) n)
    safe = \x d l -> case l of
                         Nil      -> True
                         Cons h t -> and (neq x h) 
                                         (and (neq x (add h d)) 
                                              (and (neq x (substract h d)) 
                                                   (safe x (add d 1) t)
                                              )
                                         )
    ok = \l -> case l of
                   Nil      -> True
                   Cons h t -> safe h 1 t
    mapCons = \xs l -> case l of
                           Nil      -> Nil
                           Cons h t -> Cons (Cons h xs) (mapCons xs t)
    filterOk = \l -> case l of
                         Nil      -> Nil
                         Cons h t -> case ok h of
                                         True  -> Cons h (filterOk t)
                                         False -> filterOk t
    addOne = \nq l -> filterOk (mapCons l (upto 1 nq))
    concatMapAddOne = \nq l -> case l of
                                   Nil      -> Nil
                                   Cons h t -> append (addOne nq h)
                                                      (concatMapAddOne nq t)
    generate = \nq n -> case eq n 0 of
                            True  -> Cons Nil Nil
                            False -> concatMapAddOne nq (generate nq (substract n 1))
    solve = \nq -> length (generate nq nq)
in solve 8
