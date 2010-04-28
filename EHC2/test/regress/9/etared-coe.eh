{- Via Christof Douma:
   Eta reduction related problem (because of previous absence of proper eta reduction).
   See get.
-}
let foreign import jazy "primAddInt" add :: Int -> Int -> Int
    undefined = 1
in
let data T a b = T a b | DummyT -- keep ehc from using records
    data E = E | DummyE -- keep ehc from using records
in
let return :: a -> Int -> T Int a
    return = \a s -> T s a
    
    bind :: (Int -> T Int a) -> (a -> Int -> T Int b) -> Int -> T Int b
    bind = \m f s -> case m s of
		       T s' r -> f r s'

    run :: (Int -> T Int a) -> a
    run = \m -> case m 0 of
                  T _ r -> r

    swap :: Int -> Int -> T Int Int
    swap = \n s -> T n s

{-
    get :: Int -> T Int Int
    get = bind (swap undefined) (\s -> bind (swap s) (\_ -> return s))
-}

    {- bug in ehc!! This generate wrong code (get becomes a CAF somehow)
    -}
    get :: Int -> T Int Int
    get = \s -> T s s

    set :: Int -> Int -> T Int E
    set = \n s -> T n E

    plus :: Int -> Int -> T Int E
    plus = \i -> bind get (\s -> set (add i s))

in run (  bind (set 3) (\_ -> bind (plus 4) (\_ -> get)) )
