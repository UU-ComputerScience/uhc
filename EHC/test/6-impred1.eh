let const = \x y -> y
in
let data Test = Test (forall a . a->a)
    -- test :: (a -> a) -> Test
    test = \a -> const (a 3) (Test a)
in  3
