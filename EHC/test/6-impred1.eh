let t :: (a->a) -> Int
    -- test :: (a -> a) -> Test
    test = \a -> let x1 = a 3
                     -- x2 = a 'x'
                     y = t a
                 in  y
in  3
