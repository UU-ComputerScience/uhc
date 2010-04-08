-- Test of lots of nested functions

foo :: Int -> Char -> Char -> (Int -> Char -> (Int -> Char -> Int))
foo x y z = let id :: Int -> Int
                id x = x
                bar = id x
            in \a b -> let const :: Int -> Char -> Int
                           const x y = x
                           baz = const bar b
                       in \c d -> id c

main = foo 1 'x' 'y' 2 'z'

