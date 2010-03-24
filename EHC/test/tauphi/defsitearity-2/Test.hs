foo :: Int -> Char -> Char -> Int -> Char -> Int
foo x y z = let const :: Int -> Char -> Int
                const x y = x
                bar       = id x
            in \a b -> const bar y

id :: Int -> Int
id x = x

main = foo 1 'x' 'y' 2 'z'

