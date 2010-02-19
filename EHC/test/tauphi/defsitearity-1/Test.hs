stupid :: Int -> Char -> Char -> Int -> Char -> Int
stupid x y z = let c    = x
                   id :: Int -> Int
                   id x = x
               in \a b -> id c

const :: Int -> Char -> Int
const x y = x

main = const 3 'x'

