stupid :: Int -> Char -> Char -> Int -> Char -> Int
stupid x y z = let c    = id x
                   id :: Int -> Int
                   id x = x
               in \a b -> id c

const :: Int -> Char -> Int
const x y = x

caf :: Int
caf = let  id :: Int -> Int
           id x = x
      in id 34

main = const 3 'x'

