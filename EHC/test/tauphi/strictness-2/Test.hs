-- foo :: Int -> @strict Char -> @strict @unique Char -> @unique Int -> @strict Char -> Int
-- foo x y z = let const :: Int -> Char -> Int
--                 const x y = x
--                 bar       = const x y
--             in \a b -> const bar y
-- 
const :: @nonUnique @strict Int -> Char -> Int
const x y = x

id :: @strict Int -> Int
id x = x

main = id 3 -- const 2 'x'

