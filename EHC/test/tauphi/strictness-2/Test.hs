-- foo :: Int -> @strict Char -> @strict @unique Char -> @unique Int -> @strict Char -> Int
-- foo x y z = let const :: Int -> Char -> Int
--                 const x y = x
--                 bar       = const x y
--             in \a b -> const bar y
-- 
const :: @strict Int -> Char -> Int
const x y = x

main = const 2 'x'

