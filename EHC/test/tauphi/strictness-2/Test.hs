
const :: @strict Int -> @nonStrict Char -> Int
const x y = x

main = const 2 'x'

