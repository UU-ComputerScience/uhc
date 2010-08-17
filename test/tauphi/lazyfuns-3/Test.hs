f :: Int -> Int
f = undefined

g :: Int -> Int
g x = x

-- Polymorhpic version doesn't work in r1811
undefined :: Int -> Int
undefined = undefined

main = 3

