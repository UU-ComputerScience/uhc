-- decl reordering, infixity, decl syntax, operator sections

module Aap where

main :: Int
main = v5

infixr 9 *, /
infixr 8 +, -
infix  8 ^

negate :: Int -> Int
(-), (+), (*), (/), (^) :: Int -> Int -> Int
f :: Int -> Int -> Int
-- f x y = x
x `f` y = x
g1, g2 :: (Char,Int)
g1 = ('x',4)
g2@(g2a,g2b) = (,) 'x' 4
v1, v4, v5, v6, v7, v8 {-, v9, v10, v11 -} :: Int
v1 = -3
v2, v3 :: Int -> Int
v2 = (3-)
v3 = (+3)
v4 = x1+x2
   where x1, x2 :: Int
         x1 = 3
         x2 = x1
v5 = 4
v6 = (+) 3 4
v7 = 3 * 4 + 5 * 6
v8 = 3 + 4 * 5 + 6
-- v9 = 3 ^ 3 ^ 3
-- v10 = 3 ^ 3 + 3
-- v11 = v10

-- u1 :: Aap

