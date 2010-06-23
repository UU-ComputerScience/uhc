module Main where

ids = []
ids :: [forall a . a -> a]
{-

(:) :: a -> [a] -> [a]
un: :: [a] -> (a,[a])

( f1           :g1:_) = ids
((f2::Int->Int):g2:_) = ids
((f3::Int->Int):(g3::forall a . a->a):_) = ids

(f4:g4:_) = const (const ids f4') g4'
f4' :: forall a. a -> a
f4' = f4
g4' :: forall a. a -> a
g4' = g4

(f4:g4:_) = const (const ids f4') g4'
(f4',g4') = (f4,g4)

((f5::Int->Int):g5:_) :: [forall a . a -> a] = ids
((f6::Int->Int):g6:_) = ids :: [forall a . a -> a]
-}

f7g7 = const (const ids f7) g7
f7 :: Int -> Int
f7 = head f7g7
g7 :: forall a. a -> a
g7 = head (tail f7g7)

{-
((f8::forall a . a->a):(g8::Int->Int):_) = ids
-}

const' :: [a] -> a -> [a]

f9g9 = const' (const' ids f9) g9
-- f9 :: Int -> Int
f9 = head f9g9
-- g9 :: forall a. a -> a
g9 = head (tail f9g9)

f0g0 = const' (const' ids f0) g0
f0 :: Int -> Int
f0 = head f0g0
g0 :: forall a. a -> a
g0 = head (tail f0g0)


main = return ()
