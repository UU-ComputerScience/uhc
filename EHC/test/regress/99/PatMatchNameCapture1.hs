{- ----------------------------------------------------------------------------------------
   what    : name capture bug in pattern compiler
   expected: correct interspersion: "apbpc", instead of "aabbc"
---------------------------------------------------------------------------------------- -}

module PatMatchNameCapture1 where

intersperse :: a -> [a] -> [a]
intersperse _ []    = []
-- intersperse _ [y]   = [y]
intersperse _ [x]   = [x]			-- this did cause the problem, in the next pattern name capturing the 1st param x
intersperse x (h:t) = h : x : (intersperse x t)

main :: IO ()
main = print $ intersperse 'p' "abc"
