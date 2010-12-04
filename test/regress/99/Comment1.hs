{- ----------------------------------------------------------------------------------------
   what    : commenting
   expected: ok
---------------------------------------------------------------------------------------- -}

module Comment1 where

--- should be comment this

-- but this not:
(-->) :: Int -> Int -> Int
a --> b = a + b

main :: IO ()
main = return ()
