{- ----------------------------------------------------------------------------------------
   what    : bugfix triggered: pattern match involving prefix app of infix constructor
   expected: ok
---------------------------------------------------------------------------------------- -}

module PatMatchOp1 where

data a :*: b = a :*: b

t :: Int :*: Int -> ()
t ((:*:) m n) = ()

main :: IO ()
main = return ()
