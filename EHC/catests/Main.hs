module Test where

app3 :: a -> b -> c -> (a -> b -> c -> Bool) -> Bool
app3 x y z f = f x y z

-- main = print $ app3 undefined undefined undefined undefined