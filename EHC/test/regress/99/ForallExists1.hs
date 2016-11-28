{- ----------------------------------------------------------------------------------------
   what    : combi of forall and exists which reflects construction of type depending on arg which can lazily on type level be fed back
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

uselessId :: forall a . a -> exists b . b
uselessId x = x

test = let y = uselessId y in y

main = return ()


