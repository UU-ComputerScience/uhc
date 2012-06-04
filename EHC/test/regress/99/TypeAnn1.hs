{- ----------------------------------------------------------------------------------------
   what    : annotation as signature or in expr should work the same
   expected: ok, no output, no errors
---------------------------------------------------------------------------------------- -}

{-# LANGUAGE ExistentialQuantification #-}

module TypeAnn1 where

data Shape a
data Rectangle a
data Circle a

rect = undefined :: Shape (Rectangle ())

circ = undefined :: Shape (Circle ())

-- didn't work
homoExt1 = [rect, circ] :: [exists a. Shape a]

homoExt2 :: [exists a. Shape a]
homoExt2 = [rect, circ]

main = return ()
