module Main where

{-
  See the comment in src/ehc/EH/ExtraChecks.cag.
  Currently no error message is generated for the correct example below.
  However, neither a check is done for real duplicates.
-}

_ltImin :: Int
( _ltImax,_ltImin,_ltItree) =
  (3,4,5 )

main = return ()

