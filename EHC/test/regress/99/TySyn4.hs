{- ----------------------------------------------------------------------------------------
   what    : type synonym, in particular a simple renaming, ty eta reducible
   expected: ok, no output, no errors
---------------------------------------------------------------------------------------- -}

module TySyn4 where

import UHC.BoxArray

type Arr x = BoxArray x

indexArr :: Arr x -> Int -> x
indexArr = indexArray

main = return ()
