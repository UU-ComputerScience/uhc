{- ----------------------------------------------------------------------------------------
   what    : scanning of Prelude.. in Control.Category is indirectly tested here. If wrong the . there is bound to the wrong . (i.e. from Control.Category itself instead to Prelude..)
   expected: ok (if not, it loops)
---------------------------------------------------------------------------------------- -}

module Scan1 where

import qualified Control.Applicative as A

main = print $ 6 A.<$ Just 8
