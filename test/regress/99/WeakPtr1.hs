{- ----------------------------------------------------------------------------------------
   what    : weak ptr explicit finalization
   expected: inaccessibility after finalize
---------------------------------------------------------------------------------------- -}

module WeakPtr1 where

import UHC.WeakPtr
import UHC.Weak
import UHC.GC

f :: [Int] -> IO ()
f x = do print x
         w <- mkWeak x x (Just $ print "fin")
         x2 <- deRefWeak w
         print x2
         finalize w
         x3 <- deRefWeak w
         print x3

main = f [2,3]

