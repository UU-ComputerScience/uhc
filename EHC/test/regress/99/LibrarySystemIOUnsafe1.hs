{- ----------------------------------------------------------------------------------------
   what    : unsafePerformIO
   expected: ok
---------------------------------------------------------------------------------------- -}

module LibrarySystemIOUnsafe1 where

import System.IO.Unsafe

y = unsafePerformIO (do { putStrLn "y" ; return "z" }) ++ "Y"

main = putStrLn ("x" ++ y ++ "x")
