{- ----------------------------------------------------------------------------------------
   what    : EHC.Ptr test, partial impl for Foreign.Ptr
   expected: ok
---------------------------------------------------------------------------------------- -}

module EhcPtr1 where

import UHC.Ptr

main :: IO ()
main
  = do putStrLn (show nullPtr)
       putStrLn (show (nullPtr `plusPtr` 200))
       putStrLn (show (nullPtr `plusPtr` -1))
       putStrLn (show ((nullPtr `plusPtr` 5) `alignPtr` 8))
