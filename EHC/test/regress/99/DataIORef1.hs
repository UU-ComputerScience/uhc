{- ----------------------------------------------------------------------------------------
   what    : library Data.IORef
   expected: ok
---------------------------------------------------------------------------------------- -}

module DataIORef1 where

import Data.IORef

inc r
  = do v <- readIORef r
       writeIORef r (v + 1 :: Int)

main
  = do r <- newIORef (5::Int)
       v <- readIORef r
       putStrLn (show v)
       inc r
       v <- readIORef r
       putStrLn (show v)
