{- ----------------------------------------------------------------------------------------
   what    : weak ptr implicit finalization
   expected: finalization messages, but up til the 1500'th; correct intertwining with gc itself
   note    : output depends on gc frequency, which in the current config every 100 weakptrs; adapt the 1500 if not enough
---------------------------------------------------------------------------------------- -}

module WeakPtr3 where

import UHC.WeakPtr
import UHC.Weak
import UHC.GC
import Control.Monad

f1 :: Int -> IO ()
f1 _ = do let x = [23] :: [Int]
          print x
          mapM_ (\i -> mkWeak x x (Just $ print ("fin1_" ++ show i))) [1::Int .. 1500]

f2 :: Int -> IO ()
f2 _ = do let x = [45] :: [Int]
          print x
          w <- mkWeak x x (Just $ print "fin2")
          x2 <- deRefWeak w
          f1 5
          print x2

main = do f2 3
          gc
          return ()

