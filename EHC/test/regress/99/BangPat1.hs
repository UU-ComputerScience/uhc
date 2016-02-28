{-# LANGUAGE BangPatterns #-}

module BugBangPat1 where

(!) :: a -> b -> c
x ! y = undefined
x = 0 ! 0

main = return ()
