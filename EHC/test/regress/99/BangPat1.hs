{- ----------------------------------------------------------------------------------------
   what    : check whether pragma indeed turns on bang patterns
   expected: ok
---------------------------------------------------------------------------------------- -}

{-# LANGUAGE BangPatterns #-}

module BangPat1 where

(!) :: a -> b -> c
x ! y = undefined
x = 0 ! 0

main = return ()
