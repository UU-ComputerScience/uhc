{- ----------------------------------------------------------------------------------------
   what    : check whether pragma indeed turns off bang patterns, which is the default anyway
   expected: parse errors
---------------------------------------------------------------------------------------- -}

{-# LANGUAGE NoBangPatterns #-}

module BangPat2 where

(!) :: a -> b -> c
x ! y = undefined
x = 0 ! 0

main = return ()
