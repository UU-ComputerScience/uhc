{- ----------------------------------------------------------------------------------------
   what    : correct context reduction, not failing with 'cannot prove' Integral a
   expected: ok
---------------------------------------------------------------------------------------- -}

module ClassCtxtRed1 where

import Debug.Trace

data FM a = FM a

lkup :: (Eq a, Show a) => FM a -> a -> a
lkup f a = if a == a then trace (show a {- -} ++ show (4::Int)) a else a

main = return ()
