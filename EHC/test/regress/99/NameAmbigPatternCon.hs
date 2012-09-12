{-# LANGUAGE NoGenericDeriving #-}

{- ----------------------------------------------------------------------------------------
   what    : test for bug which did not detect ambiguous name references
   expected: error messages about 4 available occurrences
---------------------------------------------------------------------------------------- -}

module NameAmbigPatternCon where

import NameAmbigPatternConImpMod1
import NameAmbigPatternConImpMod2

-- intentional duplicate def
-- data Maybe a = Nothing | Just a
data Maybe a = Nothing | Just a

-- should give error
f (Just a) = id a

id x = x

main = return ()
