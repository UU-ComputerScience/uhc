{-# LANGUAGE NoGenericDeriving #-}

{- ----------------------------------------------------------------------------------------
   what    : NameAmbigPatternCon
   expected: not to be tested on its own
   constraints: exclude-if-js
   constraints: exclude-if-bc
---------------------------------------------------------------------------------------- -}

module NameAmbigPatternConImpMod2 where

import Prelude hiding (Maybe(..), id)

-- intentional duplicate def
data Maybe a = Nothing | Just a

-- should give error
f (Just a) = id a

id x = x
