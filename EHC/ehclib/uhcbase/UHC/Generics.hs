{-# LANGUAGE NoImplicitPrelude, CPP #-}

--  Only major extension required
{-#  LANGUAGE MultiParamTypeClasses  #-}

--  For aesthetics only
{-#  LANGUAGE EmptyDataDecls  #-}
{-#  LANGUAGE TypeOperators  #-}
{-#  LANGUAGE TypeSynonymInstances  #-}

--  To remove
{-#  LANGUAGE ScopedTypeVariables  #-}

-- All contents moved to UHC.Base because of necessity to have definitions already available for first datatype defined.
-- This file for now functions as placeholder only, until proper place and exporting is fixed.

module UHC.Generics
  (
  -- Generics
  -- * Generic representation types
    V1, U1(..), Par1(..), Rec1(..), K1(..), M1(..)
  , (:+:)(..), (:*:)(..), (:.:)(..)

  -- ** Synonyms for convenience
  , Rec0(..), Par0(..), R, P
  , D1(..), C1(..), S1(..), D, C, S

  -- * Meta-information
  , Datatype(..), Constructor(..), Selector(..)
  , Arity(..), Fixity(..), Associativity(..)
  , NoSelector

  -- * Representable type classes
  , Representable0(..), Representable1(..)
  )
  where

import UHC.Base
import UHC.Generics.Tuple

