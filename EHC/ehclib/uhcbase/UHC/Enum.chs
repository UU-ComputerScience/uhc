%%[99
{-# LANGUAGE NoImplicitPrelude #-}


module UHC.Enum
where

import UHC.Base0

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]

%%]

%%[99
%%]
