%%[99

module UHC.Enum
where

import UHC.Base

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]

%%]

%%[99
%%]
