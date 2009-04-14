%%[99

module UHC.Enum
where

import UHC.Prelude

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]

%%]

%%[99
%%]
