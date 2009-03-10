%%[99

module EHC.Enum
where

import EHC.Prelude

instance Enum () where
    toEnum 0           = ()
    fromEnum ()        = 0
    enumFrom ()        = [()]

%%]
