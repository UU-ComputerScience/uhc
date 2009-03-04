%%[99

module EHC.Ord
where

import EHC.Prelude
import EHC.Eq

instance Ord () where
    compare () () = EQ

%%]
