%%[99

module EHC.Show
where

import EHC.Prelude

instance Show () where
    showsPrec p () = showString "()"

%%]
