%%[99

module EHC.Read
where

import EHC.Prelude

instance Read () where
    readsPrec p = readParen False (\r -> [((),t) | ("(",s) <- lex r,
                                                   (")",t) <- lex s ])

%%]
