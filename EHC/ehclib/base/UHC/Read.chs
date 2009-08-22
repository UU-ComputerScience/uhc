%%[99

module UHC.Read
where

import UHC.Base

instance Read () where
    readsPrec p = readParen False (\r -> [((),t) | ("(",s) <- lex r,
                                                   (")",t) <- lex s ])

%%]
