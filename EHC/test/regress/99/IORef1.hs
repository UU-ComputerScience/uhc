{- ----------------------------------------------------------------------------------------
   what    : IORef evaluating yes/no contents
   expected: contents should not be evaluated, hence it should be safe to put undefined in it, hence ok, no output
---------------------------------------------------------------------------------------- -}

module IORef1 where

import Data.IORef

main = newIORef undefined
        
