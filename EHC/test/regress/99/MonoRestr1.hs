{- ----------------------------------------------------------------------------------------
   what    : monomorphism restriction for binding to pattern, poly for single var binding
   expected: defaulting resolution to Integer, unless specified otherwise (for single var only)
             (BTW: single var may be restrained once too!)
---------------------------------------------------------------------------------------- -}

module MonoRestr1 where

import Data.Typeable

-- x@(x1:_) = [1,2]
x1 = [1,2]
x2@(x2a:_) = x1
x2a' = head x1

main
  = do print x2a
       print (typeOf x2a)
       print x2a'
       print (typeOf x2a')
       print (x2a' :: Int)
       print (typeOf (x2a' :: Int))
