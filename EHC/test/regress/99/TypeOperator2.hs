{- ----------------------------------------------------------------------------------------
   what    : type level operator syntax, for classes and an instance
   expected: all ok
---------------------------------------------------------------------------------------- -}

module TypeOperator2 where

class a :<: s where

instance a :<: a where

main = return ()
