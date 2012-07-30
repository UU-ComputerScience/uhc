{- ----------------------------------------------------------------------------------------
   what    : empty context on instance, parsing of it
   expected: ok
---------------------------------------------------------------------------------------- -}

module InstanceCtxt1 where

class Foo a where
   foo :: a -> Int

instance () => Foo Int where
   foo = undefined

main = return ()
