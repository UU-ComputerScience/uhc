{- ----------------------------------------------------------------------------------------
   what    : deriving for poly kinded type
   expected: ok
---------------------------------------------------------------------------------------- -}

module Main where

-- Foo :: forall k . k -> *
data Foo a = Bar deriving Eq

{- Eq can only be derived when types are equal. When not annotated, proving fails for Eq (Foo v), where v is a tvar, which is ok.
-}
main = print ((Bar :: Foo Int) == (Bar :: Foo Int))
