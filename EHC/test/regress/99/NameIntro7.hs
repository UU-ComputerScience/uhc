{- ----------------------------------------------------------------------------------------
   what    : name introduction
   expected: error, report of duplicates
---------------------------------------------------------------------------------------- -}

module Main where

-- type variable introduction in data decl
data D a a = D

-- type variable introduction in class decl
class C b b where
  cc :: b -> b

-- type variable introduction in type decl
type T c c = Int

-- type variable introduction in quantified type
q :: forall d d . d -> d

main :: IO ()
main = putStr "Dummy"
