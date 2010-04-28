{- ----------------------------------------------------------------------------------------
   what    : type indexed programming with higher order context resolution
   expected: ok
---------------------------------------------------------------------------------------- -}

module ClassHigherOrder1 where

data Rose f p = Rose p (f (Rose f p))

class C a where
  c :: a -> String

instance C Int where
  c _ = "I"

instance (C a) => C [a] where
  c (_::[a]) = "[" ++ (c (undefined::a)) ++ "]"

instance (C (f (Rose f p)), C p) => C (Rose f p) where
  c (_::Rose f p) = "R" ++ (c (undefined::p)) ++ take 2 (c (undefined::(f (Rose f p))))

x = c (undefined :: Rose [] Int)

main = putStrLn x
