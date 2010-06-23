{- ----------------------------------------------------------------------------------------
   what    : correct context reduction, not failing with 'cannot prove' Test.C [Test.J c_3_327_0]
   expected: ok
   tests   : fix of bug caused by recursive use of instance under construction
---------------------------------------------------------------------------------------- -}

module ClassCtxtRed2 where

data K a
data J a

class D a               where d :: a -> String
instance C a => D (K a) where d (x :: K a) = "DKa" ++ c (undefined :: a)

class C a        where
  c :: a -> String
  c _ = "Ca"

instance C [a]   where
  c (x :: [a]) = "CLa"
instance C (J a) where
  c (x :: J a) = "CJa" ++ d (undefined :: K [J a])

main = putStrLn (take 30 $ c (undefined :: J Int))

