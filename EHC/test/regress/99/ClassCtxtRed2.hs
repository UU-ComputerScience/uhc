{- ----------------------------------------------------------------------------------------
   what    : correct context reduction, not failing with 'cannot prove' Test.C [Test.J c_3_327_0]
   expected: ok
   tests   : fix of bug caused by recursive use of instance under construction
---------------------------------------------------------------------------------------- -}

module ClassCtxtRed2 where

data K a
data J a

class D a               where d :: a -> String
instance C a => D (K a) where d = undefined

class C a        where c :: a -> String
instance C [a]   where c (x :: [a]) = d (undefined :: K [a])
instance C (J a) where c (x :: J a) = d (undefined :: K [J a])

main = print (c (undefined :: J Int))
