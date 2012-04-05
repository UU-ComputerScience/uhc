{- ----------------------------------------------------------------------------------------
   what    : Escape of fixed tyvar, clashing with escaped/fixed tyvar
   expected: error
---------------------------------------------------------------------------------------- -}

{-# LANGUAGE RankNTypes, NoGenericDeriving #-}
module ForallBugUHC where

data Shape = Shape { 
   getX :: forall b. IO (Maybe b)
}

-- shape :: forall a . Monad a => Maybe c_3_80_0_0 -> a Shape
shape p = do
   return Shape {
      getX = return p
   }

-- This should fail as Char /= c_3_80_0_0
s1 = shape (Just 'x')

-- This should be ok as Nothing is polymorphic, so may instantiate to fixed tyvar
s2 = shape Nothing

main = s1
