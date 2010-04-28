{- ----------------------------------------------------------------------------------------
   what    : kind signatures
   expected: error because U's kind is too general for Functor
---------------------------------------------------------------------------------------- -}

module KindSig2 where

-- the signature would make U acceptable for Functor
-- U :: * -> *
data U a = UU		-- inferred kind: forall a . a -> *

instance Functor U where
  fmap f UU = UU

main = return()
