{-# LANGUAGE ScopedTypeVariables #-}

{- ----------------------------------------------------------------------------------------
   what    : lexically scoped tyvar of function result
   expected: ok
---------------------------------------------------------------------------------------- -}

module LexScopedTyVar1 where

import Unsafe.Coerce

class GetObjectRef a where
  getObjectRef :: a -> b


cast :: forall a b. GetObjectRef b => a -> Maybe b
cast a :: Maybe b = 		-- the type of 'b' should be propagated
  if instanceOf a (getObjectRef (undefined :: b))
    then Just (unsafeCoerce a)
    else Nothing

instanceOf :: a -> b -> Bool
instanceOf = undefined

main = return ()
