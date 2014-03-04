{- ----------------------------------------------------------------------------------------
   what    : correct defaulting to Integer in instance decl, due to bug not doing so
   expected: ok
---------------------------------------------------------------------------------------- -}

module Defaulting1 where

f :: Num a => a -> ()
f _ = ()

class C a where
  it :: a


-- fix of previous bug:
-- Here I get a type error:
-- Test.hs:14-15:
--   Predicates remain unproven:
--     preds: UHC.Base.Num v_3_190_2
--              at   : 
--              trace: (UHC.Base.Num v_3_190_2,<4,0>,Test.hs:15:8): FAIL
--                     (UHC.Base.Num v_3_190_2,<4,0>,Test.hs:15:8): FAIL
instance C () where
  it = f 0

-- This one compiles fine.
it' :: ()
it' = f 0

main = print (it :: ())
