{- ----------------------------------------------------------------------------------------
   what    : (im)predicativity
   expected: types of h1 & h3 the same, h2 & h4 too
   but     : h2 & h4 are not the same, h2 should have h4's type.
             This is an instance of the inference order problem
---------------------------------------------------------------------------------------- -}

module Main where

f1 :: forall a . a -> [a] -> [a]
f2 :: forall a . [a] -> a -> [a]
ids :: [forall a. a->a]

h1  = f1 (\x->x)  ids
h2  = f1 (\x->x) ~ids						-- should be: [forall a. a->a], but is forall a.[a->a]
h2' = f1 (\x->x :: forall a . a -> a) ~ids	-- the extra annotation fixes this

h3  = f2  ids (\x->x)
h4  = f2 ~ids (\x->x)

main = return ()
