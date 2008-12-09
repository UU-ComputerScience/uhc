{- ----------------------------------------------------------------------------------------
   what    : (im)predicativity
   expected: types of cons1 & cons3 the same, cons2 & cons4 too
   but     : cons2 & cons4 are not the same, cons2 should have cons4's type.
             This is an instance of the inference order problem
---------------------------------------------------------------------------------------- -}

module Main where

cons :: forall a . a -> [a] -> [a]
cons h t = h : t
revcons :: forall a . [a] -> a -> [a]
revcons t h = h : t
ids :: [forall a. a->a]
ids = []

cons1  = cons (\x->x)  ids
cons2  = cons (\x->x) ~ids						-- should be: [forall a. a->a], but is forall a.[a->a]
cons2' = cons (\x->x :: forall a . a -> a) ~ids	-- the extra annotation fixes this

cons3  = revcons  ids (\x->x)
cons4  = revcons ~ids (\x->x)

main = return ()
