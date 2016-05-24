{- ----------------------------------------------------------------------------------------
   what    : combi of forall and exists which may not be coerced into each other
   expected: error, complaining about strengthen
---------------------------------------------------------------------------------------- -}

module Main where

import Prelude hiding (unsafeCoerce)

{-
In your implementation of ST, you seem to be unifying the following two types:

forall x . exists y . t x y

and

exists y . forall x . t x y

This is the thing your whole paper is based on.

However, the above is unsound. It allows us to implement:
-}

strengthen :: forall p. (forall x. exists y. p x y) -> (exists y. forall x. p x y)
strengthen x = x

{-
This function is a death blow to your type system. We can use it to implement unsafeCoerce:
-}

data B x y = B (x -> y) (y -> x)

bId :: forall x. exists y. B x y
bId = B id id

bsId :: exists y. forall x. B x y
bsId = strengthen bId

unpackTo :: forall y. (forall x. B x y) -> (forall x. x -> y)
unpackTo (B x _) = x

unpackFrom :: forall y. (forall x. B x y) -> (forall x. y -> x)
unpackFrom (B _ x) = x

usem :: forall y a b. (forall x. B x y) -> (a -> b)
usem b = unpackFrom b . unpackTo b

unsafeCoerce :: a -> b
unsafeCoerce = usem bsId

-- And indeed we can make a well-typed program segfault:

crash :: (a -> b) -> String
crash = unsafeCoerce

main = putStrLn (crash id)



