{- ----------------------------------------------------------------------------------------
   what    : (im)predicativity, examples from Daan Leijen. HML
   expected: 
---------------------------------------------------------------------------------------- -}

module Main where

choose :: a -> a -> a
choose x y = x

fch1 :: Int -> Int
fch2 :: forall a . a -> a

fch3 :: (Int -> Int) -> Int
fch4 :: (forall a . a -> a) -> Int

fch5 :: ((Int -> Int) -> Int) -> Int
fch6 :: ((forall a . a -> a) -> Int) -> Int

-- ok
-- Int -> Int
ch12 = choose fch1 fch2
{- mlf:
*Main> check "let f1 = \\(x :: Int) -> x in let f2 = \\y  -> y in choose f2 f1"
expr: let f1 (x::Int) = x;
          f2 y = y
      in choose f2 f1
type: Int -> Int
-}

-- ok, but fails when args are swapped: BAD order problem!!!
-- (forall a . a -> a) -> Int
ch34 = choose fch4 fch3
{- mlf:
*Main> check "let f1 = \\(f :: Int -> Int) -> 5 in let f2 = \\(g :: forall a . a -> a) -> 4 in choose f2 f1"
expr: let f1 (f::Int -> Int) = 5;
          f2 (g::forall a. a -> a) = 4
      in choose f2 f1
*** Exception: error: cannot unify types:
 type1: Int -> Int
 type2: forall a. a -> a
-}

-- ok, but fails when args are swapped: BAD order problem!!!
-- ((Int -> Int) -> Int) -> Int
ch56 = choose fch5 fch6
{- mlf:
*Main> check "let f1 = \\(f :: (Int -> Int) -> Int) -> 5 in let f2 = \\(g :: (forall a . a -> a) -> Int) -> 4 in choose f2 f1"
expr: let f1 (f::(Int -> Int) -> Int) = 5;
          f2 (g::(forall a. a -> a) -> Int) = 4
      in choose f2 f1
*** Exception: error: cannot unify types:
 type1: Int -> Int
 type2: forall a. a -> a
-}

{-
-- mlf1: ( [(Int
            ,EHC.Prelude.Bool
            )
           ]
         , [Int -> Int]
         )
-}

main = return ()
