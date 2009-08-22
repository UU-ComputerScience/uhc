-- basic case: deriving for Eq for some data type, expected outcome: 311312
-- %% inline test (prefix1) --

class Eq x where
  (==) :: x -> x -> Bool
  (/=) :: x -> x -> Bool
  x /= y = not (x == y)

class Eq x => Ord x where
  compare :: x -> x -> Ordering

instance dEqInt <: Eq Int where
  (==) = primEqInt

instance dOrdInt <: Ord Int where
  compare = primCmpInt

instance dEqList <: Eq a => Eq [a] where
  (x:xs) == (y:ys) = x == y && xs == ys
  []     == []     = True
  _      == _      = False

instance dOrdList <: Ord a => Ord [a] where
  (x:xs) `compare` (y:ys) = case x `compare` y of {LT -> LT; EQ -> xs `compare` ys; GT -> GT}
  []     `compare` []     = EQ
  _      `compare` (_:_)  = LT
  (_:_)  `compare` _      = GT

data X a = ZZ | XX a Int | YY [a]
  deriving (dEqX <: Eq,dOrdX <: Ord)

main
  = (case YY [3] `compare` YY [3] of {EQ -> 2; LT -> 1; GT -> 3})
    `primAddInt`
    (case YY [2] `compare` YY [3] of {EQ -> 20; LT -> 10; GT -> 30})
    `primAddInt`
    (case YY [3] `compare` YY [2] of {EQ -> 200; LT -> 100; GT -> 300})
    `primAddInt`
    (case ZZ `compare` YY [2] of {EQ -> 2000; LT -> 1000; GT -> 3000})
    `primAddInt`
    (case XX 3 3 `compare` YY [2] of {EQ -> 20000; LT -> 10000; GT -> 30000})
    `primAddInt`
    (case XX 3 3 `compare` ZZ of {EQ -> 200000; LT -> 100000; GT -> 300000})

