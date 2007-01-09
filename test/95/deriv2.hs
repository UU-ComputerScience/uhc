-- basic case: deriving for Eq for some [data type], expected outcome: 221
-- %% inline test (prefix1) --

class Eq x where
  (==) :: x -> x -> Bool
  (/=) :: x -> x -> Bool
  x /= y = not (x == y)

instance dEqInt <: Eq Int where
  (==) = primEqInt

instance dEqList <: Eq a => Eq [a] where
  (x:xs) == (y:ys) = x == y && xs == ys
  []     == []     = True
  _      == _      = False

data X a = XX a Int | YY [a]
  deriving (dEqX <: Eq)

main
  = (if YY [3] == YY [3] then 1 else 2)
    `primAddInt`
    (if YY [3] /= YY [3] then 10 else 20)
    `primAddInt`
    (if YY [3] == YY [4] then 100 else 200)

