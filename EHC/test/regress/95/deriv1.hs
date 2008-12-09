-- basic case: deriving for Eq for some data type, expected outcome: 2121
-- %% inline test (prefix1) --

class Eq x where
  (==) :: x -> x -> Bool
  (/=) :: x -> x -> Bool
  x /= y = not (x == y)

instance Eq Int where
  (==) = primEqInt

data X a = XX a Int | YY a
  deriving (Eq)

main
  = (if XX 3 4 /= XX 3 3 then 1 else 2)
    `primAddInt`
    (if XX 3 4 == XX 3 3 then 10 else 20)
    `primAddInt`
    (if XX 3 3 == XX 3 3 then 100 else 200)
    `primAddInt`
    (if XX 3 3 == YY 3 then 1000 else 2000)

