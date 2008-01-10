{-
 - This is a EHC modified version of th 3^8 benchmark of the nofib/imaginary
 - Original code by Lennart Augustsson
 - Changes to be compiled by EHC: John van Schie
 -}

foreign import ccall "primSubInt" (-)  :: Int -> Int -> Int
foreign import ccall "primAddInt" (+)  :: Int -> Int -> Int

foreign import ccall "primLtInt"  (<)  :: Int -> Int -> Bool

infix 8 ^^^

data Bool = False | True

data Nat = Z | S Nat

-- Num instance
plusNum :: Nat -> Nat -> Nat
plusNum Z y     = y
plusNum (S x) y = S (plusNum x y)

mulNum :: Nat -> Nat -> Nat
mulNum  x Z     = Z
mulNum  x (S y) = plusNum (mulNum x y) x    

fromInteger :: Int -> Nat
fromInteger x   = if x < 1 then Z else S (fromInteger (x-1))

-- partain:sig
int :: Nat -> Int
int Z     = 0
int (S x) = 1 + int x

(^^^) :: Nat -> Nat -> Nat
x ^^^ Z   = S Z
x ^^^ S y = mulNum x (x ^^^ y)

power = 8

main = int ((fromInteger 3) ^^^ (fromInteger power))
