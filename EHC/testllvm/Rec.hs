data Nat = Succ Nat | Zero

f (Zero)   = 55
f (Succ x) = f x

main = f (Succ (Succ (Succ Zero)))
