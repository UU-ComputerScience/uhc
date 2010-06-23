let

data AllThree a b c
  = One a
  | Two (b -> a)
  | Three (c -> c)  -- all invariant due to this one

in let

x1 = One 1
x2 = Two (\x -> x)
x3 = Three (\x -> x)

in (x1, x2, x3)
