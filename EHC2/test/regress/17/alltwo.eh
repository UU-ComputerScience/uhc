let

data AllTwo a b
  = One a
  | Two (b -> a)

in let

x1 = One 1
x2 = Two (\x -> x)

in (x1, x2)
