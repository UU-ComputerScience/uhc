let

data Ref a env
 = Zero               , env = (a, env')
 | Succ (Ref a env')  , env = (x, env')

in let

data Equal a b
  = Eq, a = b

in let

data Maybe a
  = Nothing
  | Just a

in let

match :: Ref a env -> Ref b env -> Maybe (Equal a b)
match = \r1 -> \r2 ->
  case (r1, r2) of
    (Zero, Zero) -> Just Eq
    (Succ x, Succ y) -> match x y
    (_, _)       -> Nothing

in 3
