data T = T !Bool

f = let x = True && False
  in let y = T x
     in y

main = print $ (\(T x) -> x) f