data T a = T a a

main = (\(T a b) -> b) (T 23 13)
