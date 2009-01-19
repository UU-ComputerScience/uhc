data T a = T a a

main = (\(T a b) -> a) (T 23 13)
