-- various type synomyms

-- plain
data A2 a = A2 a
type A3 a = a -> a
type A1 a = a

-- mutual recursive (useless)
type P1 a = P2 a
type P2 a = P1 a

-- mutual recursive type+data
type R1 a = R2 a
data R2 a = R2 (R1 (a->Int))
