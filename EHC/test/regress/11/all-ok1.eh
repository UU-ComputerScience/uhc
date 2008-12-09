let data L a = C a (L a) | N
in
let type String = L Char
    type S = L
in
let -- Id :: a -> a
    type Id = \a -> a
in
let v1 :: String
    v1 = C 'a' N
in  v1
