let  add  ::  Int -> Int -> Int
     f    ::  (r\x, r\y) => (r|x :: Int,y :: Int) -> Int
     f    =   \r -> add r#x r#y
in
let  v1 = f (x = 3, y = 4)
     v2 = f (y = 5, a = 'z', x = 6)
in   v2
