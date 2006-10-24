let  id :: Int -> Int
     id = \x -> x
     fst :: (Int,Char) -> Int
     fst = \(a,b) -> a
in   id (fst (id 3,'x'))
