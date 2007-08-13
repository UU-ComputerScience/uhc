let data X a = X b (b->a)
    id = \x -> x
 in let v1 :: X Int
        v1 = X (3,4) (\(x,y) -> x)
        v2 = X 3 id
        f = \(X x g) -> g x
     in f (X (f v1) id)
