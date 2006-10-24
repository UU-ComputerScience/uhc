let data L a = N | C a (L a)
 in let v1 = case C 3 N of
               C a b -> C a N
               C b a -> a
        v2 = case C 3 N of
               N -> N
               b -> b
        v3 :: L
     in v1
