let data A a b = A a (B a b)
    data B a b = B a (A a b)
    data C a b = C a (D a b)
    data D a b = D a (C b a)
 in let v :: D Int Int
     in v