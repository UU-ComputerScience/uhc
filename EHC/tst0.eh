let data Bool = False | True
    data List a = Nil | Cons a (List a)
in
let class Eq a where
      eq :: a -> a -> Bool
      ne :: a -> a -> Bool
    class Eq a => Ord a where
      lt :: a -> a -> Bool
      gt :: a -> a -> Bool
    instance dEqInt = Eq Int where
      eq = \_ _ -> True
      ne = \_ _ -> True
    instance dEqList = Eq a => Eq (List a) where
      eq = \_ _ -> True
      ne = \_ _ -> True
    instance dOrdInt = Ord Int where
      lt = \_ _ -> True
      gt = \_ _ -> True
    instance dOrdList = Ord a => Ord (List a) where
      lt = \_ _ -> True
      gt = \_ _ -> True
in
let 
{-
    f1 = eq (Cons 3 Nil) (Cons 4 Nil)
-}
    f2 = eq 3 4
{-
-}
    f3 = Cons (eq 3 4) (Cons (lt 5 6) Nil)
{-
-}
    f4 = \x -> Cons (eq x x) (Cons (lt x x) Nil)
{-
    Expr_Let prvnGraph: [(# Eq c_243_0_0 #):[232_1],(# Ord c_243_0_0 #):[243_1,213_0_3_0]]
                        [213_1_0:ARG: pr=(# Eq c_243_0_0 #) cost=111 evid=<213_1_0>
                        ,213_0_3_0:ARG: pr=(# Ord c_243_0_0 #) cost=111 evid=<213_0_3_0>
                        ,213_1_1:AND: pr=(# Eq c_243_0_0 #) edges=[213_0_3_0] cost=2 evid=(<213_0_3_0>)
                                                                                           .(-1:0)
                        ,232_1:OR: pr=(# Eq c_243_0_0 #) edges=[213_1_0,213_1_1]
                        ,243_1:AND: pr=(# Ord c_243_0_0 #) edges=[213_0_3_0] cost=0 evid=<213_0_3_0>
                        ]
-}
    f5 = \x -> eq x x
{-
    Expr_Let prvnGraph: [(# Eq c_227_0_0 #):[227_1],(# Ord c_227_0_0 #):[213_0_3_0]]
                        [213_1_0:ARG: pr=(# Eq c_227_0_0 #) cost=111 evid=<213_1_0>
                        ,213_0_3_0:ARG: pr=(# Ord c_227_0_0 #) cost=111 evid=<213_0_3_0>
                        ,213_1_1:AND: pr=(# Eq c_227_0_0 #) edges=[213_0_3_0] cost=2 evid=(<213_0_3_0>)
                                                                                           .(-1:0)
                        ,227_1:OR: pr=(# Eq c_227_0_0 #) edges=[213_1_0,213_1_1]
                        ]
-}
in  3
