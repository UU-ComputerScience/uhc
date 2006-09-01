let  data Bool = False | True
     data Term a
       = Lit a, a = Int
       | Pair (Term b) (Term c), a = (b,c)
       | Fst (Term (b,c)), a = b
       | Inc (Term Int), a = Int
       | IsZ (Term Int), a = Bool
in
let  fst :: forall a . forall b . (a,b) -> a
     fst = \(a,b) -> a
in
let  eval :: Term a -> a
     eval = \x -> case x of
                    Lit i -> i
                    Pair a b -> (eval a, eval b)
                    Fst t -> fst (eval t)
in
let  v1 = eval (Lit 3)
     v2 = eval (IsZ (Lit 3))
in
let  data Eq a b = Eq, a=c, b=c
     data Maybe a = Just a | Nothing
in
let  test :: Term a -> Term b -> Maybe (Eq a b)
     test = \x y ->
                case x of
                    Pair x1 x2 ->
                        case y of
                            Pair y1 y2 ->
                                case test x1 y1 of
                                    Just Eq ->
                                        case test x2 y2 of
                                            Just Eq -> Just Eq
                                            _ -> Nothing
                                    _ -> Nothing
                            _ -> Nothing
                    Lit x1 ->
                        case y of
                            Lit y1 -> Just Eq
                            _ -> Nothing
in   test (Pair (Lit 3) (Lit 4)) (Pair (Lit 5) (Lit 6))
