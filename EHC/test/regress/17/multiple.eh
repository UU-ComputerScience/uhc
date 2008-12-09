let

  data Maybe a = Nothing | Just a
  data List a = Nil | Cons a (List a)
  data Tuple a b = Tuple a b
  data Arr a b = Arrow (a -> b)
  
in let

  data Rose a = Branch a (List (Rose a))
  data GRose f a = GBranch a (f (GRose f a))
  data GGRose f a = GGBranch (f a (GGRose f a))

in let

  data RoseAlt a = RA (GRose List a)
  data ListAlt a = LA (GRose Maybe a)
  data InfArrList a = IAL (GGRose Arr a)
  data InfNestedTuple a = INT (GGRose Tuple a)

in 3
