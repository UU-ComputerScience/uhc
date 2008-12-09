let

data List a = Nil | Cons a (List a)

in let

data GRose f a = GBranch a (f (GRose f a))

in let

x = GBranch 3 (Cons x Nil)

in x
