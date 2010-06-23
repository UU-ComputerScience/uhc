let data Bool = True | False
in
let  auto :: (a->a) -> (b->b)
     auto = \f -> f f
     id :: a -> a
     id = \x -> x
     app :: (a->b) -> (a->b)
     app = \f -> \a -> f a
     choose :: a -> a -> a
     choose = \a -> \b -> if True then a else b
     v = (\x -> x id) auto
     w = app auto id
     x = id auto
     y = choose id auto
in   w 3

