let  data List a = Nil | Cons a (List a)                                            -- (1)
     data Bool = False | True                                                       
     not :: Bool -> Bool
     not = ...
     class Eq a where
       eq :: a -> a -> Bool                                                         
       ne :: a -> a -> Bool
     instance dEqInt <: Eq Int where                                                -- (2)
       eq = ...
       ne = \x y -> not (eq x y)
in
let  filter  ::  (a -> Bool) -> List a -> List a
     filter  =   ...
     nub     ::  Eq a => List a -> List a
     nub     =   \xx ->  case xx of
                           Nil        -> Nil
                           Cons x xs  -> Cons x (nub (filter (ne x) xs))
in   nub  (# (dEqInt | eq := \_ _ -> False) <: Eq Int #)                            -- (3)
          (Cons 3 (Cons 3 (Cons 4 Nil)))
