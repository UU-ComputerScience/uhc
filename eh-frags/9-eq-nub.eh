let  data List a = Nil | Cons a (List a)                                     -- prelude
     data Bool = False | True                                                       
     not :: Bool -> Bool
     filter  ::  (a -> Bool) -> List a -> List a
     class Eq a where
       eq :: a -> a -> Bool                                                         
       ne :: a -> a -> Bool
     instance dEqInt <: Eq Int where
       eq = primEqInt
       ne = \x y -> not (eq x y)
in   let  nub     ::  Eq a => List a -> List a
          nub     =   \xx ->  case xx of
                                Nil        -> Nil
                                Cons x xs  -> Cons x (nub (filter (ne x) xs))
     in   nub  (# (dEqInt | eq := \x y -> ...) <: Eq Int #)
               (Cons 3 (Cons 3 (Cons 4 Nil)))
