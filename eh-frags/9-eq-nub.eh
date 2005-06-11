let  data List a  = Nil | Cons a (List a)                                            
     class Eq a where
       eq :: a -> a -> Bool                                                         
       ne :: a -> a -> Bool
     instance dEqInt <: Eq Int where                                                -- (1)
       eq = primEqInt
       ne = \x y -> not (eq x y)
     nub     ::  forall a . Eq a => List a -> List a
     nub     =   \xx ->  case xx of
                           Nil        -> Nil
                           Cons x xs  -> Cons x (nub (filter (ne x) xs))
     eqMod2 :: Int -> Int -> Bool
     eqMod2 = \x y -> eq (mod x 2) (mod y 2)
in   nub  (!  ( eq =  eqMod2                                                        -- (2)
              , ne =  \x y -> not (eqMod2 x y)
              )  <: Eq Int
          !)
          (Cons 3 (Cons 3 (Cons 4 Nil)))
