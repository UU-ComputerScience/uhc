let  data Bool    = False | True
     data List a  = Nil | Cons a (List a)
in   let  class Eq a where
            eq :: a -> a -> Bool
          instance dEqInt <: Eq Int where
            eq = ...
          instance dEqList <: Eq a => Eq (List a) where
            eq = ...
in   let  f :: Eq a => a -> List a -> Bool
          f = \p q -> eq (Cons p Nil) q
     in   f 3 (Cons 4 Nil)

