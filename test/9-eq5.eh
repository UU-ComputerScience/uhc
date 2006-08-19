let  data Bool    = False | True
     data List a  = Nil | Cons a (List a)
in   let  class Eq a where
            eq :: a -> a -> Bool
          instance dEqInt <: Eq Int where
            eq = ...
          instance dEqList <: Eq a => Eq (List a) where
            eq = ...
in   let  f :: Eq a  =>  a ->  List a  -> Bool
          f = \{! dEq_a <: Eq a !}
                     ->  \p    q       -> eq  {! dEqList dEq_a <: Eq (List a) !}
                                              (Cons p Nil) q
     in   f 3 (Cons 4 Nil)

