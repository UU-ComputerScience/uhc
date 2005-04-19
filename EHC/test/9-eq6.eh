let  data Bool    = False | True
     data List a  = Nil | Cons a (List a)
in   let  class Eq a where
            eq :: a -> a -> Bool
          instance dEqInt <: Eq Int where
            eq = ...
          instance dEqList <: Eq a => Eq (List a) where
            eq = ...
in   let  f :: (Eq a => Eq (List a))  =>  Int ->  List Int  -> Bool
          f = \(! dEq_La <: Eq a => Eq (List a) !)
                                      ->  \p      q         -> eq  (Cons p Nil) q
     in   f 3 (Cons 4 Nil)

