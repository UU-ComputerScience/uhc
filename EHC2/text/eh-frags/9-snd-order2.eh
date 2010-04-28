let  data Bit        = Zero  | One
     data GRose f a  = GBranch a (f (GRose f a))
in   let  class Binary a where
            showBin :: a -> List Bit
          instance dBI <: Binary Int where
            showBin  = ...
          instance dBL <: Binary a => Binary (List a) where
            showBin  = ...
          instance dBG <:  (Binary a, Binary (f (GRose f a)))
                             => Binary (GRose f a) where
            showBin  =  \(GBranch x ts)
                          -> showBin x ++ showBin ts
in   let  v1 = showBin (GBranch 3 Nil)
     in   v1
