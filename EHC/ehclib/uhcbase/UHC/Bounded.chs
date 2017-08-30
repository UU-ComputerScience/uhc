%%[99
{-# LANGUAGE NoImplicitPrelude #-}


module UHC.Bounded
where

import UHC.Base
import UHC.Generics

instance Bounded () where
    minBound = ()
    maxBound = ()

deriving instance (Bounded a, Bounded b)
                => Bounded (a,b)

deriving instance (Bounded a, Bounded b, Bounded c)
                => Bounded (a,b,c)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d)
                => Bounded (a,b,c,d)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e)
                => Bounded (a,b,c,d,e)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f)
                => Bounded (a,b,c,d,e,f)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g)
                => Bounded (a,b,c,d,e,f,g)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h)
                => Bounded (a,b,c,d,e,f,g,h)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i)
                => Bounded (a,b,c,d,e,f,g,h,i)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j)
                => Bounded (a,b,c,d,e,f,g,h,i,j)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k)
                => Bounded (a,b,c,d,e,f,g,h,i,j,k)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l)
                => Bounded (a,b,c,d,e,f,g,h,i,j,k,l)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m)
                => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n)
                => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n)

deriving instance (Bounded a, Bounded b, Bounded c, Bounded d, Bounded e, Bounded f, Bounded g, Bounded h, Bounded i, Bounded j, Bounded k, Bounded l, Bounded m, Bounded n, Bounded o)
                => Bounded (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)

%%]

