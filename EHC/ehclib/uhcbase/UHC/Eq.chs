%%[99

{-# LANGUAGE NoImplicitPrelude #-}

module UHC.Eq
where

import UHC.Base
import UHC.Generics


%%]

#include "TupleInstance.h"

%%[99
instance Eq () where
    () == ()  =  True

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tuple instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99
deriving instance (Eq a, Eq b)
                => Eq (a,b)

deriving instance (Eq a, Eq b, Eq c)
                => Eq (a,b,c)

deriving instance (Eq a, Eq b, Eq c, Eq d)
                => Eq (a,b,c,d)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e)
                => Eq (a,b,c,d,e)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f)
                => Eq (a,b,c,d,e,f)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g)
                => Eq (a,b,c,d,e,f,g)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h)
                => Eq (a,b,c,d,e,f,g,h)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i)
                => Eq (a,b,c,d,e,f,g,h,i)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j)
                => Eq (a,b,c,d,e,f,g,h,i,j)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k)
                => Eq (a,b,c,d,e,f,g,h,i,j,k)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l)
                => Eq (a,b,c,d,e,f,g,h,i,j,k,l)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m)
                => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n)
                => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n)

deriving instance (Eq a, Eq b, Eq c, Eq d, Eq e, Eq f, Eq g, Eq h, Eq i, Eq j, Eq k, Eq l, Eq m, Eq n, Eq o)
                => Eq (a,b,c,d,e,f,g,h,i,j,k,l,m,n,o)
%%]

