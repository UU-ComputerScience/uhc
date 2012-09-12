{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, FlexibleContexts, UndecidableInstances, OverlappingInstances #-}

{- ----------------------------------------------------------------------------------------
   what    : lexically scoped tyvar for instance
   expected: ok, see comment for the complex Sup inst for depth 2
---------------------------------------------------------------------------------------- -}

module LexScopedTyVar2 where

import Data.Maybe
import Data.Dynamic

{-
class Sub a b where
   upcast :: a -> b
-}

class Sup a b where
   downcast :: a -> Maybe b

{-
class Narrow a b where
   narrow :: a -> b
-}

class Widen a b where
   widen :: a -> Maybe b

-- depth 1

{-
instance Sub (a ()) (a ()) where
   upcast = id
-}

instance Sup (a ()) (a ()) where
   downcast = Just

-- depth 2

{-
instance Sub (a (b ())) (a (b ())) where
   upcast = id
-}

instance Sup (a (b ())) (a (b ())) where
   downcast = Just

{-
instance (Sub (a ()) x, Narrow (a (b ())) (a ())) => Sub (a (b ())) x where
   upcast = upcast . (narrow :: a (b ()) -> a () )
-}

-- here ref to scoped tyvar would go wrong because type of 'a' and 'b' would not be propagated correctly
instance (Sup (a (b ())) (a (b c)), Widen (a ()) (a (b ()))) => Sup (a ()) (a (b c)) where
   downcast o = case widen o :: Maybe (a (b ())) of
                  Just r   -> downcast r 
                  Nothing  -> Nothing

{-
-- depth 3

instance Sub (a (b (c ()))) (a (b (c ()))) where
   upcast = id

instance Sup (a (b (c ()))) (a (b (c ()))) where
   downcast = Just

instance (Sub (a (b ())) x, Narrow (a (b (c ()))) (a (b ()))) => Sub (a (b (c ()))) x where
   upcast = upcast . (narrow :: a (b (c ())) -> a (b ()))

instance (Sup (a (b (c ()))) (a (b (c d))), Widen (a (b ())) (a (b (c ())))) => Sup (a (b ())) (a (b (c d))) where
   downcast o = case widen o :: Maybe (a (b (c ()))) of
                  Just r   -> downcast r
                  Nothing  -> Nothing

-- depth 4
instance Sub (a (b (c (d ())))) (a (b (c (d ())))) where
   upcast = id

instance Sup (a (b (c (d ())))) (a (b (c (d ())))) where
   downcast = Just

instance (Sub (a (b (c ()))) x, Narrow (a (b (c (d ())))) (a (b (c ())))) => Sub (a (b (c (d ())))) x where
   upcast = upcast . (narrow :: a (b (c (d ()))) -> a (b (c ())))

instance (Sup (a (b (c (d ())))) (a (b (c (d e)))), Widen (a (b (c ()))) (a (b (c (d ()))))) => Sup (a (b (c ()))) (a (b (c (d e)))) where
   downcast o = case widen o :: Maybe (a (b (c (d ())))) of
                  Just r   -> downcast r
                  Nothing  -> Nothing
-}

main = return ()
