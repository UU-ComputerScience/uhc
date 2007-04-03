{-# OPTIONS -fglasgow-exts #-}
module LL1
where

import CPS
import Prelude (String, Show)


data Expr = Id String 
          | Add Expr Expr 
          | Mul Expr Expr
          deriving Show
          
newtype V  a = V (Expr           -> a) -- id
newtype A  a = A (                  a) -- +
newtype M  a = M (                  a) -- *
newtype O  a = O (                  a) -- (
newtype C  a = C (                  a) -- )

newtype E  a = E (Expr           -> a)
newtype E' a = E'((Expr -> Expr) -> a)
newtype T  a = T (Expr           -> a)
newtype T' a = T'((Expr -> Expr) -> a)
newtype F  a = F (Expr           -> a)

quote = lift (E (\x -> x))

class Id    old new | old -> new where id    :: old -> String -> CPS new
class Add   old new | old -> new where plus  ::           old -> CPS new
class Mul   old new | old -> new where times ::           old -> CPS new
class Open  old new | old -> new where q     ::           old -> CPS new
class Close old new | old -> new where p     ::           old -> CPS new
class End   old                  where end   ::           old -> Expr




instance Id (E a) (T' (E' a)) where
  id (E ctx) s = id (T (\t -> E' (\e' -> ctx (e' t)))) s
  
instance Id (T a) (T' a) where
  id (T ctx ) s = id (F (\f -> T' (\t' -> ctx (t' f )))) s 
  
instance Id (F a) a where
  id (F ctx ) s = id (V (\v -> ctx v)) s
  
instance Id (V a) a where
  id (V ctx ) s = lift (ctx (Id s))
  
  
  
instance Add (E' a) (T (E' a)) where
  plus (E' ctx ) = plus (A (T (\t -> E' (\e' -> ctx (\l -> e' (Add l t))))))
  
instance (Add a a') => Add (T' a) a' where
  plus (T' ctx ) = plus (ctx (\e -> e))  
  
instance Add (A a) a where
  plus (A ctx ) = lift ctx
  
  
  
instance Mul (T' a) (F (T' a)) where
  times (T' ctx ) = times (M (F (\f -> T' (\t' -> ctx (\l -> t' (Mul l f ))))))
  
instance Mul (M a) a where
  times (M ctx ) = lift ctx

  
  
instance Open (E a) (E (C (T' (E' a)))) where
  q (E ctx ) = q (T (\t -> E' (\e' -> ctx (e' t))))

instance Open (T a) (E (C (T' a))) where
  q (T ctx ) = q (F (\f -> T' (\t' -> ctx (t' f ))))
  
instance Open (F a) (E (C a)) where
  q (F ctx ) = q (O (E (\e -> C (ctx e))))
  
instance Open (O a) a where
  q (O ctx ) = lift ctx


  
instance (Close a a') => Close (E' a) a' where
  p (E' ctx ) = p (ctx (\e -> e))
  
instance (Close a a') => Close (T' a) a' where
  p (T' ctx ) = p (ctx (\e -> e))
  
instance Close (C a) a where
  p (C ctx ) = lift ctx


  
instance (End a) => End (E' a) where
  end (E' ctx ) = end (ctx (\e -> e))
  
instance (End a) => End (T' a) where
  end (T' ctx ) = end (ctx (\e -> e))
  
instance End Expr where
  end e = e
