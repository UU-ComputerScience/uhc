-----------------------------------------------------------------------------
-- TIMonad:	Type inference monad
-- 
-- Part of `Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
-- 
-- This program is distributed as Free Software under the terms
-- in the file "License" that is included in the distribution
-- of this software, copies of which may be obtained from:
--             http://www.cse.ogi.edu/~mpj/thih/
-- 
-----------------------------------------------------------------------------

module TIMonad where
import Id
import Kind
import Type
import Subst
import Unify
import Pred
import Scheme

newtype TI a = TI (Subst -> Int -> (Subst, Int, a))

instance Monad TI where
  return x   = TI (\s n -> (s,n,x))
  TI f >>= g = TI (\s n -> case f s n of
                            (s',m,x) -> let TI gx = g x
                                        in  gx s' m)

runTI       :: TI a -> a
runTI (TI f) = x where (s,n,x) = f nullSubst 0

getSubst   :: TI Subst
getSubst    = TI (\s n -> (s,n,s))

unify      :: Type -> Type -> TI ()
unify t1 t2 = do s <- getSubst
                 u <- mgu (apply s t1) (apply s t2)
                 extSubst u

trim       :: [Tyvar] -> TI ()
trim vs     = TI (\s n ->
                  let s' = [ (v,t) | (v,t) <-s, v `elem` vs ]
                      force = length (tv (map snd s'))
                  in  force `seq` (s', n, ()))

extSubst   :: Subst -> TI ()
extSubst s' = TI (\s n -> (s'@@s, n, ()))

newUId :: TI Int
newUId =  TI (\s n -> (s, n+1, n))

newTVar   :: Kind -> TI Type
newTVar k  =  do n <- newUId 
                 return (TVar (Tyvar (enumId n) k))

numberPred           :: (Pred -> Int -> Pred) -> Pred -> TI Pred
numberPred c p = do n <- newUId 
                    return (c p n)

numberPreds :: Qual Type -> (Pred -> Int -> Pred) -> TI (Qual Type)
numberPreds (ps :=> t) c = do ps' <- mapM (numberPred c) ps 
                              return (ps' :=> t)

freshInst                 :: Scheme -> (Pred -> Int -> Pred) -> TI (Qual Type)
freshInst (Forall ks qt) c = do ts <- mapM newTVar ks
                                t  <- numberPreds qt c
                                return (inst ts t)

class Instantiate t where
  inst  :: [Type] -> t -> t
instance Instantiate Type where
  inst ts (TAp l r) = TAp (inst ts l) (inst ts r)
  inst ts (TGen n)  = ts !! n
  inst ts t         = t
instance Instantiate a => Instantiate [a] where
  inst ts = map (inst ts)
instance Instantiate t => Instantiate (Qual t) where
  inst ts (ps :=> t) = inst ts ps :=> inst ts t
instance Instantiate Pred where
  inst ts (IsIn c t)  = IsIn c (inst ts t)
  inst ts (Prove p i) = Prove (inst ts p) i
  inst ts (Assume p i) = Assume (inst ts p) i

-----------------------------------------------------------------------------
