-- see Dummy in Env, used to not infer * for 'a', record field

evaluate :: forall a . Env (Expr a) a -> a

-- simpele expressie (integer of var)
data Expr env a 
  = Int (Equal a Int) Int 
  | Var (Ref a env)

-- References
data Ref a env = Zero (Equal env (a,env'))
               | Suc (Equal env (x,env')) (Ref a env')


-- environment, (x,(y,(z,()))) =~= EXT x (EXT y (EXT z EMPTY))
-- Env:Env/(Forall a . a -> *) -> * -> *
data Env f env
 = EMPTY (Equal env ())
 | EXT (Equal env (a,env')) (f a) (Env f env')
 -- | Dummy (f Int) -- voor bug in kind analyse

-- GADT's
data Equal a b = Eq (forall f . f a -> f b)

main=0
