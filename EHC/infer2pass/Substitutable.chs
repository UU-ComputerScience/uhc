%%[1 import(Data.List,Common,Ty,Cnstr,TySubst,TyFtv) export(Substitutable(..))
%%]

-------------------------------------------------------------------------
-- Substitutable
-------------------------------------------------------------------------

%%[1
class Substitutable a where
  (|=>)  ::  Cnstr -> a -> a
  ftv    ::  a -> [TyVarId]
%%]

%%[1
instance Substitutable Ty where
  (|=>) = tyAppCnstr
  ftv = tyFtv
%%]

%%[1
instance Substitutable Cnstr where
  s1 |=> s2 = s1 `cnstrPlus` map (\(v,t) -> (v,s1 |=> t)) s2
  ftv       = foldr union [] . map (ftv . snd)
%%]
