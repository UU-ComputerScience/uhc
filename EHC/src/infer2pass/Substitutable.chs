%%[1 import(Data.List,Common,Ty,Cnstr,TySubst,TyFtv) export(Substitutable(..))
%%]

%%[11 import(EH4x2.Substitutable(Substitutable(..)))
%%]

%%[11 export(module EH4x2.Substitutable)
%%]

%%[11 import(Data.List,Common,Ty,Cnstr,TySubst,TyFtv)
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
