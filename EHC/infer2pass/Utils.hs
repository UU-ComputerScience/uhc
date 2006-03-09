module Utils
  where

import Data.List
import UU.Pretty

-------------------------------------------------------------------------
-- Error
-------------------------------------------------------------------------

mkErr :: [PP_Doc] -> PP_Doc
mkErr [] = empty
mkErr p  = "<ERR:" >#< vlist p >|< ">"

-------------------------------------------------------------------------
-- Unique identifier
-------------------------------------------------------------------------

newtype UID = UID [Int] deriving (Eq,Ord)
uidStart = UID [0]

rulerMk1Uniq :: UID -> (UID,UID)
rulerMk1Uniq u@(UID ls) = (uidNext u,UID (0:ls))

uidNext :: UID -> UID
uidNext (UID (l:ls)) = UID (l+1:ls)
mkUIDs :: UID -> [UID]
mkUIDs = iterate uidNext

instance Show UID where
  show (UID l)
    = concat .  intersperse "_" . map show . reverse $ l

-------------------------------------------------------------------------
-- Type
-------------------------------------------------------------------------

type TvId  = UID
data Ty    =  Ty_Any | Ty_Int | Ty_Var TvId
           |  Ty_Arr  Ty Ty
           |  Ty_All  [TvId] Ty
              deriving (Eq,Ord)
mkTyAll tvs t = if null tvs then t else Ty_All tvs t

instance Show Ty where
  show Ty_Any         = "?"
  show Ty_Int         = "Int"
  show (Ty_Var v)     = "v" ++ show v
  show (Ty_All vs t)  = "forall" ++ concat (map ((' ':) . show) vs)
                         ++ " . " ++ show t
  show (Ty_Arr t1 t2) = "(" ++ show t1 ++ ") -> " ++ show t2

-------------------------------------------------------------------------
-- Gam
-------------------------------------------------------------------------

type Gam = [(String,Ty)]

gamLookup :: String -> Gam -> (Ty,[PP_Doc])
gamLookup n g
  = maybe (Ty_Any,[n >#< "undefined"]) (\t -> (t,[]))
  $ lookup n g

-------------------------------------------------------------------------
-- Constraints
-------------------------------------------------------------------------

type Cnstr = [(TvId,Ty)]

class Substitutable a where
  (|=>)  ::  Cnstr -> a -> a
  ftv    ::  a -> [TvId]

instance Substitutable Ty where
  s  |=>  t@(Ty_Var v)  =  maybe t id (lookup v s)
  s  |=>  Ty_Arr t1 t2  =  Ty_Arr (s |=> t1) (s |=> t2)
  _  |=>  t             =  t
  ftv  (Ty_Var v)       =  [v]
  ftv  (Ty_Arr t1 t2)   =  ftv t1 `union` ftv t2
  ftv  _                =  []
instance Substitutable Cnstr where
  s1 |=> s2 = s1 ++ map (\(v,t) -> (v,s1 |=> t)) s2
  ftv       = foldr union [] . map (ftv . snd)

instance Substitutable Gam where
  s |=> g  = map (\(i,t) -> (i,s |=> t)) g
  ftv      = foldr union [] . map (ftv . snd)

-------------------------------------------------------------------------
-- Type matching (unification)
-------------------------------------------------------------------------

(<=>) :: Ty -> Ty -> (Cnstr,[PP_Doc])
Ty_Any          <=> t2          = ([],[])
t1              <=> Ty_Any      = ([],[])
Ty_Int          <=> Ty_Int      = ([],[])
Ty_Var v1       <=> Ty_Var v2
    | v1 == v2                  = ([],[])
Ty_Var v1       <=> t2
    | v1 `notElem` ftv t2       = ([(v1,t2)],[])
t1              <=> Ty_Var v2
    | v2 `notElem` ftv t1       = ([(v2,t1)],[])
Ty_Arr a1 r1    <=> Ty_Arr a2 r2
  = (sr |=> sa,ea ++ er)
  where (sa,ea) = a1 <=> a2
        (sr,er) = (sa |=> r1) <=> (sa |=> r2)
t1              <=> t2          = ([],["could not match"
                                       >#< show t1 >#< "with"
                                       >#< show t2]
                                  )

-------------------------------------------------------------------------
-- Type instantiation
-------------------------------------------------------------------------

tyInst :: UID -> Ty -> Ty
tyInst u (Ty_All vs t) = c |=> t
                       where c = zipWith (\v u -> (v,Ty_Var u))
                                         vs (mkUIDs u)
tyInst _ t             = t