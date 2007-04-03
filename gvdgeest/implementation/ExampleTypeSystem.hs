{-# OPTIONS -fglasgow-exts  -fallow-undecidable-instances  #-}
module ExampleTypeSystem where

import Data.List(isPrefixOf, union, intersect, nub)
import Data.Maybe
import Data.Monoid

import CHRSolver
import Constraints

--------------------------------------------------------------------------
-- Type system from: 
-- 'Typing Haskell in Haskell', version of November 23, 2000
-- Copyright (c) Mark P Jones and the Oregon Graduate Institute
-- of Science and Technology, 1999-2000
--------------------------------------------------------------------------
data Type = TVar   Int
          | TCon   String
          | TApp   Type Type
          deriving (Eq, Ord)

instance Show Type where
  show (TVar   i)    = "v" ++ show i
  show (TCon s)      = s
  show (TApp t1 t2)  = "(" ++ show t1 ++ " " ++ show t2 ++ ")"

instance Matchable Type (Subst Type) where
  match (TApp l r) (TApp l' r') = do  sl <- match l l'
                                      sr <- match r r'
                                      merge sl sr
  match t          (TVar u)     = return [(u, t)]
  match (TCon tc1) (TCon tc2)
           | tc1==tc2         = return mempty
  match t1 t2                 = fail "types do not match"

  subst = substitute

merge :: (Monad m, Eq a) => Subst a -> Subst a -> m (Subst a)
merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
 where  test v = fromMaybe False $ do  t1 <- lookup v s1
                                       t2 <- lookup v s2
                                       return (t1 == t2)
        agree  = all test (map fst s1 `intersect` map fst s2)

instance Substitutable Type Int (Subst Type) where  
  ftv  (TVar   i)   = [i]
  ftv  (TApp   s t) = ftv s `union` ftv t
  ftv  _            = []         
          
  substitute s (TVar u)    = case lookup u s of
                               Just t  -> t
                               Nothing -> TVar u
  substitute s (TApp l r)  = TApp (substitute s l) (substitute s r)
  substitute s t           = t

mgu :: Monad m => Type -> Type -> m (Subst Type)
mgu (TApp l r) (TApp l' r')  = do  s1 <- mgu l l'
                                   s2 <- mgu (substitute s1 r) (substitute s1 r')
                                   return (s2 @@ s1)
mgu (TVar u) t               = varBind u t
mgu t (TVar u)               = varBind u t
mgu (TCon tc1) (TCon tc2)
           | tc1==tc2        = return mempty
mgu t1 t2                    = fail "types do not unify"

varBind :: Monad m => Int -> Type -> m (Subst Type)
varBind u t | t == TVar u      = return mempty
            | u `elem` ftv t   = fail "occurs check fails"
            | otherwise        = return [(u, t)]

infixr 4 @@
(@@) :: (Substitutable b v [(a, b)]) => [(a, b)] -> [(a, b)] -> [(a, b)]
s1 @@ s2    = [ (u, substitute s1 t) | (u,t) <- s2 ] ++ s1

-- Example types:
basicTypes :: [Type]
basicTypes  = [int, bool, char, float]

list :: Type -> Type
list        = TApp (TCon "List")

tuple :: Type -> Type -> Type
tuple  x y  = TApp (TApp (TCon "(,)") x) y

v1, v2, v3, v4, c1, int, bool, char, float :: Type
v1     = TVar 1
v2     = TVar 2
v3     = TVar 3
v4     = TVar 4
v5     = TVar 5
v6     = TVar 6
c1     = TCon "c1"
int    = TCon "Int"
bool   = TCon "Bool"
char   = TCon "Char"
float  = TCon "Float"

--------------------------------------------------------------------------
-- Preds:
--------------------------------------------------------------------------
data Predicate = Predicate { className :: String, classType :: [Type]}
               | Type :=: Type
            deriving (Eq, Ord)
            
instance Substitutable Predicate Int (Subst Type) where
  ftv (Predicate _ ts) = nub $ concat (map ftv ts)
  ftv (s :=: t)        = ftv s ++ ftv t
  
  substitute = subst 
          

instance Matchable Predicate (Subst Type) where
  match (Predicate n ts) (Predicate n' ts') | n == n'   = do st <- match ts ts'
                                                             return st
                                            | otherwise = Nothing
  match _                 _                 = Nothing
                                              
  subst st (Predicate n ts)                 =  Predicate n (subst st ts)
  subst st (t1 :=: t2)                      =  (subst st t1) :=: (subst st t2)

instance Matchable a (Subst a) => Matchable [a] (Subst a) where
  match []     []      = return mempty
  match (t:ts) (r:rs)  = do  s1  <- match t r
                             s2  <- match ts (subst s1 rs) 
                             s   <- merge s1 s2
                             return s
  match _  _           = Nothing      

  subst s  rs = map (subst s) rs

instance Show Predicate where
  show (Predicate cn ts) = cn ++ " " ++ (showBetween ", " ts)
  show (t1 :=: t2)       = show t1 ++ " = " ++ show t2

showBetween _   (x:[])   = show x
showBetween sep (x:y:xs) = show x ++ sep ++ showBetween sep (y:xs)
showBetween _   _        = ""

-- Example predicates:
eqP, ordP, numP, showP, realP, enumP, integralP :: Type -> Predicate
eqP        t = Predicate "Eq"  [t]
ordP       t = Predicate "Ord" [t]
numP       t = Predicate "Num" [t]
showP      t = Predicate "Show" [t]
realP      t = Predicate "Real" [t]
enumP      t = Predicate "Enum" [t]
integralP  t = Predicate "Integral" [t]

--------------------------------------------------------------------------
-- Scopes:
--------------------------------------------------------------------------

type TreePath = [Int]

data Scope = Scope TreePath
             deriving (Eq, Ord)
             
instance Matchable Scope ()
             
instance Show Scope where
  show (Scope l) = show l

s1, s2, s3, s4 :: Pattern Scope 
s1 = Var 4
s2 = Var 5
s3 = Var 6
s4 = Var 7

p1 :: Pattern Predicate 
p1 = Var 3

visibleIn (Var i) (Scope s) subst = do (Scope s') <- lookup i (fst . snd $ subst)
                                       if s `isPrefixOf` s'
                                         then return subst
                                         else Nothing
visibleIn _       _         _     = Nothing 

assignParentScope (Var i)   (Var j)  subst = do (Scope s) <- lookup j (fst . snd $ subst)
                                                if null s
                                                  then Nothing 
                                                  else return (([], []), ([(i, Scope (init s))], ()))
assignParentScope _         _        _     = Nothing        

assignCommonScope (Var i1) (Var i2) (Var i3)  subst
  = do (Scope s1) <- lookup i1 (fst . snd $ subst)  
       (Scope s2) <- lookup i2 (fst . snd $ subst)
       let s3 = commonPrefix s1 s2
       if s1 /= s3
         then return (([], []), ([(i3, Scope s3)], ()))
         else Nothing       

assignCommonScope _        _        _        _      = Nothing                                          

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (x:xs)  (y:ys)  | x == y     = x : commonPrefix xs ys
                             | otherwise  = []
commonPrefix _       _                    = []

cp :: Eq a => [a] -> [a] -> [a]
cp xs ys = map fst $ takeWhile (uncurry (==)) $ zip xs ys


--------------------------------------------------------------------------
-- Scoped preds:
--------------------------------------------------------------------------
data ScopedPred = SPred (Pattern Predicate) (Pattern Scope)
                  deriving (Eq, Ord)
                  
instance Show ScopedPred where
  show (SPred p s) = "(" ++ show p ++ ", " ++ show s ++ ")"                     

instance Matchable ScopedPred ((Subst Predicate, Subst Type), (Subst Scope, ())) where
  match  (SPred p s)  (SPred q t)  = do  ps <- match p q
                                         ss <- match s t
                                         return (ps, ss)
                                     
  subst  (ps, ss)     (SPred p s)  = SPred (subst ps p) (subst ss s)

sPred p s = SPred (Val p) s

mkSPred p s = SPred (Val p) (Val . Scope $ s)

--------------------------------------------------------------------------
-- Meta information to attach:
--------------------------------------------------------------------------

data Annotation  =  ByInstance    String  Predicate  Scope
                 |  BySuperClass  String
                 |  ProveObl      Int
                 |  Assumption    Int
                 |  ByScope
                 deriving (Eq, Ord)

instance Show Annotation where
  show (ByInstance s _ _)  = show s
  show (BySuperClass s)  = show s
  show (ProveObl i)  =  "prove" ++ show i
  show (Assumption i)  = "assume" ++ show i
  show (ByScope)       = "scope"
