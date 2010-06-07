%%[(8 codegen) hs module {%{EH}Core.Trf.Fusion.Utils}
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.HsSyn})

import List
%%]
%%[(8 codegen) hs import({%{EH}Core.Trf.Fusion.Messages})
import Control.Monad
import Control.Monad.State
import Control.Monad.Error
--import Language.Haskell.Syntax(SrcLoc(..))
import qualified Data.Map as M(empty,adjust,Map,insert,lookup) 

import Debug.Trace

sss s a = trace (s++": "++show a) a

type GenDict = M.Map String Int

type IntState a = State GenDict a

emptyGenDict = M.empty

getFreshVar :: String -> IntState Variable
getFreshVar p = do dict<-get
                   case M.lookup p dict of
                    Just i -> put (M.adjust (+1) p dict) >> return (Vgen p i)
                    _ -> put (M.insert p 1 dict) >> return (Vgen p 0)




dfilter :: (a->Bool)->[a]->([a],[a])
dfilter p (a:as) | (p a)     = (a:r1,r2)
                 | otherwise = (r1,a:r2)
                where (r1,r2)= dfilter p as
dfilter p _ = ([],[])



bv2term :: Boundvar -> Term
bv2term (Bvar v) = Tvar v
bv2term (Bvtuple b vs) = Ttuple b (map bv2term vs)

wrap a = [a]

mapSel s i f (h:ls) =
   case lookup i s of
    Nothing -> h : mapSel s (i+1) f ls
    Just a -> f a h : mapSel s (i+1) f ls
mapSel s i f [] = []




substitutionPattern :: [(Variable,Variable)] -> Pattern -> Pattern
substitutionPattern sust pat =
  case pat of
   Pvar variable -> case lookup variable sust of
                     Just v' -> Pvar v'
                     _       -> pat
   Ptuple tupla -> Ptuple (map (substitutionPattern sust) tupla)
   Pcons cons pats -> Pcons cons (map (substitutionPattern sust) pats)
   Plit lit -> pat
   Pas v p -> case lookup v sust of
               Just v' -> Pas v' (substitutionPattern sust p)
               _ -> substitutionPattern sust p















substitution :: [(Variable,Term)] -> Term -> Term

substitution [] term = term
substitution ss t = 
   let
       except exp = let diff (v,val) = not (elem v (vars exp))
                    in substitution (filter diff ss)
   in
   case t of



     Tvar v -> case lookup v ss of
                   Just valor -> valor
                   Nothing -> t



     Tlamb bv t -> Tlamb bv $ except bv t



     Tlet v t0 t1 -> Tlet v (except v t0) (except v t1)



     Tcase t0 ps ts -> let t0new = substitution ss t0
                in Tcase t0new ps (zipWith except ps ts)



     Tlit _ -> t



     Ttuple b ts -> Ttuple b $ map (substitution ss) ts

     Tfapp v ts ->
                  let
              mts = map (substitution ss) ts
              in
                  case lookup v ss of
                   Just valor -> case valor of Tfapp v' ts' -> Tfapp v' (ts'++mts)
                                               _ -> foldl tapp valor mts
                   Nothing -> Tfapp v mts
     Tcapp cons ts ->
                  let
              nts = map (substitution ss) ts
              in
                  Tcapp cons nts
     Tapp t0 t1    ->
                  let
              newt0 = substitution ss t0
              newt1 = substitution ss t1
              in
              Tapp newt0 newt1
     Thyloapp v i ts pos t ->
                  let
              mts = map (substitution ss) ts
              t' = substitution ss t
              in
                  case lookup v ss of
                   Just valor -> case valor of Tfapp v' ts' -> Tfapp v' (ts'++thyloArgs i mts pos t')
                                               _ -> foldl tapp valor (thyloArgs i mts pos t')
                   Nothing -> Thyloapp v i mts pos t'
     t           -> t


foldrM:: (Monad m) => (a -> b-> m b) -> b -> [a] -> m b
foldrM f b0 [] =
    do
      return b0
foldrM f b0 (a:as) =
    do
      resTail <- foldrM f b0 as
      resHead <- f a resTail
      return resHead




delCases :: Term -> Term
delCases (Tlet a t0 t1) = Tlet a (delCases t0) (delCases t1)
delCases (Tcase t0 (p@(Pvar x):_) (t:_))
   | p == pany = t'
   | countx == 0 = t'
   | otherwise = case t0 of
                  Tvar u -> substitution [(x,t0)] t'
                  _ -> if isRecTuple t0 || countx == 1
                         then substitution [(x,t0)] t'
                         else Tcase t0 [p] [t']
 where t'=delCases t
       countx = countLinear x t'
       isRecTuple (Ttuple True _) = True
       isRecTuple _ = False
delCases (Tcase t0 (p:ps) (t:ts)) = 
      let (ps',ts')=select ps ts
          res = Tcase t0 (p:ps') (map delCases (t:ts'))
       in case (t0,p) of
           (Tcapp c [],Pcons c' []) | c==c' -> delCases t
                                    | otherwise -> res
           _ -> res
 where select (p@(Pvar v):_) (t:_) = ([p],[t])
       select (p:ps) (t:ts) = let (ps',ts')=select ps ts
                               in (p:ps',t:ts')
       select ps ts = (ps,ts)
delCases t = t




countLinear :: Variable -> Term -> Int
countLinear v (Tvar v') = if v==v' then 1 else 0
countLinear v (Tlit _) = 0
countLinear v (Ttuple _ ts) = sum (map (countLinear v) ts)
countLinear v (Tfapp v' ts) = sum (map (countLinear v) ts) + if v==v' then 1 else 0
countLinear v (Tcapp c ts) = sum (map (countLinear v) ts)
countLinear v (Tapp t0 t1) = countLinear v t0 + countLinear v t1
countLinear v (Tif t0 t1 t2) = countLinear v t0 + max (countLinear v t1) (countLinear v t2)
countLinear v (Tlet v' t0 t1) = if v==v' then 0 else countLinear v t0 + countLinear v t1
countLinear v (Tlamb v' t) = if elem v (vars v') then 0 else countLinear v t
countLinear v (Tcase t0 ps ts) = maximum (zipWith (\p t-> if elem v (vars p) then 0 else countLinear v t) ps ts) + countLinear v t0
countLinear v (Thyloapp v' i ts _ t) = sum (map (countLinear v) ts) + countLinear v t + if v==v' then 1 else 0
countLinear v (Tpar t) = countLinear v t
countLinear v Tbottom = 0





extractVars :: Term -> ([Boundvar],Term)
extractVars = extractVars' id
  where extractVars' f (Tlamb bv t) = extractVars' (f.(bv:)) t
        extractVars' f t = (f [],t)





vars' :: Term -> [Variable]
vars' (Tvar v) = [v]
vars' (Ttuple _ ps) = concat (map vars' ps)
vars' (Tcapp _ ps) = concat (map vars' ps)
vars' (Tlit _) = []
vars' (Tfapp fn ps) = concat ([fn]:(map vars' ps))
vars' (Tapp t1 t2) = vars' t1 ++ vars' t2
vars' (Tlamb bv t) = vars' t \\ vars bv
vars' (Tif t0 t1 t2) = vars' t0 ++ vars' t1 ++ vars' t2
vars' (Tlet v t0 t1) = vars' t0 ++ (vars' t1 \\ [v])
vars' (Tcase t ps ts) = concat (vars' t:zipWith (\pi ti ->vars' ti \\ vars pi) ps ts)
vars' Tbottom = []
vars' (Thyloapp v i ts _ t) = v:(concat (map vars' ts)++vars' t)
vars' (Tpar t) = vars' t



polishTerm :: Term -> Term
polishTerm (Ttuple b ps) = Ttuple b$ map polishTerm ps
polishTerm (Tcapp n ps) = Tcapp n $ map polishTerm ps
polishTerm (Tfapp fn ps) = Tfapp fn (map polishTerm ps)
polishTerm (Tapp t1 t2) = Tapp (polishTerm t1) (polishTerm t2)
polishTerm (Tlamb bv t) = Tlamb bv (polishTerm t)
polishTerm (Tlet v t0 t1) = Tlet v (polishTerm t0) (polishTerm t1)
polishTerm (Tcase t [Pvar v] (ti:_)) = Tlet v (polishTerm t) (polishTerm ti)
polishTerm (Tcase t (Pcons "True" []:Pcons "False" []:_) (t1:t2:_)) = Tif (polishTerm t) (polishTerm t1) (polishTerm t2)
polishTerm (Tcase t (Pcons "True" []:Pvar (Vuserdef "_"):_) (t1:t2:_)) = Tif (polishTerm t) (polishTerm t1) (polishTerm t2)
polishTerm (Tcase t (Pcons "False" []:Pcons "True" []:_) (t1:t2:_)) = Tif (polishTerm t) (polishTerm t2) (polishTerm t1)
polishTerm (Tcase t (Pcons "False" []:Pvar (Vuserdef "_"):_) (t1:t2:_)) = Tif (polishTerm t) (polishTerm t2) (polishTerm t1)
polishTerm (Tcase t ps ts) = Tcase (polishTerm t) ps (map polishTerm ts)
polishTerm t@(Tvar _) = t
polishTerm t@(Tlit _) = t
polishTerm (Tif t0 t1 t2) = Tif (polishTerm t0) (polishTerm t1) (polishTerm t2)
polishTerm (Tpar t) = Tpar (polishTerm t)
polishTerm t = t -- error ("polishTerm Term: not defined: " ++ show t)

polishDef :: Def -> Def
polishDef (Defvalue v t) = Defvalue v (polishTerm t)
polishDef d = d


removeIfAndLets :: Term -> Term
removeIfAndLets = transformTerm remIfLets
 where remIfLets _ (Tif t0 t1 t2) = Tcase t0 [Pcons "True" [],Pcons "False" []] [t1,t2]
       remIfLets _ (Tlet v t0 t1) = Tcase t0 [Pvar v] [t1]
       remIfLets _ t = t






transformTerm :: (Term -> Term -> Term) -> Term -> Term
transformTerm f t = f t (tr t)
  where tr (Tcase t ps ts) = Tcase (transformTerm f t) ps (map (transformTerm f) ts)
        tr (Tfapp v ts) = Tfapp v (map (transformTerm f) ts)
        tr (Tcapp c ts) = Tcapp c (map (transformTerm f) ts)
        tr (Ttuple b ts) = Ttuple b (map (transformTerm f) ts)
        tr (Thyloapp v i ts pos t) = Thyloapp v i (map (transformTerm f) ts) pos (transformTerm f t)
        tr (Tlamb bv t) = Tlamb bv (transformTerm f t)
        tr (Tlet v t0 t1) = Tlet v (transformTerm f t0) (transformTerm f t1)
        tr (Tapp t0 t1) = Tapp (transformTerm f t0) (transformTerm f t1)
        tr (Tif t0 t1 t2) = Tif (transformTerm f t0) (transformTerm f t1) (transformTerm f t2)
        tr (Tpar t) = Tpar (transformTerm f t)
        tr t@(Tvar _) = t
        tr t@(Tlit _) = t
        tr t@Tbottom = t




zipTree' :: [Boundvar] -> [Term] -> Maybe [(Variable, Term)]
zipTree' [] [] = Just []
zipTree' (v:vs) (t:ts) =
    case (v,t) of
     (Bvtuple _ bvs', Ttuple _ ts') -> do z1<-zipTree' bvs' ts';z2<-zipTree' vs ts;return (z1++z2)
     (Bvar bv, t) -> do z<-zipTree' vs ts;return ((bv, t):z)
     _            -> Nothing
zipTree' _ _ = Nothing





concatMaybes :: [Maybe a] -> [a]
concatMaybes [] = []
concatMaybes (x:xs) =
  let
   res = concatMaybes xs
  in
  case x of
       Just y  ->
          y : res
       Nothing ->
          res



insertAt :: a -> Int -> [a] -> [a]
insertAt x i ls = (take i ls) ++ [x] ++ (drop (i+1) ls)




deleteEvery :: (Eq a) => a -> [a] -> [a]
deleteEvery x [] = []
deleteEvery x (l:ls) = if (x==l) then (deleteEvery x ls) else l : (deleteEvery x ls)



deleteEverys :: (Eq a) => [a] -> [a] -> [a]
deleteEverys [] lista = lista
deleteEverys (d:deletes) lista = let res = (deleteEvery d lista) in deleteEverys deletes res



insertNew :: (Eq a) => [a] -> [a] -> [a]
insertNew [] lista = lista
insertNew (new:news) lista = if (elem new lista) then (insertNew news lista) else new : (insertNew news lista)



cascadeLambda :: [Boundvar] -> Term -> Term
cascadeLambda [] t = t
cascadeLambda (v:vs) t = Tlamb v (cascadeLambda vs t)



equalTerms :: [(Variable, Variable)] -> Term -> Term -> Maybe (Term,Term)
equalTerms tbl (Tlit l) (Tlit l') | l==l' = Nothing
equalTerms tbl (Tvar v) (Tvar v') | maybe (v==v') (==v')$ lookup v tbl = Nothing
equalTerms tbl (Tlamb bvs0 t0) (Tlamb bvs1 t1) = equalTerms (zip (vars bvs0) (vars bvs1) ++ tbl) t0 t1
equalTerms tbl (Tcase t0 ps0 ts0) (Tcase t1 ps1 ts1) | and (zipWith equalPatterns ps0 ps1) = 
                                equalTerms tbl t0 t1 `mplus`
                                msum ((zipWith (\(p,p') (t,t') -> equalTerms (zipPatterns p p' []++tbl) t t')
                                                   (zip ps0 ps1) (zip ts0 ts1)))
equalTerms tbl (Ttuple _ ts0) (Ttuple _ ts1) = msum$ zipWith (equalTerms tbl) ts0 ts1
equalTerms tbl (Tcapp cons0 ts0) (Tcapp cons1 ts1) | cons0==cons1 = msum$ zipWith (equalTerms tbl) ts0 ts1 
equalTerms tbl (Tfapp v0 ts0) (Tfapp v1 ts1) | maybe (v0==v1) (==v1)$ lookup v0 tbl =
                                               msum$ zipWith (equalTerms tbl) ts0 ts1
equalTerms tbl (Tlet vi0 ti0 ti1) (Tlet vd0 td0 td1) = equalTerms tbl ti0 td0 `mplus` equalTerms ((vi0,vd0):tbl) ti1 td1
equalTerms tbl (Tif tb0 ti0 ti1) (Tif tb1 td0 td1) = equalTerms tbl tb0 tb1 `mplus` equalTerms tbl ti0 td0 `mplus` equalTerms tbl ti1 td1
equalTerms tbl (Tapp ti0 ti1) (Tapp td0 td1) = equalTerms tbl ti0 td0 `mplus` equalTerms tbl ti1 td1
equalTerms tbl (Tpar t0) (Tpar t1) = equalTerms tbl t0 t1
equalTerms tbl (Tapp (Tvar v0) t0) t1 = equalTerms tbl (Tfapp v0 [t0]) t1
equalTerms tbl t0 (Tapp (Tvar v1) t1) = equalTerms tbl t0 (Tfapp v1 [t1])
equalTerms tbl t0 t1 = Just (t0,t1)

zipPatterns :: Pattern -> Pattern -> [(Variable, Variable)] -> [(Variable, Variable)]
zipPatterns (Pvar v0) (Pvar v1) tbl = (v0,v1):tbl
zipPatterns (Ptuple pis) (Ptuple pds) tbl = foldr ($) tbl$ zipWith zipPatterns pis pds
zipPatterns (Pcons ci pis) (Pcons cd pds) tbl | ci == cd = foldr ($) tbl$ zipWith zipPatterns pis pds
zipPatterns (Pas v0 p0) (Pas v1 p1) tbl  = zipPatterns p0 p1 ((v0,v1):tbl)
zipPatterns _ _ tbl = tbl



equalPatterns :: Pattern -> Pattern -> Bool
equalPatterns (Pvar v0) (Pvar v1) = True
equalPatterns (Ptuple pis) (Ptuple pds) = and$ zipWith equalPatterns pis pds
equalPatterns (Pcons ci pis) (Pcons cd pds) = ci == cd && and (zipWith equalPatterns pis pds)
equalPatterns (Plit l0) (Plit l1)  = l0==l1
equalPatterns (Pas v0 p0) (Pas v1 p1)  = equalPatterns p0 p1
equalPatterns _ _ = False

%%]