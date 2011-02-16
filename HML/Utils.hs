{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
{-# LANGUAGE FlexibleInstances     #-}
module Utils where

import Control.Monad.Error
import Pretty   
import qualified Data.Map as M
import Control.Monad
import Control.Arrow hiding (app)
import Control.Applicative((<$>), (<|>))

import Data.List
import Data.Function(on)
import Data.Maybe hiding (mapMaybe)

import EH8.EH
import EH8.Base.HsName
import EH8.Ty(tyQu_Forall)

import EqHML

import EH.Util.Pretty hiding (pp, empty)
import qualified Debug.Trace as D

-- trace = flip const 
trace = D.trace

-- | Create a HsName from a string.
mkName :: String -> HsName
mkName = mkHNmBase

type Sub     = (HsName, TyScheme)
type ErrorMessage = String
newtype Gamma = Gamma { unGam :: Env }

-- | Remove entries from a Env based on their key
remove :: Eq a => [(a, v)] -> [a] -> [(a, v)] -- Env -> [HsName] -> Env
remove env entries = foldl (.) id (map rm entries) env

-- | add to the first environment things from the second env not in the first
-- pick :: Env -> Env -> Env
pick :: Domain v => [(HsName, v)] -> [(HsName, v)] -> [(HsName, v)]
pick e1 e2 = let e2' = remove e2 (domain e1)
             in e1 `munion` e2'
             
-- | add to the first Prefix things from the second Pref not in the first
merge :: Prefix -> Prefix -> Prefix
merge pre efx = foldl' push pre (reverse efx)
 where push :: Prefix -> TyIndex -> Prefix
       push p t = if t `elem` p then p else t:p

-- | Split domains
split :: (Prefix, [HsName]) -> (Prefix, Prefix)
split (a ,[])      = ([], a)
split ([], _)      = ([], [])
split ((p@(TyIndex_Group var phi):q), vars) 
 = case var `elem` vars of
     True  -> let fvars = case phi of
                            TyScheme_Quant _ phi' -> ftv phi'
                            _                     -> []
                  (q1, q2) = split (q, (var `delete` vars) ++ fvars)
              in (p:q1, q2)
     False -> let (q1, q2) = split (q, vars)
              in (q1, p:q2)
          
-- | Helper functions to make the convertion from map to list easier          
mapMaybe = \v -> map (id *** \a -> (maybe a id (v a)))
mapFunc  = map . second
empty = []

rm :: Eq k => k -> [(k, v)] -> [(k, v)]
rm _ [] = []
rm k1 (x@(k,v):xs) | k1 == k   = rm k1 xs
                       | otherwise = x : rm k1 xs

-- | Apply a set of substitution to an environment
apply :: Env -> Gamma -> Gamma
apply sub env = Gamma $ map ((second . Utils.appAll) sub) (unGam env)
    
subT = [(mkName "a11", TyScheme_SystemF $ (TyExpr_Var $ mkName "a13") `mkArrow` (TyExpr_Var $ mkName "a23")), (mkName "a23", TyScheme_SystemF (TyExpr_Con $ mkName "Bool"))]
lstT = Gamma [(mkName "foo", TyScheme_SystemF $ TyExpr_Con $ mkName "String"), (mkName "Bar", TyScheme_SystemF $ TyExpr_Var  $ mkName "a11")]
sgh  = (mkName "Bar", TyScheme_SystemF $ TyExpr_Var  $ mkName "a11")
    
-- | Apply both sides of the env
applyEnv :: Env -> Env -> Env
applyEnv sub env
 = map (fapp sub) env
  where fapp :: Env -> Sub -> Sub
        fapp s (v, b) = (v, appAll s b)
        
-- | performs the same operation as explode but returns a TyExpr instead of a TyScheme
explode' :: Prefix -> TyScheme -> (Prefix, TyExpr)
explode' pre sche = second ftype (explode pre sche)

third :: (c -> d) -> (a,b,c) -> (a,b,d)
third f (a,b,c) = (a, b, f c)

-- | Scope a Gamma to contain only what was mentioned by the first gamma
scope :: Gamma -> Gamma -> Gamma
scope (Gamma from) (Gamma to) 
  = let lst = map fst from
        to' = filter ((`elem` lst) .fst) to
    in Gamma to'
    
-- | Push any quantified type in the environment instead of being prefixed to the type
explode :: Prefix -> TyScheme -> (Prefix, TyScheme)
explode p t | isB t
   = case sugar t of
       TyScheme_Sugar q t' -> (p `munion` q, t')
       x                   -> (p           , x )
  where isB = isBottom . nf
explode p t = (p, t)
    
-- | performs the same operation as explode but returns a TyExpr instead of a TyScheme
deep_explode' :: Prefix -> TyScheme -> (Prefix, TyExpr)
deep_explode' pre = second ftype . deep_explode pre
    
-- | A less restrictive version of explode', this is used in case expressions where we want to always
--   split the Quantifications off the TySchemes
deep_explode :: Prefix -> TyScheme -> (Prefix, TyScheme)
deep_explode p t
   = case sugar t of
       TyScheme_Sugar q t' -> (p `munion` q, t')
       x                   -> (p           , t )
       
instance Apply HsName where
   app (s, v) nm | s == nm   = case ftv v of
                                 [x] -> x
                                 _   -> nm
                 | otherwise =  nm
                 
instance Util HsName where
   ftv  = return
   vars = return
   
instance Apply RowTyExpr where
    app tx@(nm, ty) = mask
      where mask :: RowTyExpr -> RowTyExpr
            mask (RowTyExpr_Ext r t n) = 
              let r' = mask r
                  n' = app tx n
              in RowTyExpr_Ext r' t n'
            mask e = e
    
instance Apply Gamma where
   app s    gam = apply [s] gam
   appAll e gam = apply  e  gam
                   
instance Util Gamma where
    ftv  _ = []
    vars _ = []
    
instance Pretty Gamma where
    pp (Gamma a) = "Gamma: " ++ pp a
    
-- | Apply a substitution
createMask :: Sub -> TyExpr -> TyExpr
createMask (nm, ty) = mask
 where rep :: TyExpr -> TyExpr
       rep val = case isTyVar val nm of
                   False -> val
                   True  -> let ty' = strip ty
                            in  mkParens ty'
                   
       strip :: TyScheme -> TyExpr
       strip (TyScheme_SystemF  a) = a
       strip (TyScheme_Quant  _ a) = strip a
       strip (TyScheme_Sugar  _ a) = strip a
       strip (TyScheme_Forall _ a) = strip a
       strip TyScheme_Bottom       = error "A value of Bottom cannot be stripped."
                   
       mask :: TyExpr -> TyExpr
       mask val = case val of
                    TyExpr_App a b -> 
                      let a' = mask a
                          b' = mask b
                      in TyExpr_App a' b'
                    TyExpr_AppTop a ->
                      let a' = mask a
                      in TyExpr_AppTop a'
                    TyExpr_Parens a ->
                      let a' = mask a
                      in TyExpr_Parens a'
                    TyExpr_Ann a b ->
                      let b' = mask b
                      in TyExpr_Ann a b'
                    TyExpr_Quant a b c ->
                      case b == nm of
                        False -> let c' = mask c
                                 in TyExpr_Quant a b c'
                        True  -> let b' = incr b
                                     e  = app (b, mkVar b') c
                                     e' = mask e
                                 in TyExpr_Quant a b' e'
                    TyExpr_Row a ->
                      let a' = app (nm, ty) a
                      in TyExpr_Row a'
                    x -> rep x
                    
       incr :: HsName -> HsName
       incr var = let str = show var
                  in mkName $ 
                        case span (/='!') str of
                         (v,    []) -> v ++ "!1"
                         (v, (_:c)) -> v ++ ('!':show (read c + 1))
       
class Eq a => Normal a where
  nf :: a -> a
  isNf :: a -> Bool
  isNf x = nf x == x
  
class Util a => Apply a where
  app    :: Sub -> a -> a
  appAll :: Env -> a -> a
  appAll env = foldl' (flip (.)) id (map app env)
  
instance Apply a => Apply [a] where
  app s = map (app s)
  
class Util a where
  ftv  :: a -> [HsName]
  vars :: a -> [HsName]
  
  isTyVar :: a -> HsName -> Bool
  isTyVar _ _ = False
  
  isUnQualTy :: a -> Bool
  isUnQualTy = const False
  
-- | Conditionally add Parenthesis around expressions that contain any kind of Application.
--   They are most likely higher order, and if they weren't meant to be, the simplifier would take care of it
mkParens :: TyExpr -> TyExpr
mkParens x@(TyExpr_App{}) = TyExpr_Parens x
mkParens x                = x

-- | Wraps a AppTop around the right places
mkTop :: TyExpr -> TyExpr
mkTop a = if noArr a then a else TyExpr_Parens $ TyExpr_AppTop a
  
instance Util a => Util [a] where
  ftv  = concatMap ftv
  vars = concatMap vars
  
  isTyVar x nm = and (map (flip isTyVar nm) x)
  
  isUnQualTy = and . map isUnQualTy
  
instance Apply TyExpr where
    app = createMask
    
instance Apply TyIndex where
    app s (TyIndex_Group nm a) = TyIndex_Group (app s nm) (app s a)
  
instance Util TyExpr where
    ftv (TyExpr_App      a b) = nub $ ftv a ++ ftv b
    ftv (TyExpr_AppTop     a) = ftv a
    ftv (TyExpr_Parens     a) = ftv a
    ftv (TyExpr_Ann      _ a) = ftv a
    ftv (TyExpr_Var        a) = [a]
    ftv (TyExpr_VarWild    a) = [a]
    ftv (TyExpr_Quant  _ a b) = (nub $ ftv b) \\ [a]
    ftv (TyExpr_Forall _ a t) = (nub $ ftv t) \\ a
    ftv (TyExpr_Row        r) = ftv r
    ftv                     _ = []
    
    vars (TyExpr_App      a b) = vars a ++ vars b
    vars (TyExpr_AppTop     a) = vars a
    vars (TyExpr_Parens     a) = vars a
    vars (TyExpr_Ann      _ a) = vars a
    vars (TyExpr_Var        a) = [a]
    vars (TyExpr_VarWild    a) = [a]
    vars (TyExpr_Quant  _ a b) = nub (a : vars b)
    vars (TyExpr_Forall _ a t) = nub (a ++ ftv t)
    vars (TyExpr_Row        r) = vars r
    vars                     _ = []

    isTyVar (TyExpr_Var nm) hsnm = hsnm == nm
    isTyVar _ _                  = False
    
    isUnQualTy (TyExpr_Quant{})  = False
    isUnQualTy (TyExpr_Parens a) = isUnQualTy a
    isUnQualTy (TyExpr_AppTop a) = isUnQualTy a
    isUnQualTy _                 = True 
  
instance Util RowTyExpr where
    ftv (RowTyExpr_Empty    ) = []
    ftv (RowTyExpr_Ext a _ b) = nub $ ftv a ++ ftv b
    
    vars = ftv
  
instance Apply TyScheme where
    app tp@(x1,x2) = mask
         where          
           mask :: TyScheme -> TyScheme
           mask val = case val of
                        TyScheme_Quant a b -> 
                          let b' = app tp b
                          in TyScheme_Quant a b'
                        TyScheme_SystemF a ->
                          let a' = app tp a
                          in TyScheme_SystemF a'
                        TyScheme_Forall a b -> -- This will only work top level
                          let b' = app tp b
                          in TyScheme_Forall (sort $ replace (x1,head $ ftv x2) a) b'
                        _                  -> val
                        
-- | Replace every occurence of a value in a list with another value. Preserving ordering
replace :: Eq a => (a,a) -> [a] -> [a]
replace   (_,_) [] = []
replace p@(a,b) (x:xs) |  a == x   = b:replace p xs
                       | otherwise = x:replace p xs

munion = (++)

minsert k v []  = [(k,v)]
minsert k v (x@(k',v'):xs) | k == k'   = (k, v):xs
                           | otherwise = x : minsert k v xs

minsertUnique :: Eq k => k -> v -> [(k, v)] -> [(k, v)]
minsertUnique = minsert                           
-- minsertUnique k v []  = [(k,v)]
-- minsertUnique k v (x@(k',v'):xs) | k == k'   = error $ "Redefinition of value '" ++ pp k ++ "' in the environment."
                                 -- | otherwise = x : minsertUnique k v xs

instance Util TyScheme where
    ftv TyScheme_Bottom       = []
    ftv (TyScheme_SystemF  a) = ftv a
    ftv (TyScheme_Quant (Scheme_Simple a ph1) ph2) 
      = let f2 = nub $ ftv ph2
        in case a `elem` f2 of
             True  -> (nub $ ftv ph1) ++ (a `delete` f2)
             False -> f2
    ftv b@(TyScheme_Sugar{}) = ftv (desugar b)
    ftv (TyScheme_Forall a b) = (ftv b) \\ a
    
    vars TyScheme_Bottom       = []
    vars (TyScheme_SystemF  a) = vars a
    vars (TyScheme_Quant  a b) = nub $ vars a ++ vars b
    vars (TyScheme_Sugar  a b) = nub $ vars a ++ vars b
    vars (TyScheme_Forall a b) = vars b

    isTyVar (TyScheme_SystemF a) hnm = isTyVar a hnm
    isTyVar  _ _                     = False
    
    isUnQualTy (TyScheme_SystemF a) = isUnQualTy a
    isUnQualTy _                    = False  
  
instance Util TyIndex where
  ftv  (TyIndex_Group nm a) = (nub $ ftv a) \\ [nm]
  vars (TyIndex_Group nm a) = nm : vars a
    
instance Util Scheme where
    ftv (Scheme_Simple a b) = a : ftv b
    vars (Scheme_Simple a b) = a : vars b
  
instance Normal TyScheme where
  nf (TyScheme_Quant (Scheme_Simple a phi1) phi2)
       | a `notElem` (ftv phi2) = nf phi2
       | isTyVar (nf phi2) a    = nf phi1
       | isUnQualTy (nf phi1)   = let x = app (a, p) phi2
                                      p = nf phi1
                                  in nf x
       | otherwise              = TyScheme_Quant (Scheme_Simple a (nf phi1)) (nf phi2)
  nf a = a 

instance Normal TyExpr where
  nf = id
  
class Domain a where
  domain   :: a -> [HsName]
  codomain :: a -> [HsName]
  
instance Domain a => Domain [a] where
  domain   = concatMap domain
  codomain = concatMap codomain
  
instance (Domain a, Domain b) => Domain (a, b) where
  domain   = domain . fst
  codomain = codomain . snd
  
instance Domain HsName where
  domain   = return
  codomain = return
  
instance Domain TyScheme where
  domain   = nub . ftv
  codomain = nub . ftv 
  
instance Domain TyIndex where
  domain   (TyIndex_Group a b) = a : binds b
    where binds :: TyScheme -> [HsName]
          binds (TyScheme_Forall a _) = a
          binds (TyScheme_Quant a b ) = domain a ++ binds b
          binds                     _ = []
  codomain (TyIndex_Group _ a) = ftv a
  
instance Domain Scheme where
  domain   (Scheme_Simple a _) = [a]
  codomain (Scheme_Simple _ a) = nub $ vars a
  
-- | Updating a prefix
update :: (Prefix, Either Scheme Sub) -> (Prefix, Env)
update (q, Right (a, p))
  = let (q0, (q1,q2)) = second (`splitOn` a) $ split (q, ftv p)
        q2'           = app (a,p) (safeTail q2)
    in (q0 ++ q1 ++ q2', [(a,p)])
    
update (q, Left (Scheme_Simple var phi2))
  = let (q0, (q1,q2)) = second (`splitOn` var) $ split (q, ftv phi2)
        p   = nf phi2       
        q2' = app (var,p) (safeTail q2)
    in case isUnQualTy p of
         True  -> (q0 ++ q1 ++ q2', [(var, p)])
         False -> (q0 ++ q1 ++ (TyIndex_Group var (promote phi2):(safeTail q2)) ,empty)
        
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs
        
-- | Split a prefix based on a variable
splitOn :: Prefix -> HsName -> (Prefix, Prefix)
splitOn [] x -- failsave, even though we should never get this far.
  = ([], [])
splitOn (x@(TyIndex_Group v1 _):xs) v2
  = case v1 == v2 of
      True  -> ([],x:xs)
      False -> let (a,b) = splitOn xs v2
               in (x:a, b)
  
-- | Extending a Prefix with a Scheme
extend :: (Prefix, Scheme, Int) -> Bool -> ((Prefix, Env), Int)
extend (q, scheme@(Scheme_Simple var phi), frs) ren
 = let p = nf phi
   in case isUnQualTy p of
        True  -> ((q, [(var, p)]), frs)
        _     -> let (ph2, frs', subs) = if False 
                                            then renameBound frs phi
                                            else (phi, frs,[])
                 in ((q++[TyIndex_Group var (promote ph2)], subs), frs')

renameVars :: Int -> TyScheme -> (TyScheme, Int, Env)
renameVars frs ty 
 = (fc ty, frs', env)
  where
    fc (TyScheme_Quant (Scheme_Simple a b) c) = TyScheme_Quant (Scheme_Simple (ren a) (fc b)) (fc c)
    fc (TyScheme_SystemF exp) = let a = fullRename env' exp
                                in  TyScheme_SystemF a
    fc TyScheme_Bottom        = TyScheme_Bottom
    fc (TyScheme_Sugar a b)   = TyScheme_Sugar (map fi a) (fc b)
    fc (TyScheme_Forall a b)  = TyScheme_Forall (map ren a) (fc b)
   
    fi (TyIndex_Group a d) 
       = let d' = case d of
                   TyScheme_Quant (Scheme_Simple a b) c -> TyScheme_Quant (Scheme_Simple (ren a) (fc b)) (fc c)
                   TyScheme_Bottom    -> TyScheme_Bottom
         in TyIndex_Group (ren a) d'
    vc         = vars ty
    (nvc,frs') = freshM frs (length vc)
    env        = zipWith (\a b->(a, mkVar b)) vc nvc
    env'       = zipWith (\a b->(a, b)) vc nvc
    ren x      = maybe x id $ lookup x env'
    
renameBound :: Int -> TyScheme -> (TyScheme, Int, Env)
renameBound frs ty 
 = (fc ty, frs', env)
  where
    fc (TyScheme_Quant (Scheme_Simple a b) c) = TyScheme_Quant (Scheme_Simple (ren a) (fc b)) (fc c)
    fc (TyScheme_SystemF exp) = let a = fullRename env' exp
                                in  TyScheme_SystemF a
    fc TyScheme_Bottom        = TyScheme_Bottom
    fc (TyScheme_Sugar a b)   = TyScheme_Sugar (map fi a) (fc b)
    fc (TyScheme_Forall a b)  = TyScheme_Forall (map ren a) (fc b)
   
    fi (TyIndex_Group a d) 
       = let d' = case d of
                   TyScheme_Quant (Scheme_Simple a b) c -> TyScheme_Quant (Scheme_Simple (ren a) (fc b)) (fc c)
                   TyScheme_Bottom    -> TyScheme_Bottom
         in TyIndex_Group (ren a) d'
    vc         = (nub $vars ty) \\ (nub $ ftv ty)
    (nvc,frs') = freshM frs (length vc)
    env        = zipWith (\a b->(a, mkVar b)) vc nvc
    env'       = zipWith (\a b->(a, b)) vc nvc
    ren x      = maybe x id $ lookup x env'
    
-- | rename based on fullRename instead of app which avoids capturing values.
forceRename :: TyExpr -> TyExpr
forceRename exp = fullRename env exp
 where
    vcs = nub $ vars exp
    num = (map (mkName . (:[])) ['a'..'z']) ++ liftM2 (\b a -> mkName $ a : show b) [1..] ['a'..'z']
    env = zip vcs num
    
-- | Fully rename a TyExpr's variables. ALL variables, not just those that are bound
fullRename ::  [(HsName, HsName)] -> TyExpr -> TyExpr
fullRename env = rename
 where 
   rename :: TyExpr -> TyExpr
   rename (TyExpr_App      f a) = TyExpr_App (rename f) (rename a)
   rename (TyExpr_AppTop     a) = TyExpr_AppTop (rename a)
   rename (TyExpr_Parens     a) = TyExpr_Parens (rename a)
   rename (TyExpr_Ann      a t) = TyExpr_Ann a (rename t)
   rename (TyExpr_Var        a) = TyExpr_Var $ ren a
   rename (TyExpr_VarWild    a) = TyExpr_Var $ ren a
   rename (TyExpr_Quant  q v t) = TyExpr_Quant q (ren v) (rename t)
   rename (TyExpr_Forall q v t) = TyExpr_Forall q (map ren v) (rename t)
   rename (TyExpr_Row        a) = TyExpr_Row (fullRenameRow a)
   rename e = e
   
   fullRenameRow :: RowTyExpr -> RowTyExpr
   fullRenameRow RowTyExpr_Empty       = RowTyExpr_Empty
   fullRenameRow (RowTyExpr_Ext a b c) = RowTyExpr_Ext (fullRenameRow a) b (rename c)
   
   ren a = maybe a id $ lookup a env
        
-- | Checks a prefix to see if it has a binding for a variable and select it
contains :: Prefix -> HsName -> Maybe TyQuantifiedScheme
contains pref name = msum $ map (\(TyIndex_Group nm b) -> guard (nm == name) >> return b) pref

-- | Drop a quantification from a TyQuantifiedScheme
dropQuant :: TyQuantifiedScheme -> TyScheme
dropQuant = id

-- | Split a quantifier of a TyScheme
splitQuant :: TyScheme -> (Prefix, TyScheme)
splitQuant ty = let TyScheme_Sugar q t = sugar ty
                in (q, t)

-- | Drop a TyScheme to a TyExpr
dropScheme :: TyScheme -> TyExpr
dropScheme (TyScheme_SystemF a) = a
dropScheme (TyScheme_Quant{}  ) = error "Cannot drop a Quant to a TyExpr"
dropScheme (TyScheme_Sugar{}  ) = error "Cannot drop a Sugar to a TyExpr"
dropScheme (TyScheme_Forall{} ) = error "Cannot drop a Forall to a TyExpr"
dropScheme TyScheme_Bottom      = error "Cannot drop a _|_ to a TyExpr"

-- | Collect the LHS bindings (Names)
getBindLHs :: Decls -> [HsName]
getBindLHs = concatMap getBindLH
 where getBindLH :: Decl -> [HsName]
       getBindLH (Decl_Val p _) = getB p
       getBindLH _              = []
       
       getB :: PatExpr -> [HsName]
       getB (PatExpr_Var a) = [a]
       getB _               = []
      
-- | Promote/Convert a Type Scheme to a quantified type scheme. 
--   This greatly influences the types. Might need to be reconsidered and checked 
--   to see if we can simplify the types generates
promote :: TyScheme -> TyQuantifiedScheme
promote TyScheme_Bottom                            = TyScheme_Bottom
promote (TyScheme_Quant (Scheme_Simple nm ty) ty') = TyScheme_Quant (Scheme_Simple nm (promote ty)) ty'
promote (TyScheme_SystemF x)                       
  = embedF x
promote (TyScheme_Sugar{})                         = error "TyScheme_Sugar cannot be promoted. It's purely syntaxtical sugar. Please call desugar before that"
promote (TyScheme_Forall a x)                       
  = TyScheme_Forall a (promote x)

-- -- | Represent a SystemF type as a flexible type, This is not implemented for higher rank types yet
embedF :: TyExpr -> TyScheme
embedF = embed id
 where embed :: (TyScheme -> TyScheme) -> TyExpr -> TyScheme
       embed val (TyExpr_Parens     s) = embed val s
       embed val (TyExpr_Quant  _ a t) = let e = TyScheme_Quant (Scheme_Simple a TyScheme_Bottom)
                                         in  embed (val . e) t
       embed val (TyExpr_Forall _ v t) = let e = foldl' (.) id [ TyScheme_Quant (Scheme_Simple a TyScheme_Bottom) | a <- v ] 
                                         in  embed (val . e) t
       embed val e                     = val (TyScheme_SystemF e)

simple    = TyExpr_Quant tyQu_Forall (mkName "a") $ TyExpr_Quant tyQu_Forall (mkName "b") ((TyExpr_Var $ mkName "a") `mkArrow` (TyExpr_Var $ mkName "b"))
difficult = TyExpr_Quant tyQu_Forall (mkName "a") $ TyExpr_Quant tyQu_Forall (mkName "b") ((TyExpr_Parens $ TyExpr_Quant tyQu_Forall (mkName "c") $ (TyExpr_Var $ mkName "c") `mkArrow` TyExpr_Con (mkName "Int")) `mkArrow` ((TyExpr_Var $ mkName "a") `mkArrow` (TyExpr_Var $ mkName "b")))
       
-- | Generate a fresh variable and increase the counter
fresh :: Int -> (HsName, Int)
fresh =  freshT "a"
     
-- | Generate a fresh variable and increase the counter based on a template
freshT :: String -> Int -> (HsName, Int)
freshT a x = (mkName $ a ++ show x, x+1)

-- | Generate a list of fresh variables and a new counter
freshM :: Int -> Int -> ([HsName], Int)
freshM s c = fresh' c ([], s)
 where fresh' 0 d     = d
       fresh' c (l,s) = let (n, i) = fresh s
                        in fresh' (c-1) (n:l, i)

-- | Creates an arrow between two types.
--   e.g given type a and b returns the type a -> b or (-> b) a rather
mkArrow :: TyExpr -> TyExpr -> TyExpr
mkArrow a b =  TyExpr_AppTop $ TyExpr_App (TyExpr_App (TyExpr_Con $ mkName "->") a) b

mkVar :: HsName -> TyScheme
mkVar = TyScheme_SystemF . TyExpr_Var

mkQVar :: HsName -> TyScheme
mkQVar = mkQuantified . TyExpr_Var

mkSkolem :: HsName -> TyScheme
mkSkolem = TyScheme_SystemF . TyExpr_Con

-- | Desugar a forall Q.phi to (a>=phi1).(b>=phi2).phi etc
--   basically unfolds the type
desugar :: TyScheme -> TyScheme
desugar (TyScheme_Sugar q t) = let bounds = [TyScheme_Quant (Scheme_Simple nm (toScheme b)) | (TyIndex_Group nm b) <- q] 
                               in foldr (.) id bounds t
desugar x = x

-- | Insure that the TyScheme is in a sugared form if possible
sugar :: TyScheme -> TyScheme
sugar (TyScheme_Quant (Scheme_Simple nm b) t) 
    = let q' = TyIndex_Group nm (fromScheme b)
      in case t of
          (TyScheme_SystemF _) -> TyScheme_Sugar [q']   t
          _                    ->
             case sugar t of
              TyScheme_Sugar q t -> TyScheme_Sugar (q':q) t
              x                  -> TyScheme_Sugar [q']   t
sugar y@(TyScheme_SystemF{}) = TyScheme_Sugar [] y
sugar x = x

-- | Convert a Quantified Scheme to a TyScheme such that
--   toScheme . fromScheme == id
toScheme :: TyQuantifiedScheme -> TyScheme
toScheme = id

-- | Convert a TyScheme to a Quantified Scheme such that
--   toScheme . fromScheme == id
fromScheme :: TyScheme -> TyQuantifiedScheme
fromScheme = id

-- | Fully quantify a type based on the type variable it contains.
--   e.g. List a becomes forall (a>=_|_). List a
mkQuantified :: TyExpr -> TyScheme
mkQuantified exp 
  = let vars = [TyScheme_Quant (Scheme_Simple x TyScheme_Bottom) | x <- nub (ftv exp)]
    in  if null vars
           then TyScheme_SystemF exp
           else foldl' (.) id vars (TyScheme_SystemF exp)

addQuantifiers :: TyScheme -> TyScheme
addQuantifiers exp
  = let vars = [TyScheme_Quant (Scheme_Simple x TyScheme_Bottom) | x <- nub (ftv exp)]
    in  if null vars
           then exp
           else foldl' (.) id vars exp
           
class Simplify a where
  simplify :: a -> a
  clean    :: a -> a
  
instance (Simplify a, Simplify b) => Simplify (a,b) where
  simplify (k,v) = (simplify k, simplify v)
  clean    (k,v) = (simplify k, clean v)
  
instance Simplify HsName where
  simplify = id
  clean    = id 
  
-- | A strong but inefficient alpha renamer (for pretty printing)
alpha_rename :: TyScheme -> TyScheme
alpha_rename ty 
 = let nvars = map (TyScheme_SystemF . TyExpr_Var . mkName) $ (map (:[]) ['a'..'z']) ++ liftM2 (\b a -> a : show b) [1..] ['a'..'z']
       fvars = sortBy lexord $ nub $ vars ty
       lexord a b = case (compare `on` length . pp) a b of
                     LT -> LT
                     GT -> GT
                     EQ -> a `compare` b
       env   = zip fvars nvars
   in appAll env ty
     
-- | Check to see if a tyScheme is _|_    
isBottom :: TyScheme -> Bool
isBottom TyScheme_Bottom = True
isBottom _               = False
 
-- | converts a TyScheme to a SystemF type
ftype :: TyScheme -> TyExpr
ftype = ft . nf . desugar
 where ft :: TyScheme -> TyExpr
       ft (TyScheme_SystemF a) = a
       ft TyScheme_Bottom      = TyExpr_Quant tyQu_Forall (mkName "a") (TyExpr_Var (mkName "a"))
       ft (TyScheme_Quant (Scheme_Simple a TyScheme_Bottom) phi) 
            = TyExpr_Quant tyQu_Forall a (ft phi)
       ft (TyScheme_Quant (Scheme_Simple a q) phi)
            = let (TyScheme_Sugar q' p) = sugar q 
                  phi'                  = app (a, p) phi
              in ft $ desugar (TyScheme_Sugar q' phi')
              
-- | converts a TyScheme to a partial TyScheme by not removing quantifiers bound to Bottom
ptype :: TyScheme -> TyScheme
ptype = id
-- ptype = ft . desugar
 -- where ft :: TyScheme -> TyScheme
       -- ft (TyScheme_Quant y@(Scheme_Simple _ TyScheme_Bottom) x) = TyScheme_Quant y (ft x)
       -- ft (TyScheme_Quant (Scheme_Simple a q) phi)
            -- = let (TyScheme_Sugar q' p) = sugar q 
                  -- phi'                  = app (a, p) phi
              -- in ft $ desugar (TyScheme_Sugar q' phi')
       -- ft x = x
       
   
instance Simplify TyScheme where           
    -- | Simplify a type scheme for presentation, dropping trivial quantifications
    --   e.g. (a>=_|_) to a, and expanding types.
    --   simplify :: TyScheme -> TyScheme
    simplify = TyScheme_SystemF . ftype
    clean    = (\(TyScheme_SystemF a)-> TyScheme_SystemF $ forceRename a) . simplify
    
instance Simplify TyExpr where
    -- | Simplify a type expression for presentation. This version just drops superflous parenthesis
    --   simplify :: TyExpr -> TyExpr and wrap things in an AppTop for pretty printing
    simplify = simpl False
      where simpl :: Bool -> TyExpr -> TyExpr
            simpl True  p@(TyExpr_Parens    a) = let x  = noArr a
                                                     p' = TyExpr_Parens (simpl x a)
                                                 in if x then p' else TyExpr_AppTop p'
            simpl False p@(TyExpr_Parens    a) = let x  = noArr a
                                                     a' = simpl True a
                                                 in if x then a' else TyExpr_AppTop a'
            simpl _       (TyExpr_App     a b) = let e1 = noArr a
                                                     e2 = noArr b
                                                 in TyExpr_App (simpl e1 a) (simpl e2 b)
            simpl f       (TyExpr_AppTop    a) = TyExpr_AppTop    (simpl f a)
            simpl f       (TyExpr_Ann     a b) = TyExpr_Ann a     (simpl f b)
            simpl f       (TyExpr_Quant a b c) = TyExpr_Quant a b (simpl f c)
            simpl _ a                          = a
            
    clean = (\(TyScheme_SystemF a)->a) . clean . TyScheme_SystemF

-- | Checks if the expression contains an arrow. False will be returned if it does. This is needed for pretty printing in UHC
noArr :: TyExpr -> Bool
noArr (TyExpr_Con a)    = not $ pp a == "->"
noArr (TyExpr_App a b)  = noArr a && noArr b
noArr (TyExpr_Parens a) = noArr a
noArr _                 = True
            
-- | Checks to see if a pattern expression is a variable or something else
isVar :: PatExpr -> Bool
isVar (PatExpr_Var{}) = True
isVar _               = False

mkQuant :: TyExpr -> TyExpr
mkQuant x@(TyExpr_Quant{}) = x
mkQuant x                  = 
  let vars = ftv x
      f    = TyExpr_Quant tyQu_Forall
      c    = map f vars
  in  foldl (.) id c x
  
mkForall :: TyExpr -> TyExpr
mkForall (TyExpr_Quant q a t) 
 = let (xs, t') = mk t
   in TyExpr_Forall q xs t'
   where 
     mk :: TyExpr -> ([HsName], TyExpr)
     mk y@(TyExpr_Quant m b c) 
       = let (x,t') = mk c
         in case m == q of
              False -> ([a], y)
              True  -> (b:x, t')
     mk x = ([a], x)
mkForall x@(TyExpr_Forall{}) 
 = x
mkForall x 
 = TyExpr_Forall tyQu_Forall [] x
 
occursCheck :: HsName -> Prefix -> TyScheme -> Bool
occursCheck a q phi 
  = let (q0,q1,q2) = let (r1,r2) = (q `splitOn` a)
                        in case r2 of
                             (x:xs) -> (r1, [x], xs)
                             []     -> (r1, [], [])
        fvs           = ftv (TyScheme_Sugar q2 phi)
    in case null q1 of
         True  -> let f1 = trace ("a:" ++ pp a)
                      f2 = trace ("q:" ++ pp q)
                      f3 = trace ("phi: " ++ pp phi)
                  in f1 $ f2 $ f3 $ error "Cannot perform occurance check. The split does not return a value" 
         False -> a `elem` fvs
                  
type TyQuantifiedScheme = TyScheme
sche :: TyScheme -> String
sche (TyScheme_Quant{})   = "Quant"
sche (TyScheme_SystemF{}) = "SystemF"
sche (TyScheme_Bottom{})  = "Bottom"
sche (TyScheme_Sugar{})   = "Sugar"
sche (TyScheme_Forall{})  = "Forall"

con :: TyExpr -> String
con (TyExpr_Con a)    = "Con " ++ show a
con (TyExpr_Var a)    = "Var " ++ show a
con (TyExpr_App a b)  = con a ++ "; " ++ con b
con (TyExpr_AppTop a) = "AppTop (" ++ con a ++ ")"
con (TyExpr_Parens a) = "(" ++ con a ++ ")"
con (TyExpr_Quant _ _ a) = "Quant: " ++ con a
con (TyExpr_Row _ )   = "Row"
con _                 = "unmapped"