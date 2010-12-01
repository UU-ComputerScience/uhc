{-# LANGUAGE BangPatterns          #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeSynonymInstances  #-}
module Utils where

import Control.Monad.Error
import Pretty   
import qualified Data.Map as M
import Control.Monad
import Control.Arrow hiding (app)
import Control.Applicative((<$>), (<|>))

import Data.List
import Data.Maybe hiding (mapMaybe)

import EH8.EH
import EH8.Base.HsName
import EH8.Ty(tyQu_Forall)

import EqHML

import EH.Util.Pretty hiding (pp, empty)
import qualified Debug.Trace as D

trace = D.trace

-- | Create a HsName from a string.
mkName :: String -> HsName
mkName = mkHNmBase -- id

-- type Prefix  = [TyIndex]
-- type HsName  = String
-- type Env     = M.Map HsName TyScheme
type Sub     = (HsName, TyScheme)
type ErrorMessage = String
newtype Gamma = Gamma { unGam :: Env }

-- | Remove entries from a Env based on their key
remove :: Env -> [HsName] -> Env
remove env entries = foldl (.) id (map rm entries) env

-- | Filter a prefix based on the bounds and not the variable
elide :: Prefix -> TyQuantifiedScheme -> Prefix
elide []     _     = []
elide (x:xs) bound = case x of 
                       TyIndex_Group _ b | b == bound -> elide xs bound
                       _ -> x : elide xs bound

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
empty = []
rm :: Eq k => k -> [(k, v)] -> [(k, v)]
rm _ [] = []
rm k1 (x@(k,v):xs) | k1 == k   = rm k1 xs
                       | otherwise = x : rm k1 xs

-- | Apply a set of substitution to an environment
apply :: Env -> Gamma -> Either Gamma [ErrorMessage]
apply sub env 
  = let lst  = map (mapMaybe . app) sub
        set  = foldl' (.) id lst
    in Left (Gamma $ set $ unGam env)
    
applyEnv :: Env -> Env -> Env
applyEnv sub env
 = map (fapp sub) env
  where fapp :: Env -> Sub -> Sub
        fapp s (v, b) = (fromJust $ appAll s v, fromJust $ appAll s b)
    
-- | Push any quantified type in the environment instead of being prefixed to the type
explode :: Prefix -> TyScheme -> (Prefix, TyScheme)
explode p t | isB t
   = case sugar t of
       TyScheme_Sugar q t' -> (p `munion` q, t')
       x                   -> (p           , x )
  where isB t = case nf t of
                 TyScheme_Bottom -> True
                 _               -> False
explode p t = (p, t)
    
instance Apply HsName where
   app (s, v) nm | s == nm   = case ftv v of
                                 [x] -> return x
                                 _   -> return nm
                 | otherwise = return nm
                 
instance Util HsName where
   ftv  = return
   vars = return
    
instance Apply Gamma where
   app s   gam = case apply [s] gam of
                  Left e  -> Just e
                  Right _ -> Nothing
   appAll e gam = case apply e gam of
                   Left e  -> Just e
                   Right _ -> Nothing
                   
instance Util Gamma where
    ftv  _ = []
    vars _ = []
    
-- | Apply a substitution
createMask :: Sub -> (TyExpr -> Maybe TyExpr)
createMask (nm, ty) = mask
 where rep :: TyExpr -> Maybe TyExpr
       rep val = case isTyVar val nm of
                   False -> return val
                   True  -> let ty' = strip ty
                            in return $ mkParens ty'
                   
       strip :: TyScheme -> TyExpr
       strip (TyScheme_SystemF  a) = a
       strip (TyScheme_Quant  _ a) = strip a
       strip (TyScheme_Sugar  _ a) = strip a
       strip (TyScheme_Forall _ a) = strip a
       strip TyScheme_Bottom       = error "A value of Bottom cannot be stripped."
                   
       mask :: TyExpr -> Maybe TyExpr
       mask val = case val of
                    TyExpr_App a b -> 
                      do a' <- mask a
                         b' <- mask b
                         return $ TyExpr_App a' b'
                    TyExpr_AppTop a ->
                      do a' <- mask a
                         return $ TyExpr_AppTop a'
                    TyExpr_Parens a ->
                      do a' <- mask a
                         return $ TyExpr_Parens a'
                    TyExpr_Ann a b ->
                      do b' <- mask b
                         return $ TyExpr_Ann a b'
                    TyExpr_Quant a b c ->
                      case b == nm of
                        False -> do c' <- mask c
                                    return $ TyExpr_Quant a b c'
                        True  -> do let b'       = incr b
                                        (Just e) = app (b, mkVar b') c
                                    e' <- mask e
                                    return $ TyExpr_Quant a b' e'
                    var@(TyExpr_Var{}) ->
                      do rep var
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
  app    :: Sub -> a -> Maybe a
  appAll :: Env -> a -> Maybe a
  appAll env a = foldl (>=>) return (map app env) a
  
instance Apply a => Apply [a] where
  app s = return . catMaybes . map (app s)
  
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
    app s (TyIndex_Group nm a) = TyIndex_Group nm <$> app s a
  
instance Util TyExpr where
    ftv (TyExpr_App     a b) = ftv a ++ ftv b
    ftv (TyExpr_AppTop    a) = ftv a
    ftv (TyExpr_Parens    a) = ftv a
    ftv (TyExpr_Ann     _ a) = ftv a
    ftv (TyExpr_Var       a) = [a]
    ftv (TyExpr_VarWild   a) = [a]
    ftv (TyExpr_Quant _ a b) = (nub $ ftv b) \\ [a]
    ftv                    _ = []
    
    vars (TyExpr_App     a b) = vars a ++ vars b
    vars (TyExpr_AppTop    a) = vars a
    vars (TyExpr_Parens    a) = vars a
    vars (TyExpr_Ann     _ a) = vars a
    vars (TyExpr_Var       a) = [a]
    vars (TyExpr_VarWild   a) = [a]
    vars (TyExpr_Quant _ a b) = (nub $ vars b)
    vars                    _ = []

    isTyVar (TyExpr_Var nm) hsnm = hsnm == nm
    isTyVar _ _                  = False
    
    isUnQualTy (TyExpr_Quant{})  = False
    isUnQualTy (TyExpr_Parens a) = isUnQualTy a
    isUnQualTy (TyExpr_AppTop a) = isUnQualTy a
    isUnQualTy _                 = True 
    
-- instance Apply TyQuantifiedScheme where
    -- app m (TyQuantifiedScheme_Quant i s) = TyQuantifiedScheme_Quant i <$> app m s
    -- app _ TyQuantifiedScheme_Bottom      = return TyQuantifiedScheme_Bottom
  
instance Apply TyScheme where
    app tp@(x1,x2) = mask
         where          
           mask :: TyScheme -> Maybe TyScheme
           mask val = case val of
                        TyScheme_Quant a b -> 
                          do b' <- app tp b
                             return $ TyScheme_Quant a b'
                        TyScheme_SystemF a ->
                          do a' <- app tp a
                             return $ TyScheme_SystemF a'
                        TyScheme_Forall a b -> -- This will only work top level
                          do b' <- app tp b
                             return $ TyScheme_Forall (sort $ replace (x1,head $ ftv x2) a) b'
                        _                  -> return val
                        
-- | Replace every occurence of a value in a list with another value. Preserving ordering
replace :: Eq a => (a,a) -> [a] -> [a]
replace   (_,_) [] = []
replace p@(a,b) (x:xs) |  a == x   = b:replace p xs
                       | otherwise = x:replace p xs
                        
-- munion a b = let f = map (uncurry minsert)
                 -- xs = f a ++ f b
             -- in foldl' (.) id xs []
munion = (++)

minsert k v []  = [(k,v)]
minsert k v (x@(k',v'):xs) | k == k'   = (k, v):xs
                           | otherwise = x : minsert k v xs

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

-- instance Util TyQuantifiedScheme where
  -- ftv  (TyQuantifiedScheme_Quant a b) = (nub $ ftv b) \\ (ftv a)
  -- ftv  _                              = []
  -- vars (TyQuantifiedScheme_Quant a b) = nub $ (vars b) ++ (vars a)
  
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
                                  in maybe (error "Substitution failed in Normal for UnQualTy") nf x
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
  
isIn :: HsName -> TyIndex -> Bool
c `isIn` (TyIndex_Group a b) = c `elem` (a : binds b)
    where binds :: TyScheme -> [HsName]
          binds (TyScheme_Forall a _) = a
          binds (TyScheme_Quant a b ) = domain a ++ codomain a++ binds b
          binds                     _ = []
  
-- instance Domain Env where
  -- domain   = M.keys
  -- codomain = concatMap ftv . M.Elems
  
instance Domain Scheme where
  domain   (Scheme_Simple a _) = [a]
  codomain (Scheme_Simple _ a) = nub $ vars a
  
-- | Updating a prefix
update :: (Prefix, Either Scheme Sub) -> (Prefix, Env)
update (q, Right (a, p))
  = let (q0, (q1,q2)) = id *** (`splitOn` a) $ split (q, ftv p)
        q2' = case app (a,p) (safeTail q2) of
               Nothing -> error "Update failed. Could not apply substitution"
               Just ax -> trace (   "** Var:" ++ pp a
                                   ++ "\n** Q1 :" ++ pp q1
                                    ++ "\n** Q2 :" ++ pp q2) ax
    in  (q0 ++ q1 ++ q2', [(a,p)])
update (q, Left (Scheme_Simple var phi2))
  = let (q0, (q1,q2)) = id *** (`splitOn` var) $ split (q, ftv phi2)
        p   = nf phi2       
        q2' = case app (var,p) (safeTail q2) of
               Nothing -> error "Update failed. Could not apply substitution"
               Just ax -> ax
    in case isUnQualTy p of
         True  -> (q0 ++ q1 ++ q2', [(var, p)])
         False -> (q0 ++ q1 ++ (TyIndex_Group var (promote phi2 (-9)):(safeTail q2)) ,empty)
        
safeTail :: [a] -> [a]
safeTail []     = []
safeTail (x:xs) = xs
        
-- | Split a prefix based on a variable
splitOn :: Prefix -> HsName -> (Prefix, Prefix)
splitOn [] _ 
  = ([], [])
splitOn (x@(TyIndex_Group v1 _):xs) v2
  = case v1 == v2 of
      True  -> ([],x:xs)
      False -> let (a,b) = splitOn xs v2
               in (x:a, b)
-- splitOn (x:xs) v2
  -- = let d' = pullup x v2
        -- d = D.trace ("|| " ++ show (map pp d')) d'
    -- in case d of
        -- (x:x':_) -> ([x],x':xs)
        -- _        -> let (a,b) = splitOn xs v2
                    -- in  (a,x:b)
               
-- | Pullup a type that's deeply embedded in a index
pullup :: TyIndex -> HsName -> [TyIndex]
pullup x@(TyIndex_Group v1 r) v2 
  | v1 == v2  = [x]
  | otherwise = let (a, b) = findIn r v2
                    b'     = case b of
                               Just (Scheme_Simple q r) -> [TyIndex_Group q r]
                               _                        -> []
                in case b' of
                     [] -> []
                     br ->  TyIndex_Group v1 a : br
 where findIn :: TyScheme -> HsName -> (TyScheme, Maybe Scheme)
       findIn (TyScheme_Quant s@(Scheme_Simple a b) c) nm
         = case a == nm of
             True  -> (c, Just s)
             False -> let (c', s') = findIn c nm
                          (b', r)  = findIn b nm
                      in case r of
                          Nothing -> (TyScheme_Quant s c', s')
                          Just _  -> (TyScheme_Quant (Scheme_Simple a b') c, r)
       findIn a _ = (a, Nothing)
  
-- | Extending a Prefix with a Scheme
extend :: (Prefix, Scheme, Int) -> Bool -> ((Prefix, Env), Int)
extend (q, scheme@(Scheme_Simple var phi), frs) ren
 = let p = nf phi
   in case isUnQualTy p of
        True  -> ((q, [(var, p)]), frs)
        -- TyScheme_Quant s t   -> case s of
                                 -- Scheme_Simple v t' 
                                   -- -> let s' = Scheme_Simple var t'
                                          -- (Just tx) = app (v, mkVar var) t
                                          -- ty = TyScheme_Quant s' tx
                                      -- in ((q++[TyIndex_Group var (promote ty)], empty), frs)
        -- _  -> case sweep p of
               -- (schemes, ph2) | not (null schemes) ->
                  -- let (varsc, frs') = freshM frs (length schemes)
                      -- env           = zipWith (\a b-> (a, mkVar b)) (domain schemes) varsc
                      -- (Just ph2')   = appAll (M.fromAscList env) ph2
                      -- pre           = q ++ [TyIndex_Group v (promote p) | (v, p) <- zipWith (\a (Scheme_Simple _ x)->(a, x)) varsc schemes ]
                  -- in ((pre, M.fromAscList [(var, ph2')]), frs')
        _     -> let (ph2, frs', subs) = if ren 
                                            then renameVars frs phi
                                            else (phi, frs,[])
                 in trace ("@" ++ pp var ++ " == " ++ pp phi ++ " -- " ++ sche phi ++ " -- " ++ pp (promote ph2 frs') ++ " @ " ++ pp subs) ((q++[TyIndex_Group var (promote ph2 frs')], subs), frs')

renameVars :: Int -> TyScheme -> (TyScheme, Int, Env)
renameVars frs ty 
 = (fc ty, frs', env)
  where
    fc (TyScheme_Quant (Scheme_Simple a b) c) = TyScheme_Quant (Scheme_Simple (ren a) (fc b)) (fc c)
    fc (TyScheme_SystemF exp) = let -- (Just a) = appAll env exp
                                     a = fullRename env' exp
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
   rename e = e
   
   ren a = maybe a id $ lookup a env
   
-- | Collects all the schemes for TyScheme_Quant
sweep :: TyScheme -> ([Scheme], TyScheme)
sweep (TyScheme_Quant a p) 
  = let (s,r) = sweep p
    in  (a:s, r)
sweep x = ([], x)
        
-- | Checks a prefix to see if it has a binding for a variable and select it
contains :: Prefix -> HsName -> Maybe TyQuantifiedScheme
contains pref name = msum $ map (\(TyIndex_Group nm b) -> guard (nm == name) >> return b) pref
-- contains pref name = let old = (msum $ map (\(TyIndex_Group nm b) -> guard (nm == name) >> return b) pref) :: Maybe TyQuantifiedScheme
                         -- new = msum $ map spine pref
                     -- in trace ("-- Name: " ++ pp name) $ 
                        -- trace ("-- Old: "  ++ pp old ) $ 
                        -- trace ("-- New: "  ++ pp new ) new
-- contains pref name = msum $ map spine pref
 where spine x = let y = pullup x name
                 in case y of
                     (_:(TyIndex_Group _ x):_) -> Just x
                     [TyIndex_Group _ b] -> Just b
                     _       -> Nothing

-- | Drop a quantification from a TyQuantifiedScheme
dropQuant :: TyQuantifiedScheme -> TyScheme
dropQuant = id
-- dropQuant (TyScheme_Quant b t) = desugar $ TyScheme_Sugar [b] t
-- dropQuant TyScheme_Bottom      = TyScheme_Bottom

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
promote :: TyScheme -> Int -> TyQuantifiedScheme
promote TyScheme_Bottom                            _ = TyScheme_Bottom
promote (TyScheme_Quant (Scheme_Simple nm ty) ty') i = TyScheme_Quant (Scheme_Simple nm (promote ty i)) ty'
promote (TyScheme_SystemF x)                       i
  = embedF x i
promote (TyScheme_Sugar{})                         _ = error "TyScheme_Sugar cannot be promoted. It's purely syntaxtical sugar. Please call desugar before that"
promote (TyScheme_Forall a x)                      i 
  = TyScheme_Forall a (promote x i)

-- -- | Represent a SystemF type as a flexible type, This is not implemented for higher rank types yet
-- embedF :: TyExpr -> TyScheme
-- embedF = embed id
 -- where embed :: (TyScheme -> TyScheme) -> TyExpr -> TyScheme
       -- embed val (TyExpr_Parens     s) = embed val s
       -- embed val (TyExpr_Quant  _ a t) = let e = TyScheme_Quant (Scheme_Simple a TyScheme_Bottom)
                                         -- in  embed (val . e) t
       -- embed val (TyExpr_Forall _ v t) = let e = foldl' (.) id [ TyScheme_Quant (Scheme_Simple a TyScheme_Bottom) | a <- v ] 
                                         -- in  embed (val . e) t
       -- embed val e                     = val (TyScheme_SystemF e)

-- | Represent a SystemF type as a flexible type, This is not implemented for higher rank types yet
embedF :: TyExpr -> Int -> TyScheme
embedF e = uncurry ($) . second (TyScheme_SystemF . fst) . embed False e
 where embed :: Bool -> TyExpr -> Int -> (TyScheme -> TyScheme, (TyExpr, Int))
       embed b (TyExpr_Parens     s) i = if b 
                                          then let (f1, (e, i')) = embed False s i
                                                   (var, ires)   = freshT "t" i'
                                                   f2            = TyScheme_Quant (Scheme_Simple var (f1 $ TyScheme_SystemF e))
                                               in (f2, (TyExpr_Var var, ires))
                                          else second (first TyExpr_Parens) (embed b s i)
       embed b (TyExpr_Quant  _ a t) i = let e = TyScheme_Quant (Scheme_Simple a TyScheme_Bottom)
                                         in  first (e.) $ embed b t i
       embed b (TyExpr_Forall _ v t) i = let e = foldl' (.) id [ TyScheme_Quant (Scheme_Simple a TyScheme_Bottom) | a <- v ] 
                                         in  first (e.) $ embed b t i
       embed b (TyExpr_AppTop     x) i = let (f, (e, i')) = embed b x i
                                         in  (f, (TyExpr_AppTop e, i'))
       embed _ (TyExpr_App      f x) i = let (f1, (e1, i1)) = embed True f i
                                             (f2, (e2, i2)) = embed True x i1
                                         in  (f2 . f1, (TyExpr_App e1 e2, i2))
       embed _ e                     i = (id, (e, i))

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

-- | The same as mkArrow except it allows a TyScheme as first argument and resulting type
mkArrowL :: TyScheme -> TyExpr -> TyScheme
mkArrowL a b = 
  case a of
    TyScheme_Quant  c d -> TyScheme_Quant c (mkArrowL d b)
    TyScheme_SystemF  d -> TyScheme_SystemF (d `mkArrow` b)
    TyScheme_Bottom     -> TyScheme_Bottom
    TyScheme_Sugar  p d -> TyScheme_Sugar p (mkArrowL d b)
    TyScheme_Forall q d -> TyScheme_Forall q (d `mkArrowL` b)

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
-- sugar (TyScheme_SystemF a) = case mkQuantified a of
                               -- y@(TyScheme_SystemF {}) -> TyScheme_Sugar [] y
                               -- x                       -> sugar x
sugar y@(TyScheme_SystemF{}) = TyScheme_Sugar [] y
sugar x = x

-- | Convert a Quantified Scheme to a TyScheme such that
--   toScheme . fromScheme == id
toScheme :: TyQuantifiedScheme -> TyScheme
toScheme = id
-- toScheme TyQuantifiedScheme_Bottom                             = TyScheme_Bottom
-- toScheme (TyQuantifiedScheme_Quant (TyIndex_Group nm bound) t) = TyScheme_Quant (Scheme_Simple nm (toScheme bound)) t

-- | Convert a TyScheme to a Quantified Scheme such that
--   toScheme . fromScheme == id
fromScheme :: TyScheme -> TyQuantifiedScheme
fromScheme = id
-- fromScheme TyScheme_Bottom                         = TyQuantifiedScheme_Bottom
-- fromScheme (TyScheme_Quant (Scheme_Simple nm b) t) = TyQuantifiedScheme_Quant (TyIndex_Group nm (fromScheme b)) t

-- | Fully quantify a type based on the type variable it contains.
--   e.g. List a becomes forall (a>=_|_). List a
mkQuantified :: TyExpr -> TyScheme
mkQuantified exp 
  = let vars = [TyScheme_Quant (Scheme_Simple x TyScheme_Bottom) | x <- nub (ftv exp)]
    in  if null vars
           then TyScheme_SystemF exp
           else foldl' (.) id vars (TyScheme_SystemF exp)
           
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
       lexord a b = case length (pp a) `compare` length (pp b) of
                     LT -> LT
                     GT -> GT
                     EQ -> a `compare` b
       env   = zip fvars nvars
   in  maybe ty id $ appAll env ty
     
-- | Check to see if a tyScheme is _|_    
isBottom :: TyScheme -> Bool
isBottom TyScheme_Bottom = True
isBottom _               = False
   
unfold :: TyScheme ->TyScheme
unfold (TyScheme_Forall a b) 
 = let e = [ TyScheme_Quant (Scheme_Simple x TyScheme_Bottom) | x <- a ]
   in foldr ($) b e
unfold x 
 = x
 
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
                  (Just phi') = app (a, p) phi
              in ft $ desugar (TyScheme_Sugar q' phi')
              
-- | converts a TyScheme to a partial TyScheme by not removing quantifiers bound to Bottom
ptype :: TyScheme -> TyScheme
ptype = ft . desugar
 where ft :: TyScheme -> TyScheme
       ft x@(TyScheme_Quant (Scheme_Simple _ TyScheme_Bottom) _) = x
       ft (TyScheme_Quant (Scheme_Simple a q) phi)
            = let (TyScheme_Sugar q' p) = sugar q 
                  (Just phi') = app (a, p) phi
              in ft $ desugar (TyScheme_Sugar q' phi')
       ft x = x
       
   
instance Simplify TyScheme where           
    -- | Simplify a type scheme for presentation, dropping trivial quantifications
    --   e.g. (a>=_|_) to a, and expanding types.
    --   simplify :: TyScheme -> TyScheme
    simplify exp = let (v, e) = simpl' empty exp
                   in case appAll e v of
                       Just a  -> a
                       Nothing -> v
      where simpl' env s@(TyScheme_Quant scheme ty) 
              = case scheme of
                 Scheme_Simple a TyScheme_Bottom -> let (next, e2) = simpl' env ty
                                                    in case next of
                                                         TyScheme_Forall x ty' -> case a `elem` ftv ty' of
                                                                                    True  -> (TyScheme_Forall (nub $ a:x) ty', e2)
                                                                                    False -> (TyScheme_Forall x ty', e2)
                                                         z                     -> case a `elem` ftv z of
                                                                                    True  -> (TyScheme_Forall [a] z, e2)
                                                                                    False -> (TyScheme_Forall []  z, e2)
                 Scheme_Simple a bounds          -> let (newb, e1) = simpl' env bounds
                                                        env' = (a, newb): e1
                                                    in  case appAll env' ty of
                                                          Nothing -> (s, env')
                                                          Just x  -> let (next, e3) = simpl' env' x
                                                                         newv = nub $ ftv next
                                                                     in case next of
                                                                          TyScheme_Forall x' ty' -> (TyScheme_Forall (nub $ newv++x') ty', e3)
                                                                          z                      -> (TyScheme_Forall newv z, e3)
            simpl' env s@(TyScheme_Sugar{})   = simpl' env (desugar s)
            simpl' env (TyScheme_SystemF exp) = (TyScheme_SystemF (simplify exp), env)
            simpl' env scheme                 = (scheme, env)
    
    clean = alpha_rename . simplify
    
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
            
    clean = (\(TyScheme_SystemF a)->a) . alpha_rename . simplify . TyScheme_SystemF

-- | Checks if the expression contains an arrow. False will be returned if it does. This is needed for pretty printing in UHC
noArr :: TyExpr -> Bool
noArr (TyExpr_Con a)    = not $ pp a == "->"
noArr (TyExpr_App a b)  = noArr a && noArr b
-- noArr (TyExpr_AppTop a) = noArr a
noArr (TyExpr_Parens a) = noArr a
noArr _                 = True
            
-- | Checks to see if a pattern expression is a variable or something else
isVar :: PatExpr -> Bool
isVar (PatExpr_Var{}) = True
isVar _               = False

mkQuant :: TyExpr -> TyExpr
mkQuant x@(TyExpr_Quant{}) = x
mkQuant x                 = 
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
         True  -> let d1 = trace ("++Var: " ++ pp a)
                      d2 = trace ("++Pre: " ++ pp q)
                      d3 = trace ("++Split: " ++ pp (q `splitOn` a))
                  in d1 $ d2 $ d3 $ error "Cannot occurs check. The split does not return a value" 
         False -> let d1 = trace ("++Var: " ++ pp a)
                      d2 = trace ("++Pre: " ++ pp q ++ "\n++Q0 = " ++ pp q0 ++ "\n++Q1 = " ++ pp q1 ++ "\n++Q2 = " ++ pp q2)
                      d3 = trace ("++Phi: " ++ pp phi ++ "\n++Ftv: " ++ pp (ftv phi) ++ "\n\n")
                  in a `elem` fvs
                  
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
con _                 = "unmapped"