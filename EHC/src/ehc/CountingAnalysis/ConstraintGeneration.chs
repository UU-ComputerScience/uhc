%%[(8 counting) hs module{%{EH}CountingAnalysis.ConstraintGeneration}

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Monoid (mconcat, (<>))

import Control.Monad.State
import Control.Arrow (second)

import %%@{%{EH}%%}CountingAnalysis
import %%@{%{EH}%%}Base.HsName (HsName, mkHNm)

genEq :: ConstraintGeneration a => Var -> a -> a -> (GatherConstraints, Var)
genEq freshVars a1 a2 = runState (genEq' a1 a2) freshVars
genAdd :: ConstraintGeneration a => Var -> a -> a -> a -> (GatherConstraints, Var)
genAdd freshVars a1 a2 a = runState (genAdd' a1 a2 a) freshVars
genUnion :: ConstraintGeneration a => Var -> a -> a -> a -> (GatherConstraints, Var)
genUnion freshVars a1 a2 a = runState (genUnion' a1 a2 a) freshVars
genTimes :: ConstraintGeneration a => Var -> Annotation -> a -> a -> (GatherConstraints, Var)
genTimes freshVars a1 a2 a = runState (genTimes' a1 a2 a) freshVars
genCon :: ConstraintGeneration a => Var -> Annotation -> a -> a -> (GatherConstraints, Var)
genCon freshVars a1 a2 a = runState (genCon' a1 a2 a) freshVars
genSub :: ConstraintGeneration a => Var -> a -> a -> (GatherConstraints, Var)
genSub freshVars a1 a2 = runState (genUnion' a1 a2 a2) freshVars
genSup :: ConstraintGeneration a => Var -> a -> a -> (GatherConstraints, Var)
genSup freshVars = flip $ genSub freshVars

genSub' :: ConstraintGeneration a => a -> a -> State Var GatherConstraints
genSub' a1 a2 = genUnion' a1 a2 a2
genSup' :: ConstraintGeneration a => a -> a -> State Var GatherConstraints
genSup' = flip genSub'


class ConstraintGeneration a where
  genC :: C a -> Constraint
  genC = undefined
  genEq' :: a -> a -> State Var GatherConstraints
  genEq' a1 a2 = return $ singleton $ genC $ EqC a1 a2
  genAdd' :: a -> a -> a -> State Var GatherConstraints
  genAdd' a1 a2 a = return $ singleton $ genC $ PlusC a a1 a2
  genUnion' :: a -> a -> a -> State Var GatherConstraints
  genUnion' a1 a2 a = return $ singleton $ genC $ UnionC a a1 a2
  genTimes' :: Annotation -> a -> a -> State Var GatherConstraints
  genTimes' a1 a2 a = return $ singleton $ genC $ TimesC a a1 a2
  genCon' :: Annotation -> a -> a -> State Var GatherConstraints
  genCon' a1 a2 a = return $ singleton $ genC $ ConC a a1 a2

instance ConstraintGeneration a => ConstraintGeneration [a] where
  genEq' l1 l2 = do 
    xss <- mapM (uncurry genEq') $  zip l1 l2
    return $ mconcat xss
  genAdd' l1 l2 l3 = do 
    xss <- mapM (uncurry $ uncurry genAdd') $ zip (zip l1 l2) l3
    return $ mconcat xss
  genUnion' l1 l2 l3 = do 
    xss <- mapM (uncurry $ uncurry genUnion') $ zip (zip l1 l2) l3
    return $ mconcat xss
  genTimes' a l2 l3 = do 
    xss <- mapM (uncurry $ genTimes' a) $ zip l2 l3
    return $ mconcat xss
  genCon' a l2 l3 = do 
    xss <- mapM (uncurry $ genCon' a) $ zip l2 l3
    return $ mconcat xss

instance ConstraintGeneration Annotation where
  genC = Constraint_AnnC . fromGC

instance ConstraintGeneration AnnotatedType where
  genC = Constraint_TyC . fromGC

instance ConstraintGeneration TyScheme where
  genC = Constraint_SchemeC . fromGC

instance ConstraintGeneration a => ConstraintGeneration (Eta a) where
  genEq' (Eta u1 v1) (Eta u2 v2) = do
    c1 <- genEq' u1 u2
    c2 <- genEq' v1 v2
    return $ c1 <> c2
  genAdd' (Eta u1 v1) (Eta u2 v2) (Eta u v) = do
    c1 <- genAdd' u1 u2 u
    c2 <- genAdd' v1 v2 v
    return $ c1 <> c2
  genUnion' (Eta u1 v1) (Eta u2 v2) (Eta u v) = do
    c1 <- genUnion' u1 u2 u
    c2 <- genUnion' v1 v2 v
    return $ c1 <> c2
  genTimes' phi1 (Eta u2 v2) (Eta u v) = do
    c1 <- genTimes' phi1 u2 u
    c2 <- genTimes' phi1 v2 v
    return $ c1 <> c2
  genCon' phi1 (Eta u2 v2) (Eta u v) = do
    c1 <- genCon' phi1 u2 u
    c2 <- genCon' phi1 v2 v
    return $ c1 <> c2

instance ConstraintGeneration a => ConstraintGeneration (Rho a) where
  genEq' (Rho u1 v1) (Rho u2 v2) = do
    c1 <- genEq' u1 u2
    c2 <- genEq' v1 v2
    return $ c1 <> c2
  genAdd' (Rho u1 v1) (Rho u2 v2) (Rho u v) = do
    c1 <- genAdd' u1 u2 u
    c2 <- genAdd' v1 v2 v
    return $ c1 <> c2
  genUnion' (Rho u1 v1) (Rho u2 v2) (Rho u v) = do
    c1 <- genUnion' u1 u2 u
    c2 <- genUnion' v1 v2 v
    return $ c1 <> c2
  genTimes' phi1 (Rho u2 v2) (Rho u v) = do
    c1 <- genTimes' phi1 u2 u
    c2 <- genTimes' phi1 v2 v
    return $ c1 <> c2
  genCon' phi1 (Rho u2 v2) (Rho u v) = do
    c1 <- genCon' phi1 u2 u
    c2 <- genCon' phi1 v2 v
    return $ c1 <> c2

instance ConstraintGeneration EtaAnnotatedType where
  genEq' a b = genEq' (toEta a) (toEta b)
  genAdd' a b c = genAdd' (toEta a) (toEta b) (toEta c)
  genUnion' a b c = genUnion' (toEta a) (toEta b) (toEta c)
  genTimes' a b c = genTimes' a (toEta b) (toEta c)
  genCon' a b c = genCon' a (toEta b) (toEta c)

instance ConstraintGeneration RhoAnnotatedType where
  genEq' a b = genEq' (toRho a) (toRho b)
  genAdd' a b c = genAdd' (toRho a) (toRho b) (toRho c)
  genUnion' a b c = genUnion' (toRho a) (toRho b) (toRho c)
  genTimes' a b c = genTimes' a (toRho b) (toRho c)
  genCon' a b c = genCon' a (toRho b) (toRho c)

instance ConstraintGeneration EtaTyScheme where
  genEq' a b = genEq' (toEta a) (toEta b)
  genAdd' a b c = genAdd' (toEta a) (toEta b) (toEta c)
  genUnion' a b c = genUnion' (toEta a) (toEta b) (toEta c)
  genTimes' a b c = genTimes' a (toEta b) (toEta c)
  genCon' a b c = genCon' a (toEta b) (toEta c)

instance ConstraintGeneration RhoTyScheme where
  genEq' a b = genEq' (toRho a) (toRho b)
  genAdd' a b c = genAdd' (toRho a) (toRho b) (toRho c)
  genUnion' a b c = genUnion' (toRho a) (toRho b) (toRho c)
  genTimes' a b c = genTimes' a (toRho b) (toRho c)
  genCon' a b c = genCon' a (toRho b) (toRho c)

instance ConstraintGeneration Env where
  genEq' env1 env2 = do 
    y <- mapM (\x -> do
      e1 <- envLookup' env1 x
      e2 <- envLookup' env2 x
      genEq' e1 e2) xs
    return $ mconcat y
    where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2
  genAdd' env1 env2 env = do 
    y <- mapM (\x -> do
      e1 <- envLookup' env1 x
      e2 <- envLookup' env2 x
      e <- envLookup' env x
      genAdd' e1 e2 e) xs
    return $ mconcat y
    where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2 ++ M.keys env
  genUnion' env1 env2 env = do 
    y <- mapM (\x -> do
      e1 <- envLookup' env1 x
      e2 <- envLookup' env2 x
      e <- envLookup' env x
      genUnion' e1 e2 e) xs
    return $ mconcat y
    where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2 ++ M.keys env
  genTimes' phi1 env2 env = do 
    y <- mapM (\x -> do
      e2 <- envLookup' env2 x
      e <- envLookup' env x
      genTimes' phi1 e2 e) xs
    return $ mconcat y
    where xs = S.toList $ S.fromList $ M.keys env2 ++ M.keys env
  genCon' phi1 env2 env = do 
    y <- mapM (\x -> do
      e2 <- envLookup' env2 x
      e <- envLookup' env x
      genCon' phi1 e2 e) xs
    return $ mconcat y
    where xs = S.toList $ S.fromList $ M.keys env2 ++ M.keys env

computeAdd :: Compute a => Var -> a -> a -> ((GatherConstraints, a), Var)
computeAdd freshVars a1 a2 = runState (computeAdd' a1 a2) freshVars

computeUnion :: Compute a => Var -> a -> a -> ((GatherConstraints, a), Var)
computeUnion freshVars a1 a2 = runState (computeUnion' a1 a2) freshVars

computeTimes :: Compute a => Var -> Annotation -> a -> ((GatherConstraints, a), Var)
computeTimes freshVars a1 a2 = runState (computeTimes' a1 a2) freshVars

computeCon :: Compute a => Var -> Annotation -> a -> ((GatherConstraints, a), Var)
computeCon freshVars a1 a2 = runState (computeCon' a1 a2) freshVars

simpleCompute :: (Var -> a -> b -> ((GatherConstraints, b), Var)) -> a -> b -> Maybe b
simpleCompute f a b 
  | c == mempty && v == 0 = Just r
  | otherwise = Nothing
  where ((c,r),v) = f 0 a b

class Compute a where
  computeAdd' :: a -> a -> State Var (GatherConstraints, a)
  computeUnion' :: a -> a -> State Var (GatherConstraints, a)
  computeTimes' :: Annotation -> a -> State Var (GatherConstraints, a)
  computeCon' :: Annotation -> a -> State Var (GatherConstraints, a)

-- computeAnn :: (Annotation -> Annotation -> Annotation -> 
--     State Var GatherConstraints, Annotation -> Annotation -> Annotation)
--   -> Annotation -> Annotation -> State Var (GatherConstraints, Annotation)
computeAnn (_, f) (Annotation_AnnVal x) (Annotation_AnnVal y) = return (mempty, Annotation_AnnVal $ f x y)
computeAnn (f, _) x y = do
  z <- getFresh
  let a = Annotation_AnnVar z
  c <- f x y a
  return (c, a)

-- computeTy :: ([Annotation] -> [Annotation] -> State Var (GatherConstraints, [Annotation]), 
--     [AnnotatedType] -> [AnnotatedType] -> State Var (GatherConstraints, [AnnotatedType])) 
--   -> AnnotatedType -> AnnotatedType -> State Var (GatherConstraints, AnnotatedType)
computeTy ((f, g), _, _) (AnnotatedType_TyData n1 a1 t1) (AnnotatedType_TyData n2 a2 t2) | n1 == n2 = do 
  (c1, a) <- f a1 a2
  (c2, t) <- g t1 t2
  return (c1 <> c2, AnnotatedType_TyData n1 a t)
computeTy (_, (f, g), _) (AnnotatedType_TyFunc r1 e1) (AnnotatedType_TyFunc r2 e2) = do 
  (c1, r) <- f r1 r2
  (c2, e) <- g e1 e2
  return (c1 <> c2, AnnotatedType_TyFunc r e)
computeTy (_, _, f) t1 t2 = do
  x <- getFresh
  let t = AnnotatedType_TyVar $ mkHNm $ "CA" ++ show x
  c <- f t1 t2 t
  return (c, t)

-- computeTyAnn
--   :: ((Annotation -> [Annotation] -> State Var (GatherConstraints, [Annotation])
--       , Annotation -> [AnnotatedType] -> State Var (GatherConstraints, [AnnotatedType]))
--      , (Annotation -> Rho AnnotatedType -> State Var (GatherConstraints, Rho AnnotatedType)
--        , Annotation -> Eta AnnotatedType -> State Var (GatherConstraints, Eta AnnotatedType))
--      , Annotation -> AnnotatedType -> AnnotatedType -> State Var GatherConstraints)
--   -> Annotation 
--   -> AnnotatedType
--   -> State Var (GatherConstraints, AnnotatedType)
computeTyAnn ((f, g), _, _) a (AnnotatedType_TyData n as ts) = do
  (c1, ax) <- f a as
  (c2, t) <- g a ts
  return (c1 <> c2, AnnotatedType_TyData n ax t)
computeTyAnn (_, (f, g), _) a (AnnotatedType_TyFunc r2 e2) = do 
  (c1, r) <- f a r2
  (c2, e) <- g a e2
  return (c1 <> c2, AnnotatedType_TyFunc r e)
computeTyAnn (_, _, f) a t2 = do
  x <- getFresh
  let t = AnnotatedType_TyVar $ mkHNm $ "CA" ++ show x
  c <- f a t2 t
  return (c, t)

computeScheme sv@(TyScheme_SchemeVar _) s2 =
  return (singleton $ Constraint_SchemeC $ CTyScheme_EqC sv s2, sv)
computeScheme s1 sv@(TyScheme_SchemeVar _) = computeScheme sv s1
computeScheme x y = error $ show ("unsolvable computeScheme:",x,y)

computeSchemeAnn v@(TyScheme_SchemeVar _) = return (mempty, v)
computeSchemeAnn s = error $ show ("TyScheme is no var:", s)

instance Compute a => Compute [a] where
  computeAdd' xs ys = do 
    z <- mapM (uncurry computeAdd') $ zip xs ys
    let (c, l) = unzip z
    return (mconcat c, l)
  computeUnion' xs ys = do 
    z <- mapM (uncurry computeUnion') $ zip xs ys
    let (c, l) = unzip z
    return (mconcat c, l)
  computeTimes' a xs = do 
    z <- mapM (computeTimes' a) xs
    let (c, l) = unzip z
    return (mconcat c, l)
  computeCon' a xs = do 
    z <- mapM (computeCon' a) xs
    let (c, l) = unzip z
    return (mconcat c, l)

instance Compute Annotation where
  computeAdd' = computeAnn (genAdd', annAdd)
  computeUnion' = computeAnn (genUnion', annUnion)
  computeTimes' = computeAnn (genTimes', annTimes)
  computeCon' = computeAnn (genCon', annCon)

instance Compute AnnotatedType where
  computeAdd' = computeTy ((computeAdd', computeAdd'), (computeAdd', computeAdd'), genAdd')
  computeUnion' = computeTy ((computeUnion', computeUnion'), (computeUnion', computeUnion'), genUnion')
  computeTimes' = computeTyAnn ((computeTimes', computeTimes'), (computeTimes', computeTimes'), genTimes')
  computeCon' = computeTyAnn ((computeCon', computeCon'), (computeCon', computeCon'), genCon')

instance Compute TyScheme where
  computeAdd' = computeScheme
  computeUnion' = computeScheme
  computeTimes' _ = computeSchemeAnn
  computeCon' _ = computeSchemeAnn

instance Compute a => Compute (Eta a) where
  computeAdd' (Eta u1 v1) (Eta u2 v2) = do
    (c1, u) <- computeAdd' u1 u2
    (c2, v) <- computeAdd' v1 v2
    return (c1 <> c2, Eta u v)
  computeUnion' (Eta u1 v1) (Eta u2 v2) = do
    (c1, u) <- computeUnion' u1 u2
    (c2, v) <- computeUnion' v1 v2
    return (c1 <> c2, Eta u v)
  computeTimes' phi1 (Eta u2 v2) = do
    (c1, u) <- computeTimes' phi1 u2
    (c2, v) <- computeTimes' phi1 v2
    return (c1 <> c2, Eta u v)
  computeCon' phi1 (Eta u2 v2) = do
    (c1, u) <- computeCon' phi1 u2
    (c2, v) <- computeCon' phi1 v2
    return (c1 <> c2, Eta u v)

instance Compute a => Compute (Rho a) where
  computeAdd' (Rho u1 v1) (Rho u2 v2) = do
    (c1, u) <- computeAdd' u1 u2
    (c2, v) <- computeAdd' v1 v2
    return (c1 <> c2, Rho u v)
  computeUnion' (Rho u1 v1) (Rho u2 v2) = do
    (c1, u) <- computeUnion' u1 u2
    (c2, v) <- computeUnion' v1 v2
    return (c1 <> c2, Rho u v)
  computeTimes' phi1 (Rho u2 v2) = do
    (c1, u) <- computeTimes' phi1 u2
    (c2, v) <- computeTimes' phi1 v2
    return (c1 <> c2, Rho u v)
  computeCon' phi1 (Rho u2 v2) = do
    (c1, u) <- computeCon' phi1 u2
    (c2, v) <- computeCon' phi1 v2
    return (c1 <> c2, Rho u v)

instance Compute EtaAnnotatedType where
  computeAdd' a b = do
    (x, y) <- computeAdd' (toEta a) (toEta b)
    return (x, fromEta y)
  computeUnion' a b = do
    (x, y) <- computeUnion' (toEta a) (toEta b)
    return (x, fromEta y)
  computeTimes' a b = do
    (x, y) <- computeTimes' a (toEta b)
    return (x, fromEta y)
  computeCon' a b = do
    (x, y) <- computeCon' a (toEta b)
    return (x, fromEta y)

instance Compute RhoAnnotatedType where
  computeAdd' a b = do
    (x, y) <- computeAdd' (toRho a) (toRho b)
    return (x, fromRho y)
  computeUnion' a b = do
    (x, y) <- computeUnion' (toRho a) (toRho b)
    return (x, fromRho y)
  computeTimes' a b = do
    (x, y) <- computeTimes' a (toRho b)
    return (x, fromRho y)
  computeCon' a b = do
    (x, y) <- computeCon' a (toRho b)
    return (x, fromRho y)

instance Compute EtaTyScheme where
  computeAdd' a b = do
    (x, y) <- computeAdd' (toEta a) (toEta b)
    return (x, fromEta y)
  computeUnion' a b = do
    (x, y) <- computeUnion' (toEta a) (toEta b)
    return (x, fromEta y)
  computeTimes' a b = do
    (x, y) <- computeTimes' a (toEta b)
    return (x, fromEta y)
  computeCon' a b = do
    (x, y) <- computeCon' a (toEta b)
    return (x, fromEta y)

instance Compute RhoTyScheme where
  computeAdd' a b = do
    (x, y) <- computeAdd' (toRho a) (toRho b)
    return (x, fromRho y)
  computeUnion' a b = do
    (x, y) <- computeUnion' (toRho a) (toRho b)
    return (x, fromRho y)
  computeTimes' a b = do
    (x, y) <- computeTimes' a (toRho b)
    return (x, fromRho y)
  computeCon' a b = do
    (x, y) <- computeCon' a (toRho b)
    return (x, fromRho y)

instance Compute Env where
  computeAdd' env1 env2 = do
    y <- mapM (\x -> do
      e1 <- envLookup' env1 x
      e2 <- envLookup' env2 x
      (c, e) <- computeAdd' e1 e2
      return (c, (x, e))) xs
    let (c, e) = unzip y
    return (mconcat c, M.fromList e)
    where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2
  computeUnion' env1 env2 = do
    y <- mapM (\x -> do
      e1 <- envLookup' env1 x
      e2 <- envLookup' env2 x
      (c, e) <- computeUnion' e1 e2
      return (c, (x, e))) xs
    let (c, e) = unzip y
    return (mconcat c, M.fromList e)
    where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2
  computeTimes' a env2 = do
    y <- mapM (\x -> do
      e2 <- envLookup' env2 x
      (c, e) <- computeTimes' a e2
      return (c, (x, e))) xs
    let (c, e) = unzip y
    return (mconcat c, M.fromList e)
    where xs = M.keys env2
  computeCon' a env2 = do
    y <- mapM (\x -> do
      e2 <- envLookup' env2 x
      (c, e) <- computeCon' a e2
      return (c, (x, e))) xs
    let (c, e) = unzip y
    return (mconcat c, M.fromList e)
    where xs = M.keys env2

-- envLookup :: Var -> Env -> HsName -> (RhoTyScheme, Var)
-- envLookup v e h = runState (envLookup' e h) v

-- version used when combining environments
envLookup' :: Env -> HsName -> State Var (RhoTyScheme)
envLookup' env v = case M.lookup v env of
  Just x -> return x
  Nothing -> do 
    y <- getFresh
    return $ RhoTyScheme_Rho (EtaTyScheme_Eta 
      (TyScheme_SchemeVar $ SV y) 
      annZero) annZero

-- envLookup' :: Env -> HsName -> State Var (RhoTyScheme)
-- envLookup' env v = case M.lookup v env of
--   Just x -> return x
--   Nothing -> do 
--     y <- getFresh
--     return $ RhoTyScheme_Rho (EtaTyScheme_Eta 
--       (TyScheme_SForAll (S.singleton $ mkHNm $ "CA" ++ show y) 
--         S.empty [] (AnnotatedType_TyVar $ mkHNm $ "CA" ++ show y)) 
--       annZero) annZero

-- version used when a certain variable needs to be constrained
envLookupFresh :: Var -> Env -> HsName -> (RhoTyScheme, Var)
envLookupFresh v e h = runState (envLookupFresh' e h) v

envLookupFreshList :: Var -> Env -> [HsName] -> ([RhoTyScheme], Var)
envLookupFreshList v e hs = runState (mapM (envLookupFresh' e) hs) v

envLookupFresh' :: Env -> HsName -> State Var (RhoTyScheme)
envLookupFresh' env v = case M.lookup v env of
  Just x -> return x
  Nothing -> do 
    y <- getFresh
    v <- getFresh
    d <- getFresh

    return $ RhoTyScheme_Rho (EtaTyScheme_Eta
      (TyScheme_SchemeVar $ SV y) (Annotation_AnnVar v)) 
      (Annotation_AnnVar d)

getFresh :: State Var Var
getFresh = do
  x <- get
  put $ x + 1
  return x

sub :: Var -> Bool -> EtaAnnotatedType -> ((GatherConstraints, EtaAnnotatedType), Var)
sub v b t = runState (sub' b t) v

sub' :: Bool -> EtaAnnotatedType -> State Var (GatherConstraints, EtaAnnotatedType)
sub' True (EtaAnnotatedType_Eta t n2) = do
  x <- getFresh
  let n1 = Annotation_AnnVar x
  c <- genSub' n1 n2
  return (c, EtaAnnotatedType_Eta t n1)
sub' False t = return (mempty, t)

%%]
