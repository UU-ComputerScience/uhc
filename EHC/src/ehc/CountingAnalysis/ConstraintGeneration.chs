%%[(8 counting) hs module{%{EH}CountingAnalysis.ConstraintGeneration}

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Control.Monad.State
import Data.Monoid
import UHC.Util.Utils
import Data.List (transpose)

import %%@{%{EH}%%}CountingAnalysis
import %%@{%{EH}%%}Base.HsName (HsName, mkHNm)

annPlus :: AnnVal -> AnnVal -> AnnVal
annPlus a1 a2 = S.fromList [x .+ y | x <- S.toList a1, y <- S.toList a2] 

annUnion :: AnnVal -> AnnVal -> AnnVal
annUnion = S.union

annTimes :: AnnVal -> AnnVal -> AnnVal
annTimes a1 a2 = S.fromList [annFromInt (sum $ map annToInt y) | x <- S.toList a1, y <- f (annToInt x) $ S.toList a2]
  where f 0 _ = [[]] 
        f _ [] = []
        f n y@(x:xs) = f n xs ++ map (x:) (f (n-1) y)

annCond :: AnnVal -> AnnVal -> AnnVal
annCond a1 a2 = S.unions $ map (\x -> if x == AnnPrim_Zero then annZero' else a2) $ S.toList a1

(.+) :: AnnPrim -> AnnPrim -> AnnPrim
x .+ y = annFromInt $ annToInt x + annToInt y

annToInt :: AnnPrim -> Int
annToInt AnnPrim_Zero = 0
annToInt AnnPrim_One = 1
annToInt AnnPrim_Infinity = 2;

annFromInt :: Int -> AnnPrim
annFromInt 0 = AnnPrim_Zero
annFromInt 1 = AnnPrim_One
annFromInt _ = AnnPrim_Infinity

class ConstraintGeneration a where
  genEq :: a -> a -> Constraints
  genPlus :: a -> a -> a -> Constraints
  genUnion :: a -> a -> a -> Constraints
  genTimes :: Annotation -> a -> a -> Constraints
  genCond :: Annotation -> a -> a -> Constraints
  genSub :: a -> a -> Constraints
  genSub a1 a2 = genUnion a1 a2 a2
  genSup :: a -> a -> Constraints
  genSup = flip genSub

instance ConstraintGeneration Annotation where
  genEq a1 a2 | a1 == a2 = []
              | otherwise = [Constraint_Eq $ ConstraintEq_Ann a1 a2]
  genPlus a1 a2 = (:[]) . Constraint_Ann . ConstraintAnn_Plus a1 a2
  genUnion a1 a2 = (:[]) . Constraint_Ann . ConstraintAnn_Union a1 a2
  genTimes a1 a2 = (:[]) . Constraint_Ann . ConstraintAnn_Times a1 a2
  genCond a1 a2 = (:[]) . Constraint_Ann . ConstraintAnn_Cond a1 a2

instance ConstraintGeneration Type where
  genEq t1 t2 | t1 == t2 = []
              | otherwise = [Constraint_Eq $ ConstraintEq_Type t1 t2]
  genPlus t1 t2 t = genEq t t1 <> genEq t t2
  genUnion t1 t2 t = genEq t t1 <> genEq t t2
  genTimes _ t2 t = genEq t t2
  genCond _ t2 t = genEq t t2
  
instance ConstraintGeneration Scheme where
  genEq s1 s2 | s1 == s2 = []
              | otherwise = [Constraint_Eq $ ConstraintEq_Scheme s1 s2]
  genPlus s1 s2 s = genEq s s1 <> genEq s s2
  genUnion s1 s2 s = genEq s s1 <> genEq s s2
  genTimes _ s2 s = genEq s s2
  genCond _ s2 s = genEq s s2
  
instance ConstraintGeneration a => ConstraintGeneration (GEta a) where
    genEq e1 e2 = genEq (getType e1) (getType e2) <> genEq (getUsage e1) (getUsage e2)
    genPlus e1 e2 e = genPlus (getType e1) (getType e2) (getType e) <> genPlus (getUsage e1) (getUsage e2) (getUsage e)
    genUnion e1 e2 e = genUnion (getType e1) (getType e2) (getType e) <> genUnion (getUsage e1) (getUsage e2) (getUsage e)
    genTimes a e2 e = genTimes a (getType e2) (getType e) <> genTimes a (getUsage e2) (getUsage e)
    genCond a e2 e = genCond a (getType e2) (getType e) <> genCond a (getUsage e2) (getUsage e)

instance ConstraintGeneration a => ConstraintGeneration (GRho a) where
    genEq e1 e2 = genEq (getType e1) (getType e2) 
      <> genEq (getUsage e1) (getUsage e2) 
      <> genEq (getDemand e1) (getDemand e2)
    genPlus e1 e2 e = genPlus (getType e1) (getType e2) (getType e) 
      <> genPlus (getUsage e1) (getUsage e2) (getUsage e) 
      <> genPlus (getDemand e1) (getDemand e2) (getDemand e)
    genUnion e1 e2 e = genUnion (getType e1) (getType e2) (getType e)
      <> genUnion (getUsage e1) (getUsage e2) (getUsage e) 
      <> genUnion (getDemand e1) (getDemand e2) (getDemand e)
    genTimes a e2 e = genTimes a (getType e2) (getType e) 
      <> genTimes a (getUsage e2) (getUsage e) 
      <> genTimes a (getDemand e2) (getDemand e)
    genCond a e2 e = genCond a (getType e2) (getType e) 
      <> genCond a (getUsage e2) (getUsage e) 
      <> genCond a (getDemand e2) (getDemand e)

instance ConstraintGeneration EtaType where
  genEq a b = genEq (toGEta a) (toGEta b)
  genPlus a b c = genPlus (toGEta a) (toGEta b) (toGEta c)
  genUnion a b c = genUnion (toGEta a) (toGEta b) (toGEta c)
  genTimes a b c = genTimes a (toGEta b) (toGEta c)
  genCond a b c = genCond a (toGEta b) (toGEta c)

instance ConstraintGeneration RhoType where
  genEq a b = genEq (toGRho a) (toGRho b)
  genPlus a b c = genPlus (toGRho a) (toGRho b) (toGRho c)
  genUnion a b c = genUnion (toGRho a) (toGRho b) (toGRho c)
  genTimes a b c = genTimes a (toGRho b) (toGRho c)
  genCond a b c = genCond a (toGRho b) (toGRho c)

instance ConstraintGeneration EtaScheme where
  genEq a b = genEq (toGEta a) (toGEta b)
  genPlus a b c = genPlus (toGEta a) (toGEta b) (toGEta c)
  genUnion a b c = genUnion (toGEta a) (toGEta b) (toGEta c)
  genTimes a b c = genTimes a (toGEta b) (toGEta c)
  genCond a b c = genCond a (toGEta b) (toGEta c)

instance ConstraintGeneration RhoScheme where
  genEq a b = genEq (toGRho a) (toGRho b)
  genPlus a b c = genPlus (toGRho a) (toGRho b) (toGRho c)
  genUnion a b c = genUnion (toGRho a) (toGRho b) (toGRho c)
  genTimes a b c = genTimes a (toGRho b) (toGRho c)
  genCond a b c = genCond a (toGRho b) (toGRho c)

instance ConstraintGeneration a => ConstraintGeneration [a] where
  genEq l1 l2 = mconcat $ zipWith genEq l1 l2
  genPlus l1 l2 l3 = mconcat $ zipWith (uncurry genPlus) (zip l1 l2) l3
  genUnion l1 l2 l3 = mconcat $ zipWith (uncurry genUnion) (zip l1 l2) l3
  genTimes a l2 l3 = mconcat $ zipWith (genTimes a) l2 l3
  genCond a l2 l3 = mconcat $ zipWith (genCond a) l2 l3

type EnvState = (Var, Var -> HsName)  

genEqEnv :: EnvState -> Env -> Env -> (Var, Constraints)
genEqEnv s e1 e2 = let (c, (v,_)) = runState (genEqEnv' e1 e2) s in (v,c) 

genPlusEnv :: EnvState -> Env -> Env -> Env -> (Var, Constraints)
genPlusEnv s e1 e2 e = let (c, (v,_)) = runState (genPlusEnv' e1 e2 e) s in (v,c) 

genUnionEnv :: EnvState -> Env -> Env -> Env -> (Var, Constraints)
genUnionEnv s e1 e2 e = let (c, (v,_)) = runState (genUnionEnv' e1 e2 e) s in (v,c) 

genTimesEnv :: EnvState -> Annotation -> Env -> Env -> (Var, Constraints)
genTimesEnv s a e2 e = let (c, (v,_)) = runState (genTimesEnv' a e2 e) s in (v,c) 

genCondEnv :: EnvState -> Annotation -> Env -> Env -> (Var, Constraints)
genCondEnv s a e2 e = let (c, (v,_)) = runState (genCondEnv' a e2 e) s in (v,c) 

genEnv3 :: (GRho Scheme -> GRho Scheme -> GRho Scheme -> Constraints) -> Env -> Env -> Env -> State EnvState Constraints
genEnv3 f env1 env2 env = do 
  y <- mapM (\x -> do
    e1 <- envLookup' env1 Nothing x
    e2 <- envLookup' env2 Nothing x
    e <- envLookup' env Nothing x
    return $ f (toGRho e1) (toGRho e2) (toGRho e)) xs
  return $ mconcat y
  where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2 ++ M.keys env 

genEnv2 :: (Annotation -> GRho Scheme -> GRho Scheme -> Constraints) -> Annotation -> Env -> Env -> State EnvState Constraints
genEnv2 f phi1 env2 env = do 
  y <- mapM (\x -> do
    e2 <- envLookup' env2 Nothing x
    e <- envLookup' env Nothing x
    return $ genTimes phi1 (toGRho e2) (toGRho e)) xs
  return $ mconcat y
  where xs = S.toList $ S.fromList $ M.keys env2 ++ M.keys env

genEqEnv' :: Env -> Env -> State EnvState Constraints
genEqEnv' env1 env2 = do 
  y <- mapM (\x -> do
    e1 <- envLookup' env1 Nothing x
    e2 <- envLookup' env2 Nothing x
    return $ genEq (toGRho e1) (toGRho e2)) xs
  return $ mconcat y
  where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2

genPlusEnv' :: Env -> Env -> Env -> State EnvState Constraints
genPlusEnv' = genEnv3 genPlus

genUnionEnv' :: Env -> Env -> Env -> State EnvState Constraints
genUnionEnv' = genEnv3 genUnion

genTimesEnv' :: Annotation -> Env -> Env -> State EnvState Constraints
genTimesEnv' = genEnv2 genTimes

genCondEnv' :: Annotation -> Env -> Env -> State EnvState Constraints
genCondEnv' = genEnv2 genCond

envLookup' :: Env -> Maybe Scheme -> HsName -> State EnvState RhoScheme
envLookup' env ms v = case M.lookup v env of
  Just x -> return x
  Nothing -> do 
    y <- case ms of 
      Nothing -> fmap Scheme_Var getFresh
      Just s -> return s
    return $ RhoScheme_Rho (EtaScheme_Eta y annZero) annZero

envLookupFresh :: EnvState -> Env -> HsName -> (RhoScheme, Var)
envLookupFresh s e h = let (r, (v,_)) = runState (envLookupFresh' e h) s in (r, v)

envLookupFreshList :: EnvState -> Env -> [HsName] -> ([RhoScheme], Var)
envLookupFreshList s e hs = let (r, (v,_)) = runState (mapM (envLookupFresh' e) hs) s in (r, v)

envLookupFresh' :: Env -> HsName -> State EnvState RhoScheme
envLookupFresh' env v = case M.lookup v env of
  Just x -> return x
  Nothing -> do 
    y <- getFresh
    v <- getFresh
    d <- getFresh
    return $ RhoScheme_Rho (EtaScheme_Eta
      (Scheme_Var y) (Annotation_Var v)) 
      (Annotation_Var d)

getFresh :: State EnvState HsName
getFresh = do
  (v, f) <- get
  put (v + 1, f)
  return $ f v
  
computePlus :: Compute a => EnvState -> a -> a -> ((a, Constraints), Var)
computePlus s a1 a2 = let (c, (v,_)) = runState (computePlus' a1 a2) s in (c, v)

computeUnion :: Compute a => EnvState -> a -> a -> ((a, Constraints), Var)
computeUnion s a1 a2 = let (c, (v,_)) = runState (computeUnion' a1 a2) s in (c, v)

computeTimes :: Compute a => EnvState -> Annotation -> a -> ((a, Constraints), Var)
computeTimes s a1 a2 = let (c, (v,_)) = runState (computeTimes' a1 a2) s in (c, v)

computeCond :: Compute a => EnvState -> Annotation -> a -> ((a, Constraints), Var)
computeCond s a1 a2 = let (c, (v,_)) = runState (computeCond' a1 a2) s in (c, v)

simpleCompute :: (EnvState -> a -> b -> ((Constraints, b), Var)) -> a -> b -> Maybe b
simpleCompute f a b 
  | c == mempty && v == 0 = Just r
  | otherwise = Nothing
  where ((c,r),v) = f (0, const (mkHNm "")) a b

class Compute a where
  computePlus' :: a -> a -> State EnvState (a, Constraints)
  computeUnion' :: a -> a -> State EnvState (a, Constraints)
  computeTimes' :: Annotation -> a -> State EnvState (a, Constraints)
  computeCond' :: Annotation -> a -> State EnvState (a, Constraints)

computeFresh :: (HsName -> a) -> (a -> a -> a -> Constraints) -> a -> a -> State EnvState (a, Constraints)
computeFresh g f a1 a2 = do
  a' <- getFresh
  let a = g a'
  let c = f a1 a2 a
  return (a, c)
  
computeAnnotation = computeFresh Annotation_Var

computeEq :: ConstraintGeneration a =>  a -> a -> State EnvState (a, Constraints)
computeEq a1 a2 = return (a1, genEq a1 a2)

instance Compute a => Compute [a] where
  computePlus' xs ys = do 
    zs <- zipWithM computePlus' xs ys
    let (as, cs) = unzip zs
    return (as, mconcat cs)
  computeUnion' xs ys = do 
    zs <- zipWithM computeUnion' xs ys
    let (as, cs) = unzip zs
    return (as, mconcat cs)
  computeTimes' a ys = do 
    zs <- mapM (computeTimes' a) ys
    let (as, cs) = unzip zs
    return (as, mconcat cs)
  computeCond' a ys = do 
    zs <- mapM (computeCond' a) ys
    let (as, cs) = unzip zs
    return (as, mconcat cs)

instance Compute Annotation where
  computePlus' a b 
    | a == annZero = return (b, mempty)
    | b == annZero = return (a, mempty)
    | otherwise = computeAnnotation genPlus a b
  computeUnion' = computeAnnotation genUnion
  computeTimes' a b
    | a == annOne = return (b, mempty)
    | b == annOne = return (a, mempty)
    | otherwise = computeAnnotation genTimes a b
  computeCond' = computeAnnotation genCond

instance Compute Type where
  computePlus' = computeEq
  computeUnion' = computeEq
  computeTimes' _ = return . (,mempty)
  computeCond' _ = return . (,mempty)

instance Compute Scheme where
  computePlus' = computeEq
  computeUnion' = computeEq
  computeTimes' _ = return . (,mempty)
  computeCond' _ = return . (,mempty)
  
instance Compute a => Compute (GEta a) where
  computePlus' (GEta u1 v1) (GEta u2 v2) = do
    (u, c1) <- computePlus' u1 u2
    (v, c2) <- computePlus' v1 v2
    return (GEta u v, c1 <> c2)
  computeUnion' (GEta u1 v1) (GEta u2 v2) = do
    (u, c1) <- computeUnion' u1 u2
    (v, c2) <- computeUnion' v1 v2
    return (GEta u v, c1 <> c2)
  computeTimes' phi1 (GEta u2 v2) = do
    (u, c1) <- computeTimes' phi1 u2
    (v, c2) <- computeTimes' phi1 v2
    return (GEta u v, c1 <> c2)
  computeCond' phi1 (GEta u2 v2) = do
    (u, c1) <- computeCond' phi1 u2
    (v, c2) <- computeCond' phi1 v2
    return (GEta u v, c1 <> c2)

instance Compute a => Compute (GRho a) where
  computePlus' (GRho u1 v1) (GRho u2 v2) = do
    (u, c1) <- computePlus' u1 u2
    (v, c2) <- computePlus' v1 v2
    return (GRho u v, c1 <> c2)
  computeUnion' (GRho u1 v1) (GRho u2 v2) = do
    (u, c1) <- computeUnion' u1 u2
    (v, c2) <- computeUnion' v1 v2
    return (GRho u v, c1 <> c2)
  computeTimes' phi1 (GRho u2 v2) = do
    (u, c1) <- computeTimes' phi1 u2
    (v, c2) <- computeTimes' phi1 v2
    return (GRho u v, c1 <> c2)
  computeCond' phi1 (GRho u2 v2) = do
    (u, c1) <- computeCond' phi1 u2
    (v, c2) <- computeCond' phi1 v2
    return (GRho u v, c1 <> c2)

instance Compute Env where
  computePlus' env1 env2 = do
    y <- mapM (\x -> do
      e1 <- envLookup' env1 (getType <$> M.lookup x env2) x
      e2 <- envLookup' env2 (getType <$> M.lookup x env1) x
      (e, c) <- computePlus' (toGRho e1) (toGRho e2)
      return ((x, fromGRho e), c)) xs
    let (e, c) = unzip y
    return (M.fromList e, mconcat c)
    where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2
  computeUnion' env1 env2 = do
    y <- mapM (\x -> do
      e1 <- envLookup' env1 (getType <$> M.lookup x env2) x
      e2 <- envLookup' env2 (getType <$> M.lookup x env1) x
      (e, c) <- computeUnion' (toGRho e1) (toGRho e2)
      return ((x, fromGRho e), c)) xs
    let (e, c) = unzip y
    return (M.fromList e, mconcat c)
    where xs = S.toList $ S.fromList $ M.keys env1 ++ M.keys env2
  computeTimes' a env2 = do
    y <- mapM (\x -> do
      e2 <- envLookup' env2 Nothing x
      (e, c) <- computeTimes' a (toGRho e2)
      return ((x, fromGRho e), c)) xs
    let (e, c) = unzip y
    return (M.fromList e, mconcat c)
    where xs = M.keys env2
  computeCond' a env2 = do
    y <- mapM (\x -> do
      e2 <- envLookup' env2 Nothing x
      (e, c) <- computeCond' a (toGRho e2)
      return ((x, fromGRho e), c)) xs
    let (e, c) = unzip y
    return (M.fromList e, mconcat c)
    where xs = M.keys env2

computeCondList :: Compute a => EnvState -> [Annotation] -> [a] -> (([a], Constraints), Var)
computeCondList s xs ys = let (c, (v,_)) = runState (computeCondList' xs ys) s in (c, v)

computeCondList' :: Compute a => [Annotation] -> [a] -> State EnvState ([a], Constraints)
computeCondList' xs ys = do 
  zs <- zipWithM computeCond' xs ys
  let (as, cs) = unzip zs
  return (as, mconcat cs)
  
-- TODO controleren of de juiste deltai wordt gebruikt
computeCondMatrix :: Compute a => EnvState -> [Annotation] -> [[a]] -> (([[a]], Constraints), Var)
computeCondMatrix s xs ys = let (c, (v,_)) = runState (computeCondMatrix' xs ys) s in (c, v)

computeCondMatrix' :: Compute a => [Annotation] -> [[a]] -> State EnvState ([[a]], Constraints)
computeCondMatrix' xs ys = do 
  zs <- mapM (computeCondList' xs) ys
  let (as, cs) = unzip zs
  return (as, mconcat cs)

bigPlus :: Compute a => EnvState -> [a] -> ((a, Constraints), Var)
bigPlus s xs = let (c, (v,_)) = runState (bigPlus' xs) s in (c, v)

bigPlus' :: Compute a => [a] -> State EnvState (a, Constraints)
bigPlus' [] = panic "Bigplus' on empty list"
bigPlus' [x] = return (x, mempty)
bigPlus' (x:xs) = do
  (y1, c1) <- bigPlus' xs
  (y2, c2) <- computePlus' x y1
  return (y2, c1 <> c2)
  
bigPlusMatrix :: Compute a => EnvState -> [[a]] -> (([a], Constraints), Var)
bigPlusMatrix s xs = let (c, (v,_)) = runState (bigPlusMatrix' $ transpose xs) s in (c, v)

bigPlusMatrix' :: Compute a => [[a]] -> State EnvState ([a], Constraints)
bigPlusMatrix' xs = do
  zs <- mapM bigPlus' xs
  let (as, cs) = unzip zs
  return (as, mconcat cs)

bigUnion :: Compute a => EnvState -> [a] -> ((a, Constraints), Var)
bigUnion s xs = let (c, (v,_)) = runState (bigUnion' xs) s in (c, v)

bigUnion' :: Compute a => [a] -> State EnvState (a, Constraints)
bigUnion' [] = panic "bigUnion' on empty list"
bigUnion' [x] = return (x, mempty)
bigUnion' (x:xs) = do
  (y1, c1) <- bigUnion' xs
  (y2, c2) <- computeUnion' x y1
  return (y2, c1 <> c2)

sub :: EnvState -> (Annotation -> Annotation -> Constraints) -> EtaType -> ((EtaType, Constraints), Var)
sub s f e = let (c, (v,_)) = runState (sub' f e) s in (c, v)

sub' :: (Annotation -> Annotation -> Constraints) -> EtaType -> State EnvState (EtaType, Constraints)
sub' f (EtaType_Eta tau nu) = do
  anu' <- getFresh
  let nu' = Annotation_Var anu'
  return (EtaType_Eta tau nu', f nu' nu) 
  
envDeleteList :: Env -> HsNames -> Env
envDeleteList = foldr M.delete

envFilterList :: Map HsName a -> Set HsName -> Map HsName a
envFilterList r hs = M.filterWithKey (\k _ -> k `S.member` hs) r


%%]
