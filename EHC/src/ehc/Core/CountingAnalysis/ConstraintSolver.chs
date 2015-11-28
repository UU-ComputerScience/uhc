%%[(8 counting) hs module{%{EH}Core.CountingAnalysis.ConstraintSolver}

import %%@{%{EH}%%}Core.CountingAnalysis.Types
import %%@{%{EH}%%}Core.CountingAnalysis.ConstraintGeneration
import %%@{%{EH}%%}Base.HsName (HsName)

import UHC.Util.Utils (panic)

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue as Q
import qualified Data.Foldable as F
import Control.Arrow (second)
import Data.Monoid ((<>))
import Data.Maybe (fromJust, fromMaybe, isJust)
import Data.Proxy (Proxy)

type AnnConstraint = C Annotation
type TyConstraint = C AnnotatedType
type SchemeConstraint = C TyScheme
type InstConstraint = (TyScheme, AnnotatedType)
type GenConstraint = (Rho AnnotatedType, Constraints, Env, Rho TyScheme)

type AnnConstraints = BankersDequeue AnnConstraint
type TyConstraints = BankersDequeue TyConstraint
type SchemeConstraints = BankersDequeue SchemeConstraint
type InstConstraints = BankersDequeue InstConstraint
type GenConstraints = BankersDequeue GenConstraint

data Solution = Solution 
  { annSol :: Map Var Annotation
  , tySol :: Map HsName AnnotatedType
  , schemeSol :: Map SchemeVar TyScheme
  }
  deriving (Show)

data PartSolution = PartSolution 
  { annPartSol :: Map Var (Set AnnVal)
  -- , tyPartSol :: Map HsName AnnotatedType
  }
  deriving (Show)

data ConstraintMap = ConstraintMap
  { annCMap :: Map Var Constraints
  , tyCMap :: Map HsName Constraints
  }
  deriving (Show)

data ConstraintsSplit = ConstraintsSplit
  { annConstraints :: AnnConstraints
  , tyConstraints :: TyConstraints
  , schemeConstraints :: SchemeConstraints
  , instConstraints :: InstConstraints
  , genConstraints :: GenConstraints
  }
  deriving (Show)

type WorkList = ConstraintsSplit
data WorkListState = WorkListState
  { workList :: WorkList
  , constraintMap :: ConstraintMap
  , partSolution :: PartSolution
  , solution :: Solution
  , constraints :: Constraints
  }
  deriving (Show)

popAnnConstraint :: ConstraintsSplit -> (Maybe AnnConstraint, ConstraintsSplit)
popAnnConstraint cs@(ConstraintsSplit{annConstraints = as}) = case n of
  Just (c, as') -> (Just c, cs {annConstraints = as'})
  Nothing -> (Nothing, cs)
  where n = Q.popFront as

popTyConstraint :: ConstraintsSplit -> (Maybe TyConstraint, ConstraintsSplit)
popTyConstraint cs@(ConstraintsSplit{tyConstraints = as}) = case n of
  Just (c, as') -> (Just c, cs {tyConstraints = as'})
  Nothing -> (Nothing, cs)
  where n = Q.popFront as

popSchemeConstraint :: ConstraintsSplit -> (Maybe SchemeConstraint, ConstraintsSplit)
popSchemeConstraint cs@(ConstraintsSplit{schemeConstraints = as}) = case n of
  Just (c, as') -> (Just c, cs {schemeConstraints = as'})
  Nothing -> (Nothing, cs)
  where n = Q.popFront as

popInstConstraint :: ConstraintsSplit -> (Maybe InstConstraint, ConstraintsSplit)
popInstConstraint cs@(ConstraintsSplit{instConstraints = as}) = case n of
  Just (c, as') -> (Just c, cs {instConstraints = as'})
  Nothing -> (Nothing, cs)
  where n = Q.popFront as

popGenConstraint :: ConstraintsSplit -> (Maybe GenConstraint, ConstraintsSplit)
popGenConstraint cs@(ConstraintsSplit{genConstraints = as}) = case n of
  Just (c, as') -> (Just c, cs {genConstraints = as'})
  Nothing -> (Nothing, cs)
  where n = Q.popFront as


instance Monoid ConstraintsSplit where
  mempty = ConstraintsSplit Q.empty Q.empty Q.empty Q.empty Q.empty
  ConstraintsSplit a1 t1 s1 i1 g1 `mappend` ConstraintsSplit a2 t2 s2 i2 g2
    = ConstraintsSplit (combine a1 a2) 
        (combine t1 t2) (combine s1 s2) 
        (combine i1 i2) (combine g1 g2)

instance Monoid Solution where
  mempty = Solution M.empty M.empty M.empty
  Solution a1 t1 s1 `mappend` Solution a2 t2 s2 = Solution (a1 <> a2) (t1 <> t2) (s1 <> s2)

instance Monoid PartSolution where
  mempty = PartSolution M.empty
  PartSolution a1 `mappend` PartSolution a2 = PartSolution (a1 <> a2)

instance Monoid ConstraintMap where
  mempty = ConstraintMap M.empty M.empty
  ConstraintMap a1 t1 `mappend` ConstraintMap a2 t2 = ConstraintMap (a1 <> a2) (t1 <> t2)

topAnn :: Set AnnVal
topAnn = annPowWithoutEmpty annTop'

combine :: BankersDequeue a -> BankersDequeue a -> BankersDequeue a
combine = foldr $ flip Q.pushBack

addConstrs :: (a -> ConstraintsSplit -> ConstraintsSplit) -> [a] -> ConstraintsSplit -> ConstraintsSplit
addConstrs  = flip . foldr

addAnn :: AnnConstraint -> ConstraintsSplit -> ConstraintsSplit
addAnn c cs@(ConstraintsSplit{annConstraints = a}) = cs {annConstraints = Q.pushFront a c}

addTy :: TyConstraint -> ConstraintsSplit -> ConstraintsSplit
addTy c cs@(ConstraintsSplit{tyConstraints = a}) = cs {tyConstraints = Q.pushFront a c}

addScheme :: SchemeConstraint -> ConstraintsSplit -> ConstraintsSplit
addScheme c cs@(ConstraintsSplit{schemeConstraints = a}) = cs {schemeConstraints = Q.pushFront a c}

addInst :: InstConstraint -> ConstraintsSplit -> ConstraintsSplit
addInst c cs@(ConstraintsSplit{instConstraints = a}) = cs {instConstraints = Q.pushFront a c}

addGen :: GenConstraint -> ConstraintsSplit -> ConstraintsSplit
addGen c cs@(ConstraintsSplit{genConstraints = a}) = cs {genConstraints = Q.pushFront a c}

splitConstraints :: Constraints -> ConstraintsSplit
splitConstraints [] = mempty
splitConstraints (x:xs) = case x of
  AnnC c -> addAnn c sc
  TyC c -> addTy c sc
  SchemeC c -> addScheme c sc
  InstC t a -> addInst (t,a) sc
  GenC rt cs e rs -> addGen (rt,cs,e,rs) sc
  where sc = splitConstraints xs

toConstraintMap :: Constraints -> ConstraintMap
toConstraintMap cs = ConstraintMap af tf
  where toCMap :: Ord a => (Constraint -> Set a) -> Map a Constraints
        toCMap f = M.fromListWith (++) $ concatMap 
                    (\c -> map (\v -> (v,[c])) $ S.toList $ f c) cs
        af = toCMap fav
        tf = toCMap ftv

solveConstraints :: Var -> Constraints -> Solution
solveConstraints _ _ = mempty

solveAllConstraints :: Var -> WorkListState -> (Var, WorkListState)
solveAllConstraints = undefined

solveAnnConstrs :: WorkListState -> WorkListState
solveAnnConstrs wls@(WorkListState{..}) = 
  case popAnnConstraint workList of
    (Just c, wl') -> solveAnnConstrs $ solveAnnConstr (wls {workList = wl'}) c
    (Nothing, _) -> simplifyAnn wls

solveAnnConstr :: WorkListState -> AnnConstraint -> WorkListState
solveAnnConstr wls@(WorkListState{..}) c 
  = wls {workList = wl', partSolution = ps'}
  where
    -- in every sublist the Var is the Same
    fc :: [[(Var, AnnVal)]]
    fc = map (\(v,sa) -> map (\x -> (v,x)) $ S.toList sa) $ M.toList lav
    -- old values as they appear in the solution
    lav :: Map Var (Set AnnVal)
    lav = lookupVar partSolution $ S.toList $ fav c
    -- every Var appears exactly once in every sublist
    -- this is the cartesian product of fc
    tl :: [[(Var, AnnVal)]]
    tl = sequence fc
    -- new values as dictated
    ch :: Map Var (Set AnnVal)
    ch = M.fromListWith S.union $ map (second S.singleton) $ concat 
          $ filter (\xs -> checkConstr $ subst (undefined :: Proxy Var) xs c) tl
    diffAnnVals = [v | (v, sa) <- M.toList ch, Just sa /= M.lookup v lav]
    listOfNewConstr = concatMap (lookupVar constraintMap) diffAnnVals
    wl' = workList <> splitConstraints listOfNewConstr
    -- ch must be first as it union is left biased and ch should overwrite in ps
    ps' = partSolution { annPartSol = M.union ch (annPartSol partSolution) }


checkConstr :: (Eq a, Compute a) => C a -> Bool
checkConstr (EqC a b) = a == b
checkConstr (PlusC a b c) = simpleCompute computeAdd a b == Just c
checkConstr (UnionC a b c) = simpleCompute computeUnion a b == Just c
checkConstr (TimesC a b c) = simpleCompute computeTimes b a == Just c
checkConstr (ConC a b c) = simpleCompute computeCon b a == Just c
 
simplifyAnn :: WorkListState -> WorkListState
simplifyAnn = removeUseLessConstraints . simplifyAnnEquality . simplifyAnnPartSolution

simplifyAnnPartSolution :: WorkListState -> WorkListState
simplifyAnnPartSolution wls@(WorkListState{..}) = wls 
  { workList = subst (undefined :: Proxy Var) s' workList
  , constraintMap = subst (undefined :: Proxy Var) s' constraintMap
  , partSolution = ps'
  , solution = solution <> s'
  , constraints = subst (undefined :: Proxy Var) s' constraints
  } 
  where (ps', s') = solvePartSolution partSolution

solvePartSolution :: PartSolution -> (PartSolution, Solution)
solvePartSolution (PartSolution am) = (PartSolution amps, mempty {annSol = M.map AnnVal ams})
  where 
    (amps, ams) = sps am

    sps :: Ord a => Map a (Set b) -> (Map a (Set b), Map a b)
    sps m = (M.fromList l1, M.fromList l2)
      where (l1,l2) = sps' $ M.toList m

    sps' :: [(a, Set b)] -> ([(a, Set b)], [(a, b)])
    sps' [] = ([], [])
    sps' (ps@(x, s):xs) 
      | length ss == 1 = (ps' , (x, head ss):s')
      | otherwise = (ps:ps', s')
      where (ps', s') = sps' xs
            ss = S.toList s

simplifyAnnEquality :: WorkListState -> WorkListState
simplifyAnnEquality wls@(WorkListState{..}) = wls 
  { workList = subst (undefined :: Proxy Var) s' workList
  , constraintMap = subst (undefined :: Proxy Var) s' constraintMap
  , partSolution = partSolution {annPartSol = ps'}
  , solution = solution <> mempty {annSol = s'}
  , constraints = subst (undefined :: Proxy Var) s' constraints
  } 
  where (ps', s') = solveEquality (toAnnC constraints) $ annPartSol partSolution
        toAnnC [] = []
        toAnnC (AnnC c:xs) = c : toAnnC xs
        toAnnC (_:xs) = toAnnC xs

solveEquality :: [AnnConstraint]-> Map Var (Set AnnVal) -> (Map Var (Set AnnVal), Map Var Annotation)
solveEquality [] ps = (ps, mempty)
solveEquality (c:cs) ps 
  | M.size lav > 1 = (ps', sol)
  | otherwise = solveEquality cs ps
  where
    sol = toSol $ equalVars ch
    ps' = remSol $ map fst $ M.toList sol
    remSol [] = ps
    remSol (x:xs) = M.delete x $ remSol xs
    -- in every sublist the Var is the Same
    fc :: [[(Var, AnnVal)]]
    fc = map (\(v,sa) -> map (\x -> (v,x)) $ S.toList sa) $ M.toList lav
    -- old values as they appear in the solution
    lav :: Map Var (Set AnnVal)
    lav = M.map (fromMaybe topAnn) $ lookupVar ps $ S.toList $ fav c
    -- every Var appears exactly once in every sublist
    -- this is the cartesian product of fc
    tl :: [[(Var, AnnVal)]]
    tl = sequence fc
    -- new values as dictated
    ch :: [[(Var, AnnVal)]]
    ch = filter (\xs -> checkConstr $ subst (undefined :: Proxy Var) xs c) tl

    toSol :: Map Var (Set Var) -> Map Var Annotation
    toSol = toSol' S.empty . M.toList

    toSol' :: Set Var -> [(Var, Set Var)] -> Map Var Annotation
    toSol' _ [] = M.empty
    toSol' s ((v,sv):xs) 
      | v `S.member` s = toSol' s xs
      | otherwise = M.union (f $ S.toList sv) $ toSol' (S.union s sv) xs
      where f :: [Var] -> Map Var Annotation
            f [] = M.empty
            f (y:ys) = M.insert y (AnnVar v) $ f ys

    equalVars :: [[(Var, AnnVal)]] -> Map Var (Set Var)
    equalVars [] = M.empty
    equalVars (xs:xss) = M.unionWith S.intersection toM $ equalVars xss
      where 
        toM :: Map Var (Set Var)
        toM = toM2 $ map snd $ M.toList $ revMap $ M.fromList xs
        toM2 :: [Set Var] -> Map Var (Set Var)
        toM2 [] = M.empty
        toM2 (y:ys) = M.union (toM2 ys) $ M.fromList $ map (\v -> (v,y)) $ S.toList y

revMap :: (Ord a, Ord b) => Map a b -> Map b (Set a)
revMap = revMap' . M.toList
  where revMap' [] = M.empty
        revMap' ((k,v):xs) = M.insertWith (S.union) v (S.singleton k) $ revMap' xs

-- only call this with empty annWorklist as there they are not removed
-- and with every annvar present inside partSolution
removeUseLessConstraints :: WorkListState -> WorkListState
removeUseLessConstraints wls@(WorkListState{..}) 
  = wls {constraintMap = cm, constraints = cs}
  where cm = ConstraintMap 
              (M.map (filter useFull) $ annCMap constraintMap)
              (M.map (filter useFull) $ tyCMap constraintMap)
        cs = filter useFull constraints
        useFull c = isNotAnnC || sfav > 1 || (sfav /= 0 && b)
          where sfav = S.size fv
                fv = fav c 
                av = head $ S.toList fv
                b = lookupVar partSolution av /= topAnn
                isNotAnnC = case c of
                  AnnC _ -> False
                  _ -> True

solveTyConstrs :: Var -> WorkListState -> (Var, WorkListState)
solveTyConstrs v wls@(WorkListState{..}) = 
  case popTyConstraint workList of
    (Just c, wl') -> uncurry solveTyConstrs
      $ solveTyConstr v (wls {workList = wl'}) c
    (Nothing, _) -> (v, wls)

solveTyConstr :: Var -> WorkListState -> TyConstraint -> (Var, WorkListState)
solveTyConstr v wls@(WorkListState{..}) tc = (v', wls')
  where
    fv = ftv tc
    mfv :: Map HsName (Maybe AnnotatedType)
    mfv = lookupVar solution fv
    mat = M.map fromJust $ M.filter isJust mfv
    -- on the fly solution inlining
    c' = subst (undefined :: Proxy HsName) mat tc
    (v', cs', s') = solveTyConstr' v c'
    diffTys = [v | (v, ty) <- M.toList s', Nothing == M.lookup v mat]
    listOfNewConstr = cs' ++ concatMap (lookupVar constraintMap) diffTys
    wls' = wls {
      solution = solution { tySol = tySol solution <> s' },
      constraints = cs' ++ constraints,
      constraintMap = constraintMap <> toConstraintMap cs',
      workList = workList <> splitConstraints listOfNewConstr
      }

    solveTyConstr' :: Var -> TyConstraint -> (Var, Constraints, Map HsName AnnotatedType)
    solveTyConstr' fresh c = case c of
      EqC (TyVar a) v@(TyVar _) -> (fresh, [], M.singleton a v)
      EqC (TyVar a) f@(TyFunc{}) -> (fresh, [], M.singleton a f)
      EqC (TyVar a) d@(TyData{}) -> (fresh, [], M.singleton a d)
      EqC a v@(TyVar _) -> solveTyConstr' fresh $ EqC v a
      PlusC (TyVar _) (TyVar _) (TyVar _) -> (fresh, [], M.empty) -- unsolvable
      PlusC (TyVar a) (TyVar b) f@(TyFunc{}) -> (fresh, [], M.fromList [(a, f), (b, f)])
      PlusC (TyVar a) (TyVar b) (TyData dn as ts) -> let
        n = length as
        av = [fresh .. fresh + n - 1]
        av1 = map AnnVar av
        av2 = map (AnnVar  . (+n)) av
        cs = map (\(x,y,z) -> AnnC $ PlusC x y z) $ zip3 av1 av2 as
        in (fresh + 2*n, cs, M.fromList [(a, TyData dn av1 ts), (b, TyData dn av2 ts)])
      PlusC v1@(TyVar _) a v2@(TyVar _) -> solveTyConstr' fresh $ PlusC v1 v2 a
      UnionC (TyVar _) (TyVar _) (TyVar _) -> (fresh, [], M.empty) -- unsolvable
      UnionC (TyVar a) (TyVar b) f@(TyFunc{}) -> (fresh, [], M.fromList [(a, f), (b, f)])
      UnionC (TyVar a) (TyVar b) (TyData dn as ts) -> let
        n = length as
        av = [fresh .. fresh + n - 1]
        av1 = map AnnVar av
        av2 = map (AnnVar  . (+n)) av
        cs = map (\(x,y,z) -> AnnC $ UnionC x y z) $ zip3 av1 av2 as
        in (fresh + 2*n, cs, M.fromList [(a, TyData dn av1 ts), (b, TyData dn av2 ts)])
      UnionC v1@(TyVar _) a v2@(TyVar _) -> solveTyConstr' fresh $ UnionC v1 v2 a
      TimesC (TyVar _) an (TyVar _) -> (fresh, [], M.empty) -- unsolvable
      TimesC (TyVar a) an f@(TyFunc{}) -> (fresh, [], M.singleton a f)
      TimesC (TyVar a) an (TyData dn as ts) -> let
        n = length as
        av = [fresh .. fresh + n - 1]
        av1 = map AnnVar av
        av2 = map (AnnVar  . (+n)) av
        cs = map (\(x,y,z) -> AnnC $ TimesC x y z) $ zip3 av1 av2 as
        in (fresh + 2*n, cs, M.singleton a (TyData dn av1 ts))
      ConC (TyVar _) an (TyVar _) -> (fresh, [], M.empty) -- unsolvable
      ConC (TyVar a) an f@(TyFunc{}) -> (fresh, [], M.singleton a f)
      ConC (TyVar a) an (TyData dn as ts) -> let
        n = length as
        av = [fresh .. fresh + n - 1]
        av1 = map AnnVar av
        av2 = map (AnnVar  . (+n)) av
        cs = map (\(x,y,z) -> AnnC $ ConC x y z) $ zip3 av1 av2 as
        in (fresh + 2*n, cs, M.singleton a (TyData dn av1 ts))
      _ -> (fresh, [], M.empty) -- unsolvable panic $ "solveTyConstr': " ++ show c

solveGenConstr :: Var -> WorkListState -> GenConstraint -> (Var, WorkListState)
solveGenConstr v wls c = case c of
  (Rho (Eta rt a1) b1, cs, e, Rho (Eta (SchemeVar sv) a2) b2) -> let
    wlsc2 = wls {
      workList = splitConstraints cs,
      constraintMap = toConstraintMap cs,
      constraints = cs
      }
    (v', wlsc2') = solveAllConstraints v wlsc2
    cs' = constraints wlsc2
    -- subst necessary for ftv calculation
    rt' = subst (undefined :: Proxy HsName) (solution wlsc2) rt
    fv = (ftv cs' `S.union` ftv rt') S.\\ (ftv e {-`S.union` S.fromList [a1,b1,a2,b2]-})
    wls' = wls {
      solution = solution wlsc2' <> mempty {schemeSol = M.singleton sv $ SForAll fv cs' rt'},
      partSolution = partSolution wlsc2
      }
    in (v', wls')
  _ -> panic $ " solveGenConstr: " ++ show c

solveInstConstr :: Var -> WorkListState -> InstConstraint -> (Var, WorkListState)
solveInstConstr fresh wls@(WorkListState{..}) c = case c of
  (SForAll vs cs t, TyVar v) -> let
    fv = [fresh .. fresh + S.size vs - 1]
    lom = M.fromList $ zip (S.toList vs) fv
    cs' = subst lom cs
    wls' = wls { 
      solution = solution <> M.singleton v (subst lom t),
      constraints = cs' ++ constraints,
      constraintMap = constraintMap <> toConstraintMap cs',
      workList = workList <> splitConstraints cs'
      }
    in (fresh + S.size vs, wls')
  _ -> panic $ " solveInstConstr: " ++ show c

solveSchemeConstr :: Var -> WorkListState -> SchemeConstraint -> (Var, WorkListState)
solveSchemeConstr fresh wls@(WorkListState{..}) c = case c of
  EqC (SForAll 

class FreeAnnVars a where
  fav :: a -> Set Var

instance FreeAnnVars Annotation where
  fav (AnnVar v) = S.singleton v
  fav (AnnVal _) = S.empty

instance FreeAnnVars AnnotatedType where
  fav (TyVar _) = S.empty
  fav (TyData _ as ts) = S.union (fav as) (fav ts)
  fav (TyFunc rt et) = S.union (fav rt) (fav et)
  fav (TyRow ts) = fav ts
  fav (TyError _) = S.empty

instance FreeAnnVars a => FreeAnnVars (Eta a) where
  fav (Eta a an) = S.union (fav a) (fav an)

instance FreeAnnVars a => FreeAnnVars (Rho a) where
  fav (Rho a an) = S.union (fav a) (fav an)

instance FreeAnnVars TyScheme where
  fav (SchemeVar _) = S.empty
  fav (SForAll _ cs t) = S.union (fav cs) (fav t)

instance FreeAnnVars Constraint where
  fav (AnnC c) = fav c
  fav (TyC c) = fav c
  fav (SchemeC c) = fav c
  fav (InstC s t) = S.union (fav s) (fav t)
  fav (GenC t cs e s) = S.unions [fav t, fav cs, fav e, fav s] --todo maybe env

instance FreeAnnVars a => FreeAnnVars (C a) where
  fav (EqC a b) = S.union (fav a) (fav b)
  fav (PlusC a b c) = S.unions [fav a, fav b, fav c]
  fav (UnionC a b c) = S.unions [fav a, fav b, fav c]
  fav (TimesC a b c) = S.unions [fav a, fav b, fav c]
  fav (ConC a b c) = S.unions [fav a, fav b, fav c]

instance FreeAnnVars a => FreeAnnVars [a] where
  fav = S.unions . map fav

instance FreeAnnVars (Map HsName (Rho TyScheme)) where
  -- fav = fav . map snd . M.toList
  fav _ = S.empty

class FreeTyVars a where
  ftv :: a -> Set HsName

instance FreeTyVars AnnotatedType where
  ftv (TyVar v) = S.singleton v
  ftv (TyData _ _ ts) = ftv ts
  ftv (TyFunc rt et) = S.union (ftv rt) (ftv et)
  ftv (TyRow ts) = ftv ts
  ftv (TyError _) = S.empty

instance FreeTyVars a => FreeTyVars (Eta a) where
  ftv (Eta a _) = ftv a

instance FreeTyVars a => FreeTyVars (Rho a) where
  ftv (Rho a _) = ftv a

instance FreeTyVars TyScheme where
  ftv (SchemeVar v) = S.empty
  ftv (SForAll tvs _ t) = ftv t S.\\ tvs

instance FreeTyVars Constraint where
  ftv (AnnC c) = S.empty
  ftv (TyC c) = ftv c
  ftv (SchemeC c) = ftv c
  ftv (InstC s t) = S.union (ftv s) (ftv t)
  ftv (GenC t cs e s) = S.unions [ftv t, ftv cs, ftv e, ftv s]

instance FreeTyVars a => FreeTyVars (C a) where
  ftv (EqC a b) = S.union (ftv a) (ftv b)
  ftv (PlusC a b c) = S.unions [ftv a, ftv b, ftv c]
  ftv (UnionC a b c) = S.unions [ftv a, ftv b, ftv c]
  ftv (TimesC a _ c) = S.unions [ftv a, ftv c]
  ftv (ConC a _ c) = S.unions [ftv a, ftv c]

instance FreeTyVars a => FreeTyVars [a] where
  ftv = S.unions . map ftv

instance FreeTyVars (Map HsName (Rho TyScheme)) where
  -- ftv = ftv . map snd . M.toList
  ftv _ = S.empty

class LookupVar a b c where
  lookupVar :: b -> a -> c

instance LookupVar Var PartSolution (Set AnnVal) where
  lookupVar (PartSolution{annPartSol = s}) = fromMaybe topAnn . lookupVar s

instance LookupVar Var ConstraintMap Constraints where
  lookupVar (ConstraintMap{annCMap = m}) = fromMaybe [] . lookupVar m

instance LookupVar HsName ConstraintMap Constraints where
  lookupVar (ConstraintMap{tyCMap = m}) = fromMaybe [] . lookupVar m

instance (Ord a, LookupVar a b c) => LookupVar [a] b (Map a c) where
  lookupVar e = M.fromList . map (\v -> (v, lookupVar e v))

instance (Ord a, LookupVar a b c) => LookupVar (Set a) b (Map a c) where
  lookupVar = flip $ flip lookupVar . S.toList

instance Eq a => LookupVar a (a,b) (Maybe b) where
  lookupVar (v,a) v' | v == v' = Just a
                     | otherwise = Nothing

instance (Ord a, Eq a) => LookupVar a (Map a b) (Maybe b) where
  lookupVar = flip M.lookup

instance Eq a => LookupVar a [(a,b)] (Maybe b) where
  lookupVar = flip lookup

instance Eq a => LookupVar a [(a,b)] b where
  lookupVar xs = fromJust . lookupVar xs

-- annToAnnVal :: Maybe Annotation -> Maybe AnnVal
-- annToAnnVal m = case m of
--   Just (AnnVal a) -> Just a
--   _ -> Nothing

instance LookupVar Var (Map Var Annotation) (Maybe AnnVal) where
  lookupVar m v = case lookupVar m v of
    Just (AnnVal a) -> Just a
    _ -> Nothing

instance LookupVar Var Solution (Maybe Annotation) where
  lookupVar (Solution am _ _) = lookupVar am

instance LookupVar Var Solution (Maybe AnnVal) where
  lookupVar (Solution am _ _) = lookupVar am

instance LookupVar HsName Solution (Maybe AnnotatedType) where
  lookupVar (Solution _ tm _) = lookupVar tm

-- instance LookupVar v m (Maybe Annotation) => LookupVar v m (Maybe AnnVal) where
--   lookupVar m v = case lookupVar m v of
--     Just (AnnVal a) -> Just a
--     _ -> Nothing

class SubstVar t a b where
  subst :: Proxy t -> a -> b -> b
  subst _ = flip const

instance LookupVar Var m (Maybe AnnVal) => SubstVar Var m Annotation where
  subst _ m v@(AnnVar v') = case lookupVar m v' of
    Just a -> AnnVal a
    _ -> v

instance SubstVar HsName m Annotation where
instance SubstVar SchemeVar m Annotation where

instance LookupVar Var m (Maybe AnnVal) => SubstVar Var m AnnotatedType where
  subst _ s c = case c of
    TyVar _ -> c
    TyData n as ts -> TyData n (subst (undefined :: Proxy Var) s as) (subst (undefined :: Proxy Var) s ts)
    TyFunc rt et -> TyFunc (subst (undefined :: Proxy Var) s rt) (subst (undefined :: Proxy Var) s et)
    TyRow ts -> TyRow $ map (subst (undefined :: Proxy Var) s) ts
    TyError _ -> c

instance LookupVar HsName m (Maybe AnnotatedType) => SubstVar HsName m AnnotatedType where
  subst _ s c = case c of
    TyVar v -> fromMaybe c $ lookupVar s v 
    TyData n as ts -> TyData n as (subst (undefined :: Proxy HsName) s ts)
    TyFunc rt et -> TyFunc (subst (undefined :: Proxy HsName) s rt) (subst (undefined :: Proxy HsName) s et)
    TyRow ts -> TyRow $ map (subst (undefined :: Proxy HsName) s) ts
    TyError _ -> c

instance SubstVar SchemeVar m AnnotatedType where

instance (SubstVar Var m Annotation, SubstVar Var m AnnotatedType) => SubstVar Var m TyScheme where
  subst _ s c = case c of
    SchemeVar _ -> c
    SForAll ns cs t -> SForAll ns (subst (undefined :: Proxy Var) s cs) (subst (undefined :: Proxy Var) s t)

instance (SubstVar HsName m Annotation, SubstVar HsName m AnnotatedType) => SubstVar HsName m TyScheme where
  subst _ s c = case c of
    SchemeVar _ -> c
    SForAll ns cs t -> SForAll ns (subst (undefined :: Proxy HsName) s cs) (subst (undefined :: Proxy HsName) s t)


instance LookupVar SchemeVar m (Maybe TyScheme) => SubstVar SchemeVar m TyScheme where
  subst _ s c = case c of
    SchemeVar v -> fromMaybe c $ lookupVar s v 
    SForAll ns cs t -> SForAll ns (subst (undefined :: Proxy SchemeVar) s cs) (subst (undefined :: Proxy SchemeVar) s t)


instance (SubstVar v m Annotation, SubstVar v m a) => SubstVar v m (Eta a) where
  subst _ s (Eta x a) = Eta (subst (undefined :: Proxy v) s x) (subst (undefined :: Proxy v) s a)

instance (SubstVar v m Annotation, SubstVar v m a) => SubstVar v m (Rho a) where
  subst _ s (Rho x a) = Rho (subst (undefined :: Proxy v) s x) (subst (undefined :: Proxy v) s a)

instance (SubstVar v m Annotation, SubstVar v m AnnotatedType
    , SubstVar v m TyScheme)
    => SubstVar v m Constraint where
  subst _ s c = case c of
    AnnC c -> AnnC (subst (undefined :: Proxy v) s c)
    TyC c -> TyC (subst (undefined :: Proxy v) s c)
    SchemeC c -> SchemeC (subst (undefined :: Proxy v) s c)
    InstC ts t-> InstC (subst (undefined :: Proxy v) s ts) (subst (undefined :: Proxy v) s t)
    GenC rt cs e rs -> GenC (subst (undefined :: Proxy v) s rt) (subst (undefined :: Proxy v) s cs) (subst (undefined :: Proxy v) s e) (subst (undefined :: Proxy v) s rs)

instance (SubstVar v a Annotation, SubstVar v a b) => SubstVar v a (C b) where
  subst _ s c = case c of
    EqC a b -> EqC (subst (undefined :: Proxy v) s a) (subst (undefined :: Proxy v) s b)
    PlusC a b c -> PlusC (subst (undefined :: Proxy v) s a) (subst (undefined :: Proxy v) s b) (subst (undefined :: Proxy v) s c)
    UnionC a b c -> UnionC (subst (undefined :: Proxy v) s a) (subst (undefined :: Proxy v) s b) (subst (undefined :: Proxy v) s c)
    TimesC a b c -> TimesC (subst (undefined :: Proxy v) s a) (subst (undefined :: Proxy v) s b) (subst (undefined :: Proxy v) s c)
    ConC a b c -> ConC (subst (undefined :: Proxy v) s a) (subst (undefined :: Proxy v) s b) (subst (undefined :: Proxy v) s c)

-- instance SubstVar a (Rho TyScheme) => SubstVar a (Map HsName (Rho TyScheme)) where
--   subst _ = M.map . subst

-- subtstMap :: SubstVar a b => a -> [b] -> [b]
-- subtstMap = map . subst

-- listSubst _ :: SubstVar a b => [a] -> b -> b
-- listSubst _ = flip $ foldr subst

instance SubstVar v a b => SubstVar v a [b] where
  subst _ = map . subst (undefined :: Proxy v)

-- instance SubstVar a b => SubstVar [a] b where
  -- subst _ = flip $ foldr subst

-- instance SubstVar (Map Var Annotation) m => SubstVar Solution m where
--   subst _ (Solution am _) = subst _ am

-- substSolution :: SubstVar (Map Var Annotation) m => Solution -> m -> m
-- substSolution (Solution am _) = subst _ am

instance (SubstVar v m (Map Var Constraints), SubstVar v m (Map HsName Constraints))
    => SubstVar v m ConstraintMap where
  subst _ m (ConstraintMap am tm) = ConstraintMap (subst (undefined :: Proxy v) m am) (subst (undefined :: Proxy v) m tm)

instance (SubstVar v m AnnConstraints, SubstVar v m TyConstraints, SubstVar v m SchemeConstraints
    , SubstVar v m InstConstraints, SubstVar v m GenConstraints) 
    => SubstVar v m ConstraintsSplit where
  subst _ m (ConstraintsSplit a t c i g) = ConstraintsSplit 
    (subst (undefined :: Proxy v) m a) (subst (undefined :: Proxy v) m t) (subst (undefined :: Proxy v) m c) (subst (undefined :: Proxy v) m i) (subst (undefined :: Proxy v) m g)

instance SubstVar v m c => SubstVar v m (Map a c) where
  subst _ = M.map . subst (undefined :: Proxy v)

instance SubstVar v m c => SubstVar v m (BankersDequeue c) where
  subst _ = fmap . subst (undefined :: Proxy v)

instance (SubstVar v m a, SubstVar v m b) => SubstVar v m (a,b) where
  subst _ m (a,b) = (subst (undefined :: Proxy v) m a, subst (undefined :: Proxy v) m b)

instance (SubstVar v m a, SubstVar v m b, SubstVar v m c, SubstVar v m d) => SubstVar v m (a,b,c,d) where
  subst _ m (a,b,c,d) = (subst (undefined :: Proxy v) m a, subst (undefined :: Proxy v) m b, subst (undefined :: Proxy v) m c, subst (undefined :: Proxy v) m d)

%%]
