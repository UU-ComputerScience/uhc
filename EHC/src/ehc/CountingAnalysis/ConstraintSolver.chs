%%[0 hs
{-# LANGUAGE TemplateHaskell #-}  
{-# LANGUAGE DoAndIfThenElse #-}  
%%]

%%[(8 counting) hs module{%{EH}CountingAnalysis.ConstraintSolver}

import %%@{%{EH}%%}Base.HsName (HsName, mkHNm)
import %%@{%{EH}%%}CountingAnalysis.ConstraintGeneration hiding (getFresh)
import %%@{%{EH}%%}CountingAnalysis.Pretty
import %%@{%{EH}%%}CountingAnalysis
import qualified %%@{%{EH}%%}CountingAnalysis.Substitution as S
import qualified %%@{%{EH}%%}CountingAnalysis.ExtractVar as E
import UHC.Util.Utils (panic)
import UHC.Util.Pretty

import %%@{%{EH}%%}Base.Target (FFIWay)
import %%@{%{EH}%%}Foreign (ForeignEnt)
import %%@{%{EH}%%}Ty (Ty)

import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Set (Set)
import qualified Data.Set as S
-- import Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue as Qq
import Control.Monad.State
import Control.Lens
import Data.Monoid
import Data.Maybe
import Control.Arrow (second)
import Data.Function (on)
import Data.List (foldl')
import Data.Foldable (toList)

%%[[
import System.IO.Unsafe
import Debug.Trace
%%][100
%%]]

traceShowS = flip const
traceShowT = flip const
genTrace2 = flip const
-- genTrace2 = traceShow
traceShow2def = flip const
-- traceShow2def x y = traceShow (x,y) y

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Double ended queue
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 counting) hs

-- type Queue a = (Set a, Qq.BankersDequeue a)

-- pushBack :: Ord a => Queue a -> a -> Queue a
-- pushBack q@(s,xs) x 
--   | x `S.member` s = q
--   | otherwise = (S.insert x s, Qq.pushBack xs x)

-- pushFront :: Ord a => Queue a -> a -> Queue a
-- pushFront q@(s,xs) x
--   | x `S.member` s = q
--   | otherwise = (S.insert x s, Qq.pushFront xs x )

-- popFront :: Ord a => Queue a -> Maybe (a, Queue a)
-- popFront (s,q) = case Qq.popFront q of
--   Nothing -> Nothing
--   Just (x, q') -> Just (x, (S.delete x s, q'))

-- emptyQueue :: Ord a => Queue a
-- emptyQueue = (S.empty, Qq.empty)

-- nullQueue :: Ord a => Queue a -> Bool
-- nullQueue (_, q) = Qq.null q

-- fromListQueue :: Ord a => [a] -> Queue a
-- fromListQueue xs = (S.fromList xs, Qq.fromList xs)

-- toListQueue :: Ord a => Queue a -> [a]
-- toListQueue (_,xs) = seq ("toListQueu", length xs') xs'
--   where xs' = toList xs

-- addToQueue :: Ord a => [a] -> Queue a -> Queue a
-- addToQueue xs q = foldl' pushFront q xs

-- addToQueueBack :: Ord a => [a] -> Queue a -> Queue a
-- addToQueueBack xs q = foldl' pushBack q xs

type Queue a = (Set a, [a])

-- pushBack :: Ord a => Queue a -> a -> Queue a
-- pushBack q@(s,xs) x 
--   | x `S.member` s = q
--   | otherwise = (S.insert x s, xs ++ [x])

pushFront :: Ord a => Queue a -> a -> Queue a
pushFront q@(s,xs) x
  | x `S.member` s = q
  | otherwise = (S.insert x s, x:xs )

popFront :: Ord a => Queue a -> Maybe (a, Queue a)
popFront (_,[]) = Nothing
popFront (s, x:xs) = Just (x, (S.delete x s, xs))

emptyQueue :: Ord a => Queue a
emptyQueue = (S.empty, [])

nullQueue :: Ord a => Queue a -> Bool
nullQueue (_, q) = null q

fromListQueue :: Ord a => [a] -> Queue a
fromListQueue xs = (S.fromList xs, xs)

addToQueue :: Ord a => [a] -> Queue a -> Queue a
addToQueue [] q = q
addToQueue xs (s,q) = traceShowS ("atq", length xs) (S.union s $ S.fromList xs', xs' ++ q)
  where xs' = filter (`S.notMember` s) xs

addToQueueBack :: Ord a => [a] -> Queue a -> Queue a
-- addToQueueBack xs (s,q) = (S.union s $ S.fromList xs', q ++ xs')
--   where xs' = filter (`S.notMember` s) xs
addToQueueBack = addToQueue

toListQueue :: Ord a => Queue a -> [a]
toListQueue (_,xs) = xs

makeDeepSeqQueue :: Show a => Queue a -> Queue a
makeDeepSeqQueue q@(s,xs) = seq (length (show s) + length (show xs)) q

-- type Queue a = [a]

-- pushBack :: Ord a => Queue a -> a -> Queue a
-- pushBack xs x = xs ++ [x]

-- pushFront :: Ord a => Queue a -> a -> Queue a
-- pushFront xs x = x:xs

-- popFront :: Ord a => Queue a -> Maybe (a, Queue a)
-- popFront [] = Nothing
-- popFront (x:xs) = Just (x, xs)

-- emptyQueue :: Ord a => Queue a
-- emptyQueue = []

-- nullQueue :: Ord a => Queue a -> Bool
-- nullQueue = null

-- fromListQueue :: Ord a => [a] -> Queue a
-- fromListQueue = id

-- addToQueue :: Ord a => [a] -> Queue a -> Queue a
-- addToQueue xs q = xs ++ q

-- addToQueueBack :: Ord a => [a] -> Queue a -> Queue a
-- addToQueueBack xs q = q ++ xs

-- toListQueue :: Ord a => Queue a -> [a]
-- toListQueue = id

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 counting) hs export(solveDef, emptySolveState, SolveState(..), solution, partSolution, freshVar, toHsName)
data SolveState = SolveState
  { _solution :: !Solution
  , _partSolution :: !PartSolution
  , _constraintMap :: !(Map Var (Set Var))
  , _allConstraints :: !(Map Var Constraint)
  , _workListConstraints :: !(Map HsName (Set Var))
  , _currentWorkList :: !(Queue Var)
  , _changedVars :: !(Set HsName)
  , _freshVar :: !Var
  , _toHsName :: !(Var -> HsName)
  , _dontDefault :: !(Set HsName)
  }

instance Show SolveState where
  show (SolveState s ps cm ac wkc cw cv fv _ dd) = "SolveState: " ++ show (s,ps,cm,ac,wkc,cw,cv,fv,dd)
  
emptySolveState :: SolveState
emptySolveState = SolveState
  { _solution = emptySolution
  , _partSolution = M.empty
  , _constraintMap = M.empty
  , _allConstraints = M.empty
  , _workListConstraints = M.empty
  , _currentWorkList = emptyQueue
  , _changedVars = S.empty
  , _freshVar = 0
  , _toHsName = \x -> error "empty toHsName"
  , _dontDefault = S.empty
  }
  
makeLenses ''Solution
makeLenses ''SolveState

getFresh :: SolveT Var
getFresh = do
  v <- use freshVar
  freshVar .= succ v
  return v

getFresh' :: SolveT HsName
getFresh' = do
  v <- getFresh
  f <- use toHsName
  return $ f v

isTypeConstraint :: Constraint -> Bool
isTypeConstraint (Constraint_Ann _) = False
isTypeConstraint (Constraint_Eq ConstraintEq_Ann{}) = False
isTypeConstraint _ = True

class AddNewConstraint a where
  addNewConstraint :: a -> SolveT ()

instance AddNewConstraint ConstraintEq where
  addNewConstraint = addNewConstraint . Constraint_Eq

instance AddNewConstraint ConstraintAnn where
  addNewConstraint = addNewConstraint . Constraint_Ann

instance AddNewConstraint Constraint where
  addNewConstraint x = addNewConstraint [x]

instance AddNewConstraint GatherConstraints where
  addNewConstraint = addNewConstraint . toConstraints

instance AddNewConstraint Constraints where
  addNewConstraint [] = return ()
  addNewConstraint cs = do
    fvs <- traceShow ("anc-cs", length cs, f cs) $ replicateM (length cs) getFresh
    let zipped = traceShowS ("zipStart", cs, fvs) $ zip fvs $ sortConstraints cs
    acs <- use $ traceShowS ("zipp", zipped) allConstraints
    let nacs = M.union (traceShowS zipped $ M.fromList zipped) acs
    allConstraints .= traceRun "anc-cs-acs" (makeDeepSeqAcs nacs)
    cwl <- use currentWorkList
    let ncwl = addToQueue (map fst zipped) cwl
    currentWorkList .= traceRun "anc-cwl-acs" (makeDeepSeqQueue ncwl)
    mapM_ updateWorkListConstraint zipped

f [Constraint_Eq c@(ConstraintEq_Type t1 t2)] = show (ca t1, ca t2, ca2 t1, ca2 t2, ca3 t1, ca3 t2, c)
f _ = ""
ca (Type_App t1 t2) = 1 + ca t2
ca _ = 0
ca2 (Type_Data _ _ ts) = 1 + sum (map ca2 ts)
ca2 _ = 0
ca3 (Type_Data _ _ ts) = 1 + length ts
ca3 _ = 0


splitTypeApp :: Type -> [Type]
splitTypeApp t = seq (length xs) xs
  where xs = splitTypeApp' t

splitTypeApp' :: Type -> [Type]
splitTypeApp' (Type_App t1 t2) = t1 : splitTypeApp t2
splitTypeApp' _ = []

makeDeepSeq :: Map a b -> Map a b
makeDeepSeq x = seq (M.size x) x

makeDeepSeq2 :: Show a => a -> a
makeDeepSeq2 x = seq (length $ show x) x

makeDeepSeqPs :: PartSolution -> PartSolution
-- makeDeepSeqPs = id
makeDeepSeqPs ps = seq (M.size ps) ps
-- makeDeepSeqPs ps = seq (M.size ps + (sum $ map (length . S.toList) $ M.elems ps)) ps

makeDeepSeqAcs :: Map Var Constraint -> Map Var Constraint
makeDeepSeqAcs x = seq (M.size x) x

makeDeepSeqSol s@(Solution a b c) = seq (M.size a + M.size b + M.size c) s
makeDeepSeqList xs = seq (length xs) xs
makeDeepSeqSet s = seq (S.size s) s
-- makeDeepSeqQueue s = seq (length $ toListQueue s) s

updateWorkListConstraint :: (Var, Constraint) -> SolveT ()
updateWorkListConstraint (v,c) | test c = do
  let (as,ts,ss) = E.extractVars c
      hsns = S.toList $ S.unions [as,ts,ss]
  wlcs <- use workListConstraints
  let nwlcs = foldl' (\m1 x -> M.insertWith S.union x (S.singleton v) m1) wlcs hsns
  workListConstraints .= traceRun "uwlc-wlcs" (makeDeepSeq nwlcs)
  where test (Constraint_Ann _) = True
        test (Constraint_Inst{}) = True
        test _ = False
updateWorkListConstraint _ = return ()

type SolveT a = StateT SolveState Identity a

class Solve a where
  solve :: a -> SolveT ()

class SolveSingle a where
  solveSingle :: a -> SolveT Bool
 

instance SolveSingle Constraint where
  solveSingle (Constraint_Ann c) = solveSingle c
  solveSingle (Constraint_Eq c) = solveSingle c
  solveSingle (Constraint_Gen name tau nu delta nu0 delta0 c1 env sigma) = do
    when (delta /= delta0) (solveSingle (head $ toConstraints $ genEq delta delta0) >> return ())
    when (nu /= nu0) (solveSingle (head $ toConstraints $ genEq nu nu0) >> return ()) 
    s1 <- use solution
    return s1    
    conMap <- use constraintMap
    return conMap
    c2 <- genTrace2 (name,"Gen: ", sigma) $ solveFixMulti $ conMap M.! c1
    c2' <- getConstraints c2
    cm <- use constraintMap
    let ncm = M.insert c1 c2 cm
    constraintMap .= traceRun "ss-c-cm" (makeDeepSeq ncm)
    checkConstraints c2
    s <- traceShowS ("#######",c2,"$$$$$") $ use solution
    let tau' = S.substSolution tau s 
        env' = S.substSolution env s 
        nu' = S.substSolution nu s 
        delta' = S.substSolution delta s
        nu0' = S.substSolution nu0 s 
        delta0' = S.substSolution delta0 s 
        (favc2, ftvc2, _) = E.extractVars c2'
        (favtau', ftvtau', _) = E.extractVars tau'
        (favenv', ftvenv', _) = E.extractVars env'
        favnu' = E.extractAnnVars nu'
        favdelta' = E.extractAnnVars delta'
        favnu0' = E.extractAnnVars nu0'
        favdelta0' = E.extractAnnVars delta0'
        valpha = S.union ftvc2 ftvtau' S.\\ ftvenv'
        -- vbeta = S.union favc2 favtau' S.\\ S.unions [favenv', favnu', favdelta', favnu0', favdelta0']
        vbeta = favtau' S.\\ S.unions [favenv', favnu', favdelta', favnu0', favdelta0']
    s2 <- use solution
    let ns1 = s2 & annSol %~ flip (foldr M.delete) (traceShowT vbeta vbeta)
    solution .= traceRun "ss-c-s2" (makeDeepSeqSol ns1)
    s3 <- use solution
    let ns2 = s3 & tySol %~ flip (foldr M.delete) valpha
    solution .= traceRun "ss-c-s3" (makeDeepSeqSol ns2)
    dd <- use dontDefault
    let ndd = S.union vbeta dd
    dontDefault .= traceRun "ss-c-dd" (makeDeepSeqSet ndd)
    addNewConstraint $ Constraint_Eq $ ConstraintEq_Scheme sigma $ Scheme_ForallTemp vbeta valpha c2 tau'
    return True
  solveSingle (Constraint_Inst _ (Scheme_ForallTemp beta1 alpha1 c tau1) tau2) = do
    alpha2 <- replicateM (S.size alpha1) getFresh'
    beta2 <- replicateM (S.size beta1) getFresh'
    let sol = traceShowS ("help", sol, pp sol) $ Solution (M.fromList $ zip (S.toList beta1) (map Annotation_Var beta2)) (M.fromList $ zip (S.toList alpha1) (map Type_Var alpha2)) M.empty
        tau' = S.substSolution tau1 sol
    cwl <- use currentWorkList
    let ncwl = addToQueue (S.toList c) cwl
    currentWorkList .= traceRun "ss-c-i-cwl" (makeDeepSeqQueue ncwl)
    addNewConstraint $ Constraint_Eq $ ConstraintEq_Type tau' tau2
    return True
  solveSingle (Constraint_Inst _ (Scheme_Forall beta1 alpha1 c tau1) tau2) = do
    alpha2 <- traceShowS ("inst import", length c) $ replicateM (S.size alpha1) getFresh'
    beta2 <- replicateM (S.size beta1) getFresh'
    let sol = traceShowS ("help", sol, pp sol) $ Solution (M.fromList $ zip (S.toList beta1) (map Annotation_Var beta2)) (M.fromList $ zip (S.toList alpha1) (map Type_Var alpha2)) M.empty
        tau' = S.substSolution tau1 sol
        c' = S.substSolution c sol
    addNewConstraint c'
    -- cwl <- use currentWorkList
    -- currentWorkList .= traceRun "ss-c-i-cwl" (addToQueue (S.toList c) cwl)
    addNewConstraint $ Constraint_Eq $ ConstraintEq_Type tau' tau2
    return True
  -- solve c = panic $ "help me: " ++ show c 
  solveSingle c = traceShowS "unsolved" $ return False

getConstraints :: Set Var -> SolveT Constraints
getConstraints = fmap (map snd) . getConstraintsWithVar

getConstraintsWithVar :: Set Var -> SolveT [(Var, Constraint)]
getConstraintsWithVar vs = do
  acs <- use allConstraints
  return $ mapMaybe (\v -> fmap (v,) $ M.lookup v acs) $ S.toList vs 
  -- return $ traceRun "gcwv-acs" $ foldl' (\xs v -> xs ++ maybe [] (\x -> [(v,x)]) (M.lookup v acs)) [] $ S.toList vs

checkConstraints :: Set Var -> SolveT ()
checkConstraints x = do
  cs <- getConstraints x
  return $ checkConstraints' cs

checkConstraints' :: Constraints -> ()
checkConstraints' [] = ()
checkConstraints' (c@Constraint_Gen{} :_) = panic $ "GenConstraint in GenConstraint: " ++ show c
checkConstraints' (c@(Constraint_Inst name _ _):_) = panic $ show name ++ ": InstConstraint in GenConstraint: " ++ show c
checkConstraints' (c@Constraint_Eq{} :_) = panic $ "EqConstraint in GenConstraint " ++ show c
checkConstraints' (_:xs) = checkConstraints' xs

instance SolveSingle ConstraintAnn where
  solveSingle c@(ConstraintAnn_Plus a1 a2 a) = solveAnnConstraint annPlus c a a1 a2
  solveSingle c@(ConstraintAnn_Union a1 a2 a) = solveAnnConstraint annUnion c a a1 a2
  solveSingle c@(ConstraintAnn_Times a1 a2 a) = solveAnnConstraint annTimes c a a1 a2
  solveSingle c@(ConstraintAnn_Cond a1 a2 a) = solveAnnConstraint annCond c a a1 a2

solveAnnConstraint :: (AnnVal -> AnnVal -> AnnVal) -> ConstraintAnn -> Annotation -> Annotation -> Annotation -> SolveT Bool
solveAnnConstraint diamond c phi (Annotation_Val w1) (Annotation_Val w2) = do 
  traceShowS ("help4", diamond w1 w2, phi, w1, w2, c) $ addNewConstraint (ConstraintEq_Ann phi $ Annotation_Val $ diamond w1 w2)
  return True
solveAnnConstraint diamond c phi3 phi1 phi2 = do
  let favs = E.extractAnnVars c
  phiw <- mapM lookUpPartSol $ S.toList favs
  let phiw' = [[(a, av) | av <- S.toList avs] | (a,avs) <- phiw]
      phiw4 = sequence phiw'
      phiw5 = [phiw4' | phiw4' <- phiw4, S.substAnn phi3 (makeSol phiw4') == diamond' (S.substAnn phi1 $ makeSol phiw4') (S.substAnn phi2 $ makeSol phiw4')]
  if null phiw5 then do
    testForTop c phi1 phi2
    return True
  else do
    let phiw6 = M.fromListWith S.union $ map (second S.singleton) $ concat phiw5
    updatePartSolution phiw6
    ps <- use partSolution
    let useless = S.size favs == 1 && ps M.! traceShowS "useless" (head $ S.toList favs) == annPowWithoutEmpty annTop' 
    addNewConstraint $ equals $ M.toList $ equalVars phiw5
    addNewConstraint $ singles phiw6
    return useless
  where diamond' (Annotation_Val x1) (Annotation_Val x2) = Annotation_Val $ diamond x1 x2
        diamond' _  _ = panic "BUG: Substitution did not result in values for ann constraint solving"
        makeSol = M.fromList . map (second Annotation_Val)

testForTop :: ConstraintAnn -> Annotation -> Annotation -> SolveT ()
testForTop c phi1 phi2
  | phi1 == annTop = do
    fresh <- fv
    addNewConstraint $ replace (fresh, phi2)
  | phi2 == annTop = do
    fresh <- fv
    addNewConstraint $ replace (phi1, fresh)
  | otherwise = do
    ps' <- use partSolution
    sol'<- use solution
    -- error unolvable ignored
    return ()
    -- panic $ "Unsatisfiable Constraint - solveAnn: " ++ show c ++ ", partSol: " ++ show ps' ++ "sol: " ++ show sol'
  where fv = do
              v <- getFresh'
              return $ Annotation_Var v
        replace (a1, a2) = case c of
          ConstraintAnn_Plus _ _ a ->  ConstraintAnn_Plus a1 a2 a
          ConstraintAnn_Union _ _ a -> ConstraintAnn_Union a1 a2 a
          ConstraintAnn_Times _ _ a -> ConstraintAnn_Times a1 a2 a
          ConstraintAnn_Cond _ _ a -> ConstraintAnn_Cond a1 a2 a

lookUpPartSol :: HsName -> SolveT (HsName, Set AnnVal)
lookUpPartSol n = do
  s <- use partSolution
  return $ (n,) $ fromMaybe (annPowWithoutEmpty annTop') $ M.lookup n s

updatePartSolution :: Map HsName (Set AnnVal) -> SolveT ()
updatePartSolution = mapM_ updatePartSolutionSingle . M.toList
  
updatePartSolutionSingle :: (HsName, Set AnnVal) -> SolveT ()
updatePartSolutionSingle (n, sa) = do
  ps <- use partSolution
  if Just sa == M.lookup n ps then
    return ()
  else do
    ps <- use partSolution
    let nps = M.insert n sa ps
    partSolution .= traceRun "upss-ps" (traceShowS ("upss", n, sa) $ makeDeepSeqPs nps)
    updateWorkList n

singles :: PartSolution -> Constraints
singles = singles' . map (second $ head . S.toList) . M.toList . M.filter (\x -> S.size x == 1)
  where singles' [] = mempty
        singles' ((n,s):xs) = singleC (Constraint_Eq $ ConstraintEq_Ann (Annotation_Var n) $ Annotation_Val s) <> singles' xs

equalVars :: [[(HsName, AnnVal)]] -> Map HsName (Set HsName)
equalVars [] = M.empty
equalVars (xs:xss) = M.unionWith S.intersection toM $ equalVars xss
  where 
    toM :: Map HsName (Set HsName)
    toM = toM2 $ map snd $ M.toList $ revMap $ M.fromList xs
    toM2 :: [Set HsName] -> Map HsName (Set HsName)
    toM2 [] = M.empty
    toM2 (y:ys) = M.union (toM2 ys) $ M.fromList $ map (\v -> (v,y)) $ S.toList y

revMap :: (Ord a, Ord b) => Map a b -> Map b (Set a)
revMap = revMap' . M.toList
  where revMap' [] = M.empty
        revMap' ((k,v):xs) = M.insertWith S.union v (S.singleton k) $ revMap' xs

equals :: [(HsName, Set HsName)] -> Constraints
equals [] = mempty
equals ((n,ns):xs) = equals' n (S.toList ns) <> equals xs
  where equals' _ [] = mempty
        equals' x (y:ys) 
          | x == y = equals' x ys
          | otherwise = singleC (Constraint_Eq $ ConstraintEq_Ann (Annotation_Var x) $ Annotation_Var y) <> equals' x ys

instance SolveSingle ConstraintEq where
  solveSingle (ConstraintEq_Ann a1@(Annotation_Var x1) a2@(Annotation_Var x2)) = do
    s <- use solution
    if x1 == x2 then
      return True
    else case M.lookup x1 (_annSol s) of
      Just mu' -> do 
        traceShowS ("help1", a1, a2, mu') $ addNewConstraint (ConstraintEq_Ann a2 mu')
        return True
      Nothing -> case M.lookup x2 (_annSol s) of
        Just mu' -> do
          traceShowS ("help2", a1, a2, mu') $ addNewConstraint (ConstraintEq_Ann a1 mu')
          return True
        Nothing -> do
          addToSol x1 a2
          return True
  solveSingle (ConstraintEq_Ann (Annotation_Var x) mu) = do
    s <- use solution
    case M.lookup x (_annSol s) of
      Just mu' -> do
        traceShowS ("help3", x, mu, mu') $ addNewConstraint (ConstraintEq_Ann mu mu')
        return True
      Nothing -> do
        addToSol x mu
        return True
  solveSingle (ConstraintEq_Ann mu a2@(Annotation_Var _)) = do
    addNewConstraint (ConstraintEq_Ann a2 mu)
    return True
  solveSingle c@(ConstraintEq_Ann mu1 mu2) = 
    if mu1 /= mu2 then 
      if mu1 == annTop || mu2 == annTop then
        -- forget information
        return True
      else do
        s <- use solution
        ps <- use partSolution
        -- panic $ "Unsatisfiable constraint (solve eqann): " ++ show c ++ ", sol and part: " ++ show (s, ps)
        return True
    else
      return True
      
  solveSingle (ConstraintEq_Type a1@(Type_Var x1) a2@(Type_Var x2)) = do
    s <- use solution
    if x1 == x2 then
      return True
    else case M.lookup x1 (_tySol s) of
      Just mu' -> do
        addNewConstraint (ConstraintEq_Type a2 mu')
        return True
      Nothing -> case M.lookup x2 (_tySol s) of
        Just mu' -> do 
          addNewConstraint (ConstraintEq_Type a1 mu')
          return True
        Nothing -> do
          addToSol x1 a2
          return True
  solveSingle (ConstraintEq_Type (Type_Var x) mu) = do
    s <- use solution
    case M.lookup x (_tySol s) of
      Just mu' -> do
        addNewConstraint (ConstraintEq_Type mu mu')
        return True
      Nothing -> do
        addToSol x mu
        return True
  solveSingle (ConstraintEq_Type mu a2@(Type_Var _)) = solveSingle (ConstraintEq_Type a2 mu)
  solveSingle c@(ConstraintEq_Type (Type_Data n1 as1 ts1) (Type_Data n2 as2 ts2)) = 
    if n1 /= n2 then
      return $ traceShowS ("TypeEqError", n1, n2) True
      -- panic $ "Unsatisfiable constraint(solve eqtype): " ++ show c
    else do
      addNewConstraint $ genEq as1 as2 <> genEq ts1 ts2
      return True
  solveSingle c@(ConstraintEq_Type (Type_Func r1 e1) (Type_Func r2 e2)) = do
    addNewConstraint $ genEq r1 r2 <> genEq e1 e2
    return True
  solveSingle c@(ConstraintEq_Type (Type_Tup ts1) (Type_Tup ts2)) = do
    addNewConstraint $ genEq ts1 ts2
    return True
  solveSingle c@(ConstraintEq_Type t1@(Type_App t1f t1a) t2@(Type_App t2f t2a)) = 
    do
      -- ignore length errors
      -- let (x,y) = unzip $ zip (splitTypeApp t1) $ splitTypeApp t2
      -- addNewConstraint $ genEq x y
      addNewConstraint $ genEq t1f t2f <> genEq t1a t2a  
      return True
      
  solveSingle (ConstraintEq_Type a b@(Type_App{})) = do
    addNewConstraint $ ConstraintEq_Type b a
    return True
  solveSingle c@(ConstraintEq_Type (Type_App t1f t1a) (Type_Data n as ts)) = 
    if ts == [] then do
      (addNewConstraint $ traceShowS ("appError:", c) $ genEq t1f (Type_Data n as ts))
      return True 
    else
      let t = last ts
          ts' = init ts
      in do
        addNewConstraint $ genEq t1f (Type_Data n as ts') <> genEq t1a t
        return True
  solveSingle c@(ConstraintEq_Type t1@(Type_App{}) t2@(Type_Tup _)) = do
    addNewConstraint $ genEq t1 $ tupToData t2
    return True
  
  solveSingle (ConstraintEq_Scheme a1@(Scheme_Var x1) a2@(Scheme_Var x2)) = do
    s <- use solution
    if x1 == x2 then
      return True
    else case M.lookup x1 (_schemeSol s) of
      Just mu' -> do
        addNewConstraint $ ConstraintEq_Scheme a2 mu'
        return True
      Nothing -> case M.lookup x2 (_schemeSol s) of
        Just mu' -> do
          addNewConstraint $ ConstraintEq_Scheme a1 mu'
          return True
        Nothing -> do
          addToSol x1 a2
          return True
  solveSingle (ConstraintEq_Scheme (Scheme_Var x) mu) = do
    s <- use solution
    case M.lookup x (_schemeSol s) of
      Just mu' -> do
        addNewConstraint $ ConstraintEq_Scheme mu mu'
        return True
      Nothing -> do
        addToSol x mu
        return True
  solveSingle (ConstraintEq_Scheme mu a2@(Scheme_Var _)) = solveSingle (ConstraintEq_Scheme a2 mu)
  solveSingle c@(ConstraintEq_Scheme mu1@(Scheme_ForallTemp as1 ts1 _ _) mu2@(Scheme_ForallTemp as2 ts2 cs2 _)) = do
    let asol = M.filterWithKey filterNotEqual $ M.fromList $ zip (S.toList as1) $ map Annotation_Var $ S.toList as2
        tsol = M.filterWithKey filterNotEqual $ M.fromList $ zip (S.toList ts1) $ map Type_Var $ S.toList ts2
        Scheme_ForallTemp _ _ cs ts = S.substSolution mu1 $ Solution asol tsol M.empty
        mu1' = Scheme_ForallTemp as2 ts2 cs ts
    if S.size cs /= S.size cs2 then
      return True
    else if length as1 /= length as2 || length ts1 /= length ts2 then
      return True
      -- ignore failings assume correct
      -- panic $ "Unsatisfiable constraint (solve eqscheme). Quantfied variables cannot be matched: " ++ show (pp c)
    else if mu1' /= mu2 then
      panic $ "Unsatisfiable constraint (solve eqscheme): " ++ show (mu1, mu1', mu2)
    else
      return True
  solveSingle c@(ConstraintEq_Scheme mu1@(Scheme_Forall as1 ts1 _ _) mu2@(Scheme_Forall as2 ts2 cs2 _)) = do
    let asol = M.filterWithKey filterNotEqual $ M.fromList $ zip (S.toList as1) $ map Annotation_Var $ S.toList as2
        tsol = M.filterWithKey filterNotEqual $ M.fromList $ zip (S.toList ts1) $ map Type_Var $ S.toList ts2
        Scheme_Forall _ _ cs ts = S.substSolution mu1 $ Solution asol tsol M.empty
        mu1' = Scheme_Forall as2 ts2 cs ts
    if length cs /= length cs2 then
      return True
    else if length as1 /= length as2 || length ts1 /= length ts2 then
      return True
      -- ignore failings assume correct
      -- panic $ "Unsatisfiable constraint (solve eqscheme). Quantfied variables cannot be matched: " ++ show (pp c)
    else if mu1' /= mu2 then
      panic $ "Unsatisfiable constraint (solve eqscheme): " ++ show (mu1, mu1', mu2)
    else
      return True
      
  solveSingle c = traceShowS ("Unsatisfiable constraint(solve eq): ", c) $ return True
  -- solveSingle c = panic $ "Unsatisfiable constraint(solve eq): " ++ show c

tupToData :: Type -> Type
tupToData (Type_Tup ts) = Type_Data (mkHNm "_Tup") (concatMap (\x -> [getUsage x, getDemand x]) ts) $ map getType ts

class FilterNotEqual a where
  filterNotEqual :: HsName -> a -> Bool

instance FilterNotEqual Annotation where
  filterNotEqual n (Annotation_Var x) = n /= x
  filterNotEqual _ _ = True

instance FilterNotEqual Type where
  filterNotEqual n (Type_Var x) = n /= x
  filterNotEqual _ _ = True

class AddToSol a where
  addToSol :: HsName -> a -> SolveT ()

instance AddToSol Annotation where
  addToSol n (Annotation_Var x) | n == x = return ()
  addToSol n a = traceShowS ("atsA-start", n, a) $ do
    s <- use solution
    let ns = s & annSol %~ M.insert n a
    solution .= makeDeepSeqSol ns
    ps2 <- use partSolution
    let nps = M.delete n ps2
    partSolution .= makeDeepSeqPs nps
    updateWorkList n
    

instance AddToSol Type where
  addToSol n (Type_Var x) | n == x = return ()
  addToSol n t = traceShowS ("atsT-start", n, t) $  do
    sol <- use solution
    let ns = sol & tySol %~ M.insert n t
    solution .= makeDeepSeqSol ns
    updateWorkList n
   

instance AddToSol Scheme where
  addToSol n (Scheme_Var x) | n == x = return ()
  addToSol n s = traceShowS ("atsS-start", n, s) $  do
    sol <- use solution
    let ns = sol & schemeSol %~ M.insert n s
    solution .= makeDeepSeqSol ns
    updateWorkList n

updateWorkList :: HsName -> SolveT ()
updateWorkList n = do
  wlcs <- use workListConstraints
  let nc = traceRun "uwl-nc" $ S.toList $ fromMaybe S.empty $ M.lookup n wlcs
  cwl <- use currentWorkList
  let ncwl = addToQueueBack nc cwl
  currentWorkList .= traceRun "uwl-cwl" (makeDeepSeqQueue ncwl)
  cvs <- use changedVars
  let ncvs = S.insert n cvs
  changedVars .= traceRun "uwl-cvs" (makeDeepSeqSet ncvs)

solveFix :: SolveT ()
-- solveFix = return ()
solveFix = traceShowS "solveFix" $ do
  mc <- getWorkConstraint
  if traceShowS ("mc",mc) $ mc == Nothing then do
    acs <- use allConstraints
    return $ traceShowS ("acs", acs) ()
  else do
    s <- use solution
    let (v,c) = fromJust mc
        c' = seq (show ("solve: ", v)) S.substSolution c s
    updateWorkListConstraint $ traceShowS ("current",v) (v,c)
    solved <- solveSingle (traceShowS "sf-c'" c')
    asc <- use allConstraints
    if traceShowS "solved" solved then do
      let nacs = M.delete v asc
      allConstraints .= traceRun "sf-asc-solved" (makeDeepSeqAcs nacs)
    else do
      s' <- use solution
      let nacs2 = M.insert v (S.substSolution c' s') asc
      allConstraints .= traceRun "sf-asc-unsolved" (makeDeepSeqAcs nacs2)
      updateWorkListConstraint (v,c)
    solveFix

getWorkConstraint :: SolveT (Maybe (Var, Constraint))
getWorkConstraint = do
  wl <- use currentWorkList
  acs <- use allConstraints
  let (q, m) = getWorkConstraint' wl acs
  currentWorkList .= q
  return m

getWorkConstraint' wl acs = 
  if traceShowS "gwc-nullWl" $ nullQueue wl then
    traceShowS "gwc-empty" (wl, Nothing)
  else
    case traceShowS "gwc-lookup" $ M.lookup v acs of
      Nothing -> traceShowS "gwc-rec" $ getWorkConstraint' q acs
      Just c -> traceShowS "gwc-ret" (q, Just (v,c))
  where (v,q) = traceShowS ("gwc-pop", toList wl, acs) $ fromJust $ popFront wl

-- getWorkConstraint :: SolveT (Maybe (Var, Constraint))
-- getWorkConstraint = traceShow "gwc" $ do
--   wl <- use currentWorkList
--   if traceShow "gwcN" $ nullQueue wl then
--     return Nothing
--   else do
--     let (v,q) = fromJust $ popFront wl
--     currentWorkList .= q
--     acs <- use allConstraints
--     case traceShow (toList q) $ M.lookup v acs of
--       Nothing -> getWorkConstraint
--       Just c -> return $ Just (v, c)

traceRun :: Show a => String -> a -> a
-- traceRun m x = traceShow (m ++ ".start") $ seqForce x $ traceShow (m ++ ".end") x 
traceRun m x = seqForce x x

traceRun2 :: (Show c) => String -> c -> c
-- traceRun2 m x = traceShow (m ++ ".start") $ seqForce2 x $ traceShow (m ++ ".end") x 
traceRun2 _ x = seqForce2 x x

seqForce2 :: Show a => a -> b -> b
-- seqForce2 x = seq (length $ show x)
seqForce2 _ = id

seqForce :: Show a => a -> b -> b
-- seqForce x = seq (length $ show x)
seqForce _ = id

force :: Show a => String -> a -> a
force s = id
-- force s x = traceShow ("forceSolving", s, length $ show x) x

countConstraints :: Constraints -> Map a Constraints -> Int
countConstraints c cm = traceRun "countConstraints" $ length c + sum (map length $ M.elems cm) 

-- runInIsolation :: Set Var -> SolveT (Set Var)
-- runInIsolation sv = do
--   cs <- getConstraintsWithVar sv
--   acs <- use allConstraints
--   wlcs <- use workListConstraints
--   cwl <- use currentWorkList
--   allConstraints .= traceRun "rii-acs-cs" (M.fromList cs)
--   workListConstraints .= seqForce acs M.empty
--   currentWorkList .= fromListQueue (map fst cs)
--   mapM_ updateWorkListConstraint cs
--   solveFix
--   acs' <- use allConstraints
--   allConstraints .= traceRun "rii-acs-acs'" (M.union acs acs')
--   currentWorkList .= cwl
--   wlcs' <- use workListConstraints
--   workListConstraints .= traceRun "rii-wlcs" (M.unionWith S.union wlcs wlcs')
--   cvs <- use changedVars
--   changedVars .= S.empty
--   mapM_ updateWorkList $ S.toList cvs
--   return $ M.keysSet acs'

-- solveFixMulti :: Set Var -> SolveT (Set Var)
-- solveFixMulti c = do
  -- (bc', gc, ic) <- sortConstraints c
  -- bc <- return $ traceRun "sfm-bc" bc'
  -- -- cm1 <- use constraintMap
  -- -- cs <- traceShow ("sort",countConstraints (bc <> gc <> ic) cm1) $ solveFix bc
  -- cs'' <- runInIsolation bc
  -- cs <- return $ traceRun "sfm-cs''" cs''
  
  -- -- cm2 <- use constraintMap
  -- -- cs' <- traceShow ("first", countConstraints cs cm2) $ solveFix $ cs <> ic
  -- cs2 <- runInIsolation $ S.union cs ic
  -- cs' <- return $ traceRun "sfm-cs2" cs2
  
  -- -- cm3 <- use constraintMap
  -- -- traceShow ("second", countConstraints cs' cm3) $ solveFix $ cs' <> gc
  -- r <- runInIsolation $ S.union cs' gc
  -- return $ traceRun "solveFixMulti" r


-- solveFixMulti :: Set Var -> SolveT (Set Var)
-- solveFixMulti c = do
--   runInIsolation c

solveFixMulti :: Set Var -> SolveT (Set Var)
solveFixMulti c = do
  f <- getFresh
  cwl <- use currentWorkList
  currentWorkList .= makeDeepSeqQueue (fromListQueue $ S.toList c)
  traceShow ("sfm", length $ toListQueue cwl, S.size c) solveFix
  f2 <- getFresh
  currentWorkList .= cwl
  return $ S.union c $ S.fromList [f .. f2]

sortConstraintsSet :: Set Var -> SolveT (Set Var, Set Var, Set Var)
sortConstraintsSet = sortConstraintsSet' . S.toList
  where sortConstraintsSet' [] = return mempty -- (S.empty,S.empty,S.empty)
        sortConstraintsSet' (v:vs) = do
          (ec, gcs, cs) <- sortConstraintsSet' vs
          acs <- use allConstraints
          case M.lookup v acs of
            Nothing -> return (ec,gcs,cs) 
            Just c -> case c of
              Constraint_Eq{} -> return (S.insert v ec,gcs,cs) 
              Constraint_Ann{} -> return (S.insert v ec, gcs,cs) 
              Constraint_Gen{} -> return (ec, S.insert v gcs,cs) 
              Constraint_Inst{} -> return (ec, gcs,S.insert v cs) 

sortConstraints :: Constraints -> Constraints
sortConstraints cs = seq (length cs') cs'
  where cs' = ecs ++ ics ++ gcss
        (ecs, gcss, ics) = sortConstraints' cs
        sortConstraints' [] = ([],[],[])
        sortConstraints' (x:xs) = case x of
              Constraint_Eq{} -> (x:ec,gcs,cs) 
              Constraint_Ann{} -> (x:ec, gcs,cs) 
              Constraint_Gen{} -> (ec, x:gcs,cs) 
              Constraint_Inst{} -> (ec, gcs, x:cs) 
          where (ec, gcs, cs) = sortConstraints' xs

defaulting :: [(HsName, Set AnnVal)] -> (HsName, AnnVal)
defaulting ((n, as):xs) = traceRun "defaulting" $ defaulting' n (maxAnnVal as) xs
  where defaulting' beta w [] = (beta, w)
        defaulting' beta2 w2 ((beta1,wi):psi) = if w2 .< w1 
          then 
            defaulting' beta1 w1 psi
          else
            defaulting' beta2 w2 psi 
          where w1 = maxAnnVal wi

maxAnnVal :: Set AnnVal -> AnnVal
maxAnnVal = traceRun "maxAnnVal" . intToAnnVal . minimum . map annValToInt . S.toList

annValToInt :: AnnVal -> Int
annValToInt x 
  | x == annZero' = 1
  | x == annOne' = 2
  | x == S.fromList [AnnPrim_One, AnnPrim_Infinity] = 3
  | x == annW' = 4
  | x == S.fromList [AnnPrim_Zero, AnnPrim_One] = 5
  | x == S.fromList [AnnPrim_Zero, AnnPrim_Infinity] = 6
  | x == annTop' = 7
  

intToAnnVal :: Int -> AnnVal
intToAnnVal n = case n of
   1 -> annZero'
   2 -> annOne'
   3 -> S.fromList [AnnPrim_One, AnnPrim_Infinity]
   4 -> annW'
   5 -> S.fromList [AnnPrim_Zero, AnnPrim_One]
   6 -> S.fromList [AnnPrim_Zero, AnnPrim_Infinity]
   7 -> annTop'

(.<) :: AnnVal -> AnnVal -> Bool
(.<) = (>) `on` annValToInt

solveDef :: Constraints -> SolveState -> Map Var Constraints -> Solution
solveDef cs ss conMap = execState (solveDefInit cs conMap) ss ^. solution

solveDefInit :: Constraints -> Map Var Constraints -> SolveT ()
solveDefInit cs conMap = do
  addNewConstraint cs
  cm' <- mapM addConMap $ M.toList conMap
  constraintMap .= makeDeepSeq (M.fromList cm')
  solveDef'

addConMap :: (Var, Constraints) -> SolveT (Var, Set Var)
addConMap (v,cs) = do
  fvs <- replicateM (length cs) getFresh
  let zipped = zip fvs cs
  acs <- use allConstraints
  let nacs = M.union (M.fromList zipped) acs
  allConstraints .= traceRun "acm-acs" (makeDeepSeqAcs nacs)
  mapM_ updateWorkListConstraint zipped
  return (v, S.fromList fvs)

solveDef' :: SolveT ()
solveDef' = traceShow "StartDef" $ do 
  s <- use currentWorkList
  seq (makeDeepSeqQueue s) solveFix
  ps <- use partSolution
  partSolution .= traceRun "sd-ps-f" (traceShowS "psHelp" $ makeDeepSeqPs $ M.filter (\x -> S.size x > 1) ps)
  notDefaulting <- use dontDefault
  ps <- use partSolution
  let psiFinal = traceRun "solveDef-psiFinal" $ M.filterWithKey (\x _ -> S.notMember x notDefaulting) ps
  if M.null psiFinal then
    solveFinalSchemes
  else do
    let (beta, w) = traceShow2def "Def: " $ defaulting $ M.toList psiFinal
    addToSol beta $ Annotation_Val w
    cwl <- use currentWorkList
    seq (makeDeepSeqQueue s) $ traceShow ("defrec", length $ toListQueue cwl) solveDef'
    
solveFinalSchemes :: SolveT ()
solveFinalSchemes = traceShow "startFinalSchemes" $ do
  acs <- use allConstraints
  let filtered = M.filter f acs
      keys = traceShow ("still to do cs", M.size filtered, M.size acs) $ M.keys filtered
  currentWorkList .= fromListQueue (traceRun "sfss-keys" $ makeDeepSeqList keys)
  solveFix
  s <- use solution 
  s' <- mapM solveFinalScheme $ traceRun (show ("sfss-sfs", s)) s ^. schemeSol
  s2 <- use solution
  solution .= traceRun (show ("sfss-s", s2)) (makeDeepSeqSol $ s & schemeSol .~ s')
  where f (Constraint_Ann _) = False
        f _ = True

solveFinalScheme :: Scheme -> SolveT Scheme
solveFinalScheme s@(Scheme_ForallTemp as ts cs t) = traceShowS ("start sfs", s) $ do
  cs' <- getConstraints $ traceRun2 "sfs-cs" cs
  s <- use solution
  let as' = traceRun "sfs-as'" $ S.fromList $ S.substSolution (map Annotation_Var $ S.toList as) s
      ts' = traceRun "sfs-ts'" $ S.substSolution (map Type_Var $ S.toList ts) s
      t' = traceRun "sfs-ts'" $ S.substSolution t s
      hsn = traceRun "sfs-hsn" $ ifsConstraints $ Scheme_Forall (S.fromList $ f $ S.toList as') ts cs' t
      as'' = traceRun "sfs-as''" $ S.toList $ as' `S.difference` S.map Annotation_Var hsn
  return $ traceShowS "end sfs" $ removeUnusedVars $ Scheme_Forall (S.fromList $ f as'') (S.fromList $ g ts') (removeUselessConstraints cs' hsn) t'
  where f (Annotation_Var v:xs) = v : f xs
        f (_:xs) = f xs
        f [] = []
        g (Type_Var v:xs) = v : g xs
        g (_:xs) = g xs
        g [] = []
solveFinalScheme s = return s -- panic $ "schemevar in final type: " ++ show s -- return s

influenceSets :: Constraints -> Map HsName (Set HsName)
influenceSets [] = M.empty
influenceSets (Constraint_Ann c:xs) = M.unionWith S.union (M.fromList $ map (,as) $ S.toList as) $ influenceSets xs
  where as = E.extractAnnVars c
influenceSets (c:xs) = traceShow ("influanceHelp", c) $ influenceSets xs
-- influenceSets (Constraint_Eq c:xs) = panic $ show ("influence help eq", c)
-- influenceSets (c@Constraint_Inst{}:xs) = panic $ show ("influence help inst", c)  
-- influenceSets (c@Constraint_Gen{}:xs) = panic $ show ("influence help gen", c)

ifs :: Constraints -> [Set HsName]
ifs = S.toList . S.fromList . M.elems . influenceSets

ifsConstraints :: Scheme -> Set HsName
ifsConstraints (Scheme_Forall as _ cs t) = S.unions hsn'
  where ast = E.extractAnnVars t
        ifscs = ifs cs
        f shn = if shn `S.isSubsetOf` as && S.null (shn `S.intersection` ast)
          then shn
          else S.empty
        hsn' = map f ifscs

removeUselessConstraints :: Constraints -> Set HsName ->  Constraints
removeUselessConstraints xs hsn = [x | x <- xs, S.null (E.extractAnnVars x `S.intersection` hsn)]

removeUnusedVars :: Scheme -> Scheme
removeUnusedVars (Scheme_Forall as ts cs t) = Scheme_Forall as' ts cs t
  where as' = S.intersection as $ S.unions [E.extractAnnVars cs, E.extractAnnVars t]
removeUnusedVars s = s
%%]