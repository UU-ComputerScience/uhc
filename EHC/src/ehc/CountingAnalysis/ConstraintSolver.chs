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

import Data.Map (Map)
import qualified Data.Map as M
import Data.Set (Set)
import qualified Data.Set as S
import Data.Dequeue (BankersDequeue)
import qualified Data.Dequeue as Q
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
-- traceShow2def = flip const
traceShow2def x y = traceShow (x,y) y

%%]



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%% Types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 counting) hs export(solveDef, emptySolveState, SolveState(..), solution, partSolution, freshVar, toHsName)
data SolveState = SolveState
  { _solution :: Solution
  , _partSolution :: PartSolution
  , _constraintMap :: Map Var (Set Var)
  , _allConstraints :: Map Var Constraint
  , _workListConstraints :: Map HsName (Set Var)
  , _currentWorkList :: BankersDequeue Var
  , _changedVars :: Set HsName
  , _freshVar :: Var
  , _toHsName :: Var -> HsName
  , _dontDefault :: Set HsName
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
  , _currentWorkList = Q.empty
  , _changedVars = S.empty
  , _freshVar = 0
  , _toHsName = error "empty toHsName"
  , _dontDefault = S.empty
  }
  
makeLenses ''Solution
makeLenses ''SolveState

getFresh :: SolveT Var
getFresh = do
  v <- use freshVar
  freshVar %= succ
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
  addNewConstraint cs = do
    fvs <- replicateM (length cs) getFresh
    let zipped = zip fvs cs
    allConstraints %= M.union (traceShow zipped $ M.fromList zipped)
    currentWorkList %= addToQueue (map fst zipped)
    mapM_ updateWorkListConstraint zipped

updateWorkListConstraint :: (Var, Constraint) -> SolveT ()
updateWorkListConstraint (v,c) = do
  let (as,ts,ss) = E.extractVars c
      hsns = S.toList $ S.unions [as,ts,ss]
  workListConstraints %= \m -> foldl' (\m1 x -> M.insertWith S.union x (S.singleton v) m1) m hsns

addToQueue :: [a] -> BankersDequeue a -> BankersDequeue a
addToQueue xs q = foldl' Q.pushFront q xs

addToQueueBack :: [a] -> BankersDequeue a -> BankersDequeue a
addToQueueBack xs q = foldl' Q.pushBack q xs

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
    constraintMap %= M.insert c1 c2
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
    solution %= \x -> x & annSol %~ flip (foldr M.delete) (traceShowT vbeta vbeta)
    solution %= \x -> x & tySol %~ flip (foldr M.delete) valpha
    dontDefault %= S.union vbeta
    addNewConstraint $ Constraint_Eq $ ConstraintEq_Scheme sigma $ Scheme_ForallTemp vbeta valpha c2 tau'
    return True
  solveSingle (Constraint_Inst _ (Scheme_ForallTemp beta1 alpha1 c tau1) tau2) = do
    alpha2 <- replicateM (S.size alpha1) getFresh'
    beta2 <- replicateM (S.size beta1) getFresh'
    let sol = traceShow ("help", sol, pp sol) $ Solution (M.fromList $ zip (S.toList beta1) (map Annotation_Var beta2)) (M.fromList $ zip (S.toList alpha1) (map Type_Var alpha2)) M.empty
        tau' = S.substSolution tau1 sol
    currentWorkList %= addToQueue (S.toList c)
    addNewConstraint $ Constraint_Eq $ ConstraintEq_Type tau' tau2
    return True
  -- solve c = panic $ "help me: " ++ show c 
  solveSingle c = traceShow "help" $ return False

getConstraints :: Set Var -> SolveT Constraints
getConstraints = fmap (map snd) . getConstraintsWithVar

getConstraintsWithVar :: Set Var -> SolveT [(Var, Constraint)]
getConstraintsWithVar vs = do
  cs <- use allConstraints
  return $ foldl' (\xs v -> xs ++ maybe [] (\x -> [(v,x)]) (M.lookup v cs)) [] $ S.toList vs

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
    partSolution %= M.insert n sa
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
      panic $ "Unsatisfiable constraint(solve eqtype): " ++ show c
    else do
      addNewConstraint $ genEq as1 as2 <> genEq ts1 ts2
      return True
  solveSingle c@(ConstraintEq_Type (Type_Func r1 e1) (Type_Func r2 e2)) = do
    addNewConstraint $ genEq r1 r2 <> genEq e1 e2
    return True
  solveSingle c@(ConstraintEq_Type (Type_Tup ts1) (Type_Tup ts2)) = do
    addNewConstraint $ genEq ts1 ts2
    return True
  solveSingle (ConstraintEq_Type (Type_App t1f t1a) (Type_App t2f t2a)) = do
    addNewConstraint $ genEq t1f t2f <> genEq t1a t2a  
    return True
  solveSingle (ConstraintEq_Type a@(Type_Data{}) b@(Type_App{})) = do
    addNewConstraint $ ConstraintEq_Type b a
    return True
  solveSingle (ConstraintEq_Type (Type_App t1f t1a) (Type_Data n as ts)) = 
    if ts == [] then do
      (addNewConstraint $ traceShow ("appError:", n) $ genEq t1f (Type_Data n as ts))
      return True 
    else
      let t = last ts
          ts' = init ts
      in do
        addNewConstraint $ genEq t1f (Type_Data n as ts') <> genEq t1a t
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
  solveSingle c@(ConstraintEq_Scheme mu1@(Scheme_ForallTemp as1 ts1 _ _) mu2@(Scheme_ForallTemp as2 ts2 _ _)) = do
    let asol = M.filterWithKey filterNotEqual $ M.fromList $ zip (S.toList as1) $ map Annotation_Var $ S.toList as2
        tsol = M.filterWithKey filterNotEqual $ M.fromList $ zip (S.toList ts1) $ map Type_Var $ S.toList ts2
        Scheme_ForallTemp _ _ cs ts = S.substSolution mu1 $ Solution asol tsol M.empty
        mu1' = Scheme_ForallTemp as2 ts2 cs ts
    if length as1 /= length as2 || length ts1 /= length ts2 then
      panic $ "Unsatisfiable constraint (solve eqscheme). Quantfied variables cannot be matched: " ++ show c
    else if mu1' /= mu2 then
      panic $ "Unsatisfiable constraint (solve eqscheme): " ++ show (mu1, mu1', mu2)
    else
      return True
      
  solveSingle c = panic $ "Unsatisfiable constraint(solve eq): " ++ show c

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
    r1 <- solution %= \x -> x & annSol %~ M.insert n (traceShow ("atsA-s", length $ show s) a)
    s2 <- use solution
    return $ traceRun "atsA-r1" r1
    ps2 <- use partSolution
    r2 <- partSolution .= M.delete (traceShow ("atsA-s2", length $ show s2, length $ show ps2) n) ps2
    return $ traceRun "atsA-r2" r2
    ps <- use partSolution
    updateWorkList $ traceShowS "atsA-ps.start" $ traceShow ("atsA-ps", length $ show ps) $ traceShowS "atsA-ps.end" n
    traceShow "atsA-end" $ currentWorkList %= \x -> traceRun "atsA-cw" x
    

instance AddToSol Type where
  addToSol n (Type_Var x) | n == x = return ()
  addToSol n t = traceShowS ("atsT-start", n, t) $  do
    solution %= \x -> x & tySol %~ M.insert n t
    updateWorkList n
   

instance AddToSol Scheme where
  addToSol n (Scheme_Var x) | n == x = return ()
  addToSol n s = traceShowS ("atsS-start", n, s) $  do
    solution %= \x -> x & schemeSol %~ M.insert n s
    updateWorkList n

updateWorkList :: HsName -> SolveT ()
updateWorkList n = do
  wlcs <- use workListConstraints
  let nc = traceRun "uwl-nc" $ S.toList $ fromMaybe S.empty $ M.lookup n wlcs
  currentWorkList %= addToQueueBack nc 
  changedVars %= S.insert n 

solveFix :: SolveT ()
-- solveFix = return ()
solveFix = traceShow "solveFix" $ do
  wcs <- use currentWorkList
  mc <- traceShow ("worklistsize", length $ toList wcs, toList wcs) getWorkConstraint
  if traceShow ("mc",mc) $ mc == Nothing then do
    acs <- use allConstraints
    return $ traceShowS ("acs", acs) ()
  else do
    s <- use solution
    let (v,c) = fromJust mc
        c' = S.substSolution c s
    updateWorkListConstraint $ traceShow ("current",v) (v,c)
    solved <- solveSingle (traceShow "sf-c'" c')
    if traceShow "solved" solved then
      allConstraints %= M.delete v
    else do
      s' <- use solution
      allConstraints %= M.insert v (S.substSolution c' s')
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
  if traceShow "gwc-nullWl" $ Q.null wl then
    traceShow "gwc-empty" (wl, Nothing)
  else
    case traceShow "gwc-lookup" $ M.lookup v acs of
      Nothing -> traceShow "gwc-rec" $ (q, Nothing) --traceShow "gwc-rec" $ getWorkConstraint' q acs
      Just c -> traceShow "gwc-ret" (q, Just (v,c))
  where (v,q) = traceShow ("gwc-pop", toList wl) $ fromJust $ Q.popFront wl

-- getWorkConstraint :: SolveT (Maybe (Var, Constraint))
-- getWorkConstraint = traceShow "gwc" $ do
--   wl <- use currentWorkList
--   if traceShow "gwcN" $ Q.null wl then
--     return Nothing
--   else do
--     let (v,q) = fromJust $ Q.popFront wl
--     currentWorkList .= q
--     acs <- use allConstraints
--     case traceShow (toList q) $ M.lookup v acs of
--       Nothing -> getWorkConstraint
--       Just c -> return $ Just (v, c)

traceRun :: Show a => String -> a -> a
-- traceRun m x = traceShow (m ++ ".start") $ seqForce x $ traceShow (m ++ ".end") x 
traceRun m x = seqForce x x

traceRun2 :: (Show c) => (String, Constraints, Solution) -> c -> c
-- traceRun2 (m,cs,sol) x = traceShow (m ++ ".start" ++ ms) $ seqForce x $ traceShow (m ++ ".end") x 
  -- where ms = if length cs == 78 then show sol else ""
traceRun2 _ x = seqForce x x

seqForce :: Show a => a -> b -> b
seqForce x = seq (length $ show x)

force :: Show a => String -> a -> a
force s = id
-- force s x = traceShow ("forceSolving", s, length $ show x) x

countConstraints :: Constraints -> Map a Constraints -> Int
countConstraints c cm = traceRun "countConstraints" $ length c + sum (map length $ M.elems cm) 

runInIsolation :: Set Var -> SolveT (Set Var)
runInIsolation sv = do
  cs <- getConstraintsWithVar sv
  acs <- use allConstraints
  wlcs <- use workListConstraints
  cwl <- use currentWorkList
  allConstraints .= M.fromList cs
  workListConstraints .= M.empty
  currentWorkList .= Q.fromList (map fst cs)
  mapM_ updateWorkListConstraint cs
  solveFix
  acs' <- use allConstraints
  allConstraints %= flip M.union acs
  currentWorkList .= cwl
  workListConstraints %= M.unionWith S.union wlcs
  cvs <- use changedVars
  changedVars .= S.empty
  mapM_ updateWorkList $ S.toList cvs
  return $ M.keysSet acs'

solveFixMulti :: Set Var -> SolveT (Set Var)
solveFixMulti c = do
  (bc', gc, ic) <- sortConstraints c
  bc <- return $ traceRun "sfm-bc" bc'
  -- cm1 <- use constraintMap
  -- cs <- traceShow ("sort",countConstraints (bc <> gc <> ic) cm1) $ solveFix bc
  cs'' <- runInIsolation bc
  cs <- return $ traceRun "sfm-cs''" cs''
  
  -- cm2 <- use constraintMap
  -- cs' <- traceShow ("first", countConstraints cs cm2) $ solveFix $ cs <> ic
  cs2 <- runInIsolation $ S.union cs ic
  cs' <- return $ traceRun "sfm-cs2" cs2
  
  -- cm3 <- use constraintMap
  -- traceShow ("second", countConstraints cs' cm3) $ solveFix $ cs' <> gc
  r <- runInIsolation $ S.union cs' gc
  return $ traceRun "solveFixMulti" r

sortConstraints :: Set Var -> SolveT (Set Var, Set Var, Set Var)
sortConstraints = sortConstraints' . S.toList
  where sortConstraints' [] = return mempty -- (S.empty,S.empty,S.empty)
        sortConstraints' (v:vs) = do
          (ec, gcs, cs) <- sortConstraints' vs
          acs <- use allConstraints
          case M.lookup v acs of
            Nothing -> return (ec,gcs,cs) 
            Just c -> case c of
              Constraint_Eq{} -> return (S.insert v ec,gcs,cs) 
              Constraint_Ann{} -> return (S.insert v ec, gcs,cs) 
              Constraint_Gen{} -> return (ec, S.insert v gcs,cs) 
              Constraint_Inst{} -> return (ec, gcs,S.insert v cs) 

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
  constraintMap .= M.fromList cm'
  solveDef'

addConMap :: (Var, Constraints) -> SolveT (Var, Set Var)
addConMap (v,cs) = do
  fvs <- replicateM (length cs) getFresh
  let zipped = zip fvs cs
  allConstraints %= M.union (M.fromList zipped)
  mapM_ updateWorkListConstraint zipped
  return (v, S.fromList fvs)

solveDef' :: SolveT ()
solveDef' = traceShow "StartDef" $ do
  sta <- get
  wl <- use currentWorkList
  solveFixMulti $ traceRun "solveDef-sfm" $ seqForce sta $ S.fromList $ toList wl
  partSolution %= M.filter (\x -> S.size x > 1)
  notDefaulting <- use dontDefault
  ps <- use partSolution
  let psiFinal = traceRun "solveDef-psiFinal" $ M.filterWithKey (\x _ -> S.notMember x notDefaulting) ps
  if M.null psiFinal then
    solveFinalSchemes
  else do
    let (beta, w) = traceShow2def "Def: " $ defaulting $ M.toList psiFinal
    r <- traceShow ("startAddToSol", beta, w) $ addToSol beta $ Annotation_Val w
    return $ traceRun "defrec" r
    traceShow "defRec" solveDef'
    
solveFinalSchemes :: SolveT ()
solveFinalSchemes = traceShow "startFinalSchemes" $ do
  s <- use solution 
  s' <- mapM solveFinalScheme $ s ^. schemeSol
  solution %= \x -> x & schemeSol .~ s'

solveFinalScheme :: Scheme -> SolveT Scheme
solveFinalScheme s@(Scheme_ForallTemp as ts cs t) = traceShow ("finalSub") $ do
  cs' <- getConstraints cs
  s <- use solution
  let as' = S.fromList $ S.substSolution (map Annotation_Var $ S.toList as) s
      ts' = S.substSolution (map Type_Var $ S.toList ts) s
      t' = S.substSolution t s
      hsn = ifsConstraints $ Scheme_Forall (S.fromList $ f $ S.toList as') ts cs' t
      as'' = S.toList $ as' `S.difference` S.map Annotation_Var hsn
  return $ removeUnusedVars $ Scheme_Forall (S.fromList $ f as'') (S.fromList $ g ts') (removeUselessConstraints cs' hsn) t'
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