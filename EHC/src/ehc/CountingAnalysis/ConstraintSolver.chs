%%[0 hs
{-# LANGUAGE TemplateHaskell #-}  
{-# LANGUAGE DoAndIfThenElse #-}  
%%]

%%[(8 counting) hs module{%{EH}CountingAnalysis.ConstraintSolver}

import %%@{%{EH}%%}Base.HsName (HsName, mkHNm)
import %%@{%{EH}%%}CountingAnalysis.ConstraintGeneration
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
import Control.Monad.State
import Control.Lens
import Data.Monoid
import Data.Maybe
import Control.Arrow (second)
import Data.Function (on)

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
  , _constraintMap :: Map Var Constraints
  , _freshVar :: Var
  , _toHsName :: Var -> HsName
  , _dontDefault :: Set HsName
  }
  
emptySolveState :: SolveState
emptySolveState = SolveState
  { _solution = emptySolution
  , _partSolution = M.empty
  , _constraintMap = M.empty
  , _freshVar = 0
  , _toHsName = error "empty toHsName"
  , _dontDefault = S.empty
  }
  
makeLenses ''Solution
makeLenses ''SolveState

getFresh' :: SolveT HsName
getFresh' = do
  v <- use freshVar
  f <- use toHsName
  freshVar %= succ
  return $ f v

isTypeConstraint :: Constraint -> Bool
isTypeConstraint (Constraint_Ann _) = False
isTypeConstraint (Constraint_Eq ConstraintEq_Ann{}) = False
isTypeConstraint _ = True

type SolveT a = StateT SolveState Identity a

class Solve a where
  solve :: a -> SolveT GatherConstraints

instance Solve GatherConstraints where
  solve = solve . toConstraints

instance Solve Constraints where
  solve [] = return $ traceShow "empty" mempty
  solve (c1:c2) = do
    s <- use solution
    cm <- use constraintMap
    -- c1' <- solve (if isTypeConstraint c1 then traceShow (ppAnnFree s >-< "constraint:" >-< ppAnnFree (c1, cm)) c1 else c1)
    c1' <- solve c1
    phi1 <- use solution
    let c2' = S.substSolution c2 phi1 
    c3 <- solve c2'
    traceShowT ("*****************c13:", c1, c2, c2', c3, "#######################") $ return $ c1' <> c3    

instance Solve Constraint where
  solve (Constraint_Ann c) = traceShowS ("annStart",c,"annend") $ solve c
  solve (Constraint_Eq c) = traceShowS ("eqStart", c, "eqend") $ solve c
  solve (Constraint_Gen name tau nu delta nu0 delta0 c1 env sigma) = do
    solve $ genEq delta delta0
    solve $ genEq nu nu0
    s1 <- use solution
    conMap <- use constraintMap
    let c1' = S.substSolution (conMap M.! c1) s1
    c2 <- genTrace2 (name,"Gen: ", sigma) $ solveFixMulti c1' 
    constraintMap %= M.insert c1 c2
    checkConstraints c2
    s <- traceShowS ("#######",c2,"$$$$$") $ use solution
    let tau' = S.substSolution tau s 
        env' = S.substSolution env s 
        nu' = S.substSolution nu s 
        delta' = S.substSolution delta s
        nu0' = S.substSolution nu0 s 
        delta0' = S.substSolution delta0 s 
        (favc2, ftvc2, _) = E.extractVars c2
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
    solve $ Constraint_Eq $ ConstraintEq_Scheme sigma $ Scheme_Forall vbeta valpha c2 tau'
    return mempty
  solve (Constraint_Inst _ (Scheme_Forall beta1 alpha1 c tau1) tau2) = do
    alpha2 <- replicateM (S.size alpha1) getFresh'
    beta2 <- replicateM (S.size beta1) getFresh'
    let sol = Solution (M.fromList $ zip (S.toList beta1) (map Annotation_Var beta2)) (M.fromList $ zip (S.toList alpha1) (map Type_Var alpha2)) M.empty
        c' = S.substSolution c sol
        tau' = S.substSolution tau1 sol
    solve $ singleC (Constraint_Eq $ ConstraintEq_Type tau' tau2)
    return $ toGatherConstraints c' 
  -- solve c = panic $ "help me: " ++ show c 
  solve c = return $ singleGC c

checkConstraints :: Constraints -> SolveT ()
checkConstraints [] = return ()
checkConstraints (c@Constraint_Gen{} :_) = panic $ "GenConstraint in GenConstraint: " ++ show c
checkConstraints (c@(Constraint_Inst name _ _):_) = panic $ show name ++ ": InstConstraint in GenConstraint: " ++ show c
checkConstraints (c@Constraint_Eq{} :_) = panic $ "EqConstraint in GenConstraint " ++ show c
checkConstraints (_:xs) = checkConstraints xs

instance Solve ConstraintAnn where
  solve c@(ConstraintAnn_Plus a1 a2 a) = solveAnnConstraint annPlus c a a1 a2
  solve c@(ConstraintAnn_Union a1 a2 a) = solveAnnConstraint annUnion c a a1 a2
  solve c@(ConstraintAnn_Times a1 a2 a) = solveAnnConstraint annTimes c a a1 a2
  solve c@(ConstraintAnn_Cond a1 a2 a) = solveAnnConstraint annCond c a a1 a2

solveAnnConstraint :: (AnnVal -> AnnVal -> AnnVal) -> ConstraintAnn -> Annotation -> Annotation -> Annotation -> SolveT GatherConstraints
solveAnnConstraint diamond c phi (Annotation_Val w1) (Annotation_Val w2) = traceShowS ("help4", diamond w1 w2, phi, w1, w2, c) $ solve (ConstraintEq_Ann phi $ Annotation_Val $ diamond w1 w2)

solveAnnConstraint diamond c phi3 phi1 phi2 = do
  let favs = E.extractAnnVars c
  phiw <- mapM lookUpPartSol $ S.toList favs
  let phiw' = [[(a, av) | av <- S.toList avs] | (a,avs) <- phiw]
      phiw4 = sequence phiw'
      phiw5 = [phiw4' | phiw4' <- phiw4, S.substAnn phi3 (makeSol phiw4') == diamond' (S.substAnn phi1 $ makeSol phiw4') (S.substAnn phi2 $ makeSol phiw4')]
  if null phiw5 then
    testForTop c phi1 phi2
  else do
    let phiw6 = M.fromListWith S.union $ map (second S.singleton) $ concat phiw5
    partSolution %= M.unionWith (flip const) phiw6
    ps <- use partSolution
    let useless = S.size favs == 1 && ps M.! traceShowS "useless" (head $ S.toList favs) == annPowWithoutEmpty annTop' 
        c2 = if useless then mempty else singleGC $ Constraint_Ann c
    solve $ singles phiw6
    solve $ equals $ M.toList $ equalVars phiw5
    return c2
  where diamond' (Annotation_Val x1) (Annotation_Val x2) = Annotation_Val $ diamond x1 x2
        diamond' _  _ = panic "BUG: Substitution did not result in values for ann constraint solving"
        makeSol = M.fromList . map (second Annotation_Val)

testForTop :: ConstraintAnn -> Annotation -> Annotation -> SolveT GatherConstraints
testForTop c phi1 phi2
  | phi1 == annTop = do
    fresh <- fv
    solve $ replace (fresh, phi2)
  | phi2 == annTop = do
    fresh <- fv
    solve $ replace (phi1, fresh)
  | otherwise = do
    ps' <- use partSolution
    sol'<- use solution
    -- error unolvable ignored
    return mempty
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

instance Solve ConstraintEq where
  solve (ConstraintEq_Ann a1@(Annotation_Var x1) a2@(Annotation_Var x2)) = do
    s <- use solution
    if x1 == x2 then
      return mempty
    else case M.lookup x1 (_annSol s) of
      Just mu' -> traceShowS ("help1", a1, a2, mu') $ solve (ConstraintEq_Ann a2 mu')
      Nothing -> case M.lookup x2 (_annSol s) of
        Just mu' -> traceShowS ("help2", a1, a2, mu') $ solve (ConstraintEq_Ann a1 mu')
        Nothing -> do
          addToSol x1 a2
          return mempty
  solve (ConstraintEq_Ann (Annotation_Var x) mu) = do
    s <- use solution
    case M.lookup x (_annSol s) of
      Just mu' -> traceShowS ("help3", x, mu, mu') $ solve (ConstraintEq_Ann mu mu')
      Nothing -> do
        addToSol x mu
        return mempty
  solve (ConstraintEq_Ann mu a2@(Annotation_Var _)) = solve (ConstraintEq_Ann a2 mu)
  solve c@(ConstraintEq_Ann mu1 mu2) = 
    if mu1 /= mu2 then 
      if mu1 == annTop || mu2 == annTop then
        -- forget information
        return mempty
      else do
        s <- use solution
        ps <- use partSolution
        -- panic $ "Unsatisfiable constraint (solve eqann): " ++ show c ++ ", sol and part: " ++ show (s, ps)
        return mempty
    else
      return mempty
      
  solve (ConstraintEq_Type a1@(Type_Var x1) a2@(Type_Var x2)) = do
    s <- use solution
    if x1 == x2 then
      return mempty
    else case M.lookup x1 (_tySol s) of
      Just mu' -> solve (ConstraintEq_Type a2 mu')
      Nothing -> case M.lookup x2 (_tySol s) of
        Just mu' -> solve (ConstraintEq_Type a1 mu')
        Nothing -> do
          addToSol x1 a2
          return mempty
  solve (ConstraintEq_Type (Type_Var x) mu) = do
    s <- use solution
    case M.lookup x (_tySol s) of
      Just mu' -> solve (ConstraintEq_Type mu mu')
      Nothing -> do
        addToSol x mu
        return mempty
  solve (ConstraintEq_Type mu a2@(Type_Var _)) = solve (ConstraintEq_Type a2 mu)
  solve c@(ConstraintEq_Type (Type_Data n1 as1 ts1) (Type_Data n2 as2 ts2)) = 
    if n1 /= n2 then
      panic $ "Unsatisfiable constraint(solve eqtype): " ++ show c
    else
      solve $ genEq as1 as2 <> genEq ts1 ts2
  solve c@(ConstraintEq_Type (Type_Func r1 e1) (Type_Func r2 e2)) = 
    solve $ genEq r1 r2 <> genEq e1 e2
  solve c@(ConstraintEq_Type (Type_Tup ts1) (Type_Tup ts2)) = 
    solve $ genEq ts1 ts2
  
  solve (ConstraintEq_Scheme a1@(Scheme_Var x1) a2@(Scheme_Var x2)) = do
    s <- use solution
    if x1 == x2 then
      return mempty
    else case M.lookup x1 (_schemeSol s) of
      Just mu' -> solve (ConstraintEq_Scheme a2 mu')
      Nothing -> case M.lookup x2 (_schemeSol s) of
        Just mu' -> solve (ConstraintEq_Scheme a1 mu')
        Nothing -> do
          addToSol x1 a2
          return mempty
  solve (ConstraintEq_Scheme (Scheme_Var x) mu) = do
    s <- use solution
    case M.lookup x (_schemeSol s) of
      Just mu' -> solve (ConstraintEq_Scheme mu mu')
      Nothing -> do
        addToSol x mu
        return mempty
  solve (ConstraintEq_Scheme mu a2@(Scheme_Var _)) = solve (ConstraintEq_Scheme a2 mu)
  solve c@(ConstraintEq_Scheme mu1@(Scheme_Forall as1 ts1 _ _) mu2@(Scheme_Forall as2 ts2 _ _)) = do
    let asol = M.fromList $ zip (S.toList as1) $ map Annotation_Var $ S.toList as2
        tsol = M.fromList $ zip (S.toList ts1) $ map Type_Var $ S.toList ts2
        Scheme_Forall _ _ cs ts = S.substSolution mu1 $ Solution asol tsol M.empty
        mu1' = Scheme_Forall as2 ts2 cs ts
    if length as1 /= length as2 || length ts1 /= length ts2 then
      panic $ "Unsatisfiable constraint (solve eqscheme). Quantfied variables cannot be matched: " ++ show c
    else if mu1' /= mu2 then
      panic $ "Unsatisfiable constraint (solve eqscheme): " ++ show (mu1, mu1', mu2)
    else
      return mempty
      
  solve c = panic $ "Unsatisfiable constraint(solve eq): " ++ show c

class AddToSol a where
  addToSol :: HsName -> a -> SolveT ()

instance AddToSol Annotation where
  addToSol n a = do
    solution %= \x -> x & annSol %~ M.insert n a
    partSolution %= M.delete n

instance AddToSol Type where
  addToSol n t = solution %= \x -> x & tySol %~ M.insert n t

instance AddToSol Scheme where
  addToSol n s = solution %= \x -> x & schemeSol %~ M.insert n s

solveFix :: Constraints -> SolveT Constraints
solveFix c = traceShowS "startFix" $ do
  s <- use solution
  ps <- use partSolution
  let c' = S.substSolution c s
  c1' <- traceShowT ("solveFixStart", c, c', "SolveFixEnd") $ solve c'
  let c1 = toConstraints c1'
  cm <- use constraintMap
  s' <- use solution
  ps' <- use partSolution
  -- let changed = traceShow ("nc:",countConstraints c1 cm, length $ show s', length $ show ps') $ c1 /= c' || s /= s' || ps /= ps'
  let changed = c1 /= c' || s /= s' || ps /= ps'
  if changed then
    solveFix c1
  else
    return c1

countConstraints :: Constraints -> Map a Constraints -> Int
countConstraints c cm = length c + sum (map length $ M.elems cm) 

solveFixMulti :: Constraints -> SolveT Constraints
solveFixMulti c = do
  let (bc, gc, ic) = sortConstraints c
  cm1 <- use constraintMap
  -- cs <- traceShow ("sort",countConstraints (bc <> gc <> ic) cm1) $ solveFix bc
  cs <- solveFix bc
  cm2 <- use constraintMap
  -- cs' <- traceShow ("first", countConstraints cs cm2) $ solveFix $ cs <> ic
  cs' <- solveFix $ cs <> ic
  cm3 <- use constraintMap
  -- traceShow ("second", countConstraints cs' cm3) $ solveFix $ cs' <> gc
  solveFix $ cs' <> gc

defaulting :: [(HsName, Set AnnVal)] -> (HsName, AnnVal)
defaulting ((n, as):xs) = defaulting' n (maxAnnVal as) xs
  where defaulting' beta w [] = (beta, w)
        defaulting' beta2 w2 ((beta1,wi):psi) = if w2 .< w1 
          then 
            defaulting' beta1 w1 psi
          else
            defaulting' beta2 w2 psi 
          where w1 = maxAnnVal wi

maxAnnVal :: Set AnnVal -> AnnVal
maxAnnVal = intToAnnVal . minimum . map annValToInt . S.toList

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

solveDef :: Constraints -> SolveState -> Solution
solveDef c ss = if M.null psiFinal then traceShow "finished" $ solveFinalSchemes ss' else 
  traceShow "solveDef" 
    $ solveDef (singleC (Constraint_Eq $ ConstraintEq_Ann (Annotation_Var beta) $ Annotation_Val w) <> c1)
    $ ss' & solution .~ phi1 & partSolution .~ psi1
  where phi = ss ^. solution
        psi = ss ^. partSolution
        c' = S.substSolution c phi
        (c1, ss') = traceShowS ("solveDefStart", c, c', "solveDefEnd") $ runState (solveFixMulti c') ss
        phi1 = ss' ^. solution
        psi1 = ss' ^. partSolution
        psi1' = M.filter (\x -> S.size x > 1) psi1
        notDefaulting = ss' ^. dontDefault
        psiFinal = M.filterWithKey (\x _ -> S.notMember x notDefaulting) psi1'
        (beta, w) = traceShow2def "Def: " $ defaulting $ M.toList psiFinal

sortConstraints :: Constraints -> (Constraints, Constraints, Constraints)
sortConstraints = traceShowS "Sorting:" . sortConstraints'
  where 
        sortConstraints' [] = ([],[],[])
        sortConstraints' (c:xs) = case c of
          Constraint_Eq{} -> (c:ec,gcs,cs) 
          Constraint_Ann{} -> (c:ec, gcs,cs) 
          Constraint_Gen{} -> (ec, c:gcs,cs) 
          Constraint_Inst{} -> (ec, gcs,c:cs) 
          where (ec, gcs, cs) = sortConstraints' xs

solveFinalSchemes :: SolveState -> Solution
solveFinalSchemes = evalState solveFinalSchemes'

solveFinalSchemes' :: SolveT Solution
solveFinalSchemes' = traceShowT "solveFinal:" $ do
  s <- use solution 
  s' <- traceShowS s $ mapM solveFinalScheme $ s ^. schemeSol
  solution %= \x -> x & schemeSol .~ s'
  traceShow "solve ready" $ use solution

solveFinalScheme :: Scheme -> SolveT Scheme
solveFinalScheme s@(Scheme_Forall as ts cs t) = traceShow ("finalSub") $ do
  cs' <- solveFixMulti cs
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