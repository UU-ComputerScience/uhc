{-# OPTIONS -fglasgow-exts  #-}
module Main (entTest, simplTest, scopeTest, scopeTest2, scopeTest3, scopeTest4, scopeTest5, fundepTest, scopeTest6)
where

import CHRSolver

import Solver
import Heuristics

import ExampleTypeSystem
import HaskellTranslation
import ScopedTranslation

-- Predicates
basicEqPreds  = map eqP  basicTypes
basicOrdPreds = map ordP basicTypes
basicNumPreds = map numP basicTypes
basicInstances = basicEqPreds ++ basicOrdPreds ++ basicNumPreds

-----------------------------------------------------------------------------
-- Test class/instance declarations
-----------------------------------------------------------------------------
eqClass       = ([]                  , eqP v1      , [])
showClass     = ([]                  , showP v1    , [])
enumClass     = ([]                  , enumP v1    , [])
ordClass      = ([eqP v1]            , ordP v1     , [BySuperClass "eqOrd"])
numClass      = ([eqP v1, showP v1]  , numP v1     , [BySuperClass "eqNum"       , BySuperClass "showNum"])
realClass     = ([ordP v1, numP v1]  , realP v1    , [BySuperClass "ordReal"     , BySuperClass "numReal"])
integralClass = ([enumP v1, realP v1], integralP v1, [BySuperClass "enumIntegral", BySuperClass "realIntegral"])

classDecls    = [eqClass, showClass, enumClass, ordClass, numClass, realClass, integralClass]

cyclicClasses = [([ordP v1], eqP v1, [BySuperClass "eqOrd"]), ([eqP v1], ordP v1, [BySuperClass "eqOrd"])]

-- testinstances
basicInsts f c = map (\t -> ([], f t, ByInstance (c ++ (show t)) (f t) global)) basicTypes
listInst   f c = ([f v1], f (list v1), ByInstance (c ++ (show (list v1))) ( f (list v1)) global)
tupleInst  f c = ([f v1, f v2], f (tuple v1 v2), ByInstance (c ++ (show (tuple v1 v2))) (f (tuple v1 v2)) global)

eqInst   = listInst eqP   "eq"   : tupleInst eqP   "eq"   : basicInsts eqP   "eq"
ordInst  = listInst ordP  "ord"  : tupleInst ordP  "ord"  : basicInsts ordP  "ord"
showInst = listInst showP "show" : tupleInst showP "show" : basicInsts showP "show"
numInst  = listInst numP  "num"  : tupleInst numP  "num"  : basicInsts numP  "num"
realInst = listInst realP "real" : tupleInst realP "real" : basicInsts realP "real"

instances = ordInst ++ eqInst ++ showInst ++ numInst ++ realInst
           -- overlapping  
           ++ [([], eqP (list (list v1)), ByInstance "overlapping"  (eqP (list (list v1))) global)]

stdEnv = genChrs classDecls instances
-----------------------------------------------------------------------------
-- Test heuristics
-----------------------------------------------------------------------------                          
-----------------------------------------------------------------------------
--  Heursitic emulating Haskell~98 context reduction
-----------------------------------------------------------------------------
haskell98 :: Annotation -> Annotation -> Ordering
haskell98 (ByInstance _ _  _)  _                    =  GT
haskell98 _                    (ByInstance _ _  _)  =  LT
haskell98 (BySuperClass    _)  _                    =  GT
haskell98 _                    (BySuperClass    _)  =  LT
haskell98 (Assumption      _)  _                    =  GT
haskell98 _                    (Assumption      _)  =  LT

h98Heuristic = toHeuristic $ binChoice haskell98

-----------------------------------------------------------------------------
--  Heursitic emulating GHC context reduction with overlapping instances
-----------------------------------------------------------------------------
ghcBinSolve :: Annotation -> Annotation -> Ordering
ghcBinSolve (Assumption      _)   _                   = GT
ghcBinSolve _                     (Assumption    _)   = LT
ghcBinSolve (BySuperClass    _)   _                   = GT
ghcBinSolve _                     (BySuperClass  _)   = LT
ghcBinSolve (ByInstance _ p  _)   (ByInstance _ q _)  = specificness p q
ghcBinSolve (ByInstance _ _  _)   _                   = GT
ghcBinSolve _                     (ByInstance _ _ _)  = LT

ghcSolve :: Eq p => SHeuristic p Annotation
ghcSolve = binChoice ghcBinSolve

ghcLocalReduce :: a -> [Annotation] -> [Annotation]
ghcLocalReduce _  reds =  let  p (BySuperClass _)  = True
                               p _                 = False
                          in   filter p reds

ghcReduce :: Eq p => SHeuristic p Annotation
ghcReduce = localChoice ghcLocalReduce

ghcHeuristic :: Eq p => Heuristic p Annotation
ghcHeuristic =  toHeuristic $
                  try  ghcSolve
                       ghcReduce

specificness :: Matchable c s => c -> c -> Ordering
specificness p q = 
  case  match p q of 
    Nothing  -> LT  
    Just _   -> case  match q p of
                  Nothing  -> GT
                  Just _   -> error "no most specific instance"
                       
-----------------------------------------------------------------------------
--   Backtracking heuristic
-----------------------------------------------------------------------------
btHeuristic = toHeuristic $ toEvidence solvable

-----------------------------------------------------------------------------
--   Heuristic for scoped predicates
-----------------------------------------------------------------------------
{--
ehclocal (ByInstance _ p  s)  (ByInstance _ q  t)  =  Just (cmpScope s t) 
ehclocal (ByInstance _ _  _)  _                    =  Just GT
ehclocal _                    (ByInstance _ _  _)  =  Just LT
ehclocal (BySuperClass    _)  _                    =  Just GT
ehclocal _                    (BySuperClass    _)  =  Just LT
ehclocal (Assumption      _)  _                    =  Just GT
ehclocal _                    _                    =  Just GT
--}

ehc :: Red ScopedPred Annotation -> Red ScopedPred Annotation -> Ordering
ehc (Red (ByInstance _ _  s) _)  (Red (ByInstance _ _  t) _)  =  cmpScope s t 
ehc (Red (ByInstance _ _  _) _)  _                            =  GT
ehc _                            (Red (ByInstance _ _  _) _)  =  LT
ehc (Red (BySuperClass    _) _)  _                            =  GT
ehc _                            (Red (BySuperClass    _) _)  =  LT
ehc (Red (Assumption      _) _)  _                            =  GT
ehc _                            (Red (Assumption      _) _)  =  LT
ehc (Red ByScope [Alts p _])     (Red ByScope [Alts q _])     =  cmpPred p q

cmpPred (SPred _ (Val s)) (SPred _ (Val t)) = cmpScope s t

cmpScope (Scope s) (Scope t) = (length s) `compare` (length t)  

scopeHeuristic =  toHeuristic $ contextBinChoice ehc 

initState  = mkState stdEnv h98Heuristic

-----------------------------------------------------------------------------
--   Simplification test
-----------------------------------------------------------------------------
listlistlisteq = Prove $ eqP   (list (list (list v3))) 
listlistnum    = Prove $ realP (list (list v3))            
groundProofs   = map Prove basicInstances

proves = zipWith  (\x i -> (x, ProveObl i)) (listlistnum:listlistlisteq:groundProofs) [1 ..]
 
simplTest = solve proves initState

-----------------------------------------------------------------------------
--   Entailment / Simplification test
-----------------------------------------------------------------------------
listeqc1 = (Prove  $ eqP  (list c1) , ProveObl 1)
numc1ass = (Assume $ numP c1        , Assumption 2)

entTest = solve (numc1ass : [listeqc1]) initState

-----------------------------------------------------------------------------
-------      SCOPING      
-----------------------------------------------------------------------------
-- Stepping through Haskell p. 144:
--
-- let instance Eq Int where ...
--     instance Eq a => Eq [a] where ...
--     g = \x y -> x == y
--  in let v1 = g [3] [4]
--         v2 = let instance Eq Int where ...
--              in g [3] [4]
--  in (v1, v2)
-----------------------------------------------------------------------------

global = Scope []

initScopeState  = mkState 
                   (scopedTrans1 classDecls (map (\(a,b,c) -> (a,b,c,global)) instances)) 
                   scopeHeuristic

scopeTest = solve constrns initScopeState
  where constrns   =  [ (Assume $ mkSPred (ordP c1) [1]            , Assumption 3)
                      , (Prove  $ mkSPred (eqP  . list $ c1) [1,1], ProveObl 1)
                      , (Prove  $ mkSPred (ordP . list $ c1) [1,2], ProveObl 2)]

initScopeState2  = mkState 
                   (scopedTrans2 classDecls (map (\(a,b,c) -> (a,b,c,global)) instances)) 
                   scopeHeuristic
  
scopeTest2 = solve constrns initScopeState2
  where constrns   =  [ (Assume $ mkSPred (eqP v2) [1]            , Assumption 3)
                      , (Prove  $ mkSPred (eqP . list $ v2) [1,1], ProveObl 1)
                      , (Prove  $ mkSPred (eqP . list $ v2) [1,2], ProveObl 2)]  
                      
scopeTest3 = solve constrns initScopeState2
  where constrns   =  [ (Assume $ mkSPred (ordP c1) [1]            , Assumption 3)
                      , (Prove  $ mkSPred (eqP  . list $ c1) [1,1], ProveObl 1)
                      , (Prove  $ mkSPred (ordP . list $ c1) [1,2], ProveObl 2)]

scopeTest4 = solve constrns initScopeState2
  where constrns   =  [ (Assume $ mkSPred (ordP v2) [1]            , Assumption 3)
                      , (Prove  $ mkSPred (ordP . list $ v2) [1,1,1], ProveObl 1)
                      , (Prove  $ mkSPred (eqP  . list $ v2) [1,1,2], ProveObl 2)]

scopeTest5 = solve constrns initScopeState2
  where constrns   =  [ (Assume $ mkSPred (eqP c1) [1]      , Assumption 3)
                      , (Prove  $ mkSPred (eqP c1) [1,1]    , ProveObl 1)
                      , (Prove  $ mkSPred (eqP c1) [1,1,1]  , ProveObl 2)]
                      
scopeTest6 = solve constrns initScopeState2
  where constrns   =  [ (Prove $ mkSPred (eqP v2) [1],      ProveObl 1)
                      , (Prove $ mkSPred (eqP v2) [1,1],    ProveObl 2)
                      , (Prove $ mkSPred (eqP v2) [1,1,2],  ProveObl 3)]
                                                                                                                                                      
                                                                
-----------------------------------------------------------------------------
-------      Functional depenencies      
-----------------------------------------------------------------------------
-- class Coll c e | c -> e

collection = Predicate "Collection"

colFundepRule, colListIntRule, colListImpRule :: Rule Predicate (Subst Type) Annotation 
colFundepRule = [Prove (collection [v1, v2]), Prove (collection [v1, v3])] 
                ==> [Prove (v2 :=: v3)]
                       
-- instance Ord a => Coll [a] a
colListIntRule = 
  let  colpred = collection [list v1, v1]
       ordPred = ordP v1
  in   [Prove colpred] ==> [Prove ordPred, Reduction colpred (ByInstance "instCol" colpred global)  [ordPred]]

colListImpRule  = 
  [Prove (collection [list v1, v2])] ==> [Prove (v1 :=: v2)]

colState  = mkImprState [colListIntRule] [colFundepRule, colListImpRule]  h98Heuristic

fundepTest = solveImpr unifyEqPred constrns colState
  where constrns   =  [ (Prove $ collection [v4, v5]           , ProveObl 1)
                      , (Prove $ collection [v4, v6]           , ProveObl 2)
                      ]

unifyEqPred :: Predicate -> Maybe (Subst Type)
unifyEqPred (t1 :=: t2)  = mgu t1 t2
unifyEqPred _            = Nothing
