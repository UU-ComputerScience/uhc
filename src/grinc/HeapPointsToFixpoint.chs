%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[8 module {%{GRIN}HeapPointsToFixpoint}
%%]
%%[8 export(solveEquations)
%%]
%%[8 import(Data.Maybe, Data.Either, Data.List, Data.Ix, Data.Monoid, Data.Array.ST, Data.Array.IArray, Control.Monad.ST, Control.Monad)
%%]
%%[8 import(qualified Data.Set as Set, qualified Data.Map as Map)
%%]
%%[8 import({%{EH}Base.Common}, {%{EH}GrinCode})
%%]
%%[8 import({%{GRIN}GRINCCommon})
%%]
%%[8 import(Debug.Trace)
%%]

%%[8.updateEnvHeap

type AbstractEnv s  = STArray s Variable AbstractValue
type AbstractHeap s = STArray s Location AbstractValue

-- The equations of the element are fed to envChangeSet
-- and the output is merged to the baseset
updateEnvElement :: EquationRhs -> AbstractValue -> AbstractEnv s -> AbstractHeap s -> ApplyMap -> Int -> ST s AbstractValue
updateEnvElement em ev env heap applyMap applyMapStartIndex = do
    { newChangeSet <- envChangeSet em env heap applyMap applyMapStartIndex
    ; return (ev `mappend` newChangeSet)
    }

updateHeapElement :: HeapEquationRhs -> AbstractValue -> AbstractEnv s -> ST s AbstractValue
updateHeapElement hm hv env 
  = do
    {  -- The equations of the element are fed to heapChangeSet
       (baseChange, sharedChange) <- heapChangeSet hm env
       -- ignore the difference between basepart and sharedpart, just merge everything
    ; return (hv `mappend` baseChange `mappend` sharedChange)
{-
      -- the first part of the output is merged to the baseset
      -- the other part of the output is merged to the sharedset if it already exists, otherwise to the baseset
    ; let  baseOld      = ahBaseSet   hv
           mbShareOld   = ahSharedSet hv
           (baseNew, sharedNew) = case mbShareOld of
                                    Nothing       -> (baseOld `mappend` baseChange `mappend` sharedChange, Nothing                                )
                                    Just shareOld -> (baseOld `mappend` baseChange                       , Just (shareOld `mappend` sharedChange) )
    ; return (he { ahBaseSet = baseNew, ahSharedSet = sharedNew })
-}
    }

%%]

%%[8.sharingAnalysis

addSharingInfo :: AbstractEnv s -> AbstractHeap s -> ST s ()
addSharingInfo env heap = return ()

{-
-- For all elements of the environment
--   if the sharing flag is up,
--     for all heap-locations it contains (if any),
--       the sharedset is merged to the baseset
--
-- in other words:
-- The sharedset is merged to the baseset for those heap-locations
-- which occur in the AV_Locations of an environmentElement with its sharing flag set

addSharingInfo env heap = getElems env >>= mapM_ (setSharingInfo heap)

setSharingInfo heap v = case v of
                           AV_Locations ls  ->  when (True) (mapM_ (setShared heap) (Set.toList ls))
                           otherwise        ->  return ()

setShared heap l = do
    { he <- lookupHeap heap l
    ; maybe (return ())  (\s -> do { let  baseSet  =  ahBaseSet he `mappend` s
                                          newElem  =  he { ahBaseSet = baseSet
                                                         , ahSharedSet = Nothing
                                                         }
                                   ; writeArray heap l newElem
                                   }
                         )
                         (ahSharedSet he)
    }
-}

%%]

%%[8.heapChangeSet
heapChangeSet :: HeapEquationRhs -> AbstractEnv s -> ST s (AbstractValue, AbstractValue)
heapChangeSet ((tag, deps), resultDep) env = do
    { locs        <- mapM getBaseSet deps
    ; resCS       <- getBaseSet resultDep
    ; exceptPtrs  <- getBaseSet (resultDep >>= return . (+1))
    ; let exceptNode  =  case exceptPtrs of
                            AV_Nothing                   -> AV_Nothing
                            AV_Locations l | Set.null l  -> AV_Nothing
                            otherwise                    -> AV_Nodes (Map.singleton throwTag [AV_Basic, exceptPtrs])
          uniqueAV    =  AV_Nodes (Map.singleton tag locs)
          sharedAV    =  exceptNode `mappend` resCS
    ; return (uniqueAV, sharedAV)
    }
    where
    getBaseSet    =  maybe (return AV_Nothing) (\v -> lookupEnv env v >>= return )
%%]

%%[8.envChangeSet


isPApp (GrTag_Lit (GrTagPApp _) _ _) = True
isPApp _                             = False

envChangeSet :: EquationRhs -> AbstractEnv s -> AbstractHeap s -> ApplyMap -> Int -> ST s AbstractValue
envChangeSet am env heap applyMap applyMapStartIndex
  = case am of
      EquationKnownToBe av     -> return av
      EquationShouldBe  vs     -> do
                                  {  rs <- mapM valAbsEnv vs
                                  ;  return (mconcat rs)
                                  }
      EquationEval      v ev   -> do
                                  {  p <- valAbsEnv v
                                  ;  evalChangeSet ev p
                                  }
      EquationApply     f a ev -> do 
                                  { pnodes  <- valAbsEnv f
                                  ; argsVal <- mapM valAbsEnv a
                                  ; applyChangeSet pnodes argsVal ev
                                  }
      EquationSelect    v n i  -> do
                                  {  p <- valAbsEnv v
                                  ;  return (selectChangeSet n i p)
                                  }
      EquationTag       t f r  -> tagChangeSet t f r
    where
    --valAbsEnv :: Variable -> ST s AbstractValue
    valAbsEnv v = do
        { elem <- lookupEnv env v
        ; return elem
        }
    --valAbsHeap :: Location -> ST s (AbstractValue, AbstractValue)
    valAbsHeap l = do
        { elem <- lookupHeap heap l
        ; return (elem, Nothing)
        --; let resultVar = snd (ahMod elem)
        --; exceptions <- maybe (return Nothing) (\v -> valAbsEnv (v+1) >>= return . Just) resultVar
        --; return (ahBaseSet elem `mappend` maybe AV_Nothing id (ahSharedSet elem), exceptions)
        }
    evalFilter (AV_Nodes nodes) = let isValueTag t = case t of
                                                         GrTag_Any          -> True
                                                         GrTag_Unboxed      -> True
                                                         GrTag_Lit cat _ _  -> case cat of
                                                                                   GrTagCon     -> True
                                                                                   GrTagPApp _  -> True
                                                                                   GrTagApp     -> False
                                                                                   GrTagFun     -> False
                                                                                   GrTagHole    -> False
                                                                                   GrTagRec     -> False
                                                         GrTag_Var _        -> error $ "tag variable unexpected: " ++ show t
                                      isValueNode t f   = isValueTag t
                                      newNodes          = Map.filterWithKey isValueNode nodes
                                  in if Map.null newNodes then AV_Nothing else AV_Nodes newNodes
    evalFilter av               = av
    --evalChangeSet :: AbstractValue -> ST s AbstractValue
    evalChangeSet exceptVar av = case av of
                                   AV_Nothing      -> return av
                                   AV_Locations ls -> do { res <- mapM valAbsHeap (Set.toList ls)
                                                         ; let (vs,es)  = unzip res
                                                               v        = mconcat (map evalFilter vs)
                                                               e        = mconcat [ x | (Just x) <- es ]
                                                         ; exceptions <- readArray env exceptVar
                                                         ; writeArray env exceptVar (e `mappend` exceptions)                                                         
                                                         ; return v
                                                         }
                                   AV_Error _      -> return av
                                   otherwise       -> return $ AV_Error "Variable passed to eval is not a location"
    selectChangeSet :: GrTag -> Int -> AbstractValue -> AbstractValue
    selectChangeSet nm idx av = case av of
                                  AV_Nothing    -> av
                                  AV_Nodes   ns -> maybe AV_Nothing (!! idx) (Map.lookup nm ns)
                                  AV_Error _    -> av
                                  otherwise     -> AV_Error "Variable passed to eval is not a node"
    --tagChangeSet :: GrTag -> [Maybe Variable] -> (Maybe Variable) -> ST s AbstractValue
    tagChangeSet t flds r = do { vars <- mapM (maybe (return AV_Basic) valAbsEnv) flds
                               ; when (isPApp t) ( do { let tagindex = applyMapStartIndex + fromJust (elemIndex t (assocLKeys applyMap))
                                                            tups = zip [tagindex+1..] vars
                                                      ; mapM (\(i,v)->writeArray env i v) (reverse tups)
                                                      ; return ()
                                                      }
                                                 )
                               ; let newNodes = AV_Nodes (Map.singleton t vars)
                               ; maybe (return newNodes) (\v -> valAbsEnv v >>= return . mappend newNodes) r
                               }
                               
    --applyChangeSet :: AbstractValue -> [AbstractValue] -> ST s AbstractValue
    applyChangeSet f argsVal exceptVar = foldM applyChangeSet1 f argsVal
        where
        --applyChangeSet1 :: AbstractValue -> AbstractValue -> ST s AbstractValue
        applyChangeSet1 f arg = let partialApplicationNodes = [ node | node@((GrTag_Lit (GrTagPApp _) _ _), _) <- getNodes f ]
                                    --getNewNode :: GrTag -> [AbstractValue] -> ST s AbstractValue
                                    getNewNode tag args       = let tagindex = applyMapStartIndex + fromJust (elemIndex tag (assocLKeys applyMap))
                                                                    newArgs = args ++ [arg]
                                                                    partialF tag2 = return $ AV_Nodes (Map.singleton tag2 newArgs)
                                                                    saturatedF var = do { exceptions <- readArray env exceptVar
                                                                                        ; e <- valAbsEnv (var+1)
                                                                                        ; writeArray env exceptVar (e `mappend` exceptions)                                                         
                                                                    	                ; valAbsEnv var
                                                                    	                }
                                    
                                                                in  do { writeArray env tagindex arg
                                                                       -- ; trace (show tagindex ++ " also changed to " ++ show arg) $ return ()
                                                                       ; either partialF saturatedF
                                                                                (fromJust $ lookup tag applyMap)
                                                                       }
                                in mapM (uncurry getNewNode) partialApplicationNodes >>= return . mconcat



%%]

%%[8 ghc (6.6,_)
--abstractBounds :: Ix i => STArray s i a -> ST s (i, i)
--abstractBounds = getBounds

--abstractIndices :: Ix i => STArray s i a -> ST s [i]
--abstractIndices a = getBounds a >>= return . range
%%]
%%[8 ghc (_,6.4.2)
--abstractBounds :: Ix i => STArray s i a -> ST s (i, i)
--abstractBounds = return . bounds

--abstractIndices :: Ix i => STArray s i a -> ST s [i]
--abstractIndices = return . indices
%%]

%%[8
lookupEnv :: AbstractEnv s -> Variable -> ST s AbstractValue
lookupEnv env idx = readArray env idx

lookupHeap :: AbstractHeap s -> Location -> ST s AbstractValue
lookupHeap heap idx = readArray heap idx

%%]

%%[8.fixpoint

fixpoint equations heapEqs procEnv procHeap 
  = countFixpoint 1
    where
    countFixpoint count = do
        { let doStepEnv  b i = procEnv  i >>= return . (b||)
        ; let doStepHeap b i = procHeap i >>= return . (b||)
        ; changesEnv  <- foldM doStepEnv  False equations
        ; changesHeap <- foldM doStepHeap False heapEqs
        ; if changesEnv || changesHeap
          then countFixpoint (count+1)
          else return count
        }
%%]

%%[8
solveEquations :: Int -> Int -> Equations -> HeapEquations -> ApplyMap -> (Int,HptMap)
solveEquations unique lenHeap equations heapEqs applyMap =
    runST (
    do { 
   	    -- create arrays
       ; env     <- newArray (0, unique + length applyMap - 1) AV_Nothing
       ; heap    <- newArray (0, lenHeap                  - 1) AV_Nothing

       ; let procEnv (i,em) 
                = do
                  { e  <- lookupEnv env i
                  ; e2 <- updateEnvElement em e env heap applyMap unique
                  ; let changed =  e /= e2
                  ; when changed (   -- trace (show i ++ " changed from " ++ show e ++ " to " ++ show e2) $
                                     writeArray env i e2
                                 )
                  -- ; when (not changed) (trace (show i ++ " not changed") $ return ())
                  ; return changed
                  }
             procHeap (i,hm) 
                = do
                  { e  <- lookupHeap heap i
                  ; e2 <- updateHeapElement hm e env
                  ; let changed =  e /= e2   -- ahBaseSet e /= ahBaseSet e2 || ahSharedSet e /= ahSharedSet e2
                  ; when changed (writeArray heap i e2)
                  ; return changed
                  }
       ; count <- fixpoint equations heapEqs procEnv procHeap
       ; addSharingInfo env heap
       
       ; absHeap <- unsafeFreeze heap
       ; absEnv  <- unsafeFreeze env
    
       --; trace (unlines ("APPLYMAP"      : map show applyMap))         $ return ()
       --; trace (unlines ("EQUATIONS"     : map show equations))        $ return ()
       --; trace (unlines ("SOLUTION"      : map show (assocs absEnv)))  $ return ()
       --; trace (unlines ("HEAPEQUATIONS" : map show heapEqs))          $ return ()
       --; trace (unlines ("HEAPSOLUTION"  : map show (assocs absHeap))) $ return ()
       
       ; return (count, (absEnv, absHeap, Map.empty))
       }
       )

%%]
