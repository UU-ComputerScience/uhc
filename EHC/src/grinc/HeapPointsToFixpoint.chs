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

%%]

%%[8.heapChange

heapChange :: HeapEquation -> AbstractEnv s -> ST s (Location,AbstractValue)
heapChange (WillStore locat tag args) env 
 = do  { let mbres       =   tagFun tag
       ; absArgs         <-  mapM getEnv args
       ; absRes          <-  getEnv mbres
       ; absExc          <-  getEnv (mbres >>= return . (+1))
       ; let absNode     =   AbsNodes (Map.singleton tag absArgs)
       ; let exceptNode  =   case absExc of
                               AbsBottom                 -> AbsBottom
                               AbsLocs ls | Set.null ls  -> AbsBottom
                               otherwise                 -> AbsNodes (Map.singleton throwTag [AbsBasic, absExc])
       ; return (locat, absNode `mappend` absRes `mappend` exceptNode)
       }
       where
       getEnv Nothing   =  return AbsBottom
       getEnv (Just v)  =  readArray env v
       tagFun (GrTag_Fun nm)  =  Just (getNr nm)
       tagFun _               =  Nothing
%%]

%%[8.envChanges

isPAppTag :: GrTag -> Bool
isPAppTag (GrTag_PApp _ _) = True
isPAppTag _                = False

isFinalTag :: GrTag -> Bool
isFinalTag  GrTag_Any        = True
isFinalTag  GrTag_Unboxed    = True
isFinalTag (GrTag_PApp _ _)  = True
isFinalTag (GrTag_Con _ _ _) = True
isFinalTag _                 = False


filterTaggedNodes :: (GrTag->Bool) -> AbstractValue -> AbstractValue
filterTaggedNodes p (AbsNodes nodes) = let newNodes = Map.filterWithKey (const . p) nodes
                                       in -- if Map.null newNodes then AbsBottom else 
                                          AbsNodes newNodes
filterTaggedNodes p av               = av




envChanges :: Equation -> AbstractEnv s -> AbstractHeap s -> ST s [(Variable,AbstractValue)]
envChanges equat env heap
  = case equat of
      IsKnown         d av         -> return [(d, av)]

      IsEqual         d v          -> do
                                      {  av <- readArray env v
                                      ;  return [(d, av)]
                                      }
      IsSelection     d v i t      -> do
                                      {  av <- readArray env v
                                      ;  let res = absSelect av i t
                                      ;  return [(d,res)]
                                      }
      IsConstruction  d t as   ev  -> do
                                      {  vars <- mapM (maybe (return AbsBasic) (readArray env)) as
                                      ;  let res = AbsNodes (Map.singleton t vars)
                                      -- ;  res <- maybe (return res) (\v -> readArray env v >>= return . mappend res) ev
                                      ;  return [(d,res)]
                                      }
      IsEvaluation    d v      ev  -> do
                                      {  av <- readArray env v
                                      ;  res <- absDeref av
                                      ;  return [(d,res)]
                                      }
      IsApplication mbd (f:as) ev  -> do 
                                      {  av         <-  readArray env f
                                      ;  absFun     <-  case mbd of
                                                         Nothing  -> absDeref av
                                                         Just _   -> return av
                                      ;  absArgs    <-  mapM (readArray env) as
                                      ;  (sfx,res)  <-  absCall absFun absArgs ev
                                      ;  case mbd of
                                           Nothing  ->  return sfx
                                           Just d   ->  return ((d,res):sfx)
                                      }
      
    where
    absSelect av i t
     = case av of
         AbsNodes  ns  -> maybe AbsBottom (!!i) (Map.lookup t ns)
         AbsBottom     -> av
         AbsError _    -> av
         _             -> AbsError "Variable passed to eval is not a node"
    absDeref av
      = case av of
          AbsLocs ls  ->  do { vs <- mapM (readArray heap) (Set.toList ls)
                             ; return (mconcat (map (filterTaggedNodes isFinalTag) vs))
                             }
          AbsBottom   ->  return av
          AbsError _  ->  return av
          _           ->  return $ AbsError "Variable passed to eval is not a location"
                                   
    absCall f args ev
      = do { ts <- mapM addArgs (getNodes (filterTaggedNodes isPAppTag f))
           ; let (sfxs,avs) = unzip ts
           ; return (concat sfxs, mconcat avs)
      	   }
      where addArgs (tag@(GrTag_PApp needs nm) , oldArgs) 
              = do { let n        = length args
                         newtag   = GrTag_PApp (needs-n) nm
                         funnr    = getNr nm
                         sfx      = zip  [funnr+2+length oldArgs ..] args
                   ; res <-  if    n<needs
                             then  return $ AbsNodes (Map.singleton newtag (oldArgs++args))
                             else  readArray env funnr
                   ; exc <-  if    n<needs
                             then  return AbsBottom
                             else  readArray env (funnr+1)
                   ; let excfx = (ev, exc)  
                   ; return (excfx:sfx, res)
                   }
%%]

%%[8

fixpoint eqs1 eqs2 proc1 proc2 
  = countFixpoint 0
    where
    countFixpoint count = do
        { let doStep1 b i = proc1 i >>= return . (b||)
        ; let doStep2 b i = proc2 i >>= return . (b||)
        ; changes1 <- foldM doStep1 False eqs1
        ; changes2 <- foldM doStep2 False eqs2
        ; if    trace ("fixpoint step " ++ show count) ((changes1 || changes2) && count<10)
          then  countFixpoint (count+1)
          else  return count
        }

procChange arr (i,e1) =
   do { e0 <- readArray arr i
      ; let e2       =  e0 `mappend` e1
            changed  =  e0 /= e2
      ; when changed (writeArray arr i e2)
      ; return changed
      }

solveEquations :: Int -> Int -> Equations -> HeapEquations -> (Int,HptMap)
solveEquations lenEnv lenHeap eqs1 eqs2 =
    runST (
    do { 
   	    -- create arrays
       ; env     <- newArray (0, lenEnv   - 1) AbsBottom
       ; heap    <- newArray (0, lenHeap  - 1) AbsBottom

       ; let procEnv equat
                = do
                  { cs <- envChanges equat env heap
                  ; bs <- mapM (procChange env) cs
                  ; return (or bs)
                  }
             procHeap equat
                = do
                  { cs <- heapChange equat env
                  ; b  <- procChange heap cs
                  ; return b
                  }
       ; count <- fixpoint eqs1 eqs2 procEnv procHeap
      
       ; absHeap <- unsafeFreeze heap
       ; absEnv  <- unsafeFreeze env
    
       ; trace (unlines ("EQUATIONS"     : map show eqs1))        $ return ()
       ; trace (unlines ("SOLUTION"      : map show (assocs absEnv)))  $ return ()
       ; trace (unlines ("HEAPEQUATIONS" : map show eqs2))          $ return ()
       ; trace (unlines ("HEAPSOLUTION"  : map show (assocs absHeap))) $ return ()
       
       ; return (count, (absEnv, absHeap, Map.empty))
       }
       )

%%]
