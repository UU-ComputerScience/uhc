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
                               AbsBottom                   -> AbsBottom
                               AbsLocs ls m | Set.null ls  -> AbsBottom
                               otherwise                   -> AbsNodes (Map.singleton throwTag [AbsBasic, absExc])
       ; return (locat, absNode `mappend` absRes `mappend` exceptNode)
       }
       where
       getEnv Nothing   =  return AbsBottom
       getEnv (Just v)  =  readArray env v
       tagFun (GrTag_Fun nm)  =  Just (getNr nm)     -- track overwrite results of Fun
       tagFun (GrTag_App nm)  =  Just (getNr nm)     -- also track overwrite results of App
       tagFun _               =  Nothing
heapChange (WillEquate locat var) env 
 = do  { absNode         <- readArray env var
       ; return  (locat, absNode)
       }
    
%%]

%%[8.envChanges

isPAppTag :: GrTag -> Bool
isPAppTag (GrTag_PApp _ _) = True
isPAppTag _                = False

isFinalTag :: GrTag -> Bool
isFinalTag  GrTag_Any        = True
isFinalTag  GrTag_Hole       = True
isFinalTag  GrTag_Unboxed    = True
isFinalTag (GrTag_PApp _ _)  = True
isFinalTag (GrTag_Con _ _ _) = True
isFinalTag _                 = False

isApplyTag (GrTag_App _)     = True
isApplyTag _                 = False


filterTaggedNodes :: (GrTag->Bool) -> AbstractValue -> AbstractValue
filterTaggedNodes p (AbsNodes nodes) = let newNodes = Map.filterWithKey (const . p) nodes
                                       in -- if Map.null newNodes then AbsBottom else 
                                          AbsNodes newNodes
filterTaggedNodes p av               = av


getApplyNodeVars :: AbstractValue -> [ Variable ]
getApplyNodeVars (AbsNodes nodes) = map tag2var (Map.keys (Map.filterWithKey (const . isApplyTag) nodes))
getApplyNodeVars _ = []

tag2var :: GrTag -> Variable
tag2var (GrTag_App nm) = getNr nm
tag2var _              = error "tag2var on non-App"

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
      IsEnumeration   d v          -> do
                                      {  av <- readArray env v
                                      ;  return [(d, absEnum av)] 	
                                      }
      IsEvaluation    d v      ev  -> do
                                      {  av <- readArray env v
                                      --;  _ <- trace ("absEval " ++ show equat) (return ())
                                      ;  res <- absEval av
                                      --;  _ <- trace ("res " ++ show res) (return ())
                                      ;  return [(d,res)]
                                      }
      IsApplication   d vs     ev  -> do 
                                      {  (absFun:absArgs)  <-  mapM (readArray env) vs
                                      ;  (sfx,res)         <-  absApply absFun absArgs (Just ev)
                                      ;  return ((d,res):sfx)
                                      }
      
    where
    absSelect av i t
     = case av of
         AbsNodes  ns  -> maybe AbsBottom (\xs -> if i<length xs then xs!!i else error ("cannot analyze FFI's returning non-nullary constructors " ++ show ns)) (Map.lookup t ns)
         AbsBottom     -> av
         AbsError _    -> av
         _             -> AbsError ("cannot select " ++ show i ++ " from " ++ show av)
         
    
    --absDeref :: AbstractValue -> ST s AbstractValue  
    absDeref av
      = case av of
          AbsLocs ls m -> do { vs <- mapM (readArray heap) (Set.toList ls)
                             ; return (mconcat vs)
                             }
          _            ->  return av

    absEnum av
      = case av of
          AbsTags ts   -> AbsNodes (Map.fromList [ (t,[]) |  t <- Set.toList ts ])
          _            -> AbsError ("cannot enumerate " ++ show av)

    --absEval :: AbstractValue -> ST s AbstractValue  
    absEval av
      = case av of
          AbsLocs ls m -> do { vs <- mapM (readArray heap) (Set.toList ls)
                             ; rs <- findFinalValues vs
                             ; return rs
                             }
          AbsBottom   ->  return av
          AbsError _  ->  return av
          _           ->  return $ AbsError ("Variable passed to eval is not a location: " ++ show av)

    --findFinalValues :: [AbstractValue] -> ST s [AbstractValue]
    findFinalValues []
      = return AbsBottom
    findFinalValues vs
      = do { let xs :: [AbstractValue]
                 xs = map (filterTaggedNodes isFinalTag) vs
           ; let ns :: [Variable]
                 ns = concat (map getApplyNodeVars vs)
           ; zs <- mapM (readArray env) ns
           ; ws <- findFinalValues zs
           ; return (mconcat (ws:xs))
           }

    --absApply :: AbstractValue -> [AbstractValue] -> Variable -> ST s ([(Variable,AbstractValue)],AbstractValue)
    absApply f args mbev
      = do { let as = getNodes (filterTaggedNodes isPAppTag f)
           ; ts <- mapM addArgs as
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
                   ; if    n>needs
                     then  do 
                           { (sfx2,res2) <- absApply res (drop needs args) mbev
                           ; return (take needs sfx ++ sfx2, res2) 
                           }
                     else  -- trace ("sfx=" ++ show sfx ++ " res=" ++ show res) $
                           return (maybe (sfx, res)
                                         (\ev -> ((ev,exc):sfx, res))
                                         mbev
                                  )
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
        -- ; _ <- trace ("fix " ++ show count) (return ())
        ; if    (changes1 || changes2) && count<100
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


--moniEqua (IsEvaluation d _ _) = d==5230
--moniEqua (IsApplication d _ _) = d==788
moniEqua _ = True

solveEquations :: Int -> Int -> Equations -> HeapEquations -> Limitations -> (Int,HptMap)
solveEquations lenEnv lenHeap eqs1 eqs2 lims =
    runST (
    do { 
       --; trace (unlines ("EQUATIONS"     : map show eqs1)) $ return ()
       --; trace (unlines ("HEAPEQUATIONS" : map show eqs2)) $ return ()
       --; trace (unlines ("LIMITATIONS"   : map show lims)) $ return ()

       -- create arrays
       ; env     <- newArray (0, lenEnv   - 1) AbsBottom
       ; heap    <- newArray (0, lenHeap  - 1) AbsBottom

       ; let procEnv equat
                = do
                  { 
                  --ah <- getAssocs heap
                  --; ae  <- getAssocs env
                  --; _ <- (if moniEqua equat then trace (unlines ("SOLUTION"      : map show ae)) else id)  $ return ()
                  --; _ <- (if moniEqua equat then trace (unlines ("HEAPSOLUTION"  : map show ah)) else id)  $ return ()
                  --; _ <- trace ("equat " ++ show equat) (return ())
                  ; cs <- envChanges equat env heap
                  --; _ <- trace ("changes " ++ show cs) (return ())
                  ; bs <- mapM (procChange env) cs
                  ; return (or bs)
                  }
             procHeap equat
                = do
                  { 
                  --  _ <- trace ("hpequ " ++ show equat) (return ())
                  -- ; ah <- getAssocs heap
                  -- ; ae  <- getAssocs env
                  -- ; _ <- trace (unlines ("SOLUTION"      : map show (ae)))  $ return ()
                  -- ; _ <- trace (unlines ("HEAPSOLUTION"  : map show (ah))) $ return ()
                  ; cs <- heapChange equat env
                  --; _ <- trace ("hpchs " ++ show cs) (return ())
                  ; b  <- procChange heap cs
                  ; return b
                  }
       ; count <- fixpoint eqs1 eqs2 procEnv procHeap
      
       ; let limsMp = Map.fromList lims
             lims2 = [ (y,z) 
                     | IsEvaluation x y _ <- eqs1 
                     , let mbz = Map.lookup x limsMp
                     , isJust mbz
                     , let z=fromJust mbz 
                     ]
                     ++ lims
             lims2Mp = Map.fromList lims2
                     

       --; trace (unlines ("EXTENDED LIMITATIONS"   : map show lims2)) $ return ()

       --; mapM (procLimit env heap) lims2      

       --; ah <- getAssocs heap
       --; ae  <- getAssocs env
       --; _ <- trace (unlines ("SOLUTION"      : map show (ae)))  $ return ()
       --; _ <- trace (unlines ("HEAPSOLUTION"  : map show (ah))) $ return ()
      
       ; absHeap <- unsafeFreeze heap
       ; absEnv  <- unsafeFreeze env
       
       ; return (count, (absEnv, absHeap, Map.empty))
       }
       )

procLimit env heap (x,ts)
 = do { av <- readArray env x
      ; av2 <- limit env heap ts av
      ; writeArray env x av2
      }


limit env heap ts (AbsNodes ns)
 = do { let kvs = Map.toList ns
            validTag (t@(GrTag_Con _ _ _) , _)
              = return (t `elem` ts)
            validTag (t@(GrTag_Fun (HNmNr f _)) , _)
              = do { ans <- readArray env f
                   ; ls  <- limit env heap ts ans
                   ; let ns2 = case ls of
                                 AbsNodes ns -> ns
                                 _           -> Map.empty
                   ; return (not (Map.null ns2))
                   }
            validTag _
              = return True
      ; kvs2 <- filterM validTag kvs
      ; return (AbsNodes (Map.fromList kvs2))
      }

limit env heap ts (AbsLocs ps m)
 = do { let validPtr p
             = do { ans <- readArray heap p
                  ; lans <- limit env heap ts ans
                  ; return (case lans of
                             AbsNodes ns2 -> (not (Map.null ns2))
                             AbsBottom    -> False
                             _            -> False
                           )
                  }
      ; ps2 <- filterM validPtr (Set.toList ps)
      ; return (AbsLocs (Set.fromList ps2) (limitIntersect m (Just (Set.fromList ts))))
      }

limit env heap ts av
 = do { return av
      }

%%]
