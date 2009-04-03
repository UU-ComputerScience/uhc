%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%[(8 codegen grin) module {%{EH}GrinCode.SolveEqs}
%%]
%%[(8 codegen grin) export(solveEquations)
%%]
%%[(8 codegen grin) import(Data.Maybe, Data.Either, Data.List, Data.Ix, Data.Monoid, Data.Array.ST, Data.Array.IArray, Control.Monad.ST, Control.Monad)
%%]
%%[(8 codegen grin) import(qualified Data.Set as Set, qualified Data.Map as Map)
%%]
%%[(8 codegen grin) import({%{EH}Base.Common}, {%{EH}GrinCode})
%%]
%%[(8 codegen grin) import({%{EH}GrinCode.Common})
%%]
%%[(8 codegen grin) import(Debug.Trace)
%%]
%%[(8 codegen grin) import(System.IO.Unsafe)
%%]

%%[(8 codegen grin).updateEnvHeap

type AbstractEnv s  = STArray s Variable AbstractValue
type AbstractHeap s = STArray s Location AbstractValue

%%]

%%[(8 codegen grin).heapChange

heapChange :: HeapEquation -> AbstractEnv s -> ST s (Location,AbstractValue)
heapChange (WillStore locat tag args) env 
 = do  { let mbres       =   tagFun tag
       ; absRes          <-  getEnv mbres
       ; absExc          <-  getEnv (mbres >>= return . (+1))
       ; let absNode     =   AbsNodes (Nodes (Map.singleton tag (map (maybe Set.empty Set.singleton) args)))
       ; let exceptNode  =   case absExc of
                               AbsBottom                          -> AbsBottom
                               AbsLocs (Locs ls m) | Set.null ls  -> AbsBottom
                               otherwise                          -> AbsNodes (Nodes (Map.singleton throwTag [(maybe Set.empty (Set.singleton . (+1)) mbres)]))
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

%%[(8 codegen grin).envChanges

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
                                      ;  res <- absSelect av i t
                                      ;  return [(d,res)]
                                      }
      IsConstruction  d t as   ev  -> do
                                      {  let res = AbsNodes (Nodes (Map.singleton t (map (maybe Set.empty Set.singleton) as)))
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
                                      {  absFun  <-  readArray env (head vs)
                                      ;  let args = map Set.singleton (tail vs)
                                      ;  (sfx,res) <-  absApply absFun args (Just ev)
                                      ;  return ((d,res):sfx)
                                      }
      
    where
    absSelect av i t
     = case av of
         AbsNodes (Nodes ns)   -> maybe (return AbsBottom)
                                        (\xs -> if i<length xs 
                                                then do { vs <- mapM (readArray env) (Set.toList (xs!!i))
                                                        ; return (mconcat vs)
                                                        }
                                                else error ("cannot analyze FFI's returning non-nullary constructors " ++ show ns)
                                         ) 
                                         (Map.lookup t ns)
         AbsBottom     -> return av
         AbsError _    -> return av
         _             -> return (AbsError ("cannot select " ++ show i ++ " from " ++ show av))
         
    
    absEnum av
      = case av of
          AbsTags ts   -> AbsNodes (Nodes (Map.fromList [ (t,[]) |  t <- Set.toList ts ]))
          _            -> AbsError ("cannot enumerate " ++ show av)

    --absEval :: AbstractValue -> ST s AbstractValue  
    absEval av
      = case av of
          AbsLocs (Locs ls m) -> do { vs <- mapM (readArray heap) (Set.toList ls)
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

    --argAbstract :: Set.Set Variable -> ST s AbstractValue
    argAbstract vset =  do  { avs <- mapM (readArray env) (Set.toList vset)
                            ; return (mconcat avs)
                            }

    --absApply :: AbstractValue -> [Set.Set Variable] -> Maybe Variable -> ST s ([(Variable,AbstractValue)],AbstractValue)
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
                   ; absArgs <- mapM argAbstract args
                   ; let sfx      = zip  [funnr+2+length oldArgs ..] absArgs
                   ; res <-  if    n<needs
                             then  return $ AbsNodes (Nodes (Map.singleton newtag (oldArgs++args)))
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

%%[(8 codegen grin)

absvalSummary :: AbstractValue -> String
absvalSummary av = case av of
                    AbsBottom   -> "BOT"
                    AbsBasic    -> "BAS"
                    AbsTags  ts -> "TAGS " ++ show (Set.size ts)
                    AbsLocs (Locs a b) -> "LOCS " ++ show (Set.size a)
                    AbsNodes (Nodes a) -> "NODS " ++ show (Map.size a)
                    AbsUnion xs -> "UNION " ++ show (Map.size xs)
                    AbsError s  -> "ERR: " ++ s

locNodeCount :: AbstractValue -> Int
locNodeCount (AbsNodes (Nodes a)) = Map.size a 
locNodeCount _ = 0


tableItemSummary :: Char -> (Int,AbstractValue) -> String
tableItemSummary c (n,a) = c : show n ++ ": " ++ absvalSummary a


fixpoint env heap eqs1 eqs2 proc1 proc2 
  = countFixpoint 0
    where
    countFixpoint count = do
        { let doStep1 b i = proc1 i >>= return . (b||)
        ; let doStep2 b i = proc2 i >>= return . (b||)
        ; changes1 <- foldM doStep1 False eqs1
        ; changes2 <- foldM doStep2 False eqs2
        --; ah <- getAssocs heap
        --; ae <- getAssocs env
        --; let s  =  unlines (("SOLUTION": map show ae) ++ ("HEAPSOLUTION"  : map show ah)) 
        --; let s2 =  unlines (("SOLUTION": map (tableItemSummary 'S') ae) ++ ("HEAPSOLUTION"  : map (tableItemSummary 'H') ah)) 
        --; let lnc = sum (map (locNodeCount . snd) ah)
        --; _ <- unsafePerformIO (do { writeFile ("hpt"++ show count++".txt") s
        --                           ; writeFile ("summ"++ show count++".txt") s2
        --                           ; return (return ())
        --                           }
        --                       )
        --; _ <- trace ("fix " ++ show count ++ " lnc=" ++ show lnc) (return ())
        ; if    (changes1 || changes2)
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

        ; let eqs1Str = unlines (map show eqs1)
        ; let eqs2Str = unlines (map show eqs2)
        ; let limsStr = unlines (map show lims)

        ; _ <- unsafePerformIO (do { writeFile ("eqs1.txt") eqs1Str
                                   ; writeFile ("eqs2.txt") eqs2Str
                                   ; writeFile ("lims.txt") limsStr
                                   ; return (return ())
                                   }
                               )



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
                  --   _ <- trace ("hpequ " ++ show equat) (return ())
                  -- ; ah <- getAssocs heap
                  -- ; ae  <- getAssocs env
                  -- ; _ <- trace (unlines ("SOLUTION"      : map show (ae)))  $ return ()
                  -- ; _ <- trace (unlines ("HEAPSOLUTION"  : map show (ah))) $ return ()
                  ; cs <- heapChange equat env
                  -- ; _ <- trace ("hpchs " ++ show cs) (return ())
                  ; b  <- procChange heap cs
                  ; return b
                  }
       ; count <- fixpoint env heap eqs1 eqs2 procEnv procHeap
      
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

       ; mapM (procLimit env heap) lims2      

       --; ah <- getAssocs heap
       --; ae  <- getAssocs env
       --; _ <- trace (unlines ("SOLUTION"      : map show (ae)))  $ return ()
       --; _ <- trace (unlines ("HEAPSOLUTION"  : map show (ah))) $ return ()
      
       ; absHeap <- unsafeFreeze heap
       ; absEnv  <- unsafeFreeze env
       
       ; return (count, (absEnv, absHeap))
       }
       )

procLimit env heap (x,ts)
 = do { av <- readArray env x
      ; av2 <- limit env heap ts av
      ; writeArray env x av2
      }


limit env heap ts (AbsNodes (Nodes ns))
 = do { let kvs = Map.toList ns
            validTag (t@(GrTag_Con _ _ _) , _)
              = return (t `elem` ts)
            validTag (t@(GrTag_Fun (HNmNr f _)) , _)
              = do { ans <- readArray env f
                   ; ls  <- limit env heap ts ans
                   ; let ns2 = case ls of
                                 AbsNodes (Nodes ns) -> ns
                                 _                   -> Map.empty
                   ; return (not (Map.null ns2))
                   }
            validTag _
              = return True
      ; kvs2 <- filterM validTag kvs
      ; return (AbsNodes (Nodes (Map.fromList kvs2)))
      }


limit env heap ts (AbsLocs (Locs ps m))
 = do { let validPtr p
             = do { ans <- readArray heap p
                  ; lans <- limit env heap ts ans
                  ; return (case lans of
                             AbsNodes (Nodes  ns2) -> (not (Map.null ns2))
                             AbsBottom    -> False
                             _            -> False
                           )
                  }
      ; ps2 <- filterM validPtr (Set.toList ps)
      ; return (AbsLocs (Locs (Set.fromList ps2) (limitIntersect m (Just (Set.fromList ts)))))
      }

limit env heap ts av
 = do { return av
      }

%%]
