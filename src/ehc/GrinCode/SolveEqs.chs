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

%%[(8 codegen grin).updateEnv

type AbstractEnv s  = STArray s Variable AbstractValue

%%]

%%[(8 codegen grin).envChanges


eqOnceApplicable :: Equation -> Bool
eqOnceApplicable (IsBasic _) = True
eqOnceApplicable (IsTags _ _) = True
eqOnceApplicable (IsPointer _ _ _) = True
eqOnceApplicable (IsConstruction _ _ _ _) = True
eqOnceApplicable _ = False

envChanges :: Equation -> AbstractEnv s -> ST s [(Variable,AbstractValue)]
envChanges equat env
  = case equat of
      IsBasic         d            -> return [(d, AbsBasic)]
      
      IsTags          d ts         -> return [(d, AbsTags (Set.fromList ts))]
      
      IsPointer       d t as       -> return [(d, AbsPtr (Nodes (Map.singleton t (map (maybe Set.empty Set.singleton) as))) Set.empty)]

      IsConstruction  d t as   ev  -> return [(d, AbsNodes (Nodes (Map.singleton t (map (maybe Set.empty Set.singleton) as))))]


      IsUpdate        d v          -> do
                                      {  av  <- readArray env v
                                      ;  let res = case av of
                                                    AbsNodes ns -> AbsPtr ns (Set.empty)
                                                    AbsBottom   -> AbsBottom
                                                    _           -> error ("IsUpdate av=" ++ show av)
                                      ;  return [(d,res)]
                                      }

      IsEqual         d v          -> do
                                      {  av <- readArray env v
                                      ;  let res = case av of
                                                     AbsPtr _ vs -> AbsPtr (Nodes Map.empty) (Set.insert v vs)
                                                     _           -> av
                                      ;  return [(d, res)]
                                      }

      IsSelection     d v i t      -> do
                                      {  av <- readArray env v
                                      ;  res <- absSelect av i t
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
                                      ;  (sfx,res) <-  absApply absFun (tail vs) (Just ev)
                                      ;  return ((d,res):sfx)
                                      }
      
    where
    absStore AbsBottom     =  AbsBottom
    absStore (AbsNodes an) =  AbsPtr an Set.empty

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
          AbsPtr an vs -> do { xw2 <- findFinalValue av
                             ; zs <- mapM (readArray env) (Set.toList vs)
                             ; avs <- mapM findFinalValue zs
                             ; return (mconcat (xw2:avs))
                             } 
          AbsBottom    ->  return av
          AbsError _   ->  return av
          _            ->  return $ AbsError ("Variable passed to eval is not a location: " ++ show av)


    findFinalValue AbsBottom
      = return AbsBottom
    findFinalValue (AbsPtr (Nodes nodes) _)
      = findFinalValueIntern nodes
    findFinalValue (AbsNodes (Nodes nodes))
      = findFinalValueIntern nodes
    findFinalValue av
      = error ("findFinalValueForPtr " ++ show av)


    findFinalValueIntern nodes
      = do { let x = AbsNodes (Nodes (Map.filterWithKey (const . isFinalTag) nodes))
           ; zs <- mapM (readArray env) [ getNr nm  | (GrTag_App nm) <- Map.keys nodes ]
           ; avs <- mapM findFinalValue zs
           ; return (mconcat (x:avs))           
           }


    --absApply :: AbstractValue -> [Variable] -> Maybe Variable -> ST s ([(Variable,AbstractValue)],AbstractValue)
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
                   ; absArgs <- mapM (readArray env) args
                   ; let sfx      = zip  [funnr+2+length oldArgs ..] absArgs
                   ; res <-  if    n<needs
                             then  return $ AbsNodes (Nodes (Map.singleton newtag (oldArgs++map Set.singleton args)))
                             else  -- trace ("res from env" ++ show funnr) $ 
                                         readArray env funnr
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

fixpoint env eqs proc
  = countFixpoint 0
    where
    countFixpoint count = do
        { let doStep b i = proc i >>= return . (b||)
        ; changes <- foldM doStep False eqs
        
        --; ae <- getAssocs env
        --; let s  =  unlines (("SOLUTION": map show ae)) 
        --; _ <- unsafePerformIO (do { writeFile ("hpt"++ show count++".txt") s
        --                           ; return (return ())
        --                           }
        --                       )
        ; _ <- trace ("fix " ++ show count) (return ())
        ; if    changes
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


--moniEqua (IsEvaluation d _ _) = d==2929
--moniEqua (IsApplication d _ _) = d==788
moniEqua _ = True

solveEquations :: Int -> Equations -> Limitations -> (Int,HptMap)
solveEquations lenEnv eqs lims =
    runST (
    do { 
       --; trace (unlines ("EQUATIONS"     : map show eqs )) $ return ()
       --; trace (unlines ("LIMITATIONS"   : map show lims)) $ return ()

        ; let eqsStr = unlines (map show eqs )
        ; let limsStr = unlines (map show lims)

        ; _ <- unsafePerformIO (do { writeFile ("eqs .txt") eqsStr
                                   ; writeFile ("lims.txt") limsStr
                                   ; return (return ())
                                   }
                               )

       -- create arrays
       ; env     <- newArray (0, lenEnv   - 1) AbsBottom

       ; let procEnv equat
                = do
                  { 
                  --; ae  <- getAssocs env
                  --; _ <- (if moniEqua equat then trace (unlines ("SOLUTION"      : map show ae)) else id)  $ return ()
                  --; _ <- trace ("equat " ++ show equat) (return ())
                  ; cs <- envChanges equat env
                  --; _ <- trace ("changes " ++ show cs) (return ())
                  ; bs <- mapM (procChange env) cs
                  ; return (or bs)
                  }
                  
       ; let (eqs1a, eqs1b) = partition eqOnceApplicable eqs
                  
       ; _ <- mapM procEnv eqs1a
                  
       ; count <- fixpoint env eqs1b procEnv
      
       ; let limsMp = Map.fromList lims
             lims2 = [ (y,z) 
                     | IsEvaluation x y _ <- eqs1b
                     , let mbz = Map.lookup x limsMp
                     , isJust mbz
                     , let z=fromJust mbz 
                     ]
                     ++ lims
             lims2Mp = Map.fromList lims2
                     

       --; trace (unlines ("EXTENDED LIMITATIONS"   : map show lims2)) $ return ()

       ; mapM (procLimit env) lims2      

       --; ae  <- getAssocs env
       --; _ <- trace (unlines ("SOLUTION"      : map show (ae)))  $ return ()
      
       ; absEnv  <- unsafeFreeze env
       
       ; return (count, absEnv)
       }
       )

procLimit env (x,ts)
 = do { av <- readArray env x
      ; av2 <- limit env ts av
      ; writeArray env x av2
      }


limit env ts (AbsNodes (Nodes ns))
 = do { let kvs = Map.toList ns
            validTag (t@(GrTag_Con _ _ _) , _)
              = return (t `elem` ts)
            validTag (t@(GrTag_Fun (HNmNr f _)) , _)
              = do { ans <- readArray env f
                   ; ls  <- limit env ts ans
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

limit env ts av
 = do { return av
      }

%%]
