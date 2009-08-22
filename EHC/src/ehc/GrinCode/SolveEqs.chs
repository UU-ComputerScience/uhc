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


readC :: STArray s Variable (Bool,Bool,AbstractValue) -> Variable -> ST s Bool
readC env v
  = do { (_,c,_) <- readArray env v
       ; return c
       }

readAV :: STArray s Variable (Bool,Bool,AbstractValue) -> Variable -> ST s AbstractValue
readAV env v
  = do { (_,_,av) <- readArray env v
       ; return av
       }

readAV2 :: Bool -> STArray s Variable (Bool,Bool,AbstractValue) -> Variable -> ST s AbstractValue
readAV2 c0 env v
  = do { (c,_,av) <- readArray env v
       ; return (if c0||c then av else AbsBottom)
       }


envChanges :: Equation -> STArray s Variable (Bool,Bool,AbstractValue) -> ST s [(Variable,AbstractValue)]
envChanges equat env
  = case equat of
      IsBasic         d            -> return [(d, AbsBasic)]
      
      IsTags          d ts         -> return [(d, AbsTags (Set.fromList ts))]
      
      IsPointer       d t as       -> return [(d, AbsPtr (Nodes (Map.singleton t (map (maybe Set.empty Set.singleton) as))) Set.empty Set.empty)]

      IsConstruction  d t as   ev  -> return [(d, AbsNodes (Nodes (Map.singleton t (map (maybe Set.empty Set.singleton) as))))]


      IsUpdate        d v          -> do
                                      {  (c,_,av)  <- readArray env v
                                      ;  if c then do {
                                      ;  let res = case av of
                                                    AbsNodes ns -> AbsPtr ns Set.empty Set.empty
                                                    -- AbsNodes ns -> AbsPtr (Nodes Map.empty) Set.empty (Set.singleton v)   -- doesnt work yet
                                                    AbsBottom   -> AbsBottom
                                                    _           -> error ("IsUpdate v=" ++ show v ++ " av=" ++ show av)
                                      ;  return [(d,res)]
                                      } else return [] }

      IsEqual         d v          -> do
                                      {  (c,_,av) <- readArray env v
                                      ;  if c then do {
                                      ;  let res = case av of
                                                     AbsPtr _ vs ws -> AbsPtr (Nodes Map.empty) (Set.insert v vs) Set.empty
                                                     _              -> av
                                      ;  return [(d, res)]
                                      } else return [] }

      IsEnumeration   d v          -> do
                                      {  (c,_,av) <- readArray env v
                                      ;  if c then do {
                                      ;  return [(d, absEnum av)] 	
                                      } else return [] }

      IsSelection     d v i t      -> do
                                      {  (c,_,av) <- readArray env v
                                      ;  res <- absSelect True av i t
                                      ;  return [(d,res)]
                                      }

      IsEvaluation    d v      ev  -> do
                                      {  (c,_,av) <- readArray env v
                                      ;  res <- absEval True av
                                      ;  return [(d,res)]
                                      }
      IsApplication   d vs     ev  -> do 
                                      {  (c,_,absFun)  <-  readArray env (head vs)
                                      ;  (sfx,res) <-  absApply True absFun (tail vs) (Just ev)
                                      ;  return ((d,res):sfx)
                                      }
    where  
    absEnum av
      = case av of
          AbsTags ts   -> AbsNodes (Nodes (Map.fromList [ (t,[]) |  t <- Set.toList ts ]))
          _            -> AbsError ("cannot enumerate " ++ show av)

    absSelect c av i t
     = case av of
         AbsNodes (Nodes ns)   -> maybe (return AbsBottom)
                                        (\xs -> if i<length xs 
                                                then do { vs <- mapM (readAV2 c env) (Set.toList (xs!!i))
                                                        ; return (mconcat vs)
                                                        }
                                                else error ("cannot analyze FFI's returning non-nullary constructors " ++ show ns)
                                         ) 
                                         (Map.lookup t ns)
         AbsBottom     -> return av
         AbsError _    -> return av
         _             -> return (AbsError ("cannot select " ++ show i ++ " from " ++ show av))
         
    absEval c av
      = case av of
          AbsPtr an vs ws -> do { xw2 <- if c then findFinalValue c av else return AbsBottom
                                ; zs <- mapM (readAV2 c env) (Set.toList vs)
                                ; avs <- mapM (findFinalValue c) zs
                                ; return (mconcat (xw2:avs))
                                } 
          AbsBottom    ->  return av
          AbsError _   ->  return av
          _            ->  return $ AbsError ("Variable passed to eval is not a location: " ++ show av)


    findFinalValue _ AbsBottom
      = return AbsBottom
    findFinalValue c (AbsPtr (Nodes nodes) _ ws)
      = do { ans <- mapM (readAV2 c env) (Set.toList ws)
           ; aws <- mapM (findFinalValue undefined) ans
           ; r <- findFinalValueIntern nodes
           ; return (mconcat (r:aws))
           }
    findFinalValue _ (AbsNodes (Nodes nodes))
      = findFinalValueIntern nodes
    findFinalValue _ av
      = error ("findFinalValueForPtr " ++ show av)

    findFinalValueIntern nodes
      = do { let x = AbsNodes (Nodes (Map.filterWithKey (const . isFinalTag) nodes))
           --; zs <- mapM (readAV2 False env) [ getNr (trace (show nm ++ " = " ++ show f ++ "\n") nm)  | (GrTag_App nm, (f:_)) <- Map.toList nodes ]
           ; zs <- mapM (readAV2 False env) [ getNr nm  | (GrTag_App nm, (f:_)) <- Map.toList nodes ]
           ; avs <- mapM (findFinalValue undefined) zs
           --; qs <- mapM (readAV2 False env) (concat [ Set.toList f | (GrTag_App nm, (f:_)) <- Map.toList nodes ])
           --; avs <- mapM (findFinalValue undefined) qs
           ; return (mconcat (x:avs))           
           }

    absApply c f args mbev
      = do { let as = getNodes (filterTaggedNodes isPAppTag f)
           ; ts <- mapM (addArgs c) as
           ; let (sfxs,avs) = unzip ts
           ; return (concat sfxs, mconcat avs)
           }
      where addArgs c (tag@(GrTag_PApp needs nm) , oldArgs) 
              = do { let n        = length args
                         newtag   = GrTag_PApp (needs-n) nm
                         funnr    = getNr nm
                   ; absArgs <- mapM (readAV2 c env) args
                   ; let sfx      = zip  [funnr+2+length oldArgs ..] absArgs
                   ; res <-  if    n<needs
                             then  return $ AbsNodes (Nodes (Map.singleton newtag (oldArgs++map Set.singleton args)))
                             else  -- trace ("res from env" ++ show funnr) $ 
                                         readAV2 c env funnr
                   ; exc <-  if    n<needs
                             then  return AbsBottom
                             else  readAV2 c env (funnr+1)
                   ; if    n>needs
                     then  do 
                           { (sfx2,res2) <- absApply c res (drop needs args) mbev
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

fixpoint procEqs env
  = countFixpoint 0
    where
    countFixpoint count = do
        { 
          changes <- procEqs
        
        ; ae <- getAssocs env
        --; let s  =  unlines (("SOLUTION": map show ae)) 
        --; _ <- unsafePerformIO (do { writeFile ("hpt"++ show count++".txt") s
        --                           ; return (return ())
        --                           }
        --                       )
        --; _ <- trace ("fix " ++ show count ++ ": " ++ show changes ++ " changes") (return ())

        ; if    changes>0
          then  countFixpoint (count+1)
          else  return count
        }

procChange :: STArray s Variable (Bool,Bool,AbstractValue) -> (Int,AbstractValue) -> ST s Bool
procChange _   (i,AbsBottom) = return False
procChange env (i,e1) =
   do { (c,_,e0) <- readArray env i
      ; let e2       =  e0 `mappend` e1
            changed  =  e0 /= e2
      ; when changed (writeArray env i (c,True,e2))
      --; when changed (trace ("change " ++ show i ++ " from " ++ show e0 ++ "\n             to " ++ show e2) (return ()))
      ; return changed
      }


solveEquations :: Int -> Equations -> Limitations -> (Int,HptMap)
solveEquations lenEnv eqs lims =
    runST (
    do { 
       --; let eqsStr = unlines (map show eqs )
       --; let limsStr = unlines (map show lims)
       --; _ <- unsafePerformIO (do { writeFile ("eqs.txt") eqsStr
       --                           ; writeFile ("lims.txt") limsStr
       --                           ; return (return ())
       --                           }
       --                       )

       -- create arrays
       ; env     <- newArray (0, lenEnv-1) (True,False,AbsBottom)

       ; let dependentsChanged (AbsPtr an vs ws)
                =  do { cs <- mapM (readC env) ({- Set.toList vs ++ -} Set.toList ws)
                      ; return (or cs)
                      }
             dependentsChanged _ 
                =  return False

       ; let close i 
                = do { (c0,c1,a) <- readArray env i
                     ; c2 <- dependentsChanged a
                     ; when c2 (writeArray env i (c0,c2,a))
                     ; return ()
                     }

       ; let shift r i 
                = do { (_,c,a) <- readArray env i
                     --; c2 <- if c1 then return True else dependentsChanged a
                     ; writeArray env i (c,False,a)
                     ; return (if c then r+1 else r)
                     }

       ; let procEq equat
                = do
                  { 
                  ; cs <- envChanges equat env
                  ; mapM_ (procChange env) cs
                  ; return ()
                  }

       ; let (eqs1a, eqs1b) = partition eqOnceApplicable eqs


       ; let procEqs = do { mapM_ procEq eqs1b
                          ; mapM close [0..lenEnv-1]
                          ; r <- foldM shift 0 [0..lenEnv-1]
                          ; return r
                          }

                  
       ; _ <- mapM procEq eqs1a
                  
       ; count <- fixpoint procEqs env
      
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
      
       ; absEnv0 <- mapArray (\(_,_,a)->a) env
       ; absEnv  <- unsafeFreeze absEnv0
       ; return (count, absEnv)
       }
       )

procLimit env (x,ts)
 = do { (c,b,av) <- readArray env x
      ; av2 <- limit env ts av
      ; writeArray env x (c,b,av2)
      }


limit env ts (AbsNodes (Nodes ns))
 = do { let kvs = Map.toList ns
            validTag (t@(GrTag_Con _ _ _) , _)
              = return (t `elem` ts)
            validTag (t@(GrTag_Fun (HNmNr f _)) , _)
              = do { ans <- readAV env f
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
