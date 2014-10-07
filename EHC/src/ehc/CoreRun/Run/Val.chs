%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core to plainly yield a value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val}
%%]

%%[(8 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Error},{%{EH}Gam},{%{EH}Gam.DataGam})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun}, {%{EH}CoreRun.Run})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun.Pretty}, UHC.Util.Pretty)
%%]

%%[(8 corerun) hs import(Control.Monad, Control.Monad.RWS.Strict, Control.Monad.Error)
%%]
%%[(8888 corerun) hs import(Control.Monad.Primitive)
%%]

%%[(8888 corerun) hs import(Data.Array, Data.Array.IO, Data.Array.MArray)
%%]
%%[(8 corerun) hs import(qualified Data.Vector as V, qualified Data.Vector.Mutable as MV)
%%]

%%[(8 corerun) hs import(Data.IORef)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Representation of run value
data RVal
  = RVal_Lit
      { rvalSExp		:: !SExp					-- ^ a simple literal
      }
  | RVal_Lam
      { rvalBody		:: !Exp						-- ^ a Exp_Lam, which also encodes a thunk
      , rvalSL			:: !HpPtr					-- ^ static link to enclosing stack frame
      }
{-
-}
  | RVal_Thunk										-- ^ special case of Lam taking 0 params
      { rvalBody		:: !Exp						-- ^ Exp taking no arguments (thunk)
      , rvalSL			:: !HpPtr					-- ^ static link to enclosing stack frame
      }
  | RVal_Node
      { rvalTag			:: !Int						-- ^ node tag
      , rvalNdVals		:: !(CRArray RVal)			-- ^ heap allocated node values
      }
  | RVal_PApp
      { rvalLam			:: !Exp						-- ^ a Exp_Lam
      , rvalArgs		:: !(CRArray RVal)			-- ^ already applied args
      }
  | RVal_App
      { rvalFun			:: !RVal					-- ^ a RVal_App or RVal_PApp
      , rvalArgs		:: !(CRArray RVal)			-- ^ already applied args
      }
  | RVal_Frame
      { rvalRef2Nm		:: Ref2Nm					-- ^ ref to name mapping
      , rvalSL			:: !HpPtr					-- ^ immediately outer lexical level frame
      , rvalLev			:: !Int						-- ^ the lexical level this frame is on
      , rvalFrVals		:: !(MV.IOVector RVal)		-- ^ actual frame values, either literals or pointers to heap locations (so we can update them, share them)
      }
  | RVal_Ptr
      { rvalPtr			:: !HpPtr					-- ^ ptr/index into heap
      }
  | RVal_BlackHole
  | RVal_None

instance Show RVal where
  show _ = "RVal"

instance PP RVal where
  pp rval = case rval of
    RVal_Lit   e     	-> pp e
    RVal_Lam   b sl    	-> pp b
    RVal_Thunk sl e    	-> ppBrackets e
    RVal_Node  t vs 	-> t >|< (ppBracketsCommas $ V.toList vs)
    RVal_PApp  f as 	-> ppBrackets $ f >|< "@p" >|< (ppParensCommas $ V.toList as)
    RVal_App   f as 	-> ppBrackets $ f >|< "@"  >|< (ppParensCommas $ V.toList as)
    RVal_Ptr   p    	-> "*" >|< p
    RVal_Frame _ sl lv vs 	-> ppBracketsCommas $ ["sl=" >|< sl, "lev=" >|< lv, "sz=" >|< MV.length vs] -- ++ (map pp $ take 3 $ V.toList vs)
    RVal_BlackHole  	-> pp "Hole"
    RVal_None       	-> pp "None"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Heap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | HpPtr is index into a heap
type HpPtr = Int

nullPtr :: HpPtr
nullPtr = -1

isNullPtr = (== nullPtr)

data Heap
  = Heap
      { hpVals		:: MV.IOVector RVal
      , hpFree		:: IORef HpPtr
      }

newHeap :: Int -> IO Heap
newHeap sz = do
  vs <- MV.replicate sz RVal_None
  fr <- newIORef 0
  return $ Heap vs fr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Frame holding locals indexed by RRef_Loc
type RValFrame = HpPtr		-- points to heap to a RVal_Frame

-- | Frame Stack
type RValStack = [RValFrame]

-- | Environment
data RValEnv
  = RValEnv
      { renvGlobals		:: !(CRArray RValFrame)			-- ^ per module frame of globals
      , renvStack		:: !(IORef RValStack)			-- ^ stack of frames, except for the top
      , renvTopFrame	:: !(IORef RValFrame)			-- ^ current frame, the actual top of the stack
      , renvHeap		:: !Heap						-- ^ heap
      , renvDoTrace		:: !Bool
      }

newRValEnv :: Int -> IO RValEnv
newRValEnv hpSz = do
  st <- newIORef []
  tp <- newIORef nullPtr
  hp <- newHeap hpSz
  return $ RValEnv V.empty st tp hp False
%%]

%%[(8 corerun) hs
-- | Get the top most frame from the stack, 'nullPtr' if absent
renvTopFrameM :: (RunSem RValEnv m RVal) => RValT m HpPtr
renvTopFrameM = do
  (RValEnv {renvTopFrame=tf}) <- get
  liftIO $ readIORef tf
{-# INLINE renvTopFrameM #-}
%%]

%%[(8 corerun) hs
-- | Dump environment
dumpEnvM :: (RunSem RValEnv m RVal) => RValT m ()
dumpEnvM = do
    topfrp <- renvTopFrameM
    st <- gets renvStack >>= (liftIO . readIORef)
    hp <- gets renvHeap
    hpfr <- liftIO $ readIORef (hpFree hp)
    liftIO $ putStrLn $ unlines
      [ "===================="
      , "Top frame=" ++ show topfrp
      , "Stack size=" ++ show (length st)
      , "Heap size=" ++ show (MV.length (hpVals hp)) ++ ", free=" ++ show hpfr
      ]
    forM_ ((if isNullPtr topfrp then [] else [topfrp]) ++ st) dumpFrame
  where dumpFrame fp = do
          liftIO $ putStrLn $ "Frame nr=" ++ show fp
          fr@(RVal_Frame {rvalFrVals=vs}) <- heapGetM fp
          liftIO $ do
            putStrLn $ show $ indent 2 fr
            forM_ [0 .. MV.length vs - 1] $ \i -> do
              v <- MV.read vs i
              putStrLn $ show $ indent 4 v
%%]

%%[(8 corerun) hs
-- | Dereference a possibly RVal_Ptr
ptr2valM :: (RunSem RValEnv m RVal) => RVal -> RValT m RVal
ptr2valM v = case v of
  RVal_Ptr p -> heapGetM p >>= ptr2valM
  _          -> return v
{-# INLINE ptr2valM #-}

-- | Dereference a RRef
ref2valM :: (RunSem RValEnv m RVal) => RRef -> RValT m RVal
ref2valM r = do
  env <- get
  case r of
    RRef_Glb m e -> do
        modFrame <- heapGetM (renvGlobals env V.! m)
        liftIO $ MV.read (rvalFrVals modFrame) e
    RRef_Loc l o -> do
        topfrp <- renvTopFrameM
        topfr <- heapGetM topfrp
        access l topfr
      where
        access l (RVal_Frame {rvalLev=frlev, rvalFrVals=vs}) | l == frlev = liftIO $ MV.read vs o 
        access l (RVal_Frame {rvalSL=sl})                                 = heapGetM sl >>= access (l-1)
        access l v                                                        = err $ "CoreRun.Run.Val.ref2valM.RRef_Loc.access:" >#< r >#< "in" >#< v
    RRef_Fld r e -> do
        v <- ref2valM r >>= ptr2valM
        case v of
          RVal_Node _ vs -> return $ vs V.! e
          _              -> err $ "CoreRun.Run.Val.ref2valM.RRef_Fld:" >#< e >#< "in" >#< v
    _ -> err $ "CoreRun.Run.Val.ref2valM.r:" >#< r
%%]

%%[(8 corerun) hs
-- | Fill (part of) a frame starting at 'lwb'
fillFrameM :: (RunSem RValEnv m RVal) => Int -> CRArray RVal -> RVal -> RValT m ()
fillFrameM lwb as (RVal_Frame {rvalFrVals=frArr}) = do
  liftIO $ forM_ (craAssocs' lwb as) $ \(i,e) -> MV.unsafeWrite frArr i e

-- | Allocate a new frame
allocFrameM :: (RunSem RValEnv m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> CRArray RVal -> RValT m HpPtr
allocFrameM r2n sl lev sz as = do
  a <- liftIO $ MV.replicate sz RVal_None
  liftIO $ forM_ (craAssocs as) $ \(i,e) -> MV.unsafeWrite a i e
  let fr = RVal_Frame r2n sl lev a
  fillFrameM 0 as fr
  heapAllocM fr

-- | Allocate and push a new stack frame
pushAllocFrameM :: (RunSem RValEnv m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> CRArray RVal -> RValT m ()
pushAllocFrameM r2n sl lev sz as = do
  p <- allocFrameM r2n sl lev sz as
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    t <- readIORef tf
    unless (isNullPtr t) $ modifyIORef st (t:)
    writeIORef tf p
%%]

%%[(8 corerun) hs
-- | Allocate on the heap, filling it with a RVal_Node
heapAllocM :: (RunSem RValEnv m RVal) => RVal -> RValT m HpPtr
heapAllocM v = do		-- TBD: GC
  hp <- gets renvHeap
  let fr = hpFree hp
  liftIO $ do
    p <- readIORef fr
    MV.write (hpVals hp) p v
    writeIORef fr (p + 1)
    return p

{-
-- | Get a value from the heap
heapGetM'' :: Heap -> HpPtr -> m RVal
heapGetM'' hp p = MV.read (hpVals hp) p
{-# INLINE heapGetM'' #-}
-}

-- | Get a value from the heap
heapGetM' :: (RunSem RValEnv m RVal) => Heap -> HpPtr -> RValT m RVal
heapGetM' hp p = liftIO $ MV.read (hpVals hp) p
{-# INLINE heapGetM' #-}

-- | Get a value from the heap
heapGetM :: (RunSem RValEnv m RVal) => HpPtr -> RValT m RVal
heapGetM p = do
  hp <- gets renvHeap
  heapGetM' hp p
{-# INLINE heapGetM #-}

-- | Set a value in the heap
heapSetM' :: (RunSem RValEnv m RVal) => Heap -> HpPtr -> RVal -> RValT m ()
heapSetM' hp p v = liftIO $ MV.write (hpVals hp) p v
{-# INLINE heapSetM' #-}

-- | Set a value in the heap
heapSetM :: (RunSem RValEnv m RVal) => HpPtr -> RVal -> RValT m ()
heapSetM p v = do
  hp <- gets renvHeap
  heapSetM' hp p v
{-# INLINE heapSetM #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(cmodRun)
cmodRun :: (RunSem RValEnv m RVal) => Mod -> RValT m RVal
cmodRun (Mod_Mod {body_Mod_Mod=e}) = do
  v <- rsemExp e >>= rsemEvl
  dumpEnvM
  return v
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem, RunT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
err :: RunSem RValEnv m RVal => PP_Doc -> RValT m a
err = throwError . rngLift emptyRange Err_PP
%%]

%%[(8 corerun) hs
-- | RunT' variant for Val
type RValT m a = RunT' RValEnv m a
-- type RValT m a = RunT' () () RValEnv m a

%%]

%%[(8 corerun) hs
-- | Trace
rsemTr :: RunSem RValEnv m RVal => String -> RValT m ()
rsemTr msg = do
      env <- get
      when (renvDoTrace env) $ liftIO $ putStrLn msg
%%]

%%[(8 corerun) hs
-- | Apply Lam in context of static link with exact right amount of params, otherwise the continuation is used
rvalAppLam :: RunSem RValEnv m RVal => HpPtr -> Exp -> CRArray RVal -> (Int -> RValT m RVal) -> RValT m RVal
rvalAppLam sl f as failcont = do
  case f of
    Exp_Lam {lev_Exp_Lam=l, nrArgs_Exp_Lam=narg, nrAllocs_Exp_Lam=sz, ref2nm_Exp_Lam=r2n, body_Exp_Lam=b}
      | V.length as == narg -> do
           rsemTr $ "V app lam =="
           pushAllocFrameM r2n sl l sz as
           rsemExp b >>= rsemEvl
      | otherwise -> failcont narg
    _   -> err $ "CoreRun.Run.Val.rvalAppLam:" >#< f

-- | Apply. Assume: function 'f' is already evaluated (responsibility lies outside)
rvalApp :: RunSem RValEnv m RVal => RVal -> CRArray RVal -> RValT m RVal
rvalApp f as = do
  rsemTr $ "V app f(" ++ show (V.length as) ++ "): " ++ show (pp f)
  case f of
    RVal_Lam {rvalSL=sl, rvalBody=b} -> do
      rvalAppLam sl b as $ \narg -> do
        if V.length as < narg 
          then do
            rsemTr $ "V app lam <"
            return $ RVal_App f as
          else do
            rsemTr $ "V app lam >"
            ap <- rvalApp f (V.take narg as) >>= rsemEvl
            rvalApp ap (V.drop narg as)
    RVal_App appf appas
      | V.length as > 0 -> do
           appf' <- rsemEvl appf
           rsemTr $ "V app app"
           rvalApp appf' (appas V.++ as)
    _   -> err $ "CoreRun.Run.Val.rvalApp:" >#< f
%%]

%%[(8 corerun) hs
instance
    ( Monad m, MonadIO m, Functor m
    ) => RunSem RValEnv m RVal
  where
    rsemInitState = liftIO $ newRValEnv 100000
  
    rsemSetup modImpL mod = do
        let modAllL = mod : modImpL
        ms <- liftIO $ MV.new (maximum (map moduleNr_Mod_Mod modAllL) + 1)
        forM_ modAllL $ \(Mod_Mod {ref2nm_Mod_Mod=r2n, moduleNr_Mod_Mod=nr, binds_Mod_Mod=bs}) -> do
          bs' <- V.forM bs rsemExp
          p <- allocFrameM r2n nullPtr 0 (V.length bs') bs'
          liftIO $ MV.write ms nr p
        ms' <- liftIO $ V.freeze ms
        modify $ \env -> env {renvGlobals = ms'}
        rsemSetTrace True

    rsemSetTrace doTrace = modify $ \env -> env {renvDoTrace = doTrace}
    
    rsemExp e = do
      rsemTr $ "E: " ++ show (pp e)
      case e of
        -- app, call
        Exp_App f as -> do
            f' <- rsemExp f >>= rsemEvl
            as' <- V.mapM rsemExp as
            rvalApp f' as'
        
        -- heap node
        Exp_Tup t as -> do
            as' <- V.mapM rsemExp as
            p <- heapAllocM $ RVal_Node (ctagTag t) as'
            return $ RVal_Ptr p

        -- lam as is, being a heap allocated thunk when 0 args are required
        Exp_Lam {nrArgs_Exp_Lam=na}
          | na == 0   -> mk RVal_Thunk >>= heapAllocM >>= (return . RVal_Ptr)
          | otherwise -> mk RVal_Lam
          where mk rv = fmap (rv e) $ renvTopFrameM

        -- let -- TBD: combine Rec/Str variants as evaluation and thunking is made explicit anyway
        Exp_LetRec {firstOff_Exp_LetRec=fillFrom, ref2nm_Exp_LetRec=r2n, binds_Exp_LetRec=bs, body_Exp_LetRec=b} -> do
            bs' <- V.forM bs rsemExp
            fr <- renvTopFrameM >>= heapGetM >>= ptr2valM
            fillFrameM fillFrom bs' fr
            rsemExp b
        Exp_LetStr {firstOff_Exp_LetStr=fillFrom, ref2nm_Exp_LetStr=r2n, binds_Exp_LetStr=bs, body_Exp_LetStr=b} -> do
            bs' <- V.forM bs rsemExp
            fr <- renvTopFrameM >>= heapGetM >>= ptr2valM
            fillFrameM fillFrom bs' fr
            rsemExp b

        -- force evaluation immediately
        Exp_Force e -> rsemExp e >>= rsemEvl

        -- simple expressions
        Exp_SExp se -> case se of
            SExp_Var r -> ref2valM r -- >>= rsemEvl
            _ -> return (RVal_Lit se)

        e -> err $ "CoreRun.Run.Val.cmodRun.rsemExp:" >#< e

	-- TBD
    rsemEvl v = case v of -- return -- e = case e of
      RVal_Thunk {rvalSL=sl, rvalBody=e} ->
        rvalAppLam sl e V.empty $ \_ -> err $ "CoreRun.Run.Val.rsemEvl.RVal_Thunk:" >#< e

      RVal_Ptr {rvalPtr=p} -> do
        hp <- gets renvHeap
        v <- heapGetM' hp p
        heapSetM' hp p RVal_BlackHole
        v' <- rsemEvl v
        heapSetM' hp p v'
        return v'

      RVal_BlackHole -> err $ "CoreRun.Run.Val.rsemEvl.RVal_BlackHole:" >#< "Black hole"
        
      _ -> return v
%%]


