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
  | RVal_Node
      { rvalTag			:: !Int						-- ^ node tag
      , rvalNdVals		:: !(CRArray RVal)			-- ^ heap allocated node values
      }
{-
  | RVal_Thunk
      { rvalSL			:: !HpPtr					-- ^ static link to enclosing stack frame
      , rvalExp			:: !Exp						-- ^ Exp taking no arguments (thunk)
      }
-}
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
    -- RVal_Thunk sl e    	-> ppBrackets e
    RVal_Node  t vs 	-> t >|< (ppBracketsCommas $ V.toList vs)
    RVal_PApp  f as 	-> ppBrackets $ f >|< "@p" >|< (ppParensCommas $ V.toList as)
    RVal_App   f as 	-> ppBrackets $ f >|< "@"  >|< (ppParensCommas $ V.toList as)
    RVal_Ptr   p    	-> "*" >|< p
    RVal_Frame _ sl lv vs 	-> ppBracketsCommas $ ["sl=" >|< sl, "lv=" >|< lv] -- ++ (map pp $ take 3 $ V.toList vs)
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
      , renvStack		:: !(IORef RValStack)			-- ^ stack of frames
      , renvTopFrame	:: !(IORef RValFrame)			-- ^ current frame, the actual top of the stack
      , renvHeap		:: !Heap						-- ^ heap
      }

newRValEnv :: Int -> IO RValEnv
newRValEnv hpSz = do
  st <- newIORef []
  tp <- newIORef nullPtr
  hp <- newHeap hpSz
  return $ RValEnv V.empty st tp hp
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
        mfr <- heapGetM (renvGlobals env V.! m)
        liftIO $ MV.read (rvalFrVals mfr) e
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
-- | Allocate a new frame
allocFrameM :: (RunSem RValEnv m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> CRArray RVal -> RValT m HpPtr
allocFrameM r2n sl lev sz as = do
  a <- liftIO $ MV.replicate sz RVal_None
  liftIO $ forM_ (craAssocs as) $ \(i,e) -> MV.unsafeWrite a i e
  heapAllocM $ RVal_Frame r2n sl lev a

-- | Allocate and push a new stack frame
pushAllocFrameM :: (RunSem RValEnv m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> CRArray RVal -> RValT m ()
pushAllocFrameM r2n sl lev sz as = do
  p <- allocFrameM r2n sl lev sz as
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    t <- readIORef tf
    modifyIORef st (t:)
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

-- | Get a value from the heap
heapGetM :: (RunSem RValEnv m RVal) => HpPtr -> RValT m RVal
heapGetM p = do
  hp <- gets renvHeap
  liftIO $ MV.read (hpVals hp) p
{-# INLINE heapGetM #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(cmodRun)
cmodRun :: (RunSem RValEnv m RVal) => Mod -> RValT m RVal
cmodRun (Mod_Mod {body_Mod_Mod=e}) = rsemExp e
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

    rsemExp e = do
      liftIO $ putStrLn $ show $ pp e
      case e of
        -- app, call
        Exp_App f as -> do
            f' <- rsemExp f >>= rsemEvl
            as' <- V.mapM rsemExp as
            case f' of
              RVal_Lam {rvalSL=sl, rvalBody=Exp_Lam {lev_Exp_Lam=l, nrAllocs_Exp_Lam=sz, ref2nm_Exp_Lam=r2n, body_Exp_Lam=b}}
                | V.length as' == sz
                  -> do pushAllocFrameM r2n sl l sz as'
                        rsemExp b
              _   -> err $ "App fails:" >#< f'
        
        -- heap node
        Exp_Tup t as -> do
            as' <- V.mapM rsemExp as
            p <- heapAllocM $ RVal_Node (ctagTag t) as'
            return $ RVal_Ptr p

        -- lam as is
        Exp_Lam {} -> fmap (RVal_Lam e) $ renvTopFrameM

        -- simple expressions
        Exp_SExp se -> case se of
            SExp_Var r -> ref2valM r
            _ -> return (RVal_Lit se)

        e -> err $ "CoreRun.Run.Val.cmodRun.rsemExp:" >#< e

	-- TBD
    rsemEvl = return -- e = case e of
%%]


