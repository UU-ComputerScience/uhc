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

%%[(8 corerun) hs import({%{EH}CoreRun}, {%{EH}CoreRun.Run}, {%{EH}CoreRun.Prim})
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
  | RVal_Int			   {-# UNPACK #-} !Int
  | RVal_Lam
      { rvalBody		:: !Exp						-- ^ a Exp_Lam, which also encodes a thunk
      , rvalSL			:: !HpPtr					-- ^ static link to enclosing stack frame
      }
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
    RVal_Int   v     	-> pp v
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
  vs <- mvecAlloc sz
  fr <- newIORef 0
  return $ Heap vs fr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment: contextual
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Environment: context/reader
data RValCxt
  = RValCxt
      { rcxtInRet		:: !Bool						-- ^ in returning context
      }

emptyRValCxt :: RValCxt
emptyRValCxt = RValCxt False
%%]

%%[(8 corerun) hs
mustReturn :: RunSem RValCxt RValEnv m RVal => RValT m a -> RValT m a
mustReturn = local (\r -> r {rcxtInRet = True})

needNotReturn :: RunSem RValCxt RValEnv m RVal => RValT m a -> RValT m a
needNotReturn = local (\r -> r {rcxtInRet = False})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment: state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Frame holding locals indexed by RRef_Loc
type RValFrame = HpPtr		-- points to heap to a RVal_Frame

-- | Frame Stack
type RValStack = [RValFrame]

-- | Environment: state
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
renvTopFrameM :: (RunSem RValCxt RValEnv m RVal) => RValT m HpPtr
renvTopFrameM = do
  (RValEnv {renvTopFrame=tf}) <- get
  liftIO $ readIORef tf
{-# INLINE renvTopFrameM #-}
%%]

%%[(8 corerun) hs
-- | Dump environment
dumpPpEnvM :: (RunSem RValCxt RValEnv m RVal) => RValT m PP_Doc
dumpPpEnvM = do
    topfrp <- renvTopFrameM
    env <- get
    st <- liftIO $ readIORef (renvStack env)
    let hp = renvHeap env
    hpfr <- liftIO $ readIORef (hpFree hp)
    needRet <- asks rcxtInRet
    let header =
			  "===================="
		  >-< "rcxtInRet=" >|< needRet
		  >-< "Top frame=" >|< topfrp
		  >-< "Stack size=" >|< length st
		  >-< "Heap size=" >|< MV.length (hpVals hp) >|< ", free=" >|< hpfr
    hpPP <- dumpHeap hp hpfr
    glPP <- dumpGlobals (renvGlobals env)
    frPPs <- forM ((if isNullPtr topfrp then [] else [topfrp]) ++ st) dumpFrame
    return $ header >-< hpPP >-< glPP >-< "====== Frames ======" >-< (indent 2 $ vlist frPPs)
  where
    dumpFrame fp = do
      fr@(RVal_Frame {rvalFrVals=vs}) <- heapGetM fp
      pps <- ppa (MV.length vs) vs
      return $ "Frame ptr=" >|< fp >-< (indent 2 $ fr >-< (indent 2 $ vlist pps))
    dumpGlobals glbls = do
      pps <- forM [0 .. V.length glbls - 1] $ \i -> do
        dumpFrame (glbls V.! i)
      return $ "====== Globals ======" >-< indent 2 (vlist pps)
    dumpHeap hp hpfr = do
      pps <- ppa hpfr (hpVals hp)
      return $ "======= Heap =======" >-< indent 2 (vlist pps)
    ppb k v = do
      ppvextra <- case v of
        RVal_Ptr p -> do 
          v' <- heapGetM p
          return $ " = " >|< v'
        _ -> return empty
      return $ k >|< ":" >#< v >|< ppvextra
    ppa sz vs = forM [0 .. sz - 1] $ \i -> liftIO (MV.read vs i) >>= ppb i
%%]

%%[(8 corerun) hs
-- | Dump environment
dumpEnvM :: (RunSem RValCxt RValEnv m RVal) => RValT m ()
dumpEnvM = dumpPpEnvM >>= (liftIO . putPPLn)
%%]

%%[(8 corerun) hs
-- | Dereference a possibly RVal_Ptr
ptr2valM :: (RunSem RValCxt RValEnv m RVal) => RVal -> RValT m RVal
ptr2valM v = case v of
  RVal_Ptr p -> heapGetM p >>= ptr2valM
  _          -> return v
{-# INLINE ptr2valM #-}

-- | Dereference a RRef
ref2valM :: (RunSem RValCxt RValEnv m RVal) => RRef -> RValT m RVal
ref2valM r = do
  rsemTr $ "R: " ++ show (pp r)
  env <- get
  case r of
    RRef_Glb m e -> do
        modFrame <- heapGetM (renvGlobals env V.! m)
        liftIO $ MV.read (rvalFrVals modFrame) e
    RRef_Loc l o -> do
        topfrp <- renvTopFrameM
        topfr <- heapGetM topfrp
        access topfr
      where
        access (RVal_Frame {rvalLev=frlev, rvalFrVals=vs}) | l == frlev = do
          -- rsemTr $ "R o=" ++ show o ++ " len(vs)=" ++ show (MV.length vs)
          liftIO $ MV.read vs o 
        access (RVal_Frame {rvalLev=frlev, rvalSL=sl})				  = do
          -- rsemTr $ "R sl=" ++ show sl ++ " frlev=" ++ show frlev ++ " l=" ++ show l
          heapGetM sl >>= access
        access v                                                        = 
          err $ "CoreRun.Run.Val.ref2valM.RRef_Loc.access:" >#< r >#< "in" >#< v
    RRef_Fld r e -> do
        v <- ref2valM r >>= ptr2valM
        case v of
          RVal_Node _ vs -> return $ vs V.! e
          _              -> err $ "CoreRun.Run.Val.ref2valM.RRef_Fld:" >#< e >#< "in" >#< v
    _ -> err $ "CoreRun.Run.Val.ref2valM.r:" >#< r
%%]

%%[(8 corerun) hs
-- | Allocate a mutable vector of given size
mvecAlloc :: Int -> IO (MV.IOVector RVal)
mvecAlloc sz =
  -- MV.new sz
  MV.replicate sz RVal_None
{-# INLINE mvecAlloc #-}

-- | Fill a mutable vector from a unmutable vector, starting with filling at the given lowerbound
mvecFillFrom :: Int -> MV.IOVector RVal -> CRArray RVal -> IO ()
mvecFillFrom lwb toarr frarr = forM_ (craAssocs' lwb frarr) $ \(i,e) -> MV.unsafeWrite toarr i e
{-# INLINE mvecFillFrom #-}
%%]

%%[(8 corerun) hs
-- | Fill (part of) a frame starting at 'lwb'
fillFrameM :: (RunSem RValCxt RValEnv m RVal) => Int -> CRArray RVal -> RVal -> RValT m ()
fillFrameM lwb as (RVal_Frame {rvalFrVals=frArr}) = do
  liftIO $ mvecFillFrom lwb frArr as

-- | Allocate a new frame
allocFrameM :: (RunSem RValCxt RValEnv m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> CRArray RVal -> RValT m HpPtr
allocFrameM r2n sl lev sz as = do
  a <- liftIO $ mvecAlloc sz
  liftIO $ mvecFillFrom 0 a as
  let fr = RVal_Frame r2n sl lev a
  fillFrameM 0 as fr
  heapAllocM fr

-- | Allocate and push a new stack frame
pushAllocFrameM :: (RunSem RValCxt RValEnv m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> CRArray RVal -> RValT m ()
pushAllocFrameM r2n sl lev sz as = do
  p <- allocFrameM r2n sl lev sz as
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    t <- readIORef tf
    unless (isNullPtr t) $ modifyIORef st (t:)
    writeIORef tf p
{-# INLINE pushAllocFrameM #-}

-- | Allocate and replace top stack frame
replaceAllocFrameM :: (RunSem RValCxt RValEnv m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> CRArray RVal -> RValT m ()
replaceAllocFrameM r2n sl lev sz as = do
  p <- allocFrameM r2n sl lev sz as
  (RValEnv {renvTopFrame=tf}) <- get
  liftIO $ writeIORef tf p
{-# INLINE replaceAllocFrameM #-}

-- | Pop a stack frame
popFrameM :: (RunSem RValCxt RValEnv m RVal) => RValT m ()
popFrameM = do
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    stk <- readIORef st
    case stk of
      [] -> writeIORef tf nullPtr
      (h:t) -> do
        writeIORef tf h
        writeIORef st t
{-# INLINE popFrameM #-}

-- | Update top frame
updTopFrameM :: (RunSem RValCxt RValEnv m RVal) => (RVal -> RValT m RVal) -> RValT m ()
updTopFrameM f = renvTopFrameM >>= flip heapUpdM f
{-# INLINE updTopFrameM #-}
%%]

%%[(8 corerun) hs
-- | Allocate on the heap, filling it with a RVal_Node
heapAllocM :: (RunSem RValCxt RValEnv m RVal) => RVal -> RValT m HpPtr
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
heapGetM' :: (RunSem RValCxt RValEnv m RVal) => Heap -> HpPtr -> RValT m RVal
heapGetM' hp p = liftIO $ MV.read (hpVals hp) p
{-# INLINE heapGetM' #-}

-- | Get a value from the heap
heapGetM :: (RunSem RValCxt RValEnv m RVal) => HpPtr -> RValT m RVal
heapGetM p = do
  hp <- gets renvHeap
  heapGetM' hp p
{-# INLINE heapGetM #-}

-- | Set a value in the heap
heapSetM' :: (RunSem RValCxt RValEnv m RVal) => Heap -> HpPtr -> RVal -> RValT m ()
heapSetM' hp p v = liftIO $ MV.write (hpVals hp) p v
{-# INLINE heapSetM' #-}

-- | Set a value in the heap
heapSetM :: (RunSem RValCxt RValEnv m RVal) => HpPtr -> RVal -> RValT m ()
heapSetM p v = do
  hp <- gets renvHeap
  heapSetM' hp p v
{-# INLINE heapSetM #-}

-- | Update a value in the heap
heapUpdM' :: (RunSem RValCxt RValEnv m RVal) => Heap -> HpPtr -> (RVal -> RValT m RVal) -> RValT m ()
heapUpdM' hp@(Heap {hpVals=vs}) p f = do
  v <- liftIO $ MV.read vs p
  v' <- f v
  liftIO $ MV.write vs p v'
{-# INLINE heapUpdM' #-}

-- | Update a value in the heap
heapUpdM :: (RunSem RValCxt RValEnv m RVal) => HpPtr -> (RVal -> RValT m RVal) -> RValT m ()
heapUpdM p f = do
  hp <- gets renvHeap
  heapUpdM' hp p f
{-# INLINE heapUpdM #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(cmodRun)
cmodRun :: (RunSem RValCxt RValEnv m RVal) => EHCOpts -> Mod -> RValT m RVal
cmodRun opts (Mod_Mod {body_Mod_Mod=e}) = do
  -- dumpEnvM
  v <- rsemExp e -- >>= rsemEvl
  dumpEnvM
  return v
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem, RunT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
err :: RunSem RValCxt RValEnv m RVal => PP_Doc -> RValT m a
err = throwError . rngLift emptyRange Err_PP
%%]

%%[(8 corerun) hs
-- | RunT' variant for Val
type RValT m a = RunT' RValCxt RValEnv m a
-- type RValT m a = RunT' () () RValEnv m a

%%]

%%[(8 corerun) hs
-- | Trace
rsemTr :: RunSem RValCxt RValEnv m RVal => String -> RValT m ()
rsemTr msg = do
      env <- get
      when (renvDoTrace env) $ do
        liftIO $ putStrLn msg
        dumpEnvM
%%]

%%[(8 corerun) hs
-- | Apply Lam in context of static link with exact right amount of params, otherwise the continuation is used
rvalAppLam :: RunSem RValCxt RValEnv m RVal => HpPtr -> Exp -> CRArray RVal -> (Int -> RValT m RVal) -> RValT m RVal
rvalAppLam sl f as failcont = do
  case f of
    Exp_Lam {lev_Exp_Lam=l, nrArgs_Exp_Lam=narg, nrAllocs_Exp_Lam=sz, ref2nm_Exp_Lam=r2n, body_Exp_Lam=b}
      | V.length as == narg -> do
           rsemTr $ "V app lam =="
           needRet <- asks rcxtInRet
           if needRet
             then do
               pushAllocFrameM r2n sl l sz as
               v <- needNotReturn $ rsemExp b
               popFrameM
               return v
             else do
               replaceAllocFrameM r2n sl l sz as
{-
               updTopFrameM $ \fr -> do
                 a <- liftIO $ do 
                   a <- mvecAlloc sz
                   mvecFillFrom 0 a as
                   return a
                 return $ fr {rvalRef2Nm=r2n, rvalSL=sl, rvalLev=l, rvalFrVals=a}
-}
               needNotReturn $ rsemExp b
      | otherwise -> failcont narg
    _   -> err $ "CoreRun.Run.Val.rvalAppLam:" >#< f

-- | Apply. Assume: function 'f' is already evaluated (responsibility lies outside)
rvalApp :: RunSem RValCxt RValEnv m RVal => RVal -> CRArray RVal -> RValT m RVal
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
            ap <- mustReturn $ rvalApp f (V.take narg as)
            rvalApp ap (V.drop narg as)
    RVal_App appf appas
      | V.length as > 0 -> do
           -- appf' <- rsemEvl appf
           rsemTr $ "V app app"
           rvalApp appf (appas V.++ as)
    _   -> err $ "CoreRun.Run.Val.rvalApp:" >#< f
%%]

%%[(8 corerun) hs
instance
    ( Monad m, MonadIO m, Functor m
    ) => RunSem RValCxt RValEnv m RVal
  where
    rsemInitState = liftIO $ newRValEnv 100000
    rsemInitReader = return emptyRValCxt
  
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
            f' <- mustReturn $ rsemExp f >>= rsemEvl
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

        -- let
        Exp_Let {firstOff_Exp_Let=fillFrom, ref2nm_Exp_Let=r2n, binds_Exp_Let=bs, body_Exp_Let=b} -> do
            bs' <- V.forM bs rsemExp
            fr <- renvTopFrameM >>= heapGetM >>= ptr2valM
            fillFrameM fillFrom bs' fr
            rsemExp b

        -- case, scrutinee already evaluated
        Exp_Case e as -> do
          (RVal_Node {rvalTag=tg}) <- rsemSExp e >>= ptr2valM
          rsemAlt $ as V.! tg
        
        -- force evaluation immediately
        Exp_Force e -> rsemExp e >>= rsemEvl

        -- setup for context requiring a return (TBD: should be done via CPS style, but is other issue)
        Exp_Ret e -> mustReturn $ rsemExp e

        -- simple expressions
        Exp_SExp se -> rsemSExp se

        -- FFI
        Exp_FFI pr as -> do
            as' <- V.mapM rsemExp as
            rsemPrim pr as'

        e -> err $ "CoreRun.Run.Val.cmodRun.rsemExp:" >#< e

    rsemSExp se = do
      case se of
        SExp_Int v -> return $ RVal_Int v
        SExp_Var r -> ref2valM r
        _ -> return (RVal_Lit se)
    {-# INLINE rsemSExp #-}

    rsemAlt a = do
      case a of
        Alt_Alt {expr_Alt_Alt=e} -> rsemExp e
    {-# INLINE rsemAlt #-}

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

    -- apply a known primitive
    rsemPrim pr as = do
        case (pr, V.toList as) of
          (RP_primAddInt, [RVal_Int i1, RVal_Int i2]) -> return $ RVal_Int $ i1 + i2
%%]


