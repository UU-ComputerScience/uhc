%%[0 hs
-- {-# LANGUAGE MagicHash #-}
-- {-# OPTIONS_GHC -O2 #-}
%%]

%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Core to plainly yield a value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val}
%%]

%%[(8 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Error})
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

%%[(8888 corerun) hs import(Data.Primitive.MutVar)
%%]

%%[(8888 corerun) hs import(GHC.Exts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Value
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RVal(..))
-- | Representation of run value
data RVal
  = 
    -- Value representations for running itself: literals
    RVal_Lit
      { rvalSExp		:: !SExp					-- ^ a simple literal
      }
  | RVal_Int			   {-# UNPACK #-} !Int
  | RVal_Integer		   !Integer
  | RVal_Float			   {-# UNPACK #-} !Float
  | RVal_Double			   {-# UNPACK #-} !Double
  | RVal_PackedString 	   !String					-- ^ packed string, equivalent of low level C string (could be replaced by something more efficient)

    -- Value representations for running itself: function, application, etc
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
  
    -- Value representations for library or runtime env (not Core specific)
  | RVal_MutVar			   !(IORef RVal)			-- ^ mutable var

instance Show RVal where
  show _ = "RVal"

instance PP RVal where
  pp rval = case rval of
    RVal_Lit   			e     		-> pp e
    RVal_Int   			v     		-> pp v
    RVal_Integer   		v     		-> pp v
    RVal_Float   		v     		-> pp v
    RVal_Double   		v     		-> pp $ show v
    RVal_PackedString	v    		-> pp $ show v
    RVal_Lam   			b sl		-> pp b
    RVal_Thunk 			sl e		-> ppBrackets e
    RVal_Node  			t vs 		-> t >|< (ppBracketsCommas $ V.toList vs)
    RVal_PApp  			f as 		-> ppBrackets $ f >|< "@p" >|< (ppParensCommas $ V.toList as)
    RVal_App   			f as 		-> ppBrackets $ f >|< "@"  >|< (ppParensCommas $ V.toList as)
    RVal_Ptr   			p    		-> "*" >|< p
    RVal_Frame 			_ sl lv vs 	-> ppBracketsCommas $ ["sl=" >|< sl, "lev=" >|< lv, "sz=" >|< MV.length vs] -- ++ (map pp $ take 3 $ V.toList vs)
    RVal_BlackHole  				-> pp "Hole"
    RVal_None       				-> pp "None"
    RVal_MutVar   		v    		-> pp "MutVar"
%%]

%%[(8 corerun) hs export(mkBool, mkTuple)
mkBool :: Bool -> RVal
mkBool b = RVal_Node (if b then 1 else 0) emptyCRArray
{-# INLINE mkBool #-}

mkTuple :: [RVal] -> RVal
mkTuple = RVal_Node 0 . mkCRArray
{-# INLINE mkTuple #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Heap
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(HpPtr, nullPtr, isNullPtr, newHeap, Heap(..))
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

%%[(8 corerun) hs export(heapGetM, heapGetM', heapAllocM, heapUpdM, heapSetM, heapSetM')
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
%%% Environment: contextual
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RValCxt(..), emptyRValCxt)
-- | Environment: context/reader
data RValCxt
  = RValCxt
      { rcxtInRet		:: !Bool						-- ^ in returning context
      -- , rcxtDoTrace		:: !Bool						-- ^ tracing
      }

emptyRValCxt :: RValCxt
emptyRValCxt = RValCxt False -- False
%%]

%%[(8 corerun) hs export(mustReturn, needNotReturn)
-- | Set return context to True
mustReturn :: RunSem RValCxt RValEnv m RVal => RValT m a -> RValT m a
mustReturn = local (\r -> r {rcxtInRet = True})
{-# INLINE mustReturn #-}

-- | Set return context to False
needNotReturn :: RunSem RValCxt RValEnv m RVal => RValT m a -> RValT m a
needNotReturn = local (\r -> r {rcxtInRet = False})
{-# INLINE needNotReturn #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment: state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RValEnv(..), newRValEnv)
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

%%[(8 corerun) hs export(mvecAlloc, mvecFillFrom)
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

%%[(8 corerun) hs export(renvTopFrameM)
-- | Get the top most frame from the stack, 'nullPtr' if absent
renvTopFrameM :: (RunSem RValCxt RValEnv m RVal) => RValT m HpPtr
renvTopFrameM = do
  (RValEnv {renvTopFrame=tf}) <- get
  liftIO $ readIORef tf
{-# INLINE renvTopFrameM #-}
%%]

%%[(8 corerun) hs
-- | Dump environment
dumpPpEnvM :: (RunSem RValCxt RValEnv m RVal) => Bool -> RValT m PP_Doc
dumpPpEnvM extensive = do
    topfrp <- renvTopFrameM
    env <- get
    st <- liftIO $ readIORef (renvStack env)
    let hp = renvHeap env
    hpfr <- liftIO $ readIORef (hpFree hp)
    needRet <- asks rcxtInRet
    let dash    = "===================="
        header1 = dash >-< "rcxtInRet=" >|< needRet
        header2 = ppCurly $ "Heap =" >|< hpfr >|< "/" >|< MV.length (hpVals hp) >|< ", Stack =" >|< ppBracketsCommas (topfrp : st)
        footer1 = dash
    hpPP <- dumpHeap hp hpfr
    glPP <- dumpGlobals (renvGlobals env)
    frPPs <- forM ((if isNullPtr topfrp then [] else [topfrp]) ++ st) dumpFrame
    if extensive
      then return $ header1 >-< header2 >-< hpPP >-< glPP >-< "====== Frames ======" >-< (indent 2 $ vlist frPPs) >-< footer1
      else return $ header2
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

%%[(8 corerun) hs export(dumpEnvM)
-- | Dump environment
dumpEnvM :: (RunSem RValCxt RValEnv m RVal) => Bool -> RValT m ()
dumpEnvM extensive = dumpPpEnvM extensive >>= (liftIO . putPPLn)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(rsemTr)
-- | Trace
rsemTr :: (PP msg, RunSem RValCxt RValEnv m RVal) => msg -> RValT m ()
rsemTr msg = whenM (gets renvDoTrace) $ do
    liftIO $ putStrLn $ show $ pp msg
    dumpEnvM False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem, RunT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RValT)
-- | RunT' variant for Val
type RValT m a = RunT' RValCxt RValEnv m a
-- type RValT m a = RunT' () () RValEnv m a

%%]

