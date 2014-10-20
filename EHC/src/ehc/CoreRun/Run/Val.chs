%%[0 hs
{-# LANGUAGE MagicHash #-}
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
%%[(8 corerun) hs import(Control.Monad.Primitive)
%%]

%%[(8888 corerun) hs import(Data.Array, Data.Array.IO, Data.Array.MArray)
%%]
%%[(8 corerun) hs import(qualified Data.Vector as V, qualified Data.Vector.Mutable as MV, qualified Data.Vector.Generic as GV)
%%]
%%[(8 corerun) hs import(Data.Data, Data.Typeable)
%%]

-- For use of underlying (Haskell) impl
%%[(8 corerun) hs import(System.IO) export(module System.IO)
%%]
%%[(8 corerun) hs import(System.IO.Unsafe)
%%]
%%[(8 corerun) hs import(Data.IORef) export(module Data.IORef)
%%]
%%[(8 corerun) hs import(Data.Int, Data.Word) export(module Data.Int, module Data.Word)
%%]
%%[(8 corerun) hs import(qualified Data.ByteString.Char8 as BSC8)
%%]
%%[(8 corerun) hs import(GHC.Ptr(Ptr(..)), GHC.Exts({Addr#})) export(module GHC.Ptr)
%%]

-- old
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
  | RVal_Char  			   {-# UNPACK #-} !Char   
  | RVal_Int   			   {-# UNPACK #-} !Int   
  | RVal_Int8  			   {-# UNPACK #-} !Int8  
  | RVal_Int16 			   {-# UNPACK #-} !Int16 
  | RVal_Int32 			   {-# UNPACK #-} !Int32 
  | RVal_Int64 			   {-# UNPACK #-} !Int64 
  | RVal_Word  			   {-# UNPACK #-} !Word  
  | RVal_Word8 			   {-# UNPACK #-} !Word8 
  | RVal_Word16			   {-# UNPACK #-} !Word16
  | RVal_Word32			   {-# UNPACK #-} !Word32
  | RVal_Word64			   {-# UNPACK #-} !Word64
  | RVal_Integer		   !Integer
  | RVal_Float			   {-# UNPACK #-} !Float
  | RVal_Double			   {-# UNPACK #-} !Double
  | RVal_PackedString 	   !BSC8.ByteString					-- ^ packed string, equivalent of low level C string (could be replaced by something more efficient)

    -- Value representations for running itself: function, application, etc
  | RVal_Lam
      { rvalBody		:: !Exp						-- ^ a Exp_Lam, which also encodes a thunk
      , rvalSLRef		:: !(IORef HpPtr)			-- ^ static link to enclosing stack frame
      }
  | RVal_Thunk										-- ^ special case of Lam taking 0 params
      { rvalBody		:: !Exp						-- ^ Exp taking no arguments (thunk)
      , rvalSLRef		:: !(IORef HpPtr)			-- ^ static link to enclosing stack frame
      }
  | RVal_Node
      { rvalTag			:: !Int						-- ^ node tag
      , rvalNdVals		:: !(MV.IOVector RVal)		-- ^ fields
      }
  | RVal_App
      { rvalFun			:: !RVal					-- ^ a RVal_App or RVal_PApp
      , rvalArgs		:: !(MV.IOVector RVal)		-- ^ already applied args
      }
  | RVal_Frame
      { rvalRef2Nm		:: Ref2Nm					-- ^ ref to name mapping
      , rvalSLRef		:: !(IORef HpPtr)			-- ^ immediately outer lexical level frame
      , rvalLev			:: !Int						-- ^ the lexical level this frame is on
      , rvalFrVals		:: !(MV.IOVector RVal)		-- ^ actual frame values, either literals or pointers to heap locations (so we can update them, share them)
      }
  | RVal_Ptr
      { rvalPtrRef		:: !(IORef HpPtr)			-- ^ ptr/index into heap
      }
  | RVal_Fwd
      { rvalPtr			:: !HpPtr					-- ^ forwarding ptr, only used during GC
      }
  | RVal_BlackHole
  | RVal_None
  
    -- Value representations for library or runtime env (not Core specific)
  | RHsV_MutVar			   !(IORef RVal)			-- ^ mutable var
  | RHsV_Handle			   !Handle					-- ^ IO handle
  | RHsV_Addr			   Addr#					-- ^ Addr inside Ptr

instance Show RVal where
  show _ = "RVal"

ppRVal :: RVal -> IO PP_Doc
ppRVal rval = case rval of
    RVal_Lit   			e     		-> dfltpp e
    RVal_Char   		v     		-> dfltpp $ show v
    RVal_Int   			v     		-> dfltpp v
    RVal_Int8   		v     		-> dfltpp $ show v
    RVal_Int16   		v     		-> dfltpp $ show v
    RVal_Int32  		v     		-> dfltpp $ show v
    RVal_Int64 			v     		-> dfltpp $ show v
    RVal_Word   		v     		-> dfltpp $ show v
    RVal_Word8   		v     		-> dfltpp $ show v
    RVal_Word16   		v     		-> dfltpp $ show v
    RVal_Word32  		v     		-> dfltpp $ show v
    RVal_Word64 		v     		-> dfltpp $ show v
    RVal_Integer   		v     		-> dfltpp v
    RVal_Float   		v     		-> dfltpp v
    RVal_Double   		v     		-> dfltpp $ show v
    RVal_PackedString	v    		-> dfltpp $ show v
    RVal_Lam   			b slref		-> dfltpp b
    RVal_Thunk 			e slref 	-> return $ ppBrackets e
    RVal_Node  			t vs 		-> dfltpp t -- r >|< (ppBracketsCommas $ V.toList vs)
    RVal_App   			f as 		-> dfltpp f -- return $ ppBrackets $ f >|< "@"  >|< (ppParensCommas $ V.toList as)
    RVal_Ptr   			pref    	-> readIORef pref >>= \p -> return $ "*"  >|< p
    RVal_Fwd   			p    		-> return $ "f*" >|< p
    RVal_Frame 			_ slref lv vs -> do
                                       sl <- readIORef slref
                                       return $ ppBracketsCommas $ ["sl=" >|< sl, "lev=" >|< lv, "sz=" >|< MV.length vs] -- ++ (map pp $ take 3 $ V.toList vs)
    RVal_BlackHole  				-> dfltpp "Hole"
    RVal_None       				-> dfltpp "None"
    RHsV_MutVar   		v    		-> dfltpp "MutVar"
    RHsV_Handle   		h    		-> dfltpp $ show h
    RHsV_Addr	   		p    		-> dfltpp $ show (Ptr p)
  where
    dfltpp :: PP x => x -> IO PP_Doc
    dfltpp = return . pp

instance PP RVal where
  pp rval = unsafePerformIO (ppRVal rval)
%%]

%%[(8 corerun) hs export(mkTuple, mkUnit)
mkTuple :: (RunSem RValCxt RValEnv m RVal) => [RVal] -> RValT m RVal
mkTuple vs = liftIO (mvecAllocFill $ mkCRArray vs) >>= \v -> return $ RVal_Node 0 v
{-# INLINE mkTuple #-}

mkUnit :: (RunSem RValCxt RValEnv m RVal) => RValT m RVal
mkUnit = mkTuple []
{-# INLINE mkUnit #-}
%%]
mkBool :: Bool -> RVal
mkBool b = RVal_Node (if b then tagBoolTrue else tagBoolFalse) emptyCRArray
{-# INLINE mkBool #-}


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Marshalling from/to Haskell values which should be visible/usable/correspond in both the RVal and Haskell world
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(HSMarshall(..))
-- | Marshalling from/to Haskell values
class HSMarshall hs where
  -- | Marshall to Haskell value, also parameterized by evaluator
  hsMarshall :: (RunSem RValCxt RValEnv m RVal) => (RVal -> RValT m RVal) -> RVal -> RValT m hs

  -- | Unmarshall from Haskell value
  hsUnmarshall :: (RunSem RValCxt RValEnv m RVal) => hs -> RValT m RVal
%%]

%%[(8 corerun) hs
instance HSMarshall Int where
  hsMarshall _ (RVal_Int v) = return v
  hsUnmarshall v = return $ RVal_Int v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall Integer where
  hsMarshall _ (RVal_Integer v) = return v
  hsUnmarshall v = return $ RVal_Integer v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall Bool where
  hsMarshall _ (RVal_Node t _) = return $ t == tagBoolTrue
  hsMarshall _ v               = err $ "CoreRun.Run.Val.HSMarshall Bool:" >#< v
  hsUnmarshall b = liftIO (mvecAllocFill emptyCRArray) >>= \v -> return $ RVal_Node (if b then tagBoolTrue else tagBoolFalse) v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall Char where
  hsMarshall _ (RVal_Char v) = return v
  hsMarshall _ v             = err $ "CoreRun.Run.Val.HSMarshall Char:" >#< v
  hsUnmarshall v = return $ RVal_Char v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall (Ptr a) where
  hsMarshall _ (RHsV_Addr v) = return $ Ptr v
  hsMarshall _ v             = err $ "CoreRun.Run.Val.HSMarshall (Ptr a):" >#< v
  hsUnmarshall (Ptr v) = return $ RHsV_Addr v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall [RVal] where
  hsMarshall evl (RVal_Node t as)
    -- | t == tagListCons = (evl $ as V.! 1) >>= hsMarshall evl >>= (return . ((as V.! 0) :))
    | t == tagListCons = liftIO (MV.read as 1) >>= evl >>= hsMarshall evl >>= \tl -> liftIO (MV.read as 0) >>= \hd -> return (hd : tl)
    | otherwise        = return []
  hsMarshall _   v     = err $ "CoreRun.Run.Val.HSMarshall [RVal]:" >#< v

  hsUnmarshall []      = liftIO (mvecAllocFill emptyCRArray) >>= \v -> return $ RVal_Node tagListNil v
  hsUnmarshall (h:t)   = hsUnmarshall t >>= \t' -> (liftIO $ mvecAllocFill $ mkCRArray [h, t']) >>= \v -> return $ RVal_Node tagListCons v

instance HSMarshall x => HSMarshall [x] where
  hsMarshall evl x = hsMarshall evl x >>= mapM (\v -> evl v >>= hsMarshall evl)
  {-# INLINE hsMarshall #-}

  hsUnmarshall v       = forM v hsUnmarshall >>= hsUnmarshall
  {-# INLINE hsUnmarshall #-}
%%]

%%[(8888 corerun) hs
-- | Datatype relation between Haskell and RVal encodings
class Data hs => HSMarshallData hs where
  hsMarshallConstrMp :: (Map.Map hs Int)
  hsMarshallTyped :: hs
  hsMarshallTyped = undefined

instance HSMarshallData IOMode where
  hsMarshallConstrMp = map showConstr $ dataTypeConstrs $ dataTypeOf hsMarshallTyped
%%]

%%[(8 corerun) hs
deriving instance Typeable IOMode
deriving instance Data IOMode
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
      { hpVals					:: {-# UNPACK #-} !(MV.IOVector RVal)
      , hpFree					:: {-# UNPACK #-} !(IORef HpPtr)
      , hpSemispaceMultiplier	:: {-# UNPACK #-} !(IORef Rational)				-- multiplier by which to enlarge/shrink subsequent semispace
      }

newHeap :: Int -> IO Heap
newHeap sz = do
  vs <- mvecAllocInit sz
  fr <- newIORef 0
  ml <- newIORef 1.5
  return $ Heap vs fr ml
%%]

%%[(8 corerun) hs
-- | Garbage collect heap (TBD)
heapGcM :: (RunSem RValCxt RValEnv m RVal) => RVal -> RValT m RVal
-- heapGcM = err $ "CoreRun.Run.Val.heapGcM: GC not yet implemented"
heapGcM curV = do
    rsemTr $ "GC starts"
    env@(RValEnv {renvHeap=hp@(Heap {hpVals=vsOld, hpSemispaceMultiplier=mlRef, hpFree=hpFrRef}), renvTopFrame=topFrRef, renvStack=stkRef, renvGlobals=globals}) <- get
    env' <- liftIO $ do
      ml <- readIORef mlRef
      vsNew <- mvecAllocInit $ round $ fromIntegral (MV.length vsOld) * ml
      greyref <- newIORef 0
      let -- copy content of ptr to new loc, leaving a forwarding on to the old location
          copyp p
            | p >= 0 = do
                -- putStrLn $ "GC copyp p=" ++ show p
                v <- MV.read vsOld p
                case v of
                  RVal_Fwd p' -> return p'
                  v -> do
                    p' <- readIORef greyref
                    MV.write vsNew p' v
                    MV.write vsOld p  (RVal_Fwd p')
                    writeIORef greyref (p'+1)
                    return p'
            | otherwise = return p
          
          -- inspect, possibly copy internal part of a RVal, stops with a ptr which later is dealt with (to prevent too deep stack growth), exploiting the in between grey/black area as queue
          copyv v = do
            case v of
              RVal_Ptr pref                               	-> modifyIORefM pref copyp
              RVal_Frame {rvalSLRef=slref, rvalFrVals=vs} 	-> modifyIORefM slref copyp >> mvecForM_ vs copyv
              RVal_Node {rvalNdVals=vs} 					-> mvecForM_ vs copyv
              RVal_App {rvalArgs=vs} 						-> mvecForM_ vs copyv
              RVal_Thunk {rvalSLRef=slref}					-> modifyIORefM slref copyp
              RVal_Lam {rvalSLRef=slref}					-> modifyIORefM slref copyp
              _  											-> return ()

          -- inspect, follow, copy content of RVal
          follow bl gr
            | bl < gr   = MV.read vsNew bl >>= copyv >> follow (bl+1) gr
            | otherwise = do
                gr' <- readIORef greyref
                if bl < gr' then follow bl gr' else return bl
      
      -- initial copy: top frame
      modifyIORefM topFrRef $ \topFr -> if isNullPtr topFr then return topFr else copyp topFr

      -- initial copy: stack
      modifyIORefM stkRef $ mapM copyp
        
      -- initial copy: globals
      globals' <- V.forM globals copyp
      
      -- initial copy: the RVal to be put on the heap
      copyv curV
      
      -- follow with initial values of grey and black (0)
      readIORef greyref >>= follow 0 >>= writeIORef hpFrRef
        
      -- final: 
      return $ env {renvGlobals = globals', renvHeap = hp {hpVals=vsNew}}

    put env'
    rsemTr $ "GC done"
    return curV
%%]

{-
              RVal_Ptr pref -> do
                p <- liftIO (readIORef pref)
                copyp p
-}


%%[(8 corerun) hs export(heapGetM, heapGetM', heapAllocM, heapUpdM, heapSetM, heapSetM')
-- | Allocate on the heap, filling it with a RVal_Node
heapAllocM :: (RunSem RValCxt RValEnv m RVal) => RVal -> RValT m HpPtr
heapAllocM v = do
  hp@(Heap {hpVals=vs, hpFree=fr}) <- gets renvHeap
  p <- liftIO $ readIORef fr
  if p >= MV.length vs
    then heapGcM v >>= heapAllocM
    else liftIO $ do
      MV.write vs p v
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

%%[(8 corerun) hs export(mustReturn, needNotReturn, rvalRetEvl)
-- | Set return context to True
mustReturn :: RunSem RValCxt RValEnv m RVal => RValT m a -> RValT m a
mustReturn = local (\r -> r {rcxtInRet = True})
{-# INLINE mustReturn #-}

-- | Set return context to False
needNotReturn :: RunSem RValCxt RValEnv m RVal => RValT m a -> RValT m a
needNotReturn = local (\r -> r {rcxtInRet = False})
{-# INLINE needNotReturn #-}

-- | Variation of `rsemEvl` in return context
rvalRetEvl :: RunSem RValCxt RValEnv m RVal => RVal -> RValT m RVal
rvalRetEvl = mustReturn . rsemEvl
{-# INLINE rvalRetEvl #-}
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

%%[(8 corerun) hs export(mvecAllocInit, mvecAlloc, mvecFillFrom, mvecAllocFill, mvecForM_, mvecAppend)
-- | Allocate a mutable vector of given size
mvecAllocWith :: Int -> a -> IO (MV.IOVector a)
mvecAllocWith sz v = MV.replicate sz v
{-# INLINE mvecAllocWith #-}

-- | Allocate a mutable vector of given size, init to default value
mvecAllocInit :: Int -> IO (MV.IOVector RVal)
mvecAllocInit sz = mvecAllocWith sz RVal_None
{-# INLINE mvecAllocInit #-}

-- | Allocate a mutable vector of given size
mvecAlloc :: PrimMonad m => Int -> m (MV.MVector (PrimState m) a)
mvecAlloc sz = MV.new sz
{-# INLINE mvecAlloc #-}

-- | Fill a mutable vector from a unmutable vector, starting with filling at the given lowerbound
mvecFillFrom :: Int -> MV.IOVector RVal -> CRArray RVal -> IO ()
mvecFillFrom lwb toarr frarr = forM_ (craAssocs' lwb frarr) $ \(i,e) -> MV.unsafeWrite toarr i e
{-# INLINE mvecFillFrom #-}

-- | Alloc and fill vector of size taken from fixed vec
mvecAllocFill :: CRArray RVal -> IO (MV.IOVector RVal)
mvecAllocFill frarr = mvecAlloc (V.length frarr) >>= \toarr -> mvecFillFrom 0 toarr frarr >> return toarr
{-# INLINE mvecAllocFill #-}

-- | Loop over a mutable vector, updating the vector as a side effect
mvecLoop :: PrimMonad m => Int -> Int -> (MV.MVector (PrimState m) a -> Int -> m b) -> MV.MVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
mvecLoop l h m v = loop l
  where loop l | l < h     = m v l >> loop (l+1)
               | otherwise = return v
{-# INLINE mvecLoop #-}

-- | Loop over a mutable vector, updating the vector as a side effect
mvecForM_ :: PrimMonad m => MV.MVector (PrimState m) a -> (a -> m b) -> m ()
mvecForM_ v m = loop 0
  where loop i | i < MV.length v = MV.read v i >>= m {- >>= MV.write v i -} >> loop (i+1)
               | otherwise       = return ()
{-# INLINE mvecForM_ #-}

-- | Append mutable 2 vectors
mvecAppend :: PrimMonad m => MV.MVector (PrimState m) a -> MV.MVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
mvecAppend v1 v2 =
    mvecAlloc l12 >>= mvecLoop 0 l1 (\v i -> MV.read v1 i >>= MV.write v i) >>= mvecLoop l1 l12 (\v i -> MV.read v2 (i-l1) >>= MV.write v i)
  where
    l1 = MV.length v1
    l2 = MV.length v2
    l12 = l1 + l2
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
-- | Get all non empty stack frames, still split up in (possibly null) top frame and rest of stack
renvAllFrameM' :: (RunSem RValCxt RValEnv m RVal) => RValT m (Maybe HpPtr,[HpPtr])
renvAllFrameM' = do
  topfrp <- renvTopFrameM
  env <- get
  st <- liftIO $ readIORef (renvStack env)
  return (if isNullPtr topfrp then Nothing else Just topfrp, st)

-- | Get all non empty stack frames
renvAllFrameM :: (RunSem RValCxt RValEnv m RVal) => RValT m [HpPtr]
renvAllFrameM = do
  (mbtop,st) <- renvAllFrameM'
  return $ maybe [] (:[]) mbtop ++ st
%%]

%%[(8 corerun) hs
-- | Dump environment
dumpPpEnvM :: (RunSem RValCxt RValEnv m RVal) => Bool -> RValT m PP_Doc
dumpPpEnvM extensive = do
    stkfrs <- renvAllFrameM
    env <- get
    let hp = renvHeap env
    hpfr <- liftIO $ readIORef (hpFree hp)
    needRet <- asks rcxtInRet
    let dash    = "===================="
        header1 = dash >-< "rcxtInRet=" >|< needRet
        header2 = ppCurly $ "Heap =" >|< hpfr >|< "/" >|< MV.length (hpVals hp) >|< ", Stack =" >|< ppBracketsCommas stkfrs
        footer1 = dash
    hpPP <- dumpHeap hp hpfr
    glPP <- dumpGlobals (renvGlobals env)
    frPPs <- forM stkfrs dumpFrame
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
        RVal_Ptr pref -> do 
          v' <- heapGetM =<< liftIO (readIORef pref)
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

