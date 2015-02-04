%%[0 hs
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE DefaultSignatures, DeriveGeneric, TypeOperators, KindSignatures #-}
-- {-# OPTIONS_GHC -O3 #-}
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

%%[(8 corerun) hs import({%{EH}CoreRun.Pretty}, UHC.Util.Pretty as PP)
%%]

%%[(8 corerun) hs import(UHC.Util.Utils)
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
%%[(8 corerun) hs import(Data.Int, Data.Word, Data.Bits) export(module Data.Int, module Data.Word)
%%]
%%[(8 corerun) hs import(qualified Data.ByteString.Char8 as BSC8)
%%]
%%[(8 corerun) hs import(GHC.Ptr(Ptr(..)), GHC.Exts({Addr#})) export(module GHC.Ptr)
%%]
%%[(8 corerun) hs import(GHC.Generics)
%%]
%%[(8 corerun) hs import(Control.Applicative)
%%]
%%[(8 corerun) hs import(qualified Data.Map as Map, Data.Maybe)
%%]

-- old
%%[(8888 corerun) hs import(Data.Primitive.MutVar)
%%]

%%[(8888 corerun) hs import(GHC.Exts)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Context
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RCxt(..))
-- | Runtime context: module + enclosing lambda. Note/TBD: reduces performance quite a bit...
data RCxt
  = RCtxt
      { rcxtMdRef			::                 (IORef HpPtr)		-- ^ the module we're in
      , rcxtSlRef			:: {-# UNPACK #-} !(IORef HpPtr)		-- ^ the enclosing lambda we're in
      }

instance Show RCxt where
  show _ = "RCxt"
%%]

%%[(8 corerun) hs export(mkRCxtSl, rcxtCloneWithNewFrame)
-- | Make fresh 'RCxt' from module and enclosing frame ptr
mkRCxt :: HpPtr -> HpPtr -> IO RCxt
mkRCxt m f = liftM2 RCtxt (newIORef m) (newIORef f)
{-# INLINE mkRCxt #-}

-- | Make fresh 'RCxt' enclosing frame ptr only (temporary hack)
mkRCxtSl :: HpPtr -> IO RCxt
-- mkRCxtSl = mkRCxt 0
mkRCxtSl f = do
  fref <- newIORef f
  return $ RCtxt undefined fref
{-# INLINE mkRCxtSl #-}

-- | Copy/share module ref, fresh frame ref
rcxtCloneWithNewFrame :: HpPtr -> RCxt -> IO RCxt
rcxtCloneWithNewFrame f cx = do
  f' <- newIORef f
  return $ cx {rcxtSlRef = f'}
{-# INLINE rcxtCloneWithNewFrame #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Run Value
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
{--
  | RVal_Int8  			   {-# UNPACK #-} !Int8  
  | RVal_Int16 			   {-# UNPACK #-} !Int16
-}
  | RVal_Int32 			   {-# UNPACK #-} !Int32 
{--
  | RVal_Int64 			   {-# UNPACK #-} !Int64 
  | RVal_Word  			   {-# UNPACK #-} !Word  
  | RVal_Word8 			   {-# UNPACK #-} !Word8 
  | RVal_Word16			   {-# UNPACK #-} !Word16
  | RVal_Word32			   {-# UNPACK #-} !Word32
  | RVal_Word64			   {-# UNPACK #-} !Word64
-}
  | RVal_Integer		   !Integer
  | RVal_Float			   {-# UNPACK #-} !Float
  | RVal_Double			   {-# UNPACK #-} !Double
  | RVal_PackedString 	   !BSC8.ByteString					-- ^ packed string, equivalent of low level C string (could be replaced by something more efficient)

    -- Value representations for running itself: function, application, etc
  | RVal_Lam
      { rvalMbNm		:: !(Maybe HsName)			-- ^ possibly bound to name
      , rvalBody		:: !Exp						-- ^ a Exp_Lam, which also encodes a thunk
      , rvalCx			:: {-# UNPACK #-} !RCxt		-- ^ links to enclosing context
      }
  -- | special case of Lam taking 0 params
  | RVal_Thunk										
      { rvalMbNm		:: !(Maybe HsName)			-- ^ possibly bound to name
      , rvalBody		:: !Exp						-- ^ Exp taking no arguments (thunk)
      , rvalCx			:: {-# UNPACK #-} !RCxt		-- ^ links to enclosing context
      }
  | RVal_NodeMV
      { rvalTag			:: !Int -- {-# UNPACK #-} !RVal		-- ^ node tag
      , rvalNdMVals		:: !RValMV					-- ^ fields, mutable
      }
{-
  | RVal_NodeV
      { rvalTag			:: !Int -- {-# UNPACK #-} !RVal		-- ^ node tag
      , rvalNdVals		:: !RValV					-- ^ fields, immutable
      }
-}
  | RVal_App
      { rvalFun			:: !RVal					-- ^ a RVal_App or RVal_PApp
      , rvalArgs		:: !RValMV					-- ^ already applied args
      }
  | RVal_Frame
      { rvalRef2Nm		:: Ref2Nm					-- ^ ref to name mapping
      , rvalCx			:: {-# UNPACK #-} !RCxt		-- ^ links to enclosing context
      , rvalFrVals		:: !RValMV					-- ^ actual frame values, either literals or pointers to heap locations (so we can update them, share them)
      , rvalFrSP		:: !(IORef Int)				-- ^ top of expr stack embedded in higher end of top frame
      }
  | RVal_Module
      { rvalModNm		:: HsName					-- ^ name of module
      , rvalModImpsMV	:: !(CRMArray RValModule)	-- ^ imported modules, constructed at link time based on a global set of modules
      , rvalFrRef		:: !(IORef HpPtr)			-- ^ frame of this module
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
  -- | mutable var
  | RHsV_MutVar			   !(IORef RVal)
  -- | IO handle
  | RHsV_Handle			   !Handle
  -- | Addr inside Ptr
  | RHsV_Addr			   Addr#

instance Show RVal where
  show _ = "RVal"

ppRVal' :: (HpPtr -> IO (Maybe RVal)) -> RVal -> IO PP_Doc
ppRVal' lkptr rval = case rval of
    RVal_Lit   			e     		-> dfltpp e
    RVal_Char   		v     		-> dfltpp $ show v
    RVal_Int   			v     		-> dfltpp v
{--
    RVal_Int8   		v     		-> dfltpp $ show v
    RVal_Int16   		v     		-> dfltpp $ show v
-}
    RVal_Int32  		v     		-> dfltpp $ show v
{--
    RVal_Int64 			v     		-> dfltpp $ show v
    RVal_Word   		v     		-> dfltpp $ show v
    RVal_Word8   		v     		-> dfltpp $ show v
    RVal_Word16   		v     		-> dfltpp $ show v
    RVal_Word32  		v     		-> dfltpp $ show v
    RVal_Word64 		v     		-> dfltpp $ show v
-}
    RVal_Integer   		v     		-> dfltpp v
    RVal_Float   		v     		-> dfltpp v
    RVal_Double   		v     		-> dfltpp $ show v
    RVal_PackedString	v    		-> dfltpp $ show v
    RVal_Lam   			mn b _		-> dfltpp b
    RVal_Thunk 			mn e _	 	-> return $ ppBrackets e
    RVal_NodeMV 		t vs 		-> do
                                       vl <- mvecToList vs
                                       return $ t >|< ppBracketsCommas vl
    RVal_App   			f as 		-> dfltpp f -- return $ ppBrackets $ f >|< "@"  >|< (ppParensCommas $ V.toList as)
    RVal_Ptr   			pref    	-> do
                                       p <- readIORef pref
                                       vpp <- lkptr p >>= maybe (return PP.empty) (\v -> ppRVal' lkptr v >>= \vpp -> return $ " -> " >|< vpp)
                                       return $ "*"  >|< p >|< vpp
    RVal_Fwd   			p    		-> return $ "f*" >|< p
    RVal_Frame 			_ rcx {- lv -} vs spref -> do
                                       sl <- readIORef (rcxtSlRef rcx)
                                       sp <- readIORef spref
                                       vl <- mvecToList vs
                                       vlpp <- forM (zip [0..(sp-1)] vl) $ \(i,v) -> ppRVal' lkptr v >>= \vpp -> return $ i >|< ":" >#< vpp
                                       return $ (ppBracketsCommas $ ["sl=" >|< sl, {- "lev=" >|< lv, -} "sz=" >|< MV.length vs, "sp=" >|< sp])
                                                >-< vlist vlpp
    RVal_Module			nm _ frref	-> do
    								   fr <- readIORef frref
    								   return $ ppBracketsCommas ["mod=" >|< nm, "fr=" >|< fr]
    RVal_BlackHole  				-> dfltpp "Hole"
    RVal_None       				-> dfltpp "None"
    RHsV_MutVar   		v    		-> dfltpp "MutVar"
    RHsV_Handle   		h    		-> dfltpp $ show h
    RHsV_Addr	   		p    		-> dfltpp $ show (Ptr p)
  where
    dfltpp :: PP x => x -> IO PP_Doc
    dfltpp = return . pp

ppRValWithHp :: Heap -> RVal -> IO PP_Doc
ppRValWithHp hp = ppRVal' (\p -> heapGetM'' hp p >>= (return . Just))

ppRVal :: RVal -> IO PP_Doc
ppRVal = ppRVal' (\_ -> return Nothing)

instance PP RVal where
  pp rval = unsafePerformIO (ppRVal rval)
%%]

%%[(8 corerun) hs export(mkTuple, mkUnit)
mkTuple :: (RunSem RValCxt RValEnv RVal m a) => [RVal] -> RValT m a
mkTuple vs = liftIO (mvecAllocFillFromV $ crarrayFromList vs) >>= rsemNode 0 >>= rsemPush
{-# INLINE mkTuple #-}

mkUnit :: (RunSem RValCxt RValEnv RVal m a) => RValT m a
mkUnit = mkTuple []
{-# INLINE mkUnit #-}
%%]

%%[(8 corerun) hs export(RValV, RValMV)
-- Vector of RVal
type RValV = CRArray RVal

-- Mutable vector of RVal
type RValMV = CRMArray RVal
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment: contextual
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RValCxt(..), emptyRValCxt)
-- | Environment: context/reader
data RValCxt
  = RValCxt
      { rcxtInRet		:: !Bool						-- ^ in returning context, True by default
      , rcxtCallCxt		:: [Maybe HsName]				-- ^ calling context stack, for debugging only
      , rcxtDatatypeMp	:: RValDatatypeMp				-- ^ dataype info for ffi
      }
      deriving Show

emptyRValCxt :: RValCxt
emptyRValCxt = RValCxt True [] m
  where m = rvalDatatypeMpUnions1
              [ Map.singleton n (emptyRValDataconstrInfo {rdciNm2Tg = Map.singleton n 0, rdciTg2Nm = crarrayFromList [n]})
              | tuparity <- [2..15], let n = "(" ++ replicate (tuparity-1) ',' ++ ")"
              ]
%%]

%%[(8 corerun) hs export(rvalTrEnterLam)
rvalTrEnterLam :: RunSem RValCxt RValEnv RVal m x => Maybe HsName -> RValT m a -> RValT m a
rvalTrEnterLam s = local (\r -> r {rcxtCallCxt = s : rcxtCallCxt r})
%%]

%%[(8 corerun) hs export(mustReturn, needNotReturn, rvalRetEvl, rvalPrimargEvl)
-- | Set return context to True
mustReturn :: RunSem RValCxt RValEnv RVal m x => RValT m a -> RValT m a
mustReturn = local (\r -> r {rcxtInRet = True})
{-# INLINE mustReturn #-}

-- | Set return context to False
needNotReturn :: RunSem RValCxt RValEnv RVal m x => RValT m a -> RValT m a
needNotReturn = local (\r -> r {rcxtInRet = False})
{-# INLINE needNotReturn #-}

-- | Variation of `rsemEvl` in return context
rvalRetEvl :: RunSem RValCxt RValEnv RVal m x => RVal -> RValT m x
rvalRetEvl = mustReturn . rsemEvl
{-# INLINE rvalRetEvl #-}

-- | Variation of `rsemEvl` in primitve argument context
rvalPrimargEvl :: RunSem RValCxt RValEnv RVal m x => RVal -> RValT m x
rvalPrimargEvl x = rvalRetEvl x >>= rsemPop >>= rsemDeref
{-# INLINE rvalPrimargEvl #-}
%%]

%%[(8 corerun) hs export(rcxtUpdDatatypes)
-- | Update with datatype info
rcxtUpdDatatypes :: RunSem RValCxt RValEnv RVal m x => [Mod] -> RValT m RValCxt
rcxtUpdDatatypes mods = do
  cx@(RValCxt {rcxtDatatypeMp=m}) <- ask 
  let cx' = cx {rcxtDatatypeMp = rvalDatatypeMpUnions1 $ m : map rvalDatatypeMpFromMod mods}
  -- liftIO $ print cx'
  return cx'
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
      { hpVals					:: {-# UNPACK #-} !RValMV						-- ^ value array
      , hpFirst					:: {-# UNPACK #-} !HpPtr						-- ^ the ptr of hpVals ! 0
      , hpFree					:: {-# UNPACK #-} !(IORef HpPtr)				-- ^ first array location free for alloc
      , hpSemispaceMultiplier	:: {-# UNPACK #-} !(IORef Rational)				-- ^ multiplier by which to enlarge/shrink subsequent semispace
      }

newHeap :: Int -> IO Heap
newHeap sz = do
  vs <- mvecAllocInit sz
  fr <- newIORef 0
  ml <- newIORef 1.5
  return $ Heap vs 0 fr ml
%%]

%%[(8 corerun) hs
-- | Garbage collect heap (TBD)
heapGcM :: (RunSem RValCxt RValEnv RVal m x) => RVal -> RValT m RVal
-- heapGcM = err $ "CoreRun.Run.Val.heapGcM: GC not yet implemented"
heapGcM curV = do
    -- rsemSetTrace True
    -- rsemTr' True $ "GC starts, curV=" >#< curV
    -- rsemTr $ "GC starts"
    env@(RValEnv {renvHeap=hp@(Heap {hpVals=vsOld, hpSemispaceMultiplier=mlRef, hpFirst=offOld, hpFree=hpFrRef})
                 , renvGcRootStack=rootStkRef, renvTopFrame=topFrRef, renvStack=stkRef
                 , renvGlobalsMV=globals
                 }) <- get
    env' <- liftIO $ do
      ml <- readIORef mlRef
      let szOld  = MV.length vsOld
          szNew  = (round $ fromIntegral szOld * ml) :: Int
          offNew = offOld + szOld
          lwbOld = offOld
          upbOld = offNew
      vsNew <- mvecAllocInit szNew
      greyref <- newIORef offNew
      let -- copy content of old ptr to new loc, leaving a forwarding on to the old location
          copyp p
            -- is this indeed an old ptr?
            | p >= lwbOld && p < upbOld = do
                let pOld = p
                    iOld = pOld - offOld
                v <- MV.read vsOld iOld
                case v of
                  RVal_Fwd pNew -> do
                    -- putStrLn $ "GC copyp Fwd p=" ++ show pOld ++ ", pNew=" ++ show pNew
                    return pNew
                  v -> do
                    pNew <- readIORef greyref
                    let iNew = pNew - offNew
                    MV.write vsNew iNew v
                    MV.write vsOld iOld (RVal_Fwd pNew)
                    writeIORef greyref (pNew+1)
                    -- putStrLn $ "GC copyp Val p=" ++ show pOld ++ ", pNew=" ++ show pNew ++ ", v=" ++ show (pp v)
                    return pNew
            | otherwise = do
                    -- putStrLn $ "GC copyp None p=" ++ show p
                    return p
          
          -- inspect RVal_Ptr only
          copypv v = do
            case v of
              RVal_Ptr pref                               	-> modifyIORefM pref copyp
              _  											-> return ()

          -- inspect, possibly copy internal part of a RVal, stops with a ptr which later is dealt with (to prevent too deep stack growth), exploiting the in between grey/black area as queue
          copyv v = do
            -- putStrLn $ "GC copyv 1 v=" ++ show (pp v)
            case v of
              RVal_Ptr pref                               	-> modifyIORefM pref copyp
              RVal_Frame {rvalCx=rcx, rvalFrVals=vs, rvalFrSP=spref}
              												-> modifyIORefM (rcxtSlRef rcx) copyp >> readIORef spref >>= \sp -> mvecForM_' 0 sp vs copyv
              RVal_NodeMV {rvalNdMVals=vs} 					-> mvecForM_ vs copyv
              RVal_App {rvalFun=f, rvalArgs=as} 			-> copyv f >> mvecForM_ as copyv
              RVal_Thunk {rvalCx=rcx}						-> modifyIORefM (rcxtSlRef rcx) copyp
              RVal_Lam {rvalCx=rcx}							-> modifyIORefM (rcxtSlRef rcx) copyp
              _  											-> return ()
            -- putStrLn $ "GC copyv 2 v=" ++ show (pp v)

          -- inspect, follow, copy content of RVal
          follow pBlk pGry
            | pBlk < pGry = MV.read vsNew (pBlk-offNew) >>= copyv >> follow (pBlk+1) pGry
            | otherwise   = do
                pGry' <- readIORef greyref
                if pBlk < pGry' then follow pBlk pGry' else return pBlk
      
      -- initial copy: top frame
      modifyIORefM topFrRef copyp --- $ \topFr -> {- if isNullPtr topFr then return topFr else -} copyp topFr

      -- initial copy: stack
      modifyIORefM stkRef $ mapM copyp
        
      -- initial copy: globals
      mvecLoop 0 (MV.length globals) (\v i -> MV.read v i >>= copyp >>= MV.write v i) globals
      
      -- initial copy: the RVal to be put on the heap
      copyv curV
      
      -- initial copy: additional roots
      readIORef rootStkRef >>= mapM_ (mapM_ copyv)
      
      -- follow with initial values of grey and black (0)
      readIORef greyref >>= follow offNew >>= \p -> writeIORef hpFrRef (p - offNew)
        
      -- final: 
      return $ env { renvHeap = hp {hpVals=vsNew, hpFirst=offNew}}

    put env'
    -- rsemTr $ "GC done"
    -- rsemTr' True $ "GC done, curV=" >#< curV
    -- rsemSetTrace False
    return curV
%%]

%%[(8 corerun) hs export(heapGetM, heapGetM', heapAllocM, heapAllocAsPtrM, heapUpdM, heapSetM, heapSetM')
-- | Allocate on the heap
heapAllocM :: (RunSem RValCxt RValEnv RVal m x) => RVal -> RValT m HpPtr
heapAllocM v = do
  hp@(Heap {hpVals=vs, hpFirst=off, hpFree=fr}) <- gets renvHeap
  p <- liftIO $ readIORef fr
  if p >= MV.length vs
    then heapGcM v >>= heapAllocM
    else liftIO $ do
      MV.write vs p v
      writeIORef fr (p + 1)
      return $ p + off

-- | Allocate on the heap, packing as RVal_Ptr
heapAllocAsPtrM :: (RunSem RValCxt RValEnv RVal m x) => RVal -> RValT m RVal
heapAllocAsPtrM v = do
  p <- heapAllocM v
  liftIO (newIORef p) >>= (return . RVal_Ptr)

{-
-}
-- | Get a value from the heap
-- heapGetM'' :: PrimMonad m => Heap -> HpPtr -> m RVal
heapGetM'' :: Heap -> HpPtr -> IO RVal
heapGetM'' hp@(Heap {hpVals=vs, hpFirst=off}) p = MV.read vs (p - off)
{-# INLINE heapGetM'' #-}

-- | Get a value from the heap
heapGetM' :: (RunSem RValCxt RValEnv RVal m x) => Heap -> HpPtr -> RValT m RVal
heapGetM' hp p = liftIO $ heapGetM'' hp p
{-# INLINE heapGetM' #-}

-- | Get a value from the heap
heapGetM :: (RunSem RValCxt RValEnv RVal m x) => HpPtr -> RValT m RVal
heapGetM p = do
  hp <- gets renvHeap
  heapGetM' hp p
{-# INLINE heapGetM #-}

-- | Set a value in the heap
heapSetM' :: (RunSem RValCxt RValEnv RVal m x) => Heap -> HpPtr -> RVal -> RValT m ()
heapSetM' hp@(Heap {hpVals=vs, hpFirst=off}) p v = liftIO $ MV.write vs (p - off) v
{-# INLINE heapSetM' #-}

-- | Set a value in the heap
heapSetM :: (RunSem RValCxt RValEnv RVal m x) => HpPtr -> RVal -> RValT m ()
heapSetM p v = do
  hp <- gets renvHeap
  heapSetM' hp p v
{-# INLINE heapSetM #-}

-- | Update a value in the heap
heapUpdM' :: (RunSem RValCxt RValEnv RVal m x) => Heap -> HpPtr -> (RVal -> RValT m RVal) -> RValT m ()
heapUpdM' hp@(Heap {hpVals=vs, hpFirst=off}) p f = do
  let p' = p - off
  v <- liftIO $ MV.read vs p'
  v' <- f v
  liftIO $ MV.write vs p' v'
{-# INLINE heapUpdM' #-}

-- | Update a value in the heap
heapUpdM :: (RunSem RValCxt RValEnv RVal m x) => HpPtr -> (RVal -> RValT m RVal) -> RValT m ()
heapUpdM p f = do
  hp <- gets renvHeap
  heapUpdM' hp p f
{-# INLINE heapUpdM #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Info for datatype constructor used by FFI and marshalling
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Mapping between constr name and tag
type RValDataconstrInfoNm2Tg = Map.Map String Int		-- from constructor name to tag
type RValDataconstrInfoTg2Nm = CRArray String			-- from tag to constructor Name

-- | Mapping between constr name and tag
data RValDataconstrInfo
  = RValDataconstrInfo
      { rdciNm2Tg 	:: RValDataconstrInfoNm2Tg		-- from constructor name to tag
      , rdciTg2Nm 	:: RValDataconstrInfoTg2Nm		-- from tag to constructor Name
      , rdciMods	:: [HsName]						-- modules (if any in which possibly multiple defs occur, >1 being an error when actually used)
      }
      deriving Show

emptyRValDataconstrInfo = RValDataconstrInfo Map.empty emptyCRArray []

-- | Mapping from type name to constr info
type RValDatatypeMp
  = Map.Map
      String						-- type name
      RValDataconstrInfo			-- data constr info

-- | Union
rvalDatatypeMpUnion :: RValDatatypeMp -> RValDatatypeMp -> RValDatatypeMp
rvalDatatypeMpUnion = Map.unionWith (\l r -> l {rdciMods = rdciMods l ++ rdciMods r})

-- | Unions
rvalDatatypeMpUnions1 :: [RValDatatypeMp] -> RValDatatypeMp
rvalDatatypeMpUnions1 = foldr1 rvalDatatypeMpUnion

-- | Extract datatype mapping from module
rvalDatatypeMpFromMod :: Mod -> RValDatatypeMp
rvalDatatypeMpFromMod (Mod_Mod {metas_Mod_Mod=metas})
  = Map.fromList
      [ ( show tn'
        , emptyRValDataconstrInfo
            { rdciNm2Tg = Map.fromList cts
            , rdciTg2Nm = crarrayFromList $ map fst $ sortOn snd cts
            , rdciMods  = mods
            })
      | Meta_Data {tyNm_Meta_Data=tn, dataCons_Meta_Data=constrs} <- metas
      , let cts = assocLMapKey show $ candtg constrs
            (tn',mods) = splitTN tn
      ]
  where candtg cs = [ (cn,t) | DataCon_Con {conNm_DataCon_Con=cn, tagNr_DataCon_Con=t} <- cs ]
%%[[8
        splitTN n = (n,[])
%%][50
        splitTN n = (hsnQualified n, maybe [] (:[]) $ hsnQualifier n)
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment: state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RValEnv(..), newRValEnv)
-- | Frame holding locals indexed by RRef_Loc
type RValFrame 	= HpPtr		-- ^ points in heap to a RVal_Frame
type RValModule = HpPtr		-- ^ points in heap to a RVal_Module

-- | Frame Stack
type RValStack = [RValFrame]

-- | Environment: state
data RValEnv
  = RValEnv
      { renvModulesMV	:: !(CRMArray RValModule)		-- ^ all modules
      , renvGlobalsMV	:: !(CRMArray RValFrame)		-- ^ per module frame of globals (will be obsolete)
      , renvStack		:: !(IORef RValStack)			-- ^ stack of frames, except for the top
      , renvTopFrame	:: !(IORef RValFrame)			-- ^ current frame, the actual top of the stack
      -- , renvFrSP		:: !(IORef Int)					-- ^ top of expr stack embedded in higher end of top frame
      , renvHeap		:: !Heap						-- ^ heap
      , renvDoTrace		:: !Bool
      , renvDoTraceExt	:: !Bool						-- ^ when tracing, do it extensively?
      , renvGcRootStack	:: !(IORef [[RVal]])			-- ^ stack of roots for GC, use is optional
      }

newRValEnv :: Int -> IO RValEnv
newRValEnv hpSz = do
  st <- newIORef []
  tp <- newIORef nullPtr
  hp <- newHeap hpSz
  rtst <- newIORef []
  md <- MV.new 0
  gl <- MV.new 0
  return $ RValEnv md gl st tp hp False False rtst
%%]

%%[(8 corerun) hs export(renvTopFramePtrM, renvTopFramePtrAndFrameM, renvTopFrameM)
-- | Get the top most frame ptr from the stack, 'nullPtr' if absent
renvTopFramePtrM :: (RunSem RValCxt RValEnv RVal m x) => RValT m HpPtr
renvTopFramePtrM = do
  (RValEnv {renvTopFrame=tf}) <- get
  liftIO $ readIORef tf
{-# INLINE renvTopFramePtrM #-}

-- | Get the top most frame ptr and frame from the stack, assuming a non 'nullPtr' ptr
renvTopFramePtrAndFrameM :: (RunSem RValCxt RValEnv RVal m x) => RValT m (HpPtr,RVal)
renvTopFramePtrAndFrameM = do
  frp <- renvTopFramePtrM
  fr  <- heapGetM frp
  return (frp,fr)
{-# INLINE renvTopFramePtrAndFrameM #-}

-- | Get the top most frame from the stack, assuming a non 'nullPtr' ptr
renvTopFrameM :: (RunSem RValCxt RValEnv RVal m x) => RValT m RVal
renvTopFrameM = renvTopFramePtrM >>= heapGetM
{-# INLINE renvTopFrameM #-}
%%]

%%[(8 corerun) hs
-- | Get all non empty stack frames, still split up in (possibly null) top frame and rest of stack
renvAllFrameM' :: (RunSem RValCxt RValEnv RVal m x) => RValT m (Maybe HpPtr,[HpPtr])
renvAllFrameM' = do
  topfrp <- renvTopFramePtrM
  env <- get
  st <- liftIO $ readIORef (renvStack env)
  return (if isNullPtr topfrp then Nothing else Just topfrp, st)

-- | Get all non empty stack frames
renvAllFrameM :: (RunSem RValCxt RValEnv RVal m x) => RValT m [HpPtr]
renvAllFrameM = do
  (mbtop,st) <- renvAllFrameM'
  return $ maybe [] (:[]) mbtop ++ st
%%]

%%[(8 corerun) hs
-- | Dump environment
dumpPpEnvM :: (RunSem RValCxt RValEnv RVal m x) => Bool -> RValT m PP_Doc
dumpPpEnvM extensive = do
    stkfrs <- renvAllFrameM
    env <- get
    callcxt <- asks rcxtCallCxt
    let hp = renvHeap env
    stkfrspp <- forM stkfrs $ dumpFrameMinimal hp
    hpfr <- liftIO $ readIORef (hpFree hp)
    needRet <- asks rcxtInRet
    let dash    = "===================="
        header1 = dash >-< "rcxtInRet=" >|< needRet
        header2 = ppCurly $ "Heap =" >|< hpfr >|< "/" >|< MV.length (hpVals hp) >|< ", CallCxt=" >|< ppBracketsCommas callcxt >|< ", Stack=" >|< ppBracketsCommas stkfrspp
        footer1 = dash
    hpPP <- dumpHeap hp hpfr
    glPP <- dumpGlobalsMV hp (renvGlobalsMV env)
    frPPs <- forM stkfrs $ dumpFrame hp
    if extensive
      then return $ header1 >-< header2 >-< hpPP >-< glPP >-< "====== Frames ======" >-< (indent 2 $ vlist frPPs) >-< footer1
      else return $ header2
  where
    dumpFrameMinimal hp fp = do
      fr@(RVal_Frame {rvalFrVals=vs, rvalFrSP=spref}) <- heapGetM fp
      sp  <- liftIO $ readIORef spref
      return $ fp >|< (ppParens $ sp >|< "/" >|< MV.length vs)
    dumpFrame hp fp = do
      fr@(RVal_Frame {rvalFrVals=vs, rvalFrSP=spref}) <- heapGetM fp
      sp  <- liftIO $ readIORef spref
      -- pps <- ppa 0 hp sp vs
      (liftIO $ ppRValWithHp hp fr) >>= \frpp -> return $ "Frame ptr=" >|< fp >|< " sp=" >|< sp >-< (indent 2 $ frpp) --  >-< (indent 2 $ vlist pps))
    dumpGlobalsMV hp glbls = do
      pps <- liftIO (mvecToList glbls) >>= \l -> forM l $ dumpFrame hp
      return $ "====== Globals ======" >-< indent 2 (vlist pps)
    dumpHeap hp@(Heap {hpFirst=off}) hpfr = do
      pps <- ppa off hp hpfr (hpVals hp)
      return $ "======= Heap =======" >-< indent 2 (vlist pps)
    ppb hp k v = (liftIO $ ppRValWithHp hp v) >>= \vpp -> return $ k >|< ":" >#< vpp
    ppa off hp sz vs = forM [0 .. sz - 1] $ \i -> liftIO (MV.read vs i) >>= ppb hp (i + off)
%%]

%%[(8 corerun) hs export(dumpEnvM)
-- | Dump environment
dumpEnvM :: (RunSem RValCxt RValEnv RVal m x) => Bool -> RValT m ()
dumpEnvM extensive = dumpPpEnvM extensive >>= \p -> liftIO $ putPPLn p >> hFlush stdout
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem, RunT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(RValT)
-- | RunT' variant for Val
type RValT m a = RunT' RValCxt RValEnv RVal m a
-- type RValT m a = RunT' () () RValEnv m a

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Marshalling from/to Haskell values which should be visible/usable/correspond in both the RVal and Haskell world
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(HSMarshall(..))
-- | Marshalling from/to Haskell values
class HSMarshall hs where
  -- | Marshall to Haskell value, also parameterized by evaluator
  hsMarshall :: (RunSem RValCxt RValEnv RVal m a) => (RVal -> RValT m a) -> RVal -> RValT m hs
  default hsMarshall :: (Generic hs, GHSMarshall (Rep hs), RunSem RValCxt RValEnv RVal m a) => (RVal -> RValT m a) -> RVal -> RValT m hs
  hsMarshall evl v = to <$> ghsMarshall evl v
  {-# INLINE hsMarshall #-}

  -- | Unmarshall from Haskell value
  hsUnmarshall :: (RunSem RValCxt RValEnv RVal m a) => hs -> RValT m a
  default hsUnmarshall :: (Generic hs, GHSMarshall (Rep hs), RunSem RValCxt RValEnv RVal m a) => hs -> RValT m a
  hsUnmarshall = ghsUnmarshall . from
  {-# INLINE hsUnmarshall #-}
%%]

%%[(8 corerun) hs
instance HSMarshall Int where
  hsMarshall _ (RVal_Int v) = return v
  hsMarshall _ v            = err $ "CoreRun.Run.Val.HSMarshall Int:" >#< v
  hsUnmarshall v = rsemPush $ RVal_Int v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall Integer where
  hsMarshall _ (RVal_Integer v) = return v
  hsMarshall _ v                = err $ "CoreRun.Run.Val.HSMarshall Integer:" >#< v
  hsUnmarshall v = rsemPush $ RVal_Integer v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall Bool {- where
  hsMarshall _ (RVal_NodeMV t _) = return $ t == tagBoolTrue
  hsMarshall _ v                 = err $ "CoreRun.Run.Val.HSMarshall Bool:" >#< v
  hsUnmarshall b = liftIO (mvecAllocFillFromV emptyCRArray) >>= rsemNode (if b then tagBoolTrue else tagBoolFalse) >>= rsemPush
  {-# INLINE hsUnmarshall #-}
  -}

instance HSMarshall Char where
  hsMarshall _ (RVal_Char v) = return v
  hsMarshall _ v             = err $ "CoreRun.Run.Val.HSMarshall Char:" >#< v
  hsUnmarshall v = rsemPush $ RVal_Char v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall (Ptr a) where
  hsMarshall _ (RHsV_Addr v) = return $ Ptr v
  hsMarshall _ v             = err $ "CoreRun.Run.Val.HSMarshall (Ptr a):" >#< v
  hsUnmarshall (Ptr v) = rsemPush $ RHsV_Addr v
  {-# INLINE hsUnmarshall #-}

instance HSMarshall Handle where
  hsMarshall _ (RHsV_Handle v) = return v
  hsMarshall _ v               = err $ "CoreRun.Run.Val.HSMarshall Handle:" >#< v
  hsUnmarshall v = rsemPush $ RHsV_Handle v
  {-# INLINE hsUnmarshall #-}

{-
instance HSMarshall [RVal] where
  hsMarshall evl (RVal_NodeMV t as)
    | t == tagListCons = liftIO (MV.read as 1) >>= evl >>= rsemPop >>= hsMarshall evl >>= \tl -> liftIO (MV.read as 0) >>= \hd -> return (hd : tl)
    | otherwise        = return []
  hsMarshall _   v     = err $ "CoreRun.Run.Val.HSMarshall [RVal]:" >#< v

  hsUnmarshall []      = liftIO (mvecAllocFillFromV emptyCRArray) >>= rsemNode tagListNil >>= rsemPush
  hsUnmarshall (h:t)   = hsUnmarshall t >>= rsemPop >>= \t' -> (liftIO $ mvecAllocFillFromV $ crarrayFromList [h, t']) >>= rsemNode tagListCons >>= rsemPush

instance HSMarshall x => HSMarshall [x] where
  hsMarshall evl x = hsMarshall evl x >>= mapM (\v -> evl v >>= rsemPop >>= hsMarshall evl)
  {-# INLINE hsMarshall #-}

  hsUnmarshall v       = forM v (\e -> hsUnmarshall e >>= rsemPop) >>= hsUnmarshall
  {-# INLINE hsUnmarshall #-}
-}

instance HSMarshall x => HSMarshall [x]
%%]

%%[(8 corerun) hs
-- | Generic marshalling from/to Haskell values
class GHSMarshall hs where
  -- | Marshall to Haskell value, also parameterized by evaluator
  ghsMarshall :: (RunSem RValCxt RValEnv RVal m a) => (RVal -> RValT m a) -> RVal -> RValT m (hs x)

  -- | Unmarshall from Haskell value
  ghsUnmarshall :: (RunSem RValCxt RValEnv RVal m a) => hs x -> RValT m a
  
instance (Datatype d, MarshallSum hs) => GHSMarshall (D1 d hs) where
  --
  ghsMarshall evl v = do
      dtmp <- asks rcxtDatatypeMp
      let nm = datatypeName (undefined :: t d hs p)
      case Map.lookup nm dtmp of
        Nothing -> err $ "Marshall to HS lacks datatype info, datatype=" ++ nm
        Just (RValDataconstrInfo {rdciMods=mods@(_:_:_)}) ->
          err $ "Marshall to HS datatype info in multiple modules, datatype=" ++ nm ++ ", mods=" ++ show mods
        Just (RValDataconstrInfo {rdciTg2Nm=tg2con}) -> do
          r <- sumExtrTagged evl tg2con v
          maybe (err $ "sumExtrTagged") (return . M1) r
  --
  ghsUnmarshall (M1 x) = do
      dtmp <- asks rcxtDatatypeMp
      let nm = datatypeName (undefined :: t d hs p)
      case Map.lookup nm dtmp of
        Nothing -> err $ "Marshall from HS lacks datatype info, datatype=" ++ nm
        Just (RValDataconstrInfo {rdciMods=mods@(_:_:_)}) ->
          err $ "Marshall from HS datatype info in multiple modules, datatype=" ++ nm ++ ", mods=" ++ show mods
        Just (RValDataconstrInfo {rdciNm2Tg=con2tg}) ->
          sumFillTagged con2tg x
%%]

%%[(8 corerun) hs
-- | Marshall util: extract values from sum
class MarshallSum hs where
  --
  sumFillTagged
    :: (RunSem RValCxt RValEnv RVal m a)
    => RValDataconstrInfoNm2Tg	-- ^ mapping from constructor name to tag
       -> hs x 					-- ^ Haskell value
       -> RValT m a
  --
  sumExtrTagged
    :: (RunSem RValCxt RValEnv RVal m a)
    => (RVal -> RValT m a)		-- ^ force evaluation for nested fields
       -> RValDataconstrInfoTg2Nm	-- ^ mapping from constructor name to tag
       -> RVal					-- ^ the 'RVal_NodeMV' holding tag and fields
       -> RValT m (Maybe (hs x))

instance ( MarshallProduct hs, ProductSize hs, Constructor c ) => MarshallSum (C1 c hs) where
  --
  sumFillTagged con2tg (M1 x) = do
      v <- liftIO $ MV.unsafeNew len
      productFillMVec v 0 len x
      let nm = conName (undefined :: t c hs p)
          mt = Map.lookup nm con2tg
      when (isNothing mt) $ err $ "Marshall from HS lacks constructor info, constructor=" ++ nm
      rsemNode (fromJust mt) v >>= rsemPush
    where
      len = (unTagged2 :: Tagged2 hs Int -> Int) productSize
      tag = Map.findWithDefault 0 (conName (undefined :: t c hs p)) con2tg
  {-# INLINE sumFillTagged #-}
  --
  sumExtrTagged evl tg2con v@(RVal_NodeMV {rvalTag=t, rvalNdMVals=mv})
    | t >= V.length tg2con = err $ "Marshall to HS illegal tag value, con=" ++ nmc ++ ", tag=" ++ show t
    | nmc == nmt           = (Just . M1) <$> productExtrMVec evl mv 0 len
    | otherwise            = return $ Nothing -- Left $ "Marshall to HS lacks constructor info, con=" ++ nmc ++ "/" ++ nmt ++ ", tag=" ++ show t
    where
      len = (unTagged2 :: Tagged2 hs Int -> Int) productSize
      nmc = conName (undefined :: t c hs p)
      nmt = tg2con V.! t
  {-# INLINE sumExtrTagged #-}

instance ( MarshallSum a, MarshallSum b ) => MarshallSum (a :+: b) where
  --
  sumFillTagged con2tg (L1 x) = sumFillTagged con2tg x
  sumFillTagged con2tg (R1 x) = sumFillTagged con2tg x
  {-# INLINE sumFillTagged #-}
  --
  -- sumExtrTagged evl tg2con v = (fmap L1 <$> sumExtrTagged evl tg2con v) <|> (fmap R1 <$> sumExtrTagged evl tg2con v)
  sumExtrTagged evl tg2con v = do
    l <- sumExtrTagged evl tg2con v
    case l of
      Just l' -> return $ fmap L1 l
      _       -> fmap R1 <$> sumExtrTagged evl tg2con v
  {-# INLINE sumExtrTagged #-}

%%]

%%[(8 corerun) hs
-- | Marshall util: extract values from product (inspired by Aeson library)
class MarshallProduct hs where
  -- | Fill vector from product
  productFillMVec
    :: (RunSem RValCxt RValEnv RVal m a)
    => CRMArray RVal	-- ^ fields vector
       -> Int 			-- ^ index
       -> Int 			-- ^ length
       -> hs x 			-- ^ Haskell value
       -> RValT m ()
  -- | Extract product from vector
  productExtrMVec
    :: (RunSem RValCxt RValEnv RVal m a)
    => (RVal -> RValT m a)		-- ^ force evaluation for nested fields
       -> CRMArray RVal			-- ^ fields vector
       -> Int					-- ^ index
       -> Int					-- ^ length
       -> RValT m (hs x)

instance (MarshallProduct a, MarshallProduct b) => MarshallProduct (a :*: b) where
  productFillMVec mv ix len (a :*: b) = do
      productFillMVec mv ix  lenL a
      productFillMVec mv ixR lenR b
    where
      lenL = len `unsafeShiftR` 1
      ixR  = ix + lenL
      lenR = len - lenL
  {-# INLINE productFillMVec #-}

  productExtrMVec evl mv ix len =
      (:*:) <$> productExtrMVec evl mv ix  lenL
            <*> productExtrMVec evl mv ixR lenR
    where
      lenL = len `unsafeShiftR` 1
      ixR  = ix + lenL
      lenR = len - lenL
  {-# INLINE productExtrMVec #-}

instance MarshallProduct hs => MarshallProduct (S1 s hs) where
  productFillMVec mv ix l (M1 x) = productFillMVec mv ix l x
  {-# INLINE productFillMVec #-}

  productExtrMVec evl mv ix l = M1 <$> productExtrMVec evl mv ix l
  {-# INLINE productExtrMVec #-}

instance HSMarshall x => MarshallProduct (K1 i x) where
  productFillMVec mv ix _ (K1 x) = do
    x' <- rsemPop =<< hsUnmarshall x
    liftIO $ MV.unsafeWrite mv ix x'
  {-# INLINE productFillMVec #-}

  productExtrMVec evl mv ix _ = do
    v <- (liftIO $ MV.read mv ix) >>= evl >>= rsemPop
    K1 <$> hsMarshall evl v
  {-# INLINE productExtrMVec #-}

instance MarshallProduct U1 where
  productFillMVec mv ix _ U1 = return ()
  {-# INLINE productFillMVec #-}

  productExtrMVec evl mv ix _ = return U1
  {-# INLINE productExtrMVec #-}
%%]

%%[(8 corerun) hs
-- | Phantom type tagging (from Aeson lib)
newtype Tagged s b = Tagged {unTagged :: b}

-- | Phantom type tagging for higher kinds (from Aeson lib)
newtype Tagged2 (s :: * -> *) b = Tagged2 {unTagged2 :: b}
%%]

%%[(8 corerun) hs
-- | Size (nr of fields) of data type constructors (from Aeson lib)
class ProductSize f where
  productSize :: Tagged2 f Int

instance (ProductSize a, ProductSize b) => ProductSize (a :*: b) where
  productSize = Tagged2 $ unTagged2 (productSize :: Tagged2 a Int) +
                          unTagged2 (productSize :: Tagged2 b Int)
  {-# INLINE productSize #-}

instance ProductSize (S1 s a) where
  productSize = Tagged2 1
  {-# INLINE productSize #-}

instance ProductSize U1 where
  productSize = Tagged2 0
  {-# INLINE productSize #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Vector utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
{-
-- | Loop over a vector, starting at lower bound 'l', ending before 'h'
vecLoop :: Monad m => Int -> Int -> (V.Vector a -> Int -> m b) -> V.Vector a -> m ()
vecLoop l h m v = loop l
  where loop l | l < h     = m v l >> loop (l+1)
               | otherwise = return ()
{-# INLINE vecLoop #-}
-}

-- | Loop over a vector from upb to lwb, starting before 'h', ending at lower bound 'l'
vecLoopReverse :: Monad m => Int -> Int -> (V.Vector a -> Int -> m b) -> V.Vector a -> m ()
vecLoopReverse l h m v = loop (h-1)
  where loop h | l <= h    = m v h >> loop (h-1)
               | otherwise = return ()
{-# INLINE vecLoopReverse #-}

%%]

%%[(8 corerun) hs export(vecReverseForM_)
-- | Right to left forM_
vecReverseForM_ :: Monad m => V.Vector a -> (a -> m x) -> m ()
vecReverseForM_ v m = vecLoopReverse 0 (V.length v) (\_ i -> m (v V.! i)) v
{-# INLINE vecReverseForM_ #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Mutable Vector utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(mvecAllocInit, mvecAlloc, mvecFillFromV, mvecReverseFillFromV, mvecFillFromMV, mvecAllocFillFromV, mvecAppend, mvecToList)
-- | Allocate a mutable vector of given size
mvecAllocWith :: PrimMonad m => Int -> a -> m (MV.MVector (PrimState m) a)
mvecAllocWith = MV.replicate
{-# INLINE mvecAllocWith #-}

-- | Allocate a mutable vector of given size, init to default value
mvecAllocInit :: Int -> IO RValMV
mvecAllocInit sz = mvecAllocWith sz RVal_None
{-# INLINE mvecAllocInit #-}

-- | Allocate a mutable vector of given size
mvecAlloc :: PrimMonad m => Int -> m (MV.MVector (PrimState m) a)
mvecAlloc = MV.new
{-# INLINE mvecAlloc #-}

-- | Loop over a mutable vector, updating the vector as a side effect, starting at lower bound 'l', ending before 'h'
mvecLoop :: PrimMonad m => Int -> Int -> (MV.MVector (PrimState m) a -> Int -> m b) -> MV.MVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
mvecLoop l h m v = loop l
  where loop l | l < h     = m v l >> loop (l+1)
               | otherwise = return v
{-# INLINE mvecLoop #-}

-- | Loop over a mutable vector from upb to lwb, updating the vector as a side effect, starting before 'h', ending at lower bound 'l'
mvecLoopReverse :: PrimMonad m => Int -> Int -> (MV.MVector (PrimState m) a -> Int -> m b) -> MV.MVector (PrimState m) a -> m (MV.MVector (PrimState m) a)
mvecLoopReverse l h m v = loop (h-1)
  where loop h | l <= h    = m v h >> loop (h-1)
               | otherwise = return v
{-# INLINE mvecLoopReverse #-}

-- | Loop over a mutable vector from upb to lwb, updating the vector as a side effect, starting before 'h', ending at lower bound 'l'
mvecLoopReverseAccum :: PrimMonad m => acc -> Int -> Int -> (acc -> Int -> m acc) -> MV.MVector (PrimState m) a -> m acc
mvecLoopReverseAccum a l h m v = loop (h-1) a
  where loop h a | l <= h    = m a h >>= loop (h-1)
                 | otherwise = return a
{-# INLINE mvecLoopReverseAccum #-}

-- | Convert to a list
mvecToList :: PrimMonad m => MV.MVector (PrimState m) a -> m [a]
mvecToList v = mvecLoopReverseAccum [] 0 (MV.length v) (\l i -> MV.read v i >>= \a -> return (a:l)) v 

-- | Fill a mutable vector from a unmutable vector, starting with filling at the given lowerbound
mvecFillFromV :: PrimMonad m => Int -> MV.MVector (PrimState m) a -> CRArray a -> m ()
mvecFillFromV lwb toarr frarr = forM_ (craAssocs' lwb frarr) $ \(i,e) -> MV.write toarr i e
{-# INLINE mvecFillFromV #-}

-- | Fill a mutable vector from a unmutable vector, starting with filling at the given lowerbound, reversing the given vector
mvecReverseFillFromV :: PrimMonad m => Int -> MV.MVector (PrimState m) a -> CRArray a -> m ()
mvecReverseFillFromV lwb toarr frarr = forM_ (craReverseAssocs' lwb frarr) $ \(i,e) -> MV.write toarr i e
{-# INLINE mvecReverseFillFromV #-}

-- | Fill a mutable vector from another mutable vector, starting with copying at the given lowerbounds, copying size elements
mvecFillFromMV' :: PrimMonad m => Int -> Int -> Int -> MV.MVector (PrimState m) a -> MV.MVector (PrimState m) a -> m ()
mvecFillFromMV' lwbTo lwbFr sz toarr frarr = mvecLoop lwbFr (lwbFr+sz) (\v i -> MV.read v i >>= MV.write toarr (i+lwbDiff)) frarr >> return ()
  where lwbDiff = lwbTo - lwbFr
{-# INLINE mvecFillFromMV' #-}

-- | Fill a mutable vector from another mutable vector, starting with copying at the given lowerbounds, copying size elements, but reversing the given vector
mvecReverseFillFromMV' :: PrimMonad m => Int -> Int -> Int -> MV.MVector (PrimState m) a -> MV.MVector (PrimState m) a -> m ()
-- mvecReverseFillFromMV' lwbTo lwbFr sz toarr frarr = mvecLoop lwbFr (lwbFr+sz) (\v i -> MV.read v (upbFr1-i) >>= MV.write toarr (i+lwbDiff)) frarr >> return ()
mvecReverseFillFromMV' lwbTo lwbFr sz toarr frarr = mvecLoopReverse lwbFr (lwbFr+sz) (\v i -> MV.read v i >>= MV.write toarr (upbFr1 - i + lwbTo)) frarr >> return ()
  where lwbDiff = lwbTo - lwbFr
        upbFr1  = lwbFr + sz - 1
{-# INLINE mvecReverseFillFromMV' #-}

-- | Fill a mutable vector from another mutable vector, starting with filling at the given lowerbound
mvecFillFromMV :: PrimMonad m => Int -> MV.MVector (PrimState m) a -> MV.MVector (PrimState m) a -> m ()
-- mvecFillFromMV lwb toarr frarr = mvecLoop 0 (MV.length frarr) (\v i -> MV.read frarr i >>= MV.write toarr (i+lwb)) frarr >> return ()
mvecFillFromMV lwb toarr frarr = mvecFillFromMV' lwb 0 (MV.length frarr) toarr frarr
{-# INLINE mvecFillFromMV #-}
-- {-# SPECIALIZE mvecFillFromMV :: Int -> MV.MVector (PrimState IO) a -> MV.MVector (PrimState IO) a -> IO () #-}

-- | Fill a mutable vector from another mutable vector, starting with filling at the given lowerbound, but reversing the given vector
mvecReverseFillFromMV :: PrimMonad m => Int -> MV.MVector (PrimState m) a -> MV.MVector (PrimState m) a -> m ()
mvecReverseFillFromMV lwb toarr frarr = mvecReverseFillFromMV' lwb 0 (MV.length frarr) toarr frarr
{-
mvecReverseFillFromMV lwb toarr frarr = mvecLoop 0 l (\v i -> MV.read frarr (l1-i) >>= MV.write toarr (i+lwb)) frarr >> return ()
  where l  = MV.length frarr
        l1 = l - 1
-}
{-# INLINE mvecReverseFillFromMV #-}

-- | Alloc and fill vector of size taken from fixed vec
mvecAllocFillFromV :: PrimMonad m => CRArray a -> m (MV.MVector (PrimState m) a)
mvecAllocFillFromV frarr = mvecAlloc (V.length frarr) >>= \toarr -> mvecFillFromV 0 toarr frarr >> return toarr
{-# INLINE mvecAllocFillFromV #-}

-- | Loop over a mutable vector, updating the vector as a side effect, with explicit range
mvecForM_' :: PrimMonad m => Int -> Int -> MV.MVector (PrimState m) a -> (a -> m b) -> m ()
mvecForM_' lwb upb v m = mvecLoop lwb upb (\v i -> MV.read v i >>= m) v >> return ()
{-# INLINE mvecForM_' #-}

-- | Loop over a mutable vector, updating the vector as a side effect
mvecForM_ :: PrimMonad m => MV.MVector (PrimState m) a -> (a -> m b) -> m ()
mvecForM_ v m = mvecForM_' 0 (MV.length v) v m
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

%%[(8 corerun) hs export(mvecReverseForM_)
-- | Right to left forM_
mvecReverseForM_ :: PrimMonad m => MV.MVector (PrimState m) a -> (a -> m x) -> m ()
mvecReverseForM_ v m = mvecLoopReverse 0 (MV.length v) (\v i -> MV.read v i >>= m) v >> return ()
{-# INLINE mvecReverseForM_ #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Dereferencing: ptr, RRef
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(ptr2valM, ref2valM)
-- | Dereference a possibly RVal_Ptr
ptr2valM :: (RunSem RValCxt RValEnv RVal m x) => RVal -> RValT m RVal
ptr2valM v = case v of
  RVal_Ptr pref -> liftIO (readIORef pref) >>= heapGetM >>= \v' -> case v' of
                     RVal_Thunk {} -> return v
                     _             -> ptr2valM v'
  _             -> return v
{-# INLINE ptr2valM #-}

-- | Dereference a RRef
ref2valM :: (RunSem RValCxt RValEnv RVal m x) => RRef -> RValT m RVal
ref2valM r = do
  -- rsemTr $ "R: " ++ show (pp r)
  env <- get
  case r of
    RRef_Glb m e -> do
        modFrame <- heapGetM =<< liftIO (MV.read (renvGlobalsMV env) m)
        liftIO $ MV.read (rvalFrVals modFrame) e
{-
    RRef_Loc l o -> do
        topfrp <- renvTopFramePtrM
        topfr <- heapGetM topfrp
        access topfr
      where
        access (RVal_Frame {rvalLev=frlev, rvalFrVals=vs}) | l == frlev = do
          -- rsemTr $ "R o=" ++ show o ++ " len(vs)=" ++ show (MV.length vs)
          liftIO $ MV.read vs o 
        access (RVal_Frame {rvalLev=frlev, rvalCx=slref})				  = do
          sl <- liftIO $ readIORef slref
          -- rsemTr $ "R sl=" ++ show sl ++ " frlev=" ++ show frlev ++ " l=" ++ show l
          heapGetM sl >>= access
        access v                                                        = 
          err $ "CoreRun.Run.Val.ref2valM.RRef_Loc.access:" >#< r >#< "in" >#< v
-}
    RRef_LDf ld o -> do
        topfr <- renvTopFrameM
        access ld topfr
      where
        access 0 (RVal_Frame {rvalFrVals=vs}) =
          liftIO $ MV.read vs o 
        access ld (RVal_Frame {rvalCx=rcx}) = do
          sl <- liftIO $ readIORef (rcxtSlRef rcx)
          fr <- heapGetM sl
          access (ld-1) fr
        access _ v = 
          err $ "CoreRun.Run.Val.ref2valM.RRef_LDf.access:" >#< r >#< "in" >#< v
    RRef_Fld r e -> do
        v <- ptr2valM =<< ref2valM r -- >>= rsemDeref
        case v of
          RVal_NodeMV _ vs -> liftIO $ MV.read vs e
          _                -> err $ "CoreRun.Run.Val.ref2valM.RRef_Fld:" >#< e >#< "in" >#< v
    RRef_Tag r -> do
        v <- ptr2valM =<< ref2valM r -- >>= rsemDeref
        case v of
          RVal_NodeMV t _ -> return $ RVal_Int t
          _               -> err $ "CoreRun.Run.Val.ref2valM.RRef_Tag:" >#< v
    _ -> err $ "CoreRun.Run.Val.ref2valM.r:" >#< r
{-# INLINE ref2valM #-}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Frame
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(updTopFrameM)
-- | Update top frame
updTopFrameM :: (RunSem RValCxt RValEnv RVal m x) => (RVal -> RValT m RVal) -> RValT m ()
updTopFrameM f = renvTopFramePtrM >>= flip heapUpdM f
{-# INLINE updTopFrameM #-}
%%]


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Expr stack inside frame: push, pop, etc
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(renvFrStkPush1, renvFrStkReversePushMV)
-- | Push on the stack embedded in the top frame
renvFrStkPush' :: RunSem RValCxt RValEnv RVal m x => (Int -> RValMV -> v -> IO Int) -> v -> RValT m ()
renvFrStkPush' pushvOn v = do
  (RValEnv {renvTopFrame=frref, renvHeap=hp}) <- get
  liftIO $ do
    (RVal_Frame {rvalFrVals=frvs, rvalFrSP=spref}) <- heapGetM'' hp =<< readIORef frref
    sp <- readIORef spref
    sp' <- pushvOn sp frvs v
    writeIORef spref sp'

-- | Push on the stack embedded in the top frame
renvFrStkPush1 :: RunSem RValCxt RValEnv RVal m x => RVal -> RValT m ()
renvFrStkPush1 = renvFrStkPush' (\sp frvs v -> MV.write frvs sp v >> return (sp + 1))
{-# INLINE renvFrStkPush1 #-}

-- | Push on the stack embedded in the top frame
renvFrStkPushMV :: RunSem RValCxt RValEnv RVal m x => RValMV -> RValT m ()
renvFrStkPushMV = renvFrStkPush' (\sp frvs vs -> mvecFillFromMV sp frvs vs >> return (sp + MV.length vs))
{-# INLINE renvFrStkPushMV #-}

-- | Push reversed on the stack embedded in the top frame
renvFrStkReversePushMV :: RunSem RValCxt RValEnv RVal m x => RValMV -> RValT m ()
renvFrStkReversePushMV = renvFrStkPush' (\sp frvs vs -> mvecReverseFillFromMV sp frvs vs >> return (sp + MV.length vs))
{-# INLINE renvFrStkReversePushMV #-}

-- | Push on the stack embedded in the top frame
renvFrStkPushV :: RunSem RValCxt RValEnv RVal m x => RValV -> RValT m ()
renvFrStkPushV = renvFrStkPush' (\sp frvs vs -> mvecFillFromV sp frvs vs >> return (sp + V.length vs))
{-# INLINE renvFrStkPushV #-}

-- | Push reversed on the stack embedded in the top frame
renvFrStkReversePushV :: RunSem RValCxt RValEnv RVal m x => RValV -> RValT m ()
renvFrStkReversePushV = renvFrStkPush' (\sp frvs vs -> mvecReverseFillFromV sp frvs vs >> return (sp + V.length vs))
{-# INLINE renvFrStkReversePushV #-}
%%]

%%[(8 corerun) hs export(renvFrStkPop1, renvFrStkPopMV, renvFrStkReversePopMV, renvFrStkReversePopInMV)
-- | Pop from the stack embedded in the top frame
renvFrStkPop' :: RunSem RValCxt RValEnv RVal m x => (RValMV -> Int -> IO v) -> Int -> RValT m v
renvFrStkPop' popvFrom sz = do
  (RValEnv {renvTopFrame=frref, renvHeap=hp}) <- get
  liftIO $ do
    (RVal_Frame {rvalFrVals=frvs, rvalFrSP=spref}) <- heapGetM'' hp =<< readIORef frref
    sp <- readIORef spref
    let sp' = sp - sz
    writeIORef spref sp'
    popvFrom frvs sp'

-- | Pop from the stack embedded in the top frame
renvFrStkPop :: RunSem RValCxt RValEnv RVal m x => Int -> RValT m ()
renvFrStkPop = renvFrStkPop' (\_ _ -> return ())
{-# INLINE renvFrStkPop #-}

-- | Pop from the stack embedded in the top frame
renvFrStkPop1 :: RunSem RValCxt RValEnv RVal m x => RValT m RVal
renvFrStkPop1 = renvFrStkPop' MV.read 1
{-# INLINE renvFrStkPop1 #-}

-- | Pop from the stack embedded in the top frame
renvFrStkPopInMV :: RunSem RValCxt RValEnv RVal m x => Int -> Int -> RValMV -> RValT m ()
renvFrStkPopInMV lwbTo sz vs = renvFrStkPop' (\frvs sp -> mvecFillFromMV' lwbTo sp sz vs frvs) sz
{-# INLINE renvFrStkPopInMV #-}

-- | Pop from the stack embedded in the top frame
renvFrStkPopMV :: RunSem RValCxt RValEnv RVal m x => Int -> RValT m RValMV
-- renvFrStkPopMV sz = renvFrStkPop' (\frvs sp -> mvecAlloc sz >>= \vs -> mvecFillFromMV' 0 sp sz vs frvs >> return vs) sz
renvFrStkPopMV sz = (liftIO $ mvecAlloc sz) >>= \vs -> renvFrStkPopInMV 0 sz vs >> return vs
{-# INLINE renvFrStkPopMV #-}

-- | Pop from the stack embedded in the top frame
renvFrStkReversePopInMV :: RunSem RValCxt RValEnv RVal m x => Int -> Int -> RValMV -> RValT m ()
renvFrStkReversePopInMV lwbTo sz vs = renvFrStkPop' (\frvs sp -> mvecReverseFillFromMV' lwbTo sp sz vs frvs) sz
{-# INLINE renvFrStkReversePopInMV #-}

-- | Pop from the stack embedded in the top frame
renvFrStkReversePopMV :: RunSem RValCxt RValEnv RVal m x => Int -> RValT m RValMV
-- renvFrStkReversePopMV sz = renvFrStkPop' (\frvs sp -> mvecAlloc sz >>= \vs -> mvecReverseFillFromMV' 0 sp sz vs frvs >> return vs) sz
renvFrStkReversePopMV sz = (liftIO $ mvecAlloc sz) >>= \vs -> renvFrStkReversePopInMV 0 sz vs >> return vs
{-# INLINE renvFrStkReversePopMV #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(rsemTr', rsemTr)
-- | Trace
rsemTr' :: (PP msg, RunSem RValCxt RValEnv RVal m x) => Bool -> msg -> RValT m ()
rsemTr' dumpExtensive msg = do
    env <- get
    when (renvDoTrace env) $ do
      liftIO $ putStrLn $ show $ pp msg
      dumpEnvM (dumpExtensive || renvDoTraceExt env)
      liftIO $ hFlush stdout
{- INLINE rsemTr' #-}

-- | Trace
rsemTr :: (PP msg, RunSem RValCxt RValEnv RVal m x) => msg -> RValT m ()
rsemTr = rsemTr' False
{- INLINE rsemTr #-}
%%]

