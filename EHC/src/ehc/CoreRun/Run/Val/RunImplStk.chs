%%[0 hs
-- {-# LANGUAGE MagicHash #-}
-- {-# OPTIONS_GHC -O2 #-}
%%]

%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% CoreRun Val runner
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val.RunImplStk}
%%]

%%[(8 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Error},{%{EH}Gam},{%{EH}Gam.DataGam})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun}, {%{EH}CoreRun.Run}, {%{EH}CoreRun.Run.Val}, {%{EH}CoreRun.Run.Val.Prim})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun.Pretty}, UHC.Util.Pretty)
%%]

%%[(8 corerun) hs import(qualified Data.Vector as V, qualified Data.Vector.Mutable as MV)
%%]

%%[(8 corerun) hs import(qualified Data.ByteString.Char8 as BSC8)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Frame, stack
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Fill (part of) a frame starting at 'lwb'
fillFrameM :: (RunSem RValCxt RValEnv RVal m RVal) => Int -> RValMV -> RVal -> RValT m ()
fillFrameM lwb as (RVal_Frame {rvalFrVals=frArr}) = do
  liftIO $ mvecFillFromMV lwb frArr as
{-# INLINE fillFrameM #-}
%%]

%%[(8 corerun) hs
-- | Allocate a new frame
implStkAllocFrameM :: (RunSem RValCxt RValEnv RVal m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> RValMV -> RValT m HpPtr
implStkAllocFrameM r2n sl lev sz as = do
  a <- liftIO $ mvecAllocInit sz
  slref <- liftIO $ newIORef sl
  spref <- liftIO $ newIORef sz -- (MV.length as) -- stack is not used, GC looks up until this location
  let fr = RVal_Frame r2n slref lev a spref
  fillFrameM 0 as fr
  heapAllocM fr

-- | Allocate and push a new stack frame
implStkPushAllocFrameM :: (RunSem RValCxt RValEnv RVal m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> RValMV -> RValT m ()
implStkPushAllocFrameM r2n sl lev sz as = do
  p <- implStkAllocFrameM r2n sl lev sz as
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    t <- readIORef tf
    unless (isNullPtr t) $ modifyIORef st (t:)
    writeIORef tf p
{-# INLINE implStkPushAllocFrameM #-}

-- | Allocate and replace top stack frame
implStkReplaceAllocFrameM :: (RunSem RValCxt RValEnv RVal m RVal) => Ref2Nm -> HpPtr -> Int -> Int -> RValMV -> RValT m ()
implStkReplaceAllocFrameM r2n sl lev sz as = do
  p <- implStkAllocFrameM r2n sl lev sz as
  (RValEnv {renvTopFrame=tf}) <- get
  liftIO $ writeIORef tf p
{-# INLINE implStkReplaceAllocFrameM #-}

-- | Pop a stack frame
implStkPopFrameM :: (RunSem RValCxt RValEnv RVal m RVal) => RValT m ()
implStkPopFrameM = do
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    stk <- readIORef st
    case stk of
      [] -> writeIORef tf nullPtr
      (h:t) -> do
        writeIORef tf h
        writeIORef st t
{-# INLINE implStkPopFrameM #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(cmodRun)
cmodRun :: (RunSem RValCxt RValEnv RVal m RVal) => EHCOpts -> Mod -> RValT m RVal
cmodRun opts (Mod_Mod {body_Mod_Mod=e}) = do
  -- dumpEnvM True
  v <- rsemExp e
%%[[8
  dumpEnvM False
%%][100
%%]]
  return v
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem, RunT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Apply Lam in context of static link with exact right amount of params, otherwise the continuation is used
rvalImplStkAppLam :: RunSem RValCxt RValEnv RVal m RVal => HpPtr -> Exp -> RValMV -> (Int -> RValT m RVal) -> RValT m RVal
rvalImplStkAppLam sl f as failcont = do
  case f of
    Exp_Lam {lev_Exp_Lam=l, nrArgs_Exp_Lam=narg, stkDepth_Exp_Lam=sz, ref2nm_Exp_Lam=r2n, body_Exp_Lam=b}
      | MV.length as == narg -> do
           -- rsemTr $ "V app lam =="
           needRet <- asks rcxtInRet
           if needRet
             then do
               implStkPushAllocFrameM r2n sl l sz as
               v <- rsemExp b
               implStkPopFrameM
               return v
             else do
               implStkReplaceAllocFrameM r2n sl l sz as
               mustReturn $ rsemExp b
      | otherwise -> failcont narg
    _   -> err $ "CoreRun.Run.Val.rvalImplStkAppLam:" >#< f
-- {-# SPECIALIZE rvalImplStkAppLam :: HpPtr -> Exp -> RValMV -> (Int -> RValT IO RVal) -> RValT IO RVal #-}
-- {-# INLINE rvalImplStkAppLam #-}

-- | Apply. Assume: function 'f' is already evaluated (responsibility lies outside)
rvalImplStkApp :: RunSem RValCxt RValEnv RVal m RVal => RVal -> RValMV -> RValT m RVal
rvalImplStkApp f as = do
  -- rsemTr $ "V app f(" ++ show (MV.length as) ++ "): " ++ show (pp f)
  case f of
    RVal_Lam {rvalSLRef=slref, rvalBody=b} -> do
      sl <- liftIO $ readIORef slref
      rvalImplStkAppLam sl b as $ \narg -> do
        if MV.length as < narg 
          then do
            -- rsemTr $ "V app lam <"
            return $ RVal_App f as
          else do
            -- rsemTr $ "V app lam >"
            ap <- {- mustReturn $ -} rvalImplStkApp f (MV.take narg as)
            rvalImplStkApp ap (MV.drop narg as)
    RVal_App appf appas
      | MV.length as > 0 -> do
           -- rsemTr $ "V app app"
           (liftIO $ mvecAppend appas as) >>= rvalImplStkApp appf
    _   -> err $ "CoreRun.Run.Val.rvalImplStkApp:" >#< f
-- {-# SPECIALIZE rvalImplStkApp :: RunSem RValCxt RValEnv RVal IO RVal => RVal -> RValMV -> RValT IO RVal #-}
-- {-# INLINE rvalImplStkApp #-}
%%]

%%[(8 corerun) hs
-- | rsemExp for RVal, without explicit use of expr stack, i.e. implicit stack via Haskell thereby preventing correct GC
rvalImplStkExp :: RunSem RValCxt RValEnv RVal m RVal => Exp -> RValT m RVal
-- {-# SPECIALIZE rvalImplStkExp :: RunSem RValCxt RValEnv RVal IO RVal => Exp -> RValT IO RVal #-}
{-# INLINE rvalImplStkExp #-}
rvalImplStkExp e = do
  -- rsemTr $ "E:" >#< e
  -- e' <- case e of
  case e of
    -- app, call
    Exp_App f as -> do
        f' <- {- mustReturn $ -} rsemExp f {- >>= rsemEvl -}
        V.mapM rsemExp as >>= (liftIO . V.thaw) >>= rvalImplStkApp f'
    
    -- heap node
    Exp_Tup t as -> do
        as' <- V.mapM rsemExp as >>= (liftIO . mvecAllocFillFromV)
        return $ RVal_Node (ctagTag t) as'

    -- lam as is, being a heap allocated thunk when 0 args are required
    Exp_Lam {nrArgs_Exp_Lam=na}
      | na == 0   -> mk RVal_Thunk >>= heapAllocM >>= (liftIO . newIORef) >>= (return . RVal_Ptr)
      | otherwise -> mk RVal_Lam
      where mk rv = do
             sl <- renvTopFrameM
             slref <- liftIO $ newIORef sl
             return $ rv e slref

    -- let
    Exp_Let {firstOff_Exp_Let=fillFrom, ref2nm_Exp_Let=r2n, binds_Exp_Let=bs, body_Exp_Let=b} -> do
        bs' <- (liftIO . V.thaw) =<< V.forM bs rsemExp
        fr <- renvTopFrameM >>= heapGetM -- >>= rsemDeref
        fillFrameM fillFrom bs' fr
        rsemExp b

    -- case, scrutinee already evaluated
    Exp_Case e as -> do
      (RVal_Node {rvalTag=tg}) <- {- rsemDeref =<< -} rsemSExp e
      rsemAlt $ as V.! tg
    
    -- force evaluation immediately
    Exp_Force e -> rsemExp e >>= rsemPop >>= rsemEvl

    -- setup for context requiring a return (TBD: should be done via CPS style, but is other issue)
    Exp_Ret e -> mustReturn $ rsemExp e

    -- setup for context requiring a return from case alternative
    Exp_RetCase _ e -> rsemExp e

    -- setup for context not requiring a return
    Exp_Tail e -> needNotReturn $ rsemExp e

    -- simple expressions
    Exp_SExp se -> rsemSExp se

    -- FFI
    Exp_FFI pr as -> V.mapM rsemExp as >>= rsemPrim pr

    e -> err $ "CoreRun.Run.Val.RunExplStk.rvalImplStkExp:" >#< e

  -- rsemTr $ "E->:" >#< (e >-< e')
  -- return e'

%%]

%%[(8 corerun) hs
instance
    ( Monad m, MonadIO m, Functor m
    ) => RunSem RValCxt RValEnv RVal m RVal
  where
    {-# SPECIALIZE instance RunSem RValCxt RValEnv RVal IO RVal #-}
    rsemInitial = do
      s <- liftIO $ newRValEnv 100000
      return (emptyRValCxt, s, undefined)
  
    rsemSetup opts modImpL mod = {- local (const emptyRValCxt) $ -} do
        -- (liftIO $ newRValEnv 100000) >>= put
        let modAllL = mod : modImpL
        ms <- liftIO $ MV.new (maximum (map moduleNr_Mod_Mod modAllL) + 1)
        forM_ modAllL $ \(Mod_Mod {ref2nm_Mod_Mod=r2n, moduleNr_Mod_Mod=nr, binds_Mod_Mod=bs}) -> do
          bs' <- (liftIO . V.thaw) =<< V.forM bs rsemExp
          p <- implStkAllocFrameM r2n nullPtr 0 (MV.length bs') bs'
          liftIO $ MV.write ms nr p
        ms' <- liftIO $ V.freeze ms
        modify $ \env -> env {renvGlobals = ms'}
        rsemSetTrace $ CoreOpt_RunTrace `elem` ehcOptCoreOpts opts
        -- return RVal_None

    rsemSetTrace doTrace = modify $ \env ->
      env {renvDoTrace = doTrace}
    
    rsemExp = rvalImplStkExp

    rsemSExp se = do
      case se of
        SExp_Int 	v -> rsemPush $ RVal_Int v
        SExp_Char 	v -> rsemPush $ RVal_Char v
        SExp_Var    r -> do v <- ref2valM r
                            -- rsemTr $ "R->V:" >#< v
                            rsemPush v
        SExp_String v -> rsemPush $ RVal_PackedString $ BSC8.pack v
        _ -> rsemPush (RVal_Lit se)
    {-# INLINE rsemSExp #-}

	-- TBD
    rsemEvl v = case v of
        RVal_Ptr {rvalPtrRef=pref} -> liftIO (readIORef pref) >>= evlPtr
        RVal_BlackHole             -> err $ "CoreRun.Run.Val.rsemEvl.RVal_BlackHole:" >#< "Black hole"
        _                          -> return v
      where
        evlPtr p = do
          hp <- gets renvHeap
          v <- heapGetM' hp p
          -- rsemTr $ "Evl: *" >|< p >|< ":" >#< v
          v' <- case v of
            RVal_Thunk {rvalSLRef=slref, rvalBody=e} -> do
              sl <- liftIO $ readIORef slref
              heapSetM' hp p RVal_BlackHole
              v' <- liftIO (mvecAlloc 0) >>= \v -> rvalImplStkAppLam sl e v $ \_ -> err $ "CoreRun.Run.Val.rsemEvl.RVal_Thunk:" >#< e
              heapSetM' hp p v'
              return v'
            RVal_Ptr {rvalPtrRef=pref} -> do
              v' <- evlPtr =<< liftIO (readIORef pref)
              heapSetM' hp p v'
              return v'
            v -> return v
          -- rsemTr $ "Evl->: *" >|< p >|< ":" >#< (v >-< v')
          return v'

    rsemDeref v = do
      v' <- ptr2valM v
      -- rsemTr $ "Deref:" >#< (v >-< v')
      return v'
    {-# INLINE rsemDeref #-}
    
    -- apply a known primitive
    rsemPrim = rvalPrim
    {-# INLINE rsemPrim #-}

    rsemPush = return
    {-# INLINE rsemPush #-}
    rsemPop  = return
    {-# INLINE rsemPop #-}
%%]


