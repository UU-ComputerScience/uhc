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

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val.RunExplStk}
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
-- | Allocate a new frame
explStkAllocFrameM :: (RunSem RValCxt RValEnv RVal m x) => Ref2Nm -> HpPtr -> Int -> Int -> Int -> RValT m HpPtr
explStkAllocFrameM r2n sl lev sz nrArgs = do
  a <- liftIO $ mvecAllocInit sz
  when (nrArgs > 0) $ renvFrStkReversePopInMV 0 nrArgs a
  slref <- liftIO $ newIORef sl
  spref <- liftIO $ newIORef nrArgs
  p <- heapAllocM $ RVal_Frame r2n slref lev a spref
  return p

-- | Push a new stack frame
explStkPushFrameM :: (RunSem RValCxt RValEnv RVal m x) => HpPtr -> RValT m ()
explStkPushFrameM frptr = do
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    t <- readIORef tf
    unless (isNullPtr t) $ modifyIORef st (t:)
    writeIORef tf frptr
{-# INLINE explStkPushFrameM #-}

-- | Allocate and push a new stack frame
explStkPushAllocFrameM :: (RunSem RValCxt RValEnv RVal m x) => Ref2Nm -> HpPtr -> Int -> Int -> Int -> RValT m ()
explStkPushAllocFrameM r2n sl lev sz nrArgs = do
  p <- explStkAllocFrameM r2n sl lev sz nrArgs
  explStkPushFrameM p
{-
  (RValEnv {renvStack=st, renvTopFrame=tf}) <- get
  liftIO $ do
    t <- readIORef tf
    unless (isNullPtr t) $ modifyIORef st (t:)
    writeIORef tf p
-}
{-# INLINE explStkPushAllocFrameM #-}

-- | Allocate and replace top stack frame
explStkReplaceAllocFrameM :: (RunSem RValCxt RValEnv RVal m x) => Ref2Nm -> HpPtr -> Int -> Int -> Int -> RValT m ()
explStkReplaceAllocFrameM r2n sl lev sz nrArgs = do
  p <- explStkAllocFrameM r2n sl lev sz nrArgs
  (RValEnv {renvTopFrame=tf}) <- get
  liftIO $ writeIORef tf p
{-# INLINE explStkReplaceAllocFrameM #-}

-- | Pop a stack frame, copying the top of the stack embedded in the frame
explStkPopFrameM :: (RunSem RValCxt RValEnv RVal m x) => RValT m HpPtr
explStkPopFrameM = do
  (RValEnv {renvStack=stref, renvTopFrame=tfref}) <- get
  liftIO $ do
    tf  <- readIORef tfref
    stk <- readIORef stref
    case stk of
      [] -> writeIORef tfref nullPtr
      (h:t) -> do
        writeIORef tfref h
        writeIORef stref t
    return tf
{-# INLINE explStkPopFrameM #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(cmodRun)
cmodRun :: (RunSem RValCxt RValEnv RVal m ()) => EHCOpts -> Mod -> RValT m ()
cmodRun opts (Mod_Mod {body_Mod_Mod=e}) = do
  -- dumpEnvM True
  rsemExp e
  -- v <- renvFrStkPop1
%%[[8
  dumpEnvM False
%%][100
%%]]
  -- return v
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem, RunT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs
-- | Apply Lam in context of static link with exact right amount of params, otherwise the continuation is used
rvalExplStkAppLam :: RunSem RValCxt RValEnv RVal m () => HpPtr -> Exp -> Int -> (Int -> RValT m ()) -> RValT m ()
rvalExplStkAppLam sl f nrActualArgs failcont = do
  case f of
    Exp_Lam {lev_Exp_Lam=l, nrArgs_Exp_Lam=nrRequiredArgs, stkDepth_Exp_Lam=sz, ref2nm_Exp_Lam=r2n, body_Exp_Lam=b}
      | nrActualArgs == nrRequiredArgs -> do
           -- rsemTr $ "V app lam =="
           needRet <- asks rcxtInRet
           if needRet
             then do
               explStkPushAllocFrameM r2n sl l sz nrActualArgs
               rsemExp b
               v <- renvFrStkPop1
               explStkPopFrameM
               renvFrStkPush1 v
             else do
               explStkReplaceAllocFrameM r2n sl l sz nrActualArgs
               mustReturn $ rsemExp b
      | otherwise -> failcont nrRequiredArgs
    _   -> err $ "CoreRun.Run.Val.rvalExplStkAppLam:" >#< f
-- {-# SPECIALIZE rvalExplStkAppLam :: HpPtr -> Exp -> RValMV -> (Int -> RValT IO RVal) -> RValT IO RVal #-}
-- {-# INLINE rvalExplStkAppLam #-}
%%]

%%[(8 corerun) hs
-- | Apply. Assume: function 'f' is already evaluated (responsibility lies outside)
rvalExplStkApp :: RunSem RValCxt RValEnv RVal m () => RVal -> Int -> RValT m ()
rvalExplStkApp f nrActualArgs = do
  -- rsemTr $ "V app f(" ++ show (MV.length as) ++ "): " ++ show (pp f)
  case f of
    RVal_Lam {rvalSLRef=slref, rvalBody=b} -> do
      sl <- liftIO $ readIORef slref
      rvalExplStkAppLam sl b nrActualArgs $ \narg -> do
        if nrActualArgs < narg 
          then do
            -- rsemTr $ "V app lam <"
            renvFrStkReversePopMV nrActualArgs >>= \as -> heapAllocAsPtrM (RVal_App f as) >>= renvFrStkPush1
          else do
            -- rsemTr $ "V app lam >"
            ap <- rvalExplStkApp f narg >> renvFrStkPop1
            rvalExplStkApp ap (nrActualArgs - narg)
    RVal_App appf appas
      | nrActualArgs > 0 -> do
           -- rsemTr $ "V app app"
           renvFrStkReversePushMV appas >> rvalExplStkApp appf (nrActualArgs + MV.length appas)
    _   -> err $ "CoreRun.Run.Val.rvalExplStkApp:" >#< f
-- {-# SPECIALIZE rvalExplStkApp :: RunSem RValCxt RValEnv RVal IO RVal => RVal -> RValMV -> RValT IO RVal #-}
-- {-# INLINE rvalExplStkApp #-}
%%]

%%[(8 corerun) hs
-- | rsemExp for RVal, without explicit use of expr stack, i.e. implicit stack via Haskell thereby preventing correct GC
rvalExplStkExp :: RunSem RValCxt RValEnv RVal m () => Exp -> RValT m ()
{-# SPECIALIZE rvalExplStkExp :: RunSem RValCxt RValEnv RVal IO () => Exp -> RValT IO () #-}
-- {-# INLINE rvalExplStkExp #-}
rvalExplStkExp e = do
  -- rsemTr' True $ "E:" >#< e
  -- e' <- case e of
  case e of
    -- app, call
    Exp_App f as -> do
        vecReverseForM_ as rsemExp
        rsemExp f >>= rsemPop >>= ptr2valM >>= \f' -> rvalExplStkApp f' (V.length as)
    
    -- heap node
    Exp_Tup t as -> do
        V.forM_ as rsemExp
        renvFrStkPopMV (V.length as) >>= rsemNode (ctagTag t) >>= rsemPush

    -- lam as is, being a heap allocated thunk when 0 args are required
    Exp_Lam {nrArgs_Exp_Lam=na}
      | na == 0   -> mk RVal_Thunk >>= heapAllocAsPtrM >>= rsemPush
      | otherwise -> mk RVal_Lam >>= heapAllocAsPtrM >>= rsemPush
      where mk rv = do
             sl <- renvTopFrameM
             slref <- liftIO $ newIORef sl
             return $ rv e slref

    -- let
    Exp_Let {firstOff_Exp_Let=fillFrom, ref2nm_Exp_Let=r2n, binds_Exp_Let=bs, body_Exp_Let=b} -> do
        V.forM_ bs rsemExp
        rsemExp b

    -- case, scrutinee already evaluated
    Exp_Case e as -> do
      v <- ptr2valM =<< rsemPop =<< rsemSExp e
      case v of
        RVal_Node {rvalTag=tg} -> rsemAlt $ as V.! tg
        _ -> err $ "CoreRun.Run.Val.RunExplStk.rvalExplStkExp.Case: scrutinee:" >#< v
    
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
    Exp_FFI pr as -> V.mapM_ rsemExp as >> renvFrStkPopMV (V.length as) >>= (liftIO . V.freeze) >>= rsemPrim pr

    e -> err $ "CoreRun.Run.Val.RunExplStk.rvalExplStkExp:" >#< e

  -- rsemTr' True $ "E->:" >#< (e) -- >-< e')
  -- return e'
%%]

%%[(8 corerun) hs
instance
    ( Monad m, MonadIO m, Functor m
    ) => RunSem RValCxt RValEnv RVal m ()
  where
    -- {-# SPECIALIZE instance RunSem RValCxt RValEnv RVal IO () #-}
    rsemInitial = do
      s <- liftIO $ newRValEnv 100000 --  
      return (emptyRValCxt, s, undefined)
  
    rsemSetup opts modImpL mod@(Mod_Mod {moduleNr_Mod_Mod=mainModNr}) = do
        rsemSetTrace True
        let modAllL = modImpL ++ [mod]
        ms <- liftIO $ MV.new (maximum (map moduleNr_Mod_Mod modAllL) + 1)
        forM_ modAllL $ \(Mod_Mod {ref2nm_Mod_Mod=r2n, moduleNr_Mod_Mod=nr, binds_Mod_Mod=bs, stkDepth_Mod_Mod=sz}) -> do
          explStkPushAllocFrameM r2n nullPtr 0 sz 0
          V.forM_ bs rsemExp
          p <- explStkPopFrameM
          liftIO $ MV.write ms nr p
        ms' <- liftIO $ V.freeze ms
        modify $ \env -> env {renvGlobals = ms'}
        explStkPushFrameM $ ms' V.! mainModNr
        rsemSetTrace $ CoreOpt_RunTrace `elem` ehcOptCoreOpts opts

    rsemSetTrace doTrace = modify $ \env ->
      env {renvDoTrace = doTrace}
    
    rsemExp = rvalExplStkExp

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

    rsemEvl v = do
        -- rsemGcEnterRootLevel
        -- rsemGcPushRoot v
        case v of
          RVal_Ptr {rvalPtrRef=pref} -> do
            rsemGcEnterRootLevel
            rsemGcPushRoot v
            liftIO (readIORef pref) >>= evlPtr pref
            rsemGcLeaveRootLevel
          RVal_BlackHole             -> err $ "CoreRun.Run.Val.rsemEvl.RVal_BlackHole:" >#< "Black hole"
          _                          -> return () -- rsemPush v
        -- rsemGcLeaveRootLevel
        rsemPush v
      where
        evlPtr pref p = do
          hp <- gets renvHeap
          v <- heapGetM' hp p
          case v of
            RVal_Thunk {rvalSLRef=slref, rvalBody=e} -> do
              -- rsemGcPushRoot v
              sl <- liftIO $ readIORef slref
              heapSetM' hp p RVal_BlackHole
              v' <- rvalExplStkAppLam sl e 0 $ \_ -> err $ "CoreRun.Run.Val.rsemEvl.RVal_Thunk:" >#< e
              hp <- gets renvHeap
              p <- liftIO (readIORef pref)
              rsemPop v' >>= \v'' -> heapSetM' hp p v''
              return ()
            RVal_Ptr {rvalPtrRef=pref} -> do
              v' <- evlPtr pref =<< liftIO (readIORef pref)
              return ()
            v -> do
              return ()

    rsemDeref v = do
      v' <- ptr2valM v
      -- rsemTr $ "Deref:" >#< (v >-< v')
      rsemPush v'
    {-# INLINE rsemDeref #-}
    
    -- apply a known primitive
    rsemPrim = rvalPrim
    {-# INLINE rsemPrim #-}

    rsemPush = renvFrStkPush1
    {-# INLINE rsemPush #-}
    rsemPop  = \_ -> renvFrStkPop1
    {-# INLINE rsemPop #-}
    rsemNode t vs = heapAllocAsPtrM $ RVal_Node t vs
    {-# INLINE rsemNode #-}

    
    rsemGcEnterRootLevel = gets renvGcRootStack >>= \r -> liftIO $ modifyIORef r $ ([]:)
    {-# INLINE rsemGcEnterRootLevel #-}
    
    rsemGcPushRoot v = gets renvGcRootStack >>= \r -> liftIO $ modifyIORef r $ \(h:t) -> (v:h) : t
    {-# INLINE rsemGcPushRoot #-}
    
    rsemGcLeaveRootLevel = gets renvGcRootStack >>= \r -> liftIO $ modifyIORef r tail
    {-# INLINE rsemGcLeaveRootLevel #-}
%%]


