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
-- | Arguments to a function, which may come from an RVal_App or from the stack
data ExplArgs = ExplArgs
  { eaVec		:: !RValV		-- ^ the accumulated part from RVal_App
  , eaStk		:: !Int			-- ^ the size of the part still on the stack
  }

emptyExplArgs = ExplArgs V.empty 0
-- {-# INLINE emptyExplArgs #-}

-- | The total nr of args
eaNrArgs :: ExplArgs -> Int
eaNrArgs (ExplArgs {eaVec=v, eaStk=na}) = V.length v + na
{-# INLINE eaNrArgs #-}

-- | Set total nr of args, taking into account what is in the vector part
eaSetNrArgs :: ExplArgs -> Int -> ExplArgs
eaSetNrArgs ea@(ExplArgs {eaVec=v}) n = ea {eaStk = n - V.length v}
{-# INLINE eaSetNrArgs #-}

-- | Pop from the ExplArgs partly embedded in the top frame and partly explicitly available
renvFrStkEaPopMV :: RunSem RValCxt RValEnv RVal m x => ExplArgs -> RValT m RValMV
renvFrStkEaPopMV ea@(ExplArgs {eaVec=v}) = (liftIO $ mvecAlloc eaLen) >>= \vs -> liftIO (mvecFillFromV 0 vs v) >> renvFrStkReversePopInMV vLen (eaLen-vLen) vs >> return vs
  where vLen  = V.length v
        eaLen = eaNrArgs ea
{-# INLINE renvFrStkEaPopMV #-}
%%]

%%[(8 corerun) hs
-- | Allocate a new frame
explStkAllocFrameM :: (RunSem RValCxt RValEnv RVal m x) => Ref2Nm -> HpPtr -> Int -> Int -> ExplArgs -> RValT m HpPtr
explStkAllocFrameM r2n sl lev sz as@(ExplArgs {eaVec=vsArgs, eaStk=nrArgs}) = do
  a <- liftIO $ mvecAllocInit sz -- (sz+3)		-- TBD: stack overflow somewhere...
  let vsLen = V.length vsArgs
  when (vsLen  > 0) $ liftIO $ mvecFillFromV 0 a vsArgs
  when (nrArgs > 0) $ renvFrStkReversePopInMV vsLen nrArgs a
  slref <- liftIO $ newIORef sl
  spref <- liftIO $ newIORef (eaNrArgs as)
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
explStkPushAllocFrameM :: (RunSem RValCxt RValEnv RVal m x) => Ref2Nm -> HpPtr -> Int -> Int -> ExplArgs -> RValT m ()
explStkPushAllocFrameM r2n sl lev sz as = do
  p <- explStkAllocFrameM r2n sl lev sz as
  explStkPushFrameM p
{-# INLINE explStkPushAllocFrameM #-}

-- | Allocate and replace top stack frame
explStkReplaceAllocFrameM :: (RunSem RValCxt RValEnv RVal m x) => Ref2Nm -> HpPtr -> Int -> Int -> ExplArgs -> RValT m ()
explStkReplaceAllocFrameM r2n sl lev sz as = do
  p <- explStkAllocFrameM r2n sl lev sz as
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
  mustReturn $ rsemExp e
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
rvalExplStkAppLam :: RunSem RValCxt RValEnv RVal m () => HpPtr -> Exp -> ExplArgs -> (Int -> RValT m ()) -> RValT m ()
rvalExplStkAppLam sl f as failcont = do
  let nrActualArgs = eaNrArgs as
  case f of
    Exp_Lam {lev_Exp_Lam=l, mbNm_Exp_Lam=mn, nrArgs_Exp_Lam=nrRequiredArgs, stkDepth_Exp_Lam=sz, ref2nm_Exp_Lam=r2n, body_Exp_Lam=b}
      | nrActualArgs == nrRequiredArgs -> do
           -- rsemTr $ ">V (" ++ show mn ++ ") app lam ==, na=" ++ show nrRequiredArgs ++ ", sz=" ++ show sz
           needRet <- asks rcxtInRet
           rvalTrEnterLam mn $ 
             if needRet
               then do
                 explStkPushAllocFrameM r2n sl l sz as
                 rsemExp b
                 v <- renvFrStkPop1
                 explStkPopFrameM
                 renvFrStkPush1 v
               else do
                 explStkReplaceAllocFrameM r2n sl l sz as
                 mustReturn $ rsemExp b
           -- rsemTr $ "<V (" ++ show mn ++ ")"
      | otherwise -> failcont nrRequiredArgs
    _   -> err $ "CoreRun.Run.Val.rvalExplStkAppLam:" >#< f
-- {-# SPECIALIZE rvalExplStkAppLam :: HpPtr -> Exp -> RValMV -> (Int -> RValT IO RVal) -> RValT IO RVal #-}
-- {-# INLINE rvalExplStkAppLam #-}
%%]

%%[(8 corerun) hs
-- | Apply. Assume: function 'f' is already evaluated (responsibility lies outside)
rvalExplStkApp :: RunSem RValCxt RValEnv RVal m () => RVal -> ExplArgs -> RValT m ()
rvalExplStkApp f as = do
  -- rsemTr $ "V app f(" ++ show (MV.length as) ++ "): " ++ show (pp f)
  let nrActualArgs = eaNrArgs as
  case f of
    RVal_Lam {rvalSLRef=slref, rvalBody=b} -> do
      sl <- liftIO $ readIORef slref
      rvalExplStkAppLam sl b as $ \narg -> do
        if nrActualArgs < narg 
          then do
            -- rsemTr $ "V app lam <"
            -- renvFrStkReversePopMV nrActualArgs >>= \as -> heapAllocAsPtrM (RVal_App f as) >>= renvFrStkPush1
            renvFrStkEaPopMV as >>= \as -> heapAllocAsPtrM (RVal_App f as) >>= renvFrStkPush1
          else do
            -- rsemTr $ "V app lam >"
            -- ap <- mustReturn $ rvalExplStkApp f (as {eaStk=narg}) >>= rsemPop >>= rsemDeref >>= rsemPop
            -- rvalExplStkApp ap (as {eaStk=nrActualArgs - narg})
            ap <- mustReturn $ rvalExplStkApp f (eaSetNrArgs as narg) >>= rsemPop >>= rsemDeref >>= rsemPop
            rvalExplStkApp ap (eaSetNrArgs emptyExplArgs (nrActualArgs - narg))
    RVal_App appf appas
      | nrActualArgs > 0 -> do
           -- rsemTr $ "V app app"
           -- renvFrStkReversePushMV appas >> rvalExplStkApp appf (as {eaStk=nrActualArgs + MV.length appas})
           appas' <- liftIO $ V.freeze appas
           rvalExplStkApp appf (as {eaVec=appas' V.++ eaVec as})
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
%%[[8
  rsemTr $ ">E:" >#< e
%%][100
%%]]
  -- e' <- case e of
  case e of
    -- app, call
    Exp_App f as -> do
        f' <- mustReturn $ do
          vecReverseForM_ as rsemExp
          rsemExp f
        rsemPop f' >>= ptr2valM >>= \f' -> rvalExplStkApp f' (emptyExplArgs {eaStk=V.length as})
    
    -- heap node
    Exp_Tup t as -> do
        V.forM_ as rsemExp
        renvFrStkPopMV (V.length as) >>= rsemNode (ctagTag t) >>= rsemPush

    -- lam as is, being a heap allocated thunk when 0 args are required
    Exp_Lam {nrArgs_Exp_Lam=na, mbNm_Exp_Lam=mn}
      | na == 0   -> mk RVal_Thunk
      | otherwise -> mk RVal_Lam
      where mk rv = do
             sl <- renvTopFrameM
             slref <- liftIO $ newIORef sl
             heapAllocAsPtrM (rv mn e slref) >>= rsemPush

    -- let
    Exp_Let {firstOff_Exp_Let=fillFrom, ref2nm_Exp_Let=r2n, binds_Exp_Let=bs, body_Exp_Let=b} -> do
        mustReturn $ V.forM_ bs rsemExp
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
    -- Exp_Ret e -> mustReturn $ rsemExp e

    -- setup for context requiring a return from case alternative
    Exp_RetCase _ e -> rsemExp e

    -- setup for context not requiring a return
    Exp_Tail e -> needNotReturn $ rsemExp e

    -- simple expressions
    Exp_SExp se -> rsemSExp se

    -- FFI
    Exp_FFI pr as -> V.mapM_ rsemExp as >> renvFrStkPopMV (V.length as) >>= (liftIO . V.freeze) >>= rsemPrim pr

    e -> err $ "CoreRun.Run.Val.RunExplStk.rvalExplStkExp:" >#< e

%%[[8
  rsemTr $ "<E:" >#< (e) -- >-< e')
%%][100
%%]]
  -- return e'
%%]

%%[(8 corerun) hs
instance
    ( Monad m, MonadIO m, Functor m
    ) => RunSem RValCxt RValEnv RVal m ()
  where
    {-# SPECIALIZE instance RunSem RValCxt RValEnv RVal IO () #-}
    rsemInitial = do
      s <- liftIO $ newRValEnv 1000 -- 100000 --  
      return (emptyRValCxt, s, undefined)
  
    rsemSetup opts modImpL mod@(Mod_Mod {moduleNr_Mod_Mod=mainModNr}) = do
        -- rsemSetTrace True
        rsemGcEnterRootLevel
        let modAllL = modImpL ++ [mod]
        ms <- liftIO $ MV.new (maximum (map moduleNr_Mod_Mod modAllL) + 1)
        forM_ modAllL $ \(Mod_Mod {ref2nm_Mod_Mod=r2n, moduleNr_Mod_Mod=nr, binds_Mod_Mod=bs, stkDepth_Mod_Mod=sz}) -> do
          -- construct frame for each module
          explStkPushAllocFrameM r2n nullPtr 0 sz emptyExplArgs
          -- holding all local defs
          V.forM_ bs rsemExp
          p <- explStkPopFrameM
          -- and store the frame into the array holding module frames
          (liftIO $ MV.write ms nr p >> newIORef p) >>= \r -> rsemGcPushRoot (RVal_Ptr r)
        -- get the module array and store it as the globals
        ms' <- liftIO $ V.freeze ms
        modify $ \env -> env {renvGlobals = ms'}
        -- use the main module's stackframe for evaluating 'main'
        explStkPushFrameM $ ms' V.! mainModNr
        rsemGcLeaveRootLevel
        rsemSetupTracing opts

    rsemSetTrace doTrace doExtensive = modify $ \env ->
      env {renvDoTrace = doTrace, renvDoTraceExt = doExtensive}
    
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
        case v of
          RVal_Ptr {rvalPtrRef=pref} -> do
            rsemGcEnterRootLevel
            rsemGcPushRoot v
            liftIO (readIORef pref) >>= evlPtr pref
            rsemGcLeaveRootLevel
          RVal_BlackHole             -> err $ "CoreRun.Run.Val.rsemEvl.RVal_BlackHole:" >#< "Black hole"
          _                          -> return () -- rsemPush v
        rsemPush v
      where
        evlPtr pref p = do
          hp <- gets renvHeap
          v <- heapGetM' hp p
          case v of
            RVal_Thunk {rvalMbNm=mn, rvalSLRef=slref, rvalBody=e} -> do
              -- rsemGcPushRoot v
              sl <- liftIO $ readIORef slref
              heapSetM' hp p RVal_BlackHole
              v' <- rvalExplStkAppLam sl e (emptyExplArgs {eaStk=0}) $ \_ -> err $ "CoreRun.Run.Val.rsemEvl.RVal_Thunk:" >#< e
              hp <- gets renvHeap
              p <- liftIO (readIORef pref)
              v'' <- rsemPop v'
              heapSetM' hp p v''
              return v''
            RVal_Ptr {rvalPtrRef=pref} -> do
              v' <- evlPtr pref =<< liftIO (readIORef pref)
              hp <- gets renvHeap
              p <- liftIO (readIORef pref)
              heapSetM' hp p v'
              return v'
            v -> do
              return v

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


