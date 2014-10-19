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

%%[(8 corerun) hs module {%{EH}CoreRun.Run.Val.Run}
%%]

%%[(8 corerun) hs import({%{EH}Base.HsName.Builtin},{%{EH}Base.Common},{%{EH}Opts},{%{EH}Ty},{%{EH}Error},{%{EH}Gam},{%{EH}Gam.DataGam})
%%]

%%[(8 corerun) hs import({%{EH}CoreRun}, {%{EH}CoreRun.Run}, {%{EH}CoreRun.Run.Val}, {%{EH}CoreRun.Run.Val.Prim})
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

%%[(8 corerun) hs import(qualified Data.ByteString.Char8 as BSC8)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Environment: state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
  -- rsemTr $ "R: " ++ show (pp r)
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
        v <- ref2valM r >>= rsemDeref
        case v of
          RVal_Node _ vs -> return $ vs V.! e
          _              -> err $ "CoreRun.Run.Val.ref2valM.RRef_Fld:" >#< e >#< "in" >#< v
    _ -> err $ "CoreRun.Run.Val.ref2valM.r:" >#< r
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Running
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) hs export(cmodRun)
cmodRun :: (RunSem RValCxt RValEnv m RVal) => EHCOpts -> Mod -> RValT m RVal
cmodRun opts (Mod_Mod {body_Mod_Mod=e}) = do
  -- dumpEnvM True
  v <- rsemExp e
%%[[99
  dumpEnvM False
%%][100
%%]]
  return v
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% RunSem, RunT
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 corerun) hs
err :: RunSem RValCxt RValEnv m RVal => PP_Doc -> RValT m a
err = throwError . rngLift emptyRange Err_PP
%%]

%%[(8 corerun) hs
-- | Apply Lam in context of static link with exact right amount of params, otherwise the continuation is used
rvalAppLam :: RunSem RValCxt RValEnv m RVal => HpPtr -> Exp -> CRArray RVal -> (Int -> RValT m RVal) -> RValT m RVal
rvalAppLam sl f as failcont = do
  case f of
    Exp_Lam {lev_Exp_Lam=l, nrArgs_Exp_Lam=narg, nrAllocs_Exp_Lam=sz, ref2nm_Exp_Lam=r2n, body_Exp_Lam=b}
      | V.length as == narg -> do
           -- rsemTr $ "V app lam =="
           needRet <- asks rcxtInRet
           if needRet
             then do
               pushAllocFrameM r2n sl l sz as
               v <- needNotReturn $ rsemExp b
               popFrameM
               return v
             else do
               replaceAllocFrameM r2n sl l sz as
               needNotReturn $ rsemExp b
      | otherwise -> failcont narg
    _   -> err $ "CoreRun.Run.Val.rvalAppLam:" >#< f

-- | Apply. Assume: function 'f' is already evaluated (responsibility lies outside)
rvalApp :: RunSem RValCxt RValEnv m RVal => RVal -> CRArray RVal -> RValT m RVal
rvalApp f as = do
  -- rsemTr $ "V app f(" ++ show (V.length as) ++ "): " ++ show (pp f)
  case f of
    RVal_Lam {rvalSL=sl, rvalBody=b} -> do
      rvalAppLam sl b as $ \narg -> do
        if V.length as < narg 
          then do
            -- rsemTr $ "V app lam <"
            return $ RVal_App f as
          else do
            -- rsemTr $ "V app lam >"
            ap <- mustReturn $ rvalApp f (V.take narg as)
            rvalApp ap (V.drop narg as)
    RVal_App appf appas
      | V.length as > 0 -> do
           -- rsemTr $ "V app app"
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
  
    rsemSetup opts modImpL mod = do
        let modAllL = mod : modImpL
        ms <- liftIO $ MV.new (maximum (map moduleNr_Mod_Mod modAllL) + 1)
        forM_ modAllL $ \(Mod_Mod {ref2nm_Mod_Mod=r2n, moduleNr_Mod_Mod=nr, binds_Mod_Mod=bs}) -> do
          bs' <- V.forM bs rsemExp
          p <- allocFrameM r2n nullPtr 0 (V.length bs') bs'
          liftIO $ MV.write ms nr p
        ms' <- liftIO $ V.freeze ms
        modify $ \env -> env {renvGlobals = ms'}
        rsemSetTrace $ CoreOpt_RunTrace `elem` ehcOptCoreOpts opts

    rsemSetTrace doTrace = modify $ \env ->
      env {renvDoTrace = doTrace}
    
    rsemExp e = do
      -- rsemTr $ "E:" >#< e
      e' <- case e of
        -- app, call
        Exp_App f as -> do
            f' <- mustReturn $ rsemExp f >>= rsemEvl
            as' <- V.mapM rsemExp as
            rvalApp f' as'
        
        -- heap node
        Exp_Tup t as -> do
            as' <- V.mapM rsemExp as
            return $ RVal_Node (ctagTag t) as'
{-
            as' <- V.mapM rsemExp as
            p <- heapAllocM $ RVal_Node (ctagTag t) as'
            return $ RVal_Ptr p
-}

        -- lam as is, being a heap allocated thunk when 0 args are required
        Exp_Lam {nrArgs_Exp_Lam=na}
          | na == 0   -> mk RVal_Thunk >>= heapAllocM >>= (return . RVal_Ptr)
          | otherwise -> mk RVal_Lam
          where mk rv = fmap (rv e) $ renvTopFrameM

        -- let
        Exp_Let {firstOff_Exp_Let=fillFrom, ref2nm_Exp_Let=r2n, binds_Exp_Let=bs, body_Exp_Let=b} -> do
            bs' <- V.forM bs rsemExp
            fr <- renvTopFrameM >>= heapGetM >>= rsemDeref
            fillFrameM fillFrom bs' fr
            rsemExp b

        -- case, scrutinee already evaluated
        Exp_Case e as -> do
          (RVal_Node {rvalTag=tg}) <- rsemDeref =<< rsemSExp e
          rsemAlt $ as V.! tg
        
        -- force evaluation immediately
        Exp_Force e -> rsemExp e >>= rsemEvl

        -- setup for context requiring a return (TBD: should be done via CPS style, but is other issue)
        Exp_Ret e -> mustReturn $ rsemExp e

        -- simple expressions
        Exp_SExp se -> rsemSExp se

        -- FFI
        Exp_FFI pr as -> V.mapM rsemExp as >>= rsemPrim pr

        e -> err $ "CoreRun.Run.Val.cmodRun.rsemExp:" >#< e

      -- rsemTr $ "E->:" >#< (e >-< e')
      return e'

    rsemSExp se = do
      case se of
        SExp_Int 	v -> return $ RVal_Int v
        SExp_Char 	v -> return $ RVal_Char v
        SExp_Var    r -> do v <- ref2valM r
                            -- rsemTr $ "R->V:" >#< v
                            return v
        SExp_String v -> return $ RVal_PackedString $ BSC8.pack v
        _ -> return (RVal_Lit se)
    {-# INLINE rsemSExp #-}

    rsemAlt a = do
      case a of
        Alt_Alt {expr_Alt_Alt=e} -> rsemExp e
    {-# INLINE rsemAlt #-}

	-- TBD
    rsemEvl v = case v of
        RVal_Ptr {rvalPtr=p} -> evlPtr p
        RVal_BlackHole       -> err $ "CoreRun.Run.Val.rsemEvl.RVal_BlackHole:" >#< "Black hole"
        _                    -> return v
      where
        evlPtr p = do
          hp <- gets renvHeap
          v <- heapGetM' hp p
          -- rsemTr $ "Evl: *" >|< p >|< ":" >#< v
          v' <- case v of
            RVal_Thunk {rvalSL=sl, rvalBody=e} -> do
              heapSetM' hp p RVal_BlackHole
              v' <- rvalAppLam sl e V.empty $ \_ -> err $ "CoreRun.Run.Val.rsemEvl.RVal_Thunk:" >#< e
              heapSetM' hp p v'
              return v'
            RVal_Ptr {rvalPtr=p} -> do
              v' <- evlPtr p
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
%%]


