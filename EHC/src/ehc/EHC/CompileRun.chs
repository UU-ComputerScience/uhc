%%[0 hs
{-# LANGUAGE TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Run
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An EHC compile run maintains info for one compilation invocation

%%[8 module {%{EH}EHC.CompileRun}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]
%%[8 import(System.IO, System.Exit, System.Environment, System.Process)
%%]
%%[99 import(UHC.Util.Time, System.CPUTime, System.Locale, Data.IORef, System.IO.Unsafe)
%%]
%%[99 import(System.Directory)
%%]
%%[8 import(Control.Monad.State hiding (get), qualified Control.Monad.State as MS)
%%]
%%[8 import(Control.Monad.Error)
%%]
%%[8 import(Control.Exception as CE)
%%]
%%[8 import(UHC.Util.Lens)
%%]
%%[99 import(UHC.Util.FPath)
%%]
%%[99 import({%{EH}Base.PackageDatabase})
%%]

%%[(8 codegen) hs import({%{EH}CodeGen.ValAccess} as VA)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[50 import({%{EH}EHC.CompileGroup})
%%]
-- Build function state
%%[8 import({%{EH}EHC.BuildFunction})
%%]
-- Language syntax: Core
%%[(8 codegen) import( qualified {%{EH}Core} as Core)
%%]
-- Language syntax: CoreRun
%%[(8 corerun) import( qualified {%{EH}CoreRun} as CoreRun)
%%]
-- Language syntax: TyCore
%%[(8 codegen tycore) import(qualified {%{EH}TyCore} as C)
%%]
-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Language semantics: Core
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

-- HI Syntax and semantics, HS module semantics
%%[5050 import(qualified {%{EH}HI} as HI)
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
-- module admin
%%[50 import({%{EH}Module.ImportExport})
%%]

-- Misc
%%[102 import({%{EH}Debug.HighWaterMark})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(EHCTime, EHCTimeDiff, getEHCTime, ehcTimeDiff, ehcTimeDiffFmt)
type EHCTime = Integer
type EHCTimeDiff = Integer

getEHCTime :: IO EHCTime
getEHCTime = getCPUTime

ehcTimeDiff :: EHCTime -> EHCTime -> EHCTimeDiff
ehcTimeDiff = (-)

ehcTimeDiffFmt :: EHCTimeDiff -> String
ehcTimeDiffFmt t
  = fm 2 hrs ++ ":" ++ fm 2 mins ++ ":" ++ fm 2 secs ++ ":" ++ fm 6 (psecs `div` 1000000)
  where (r0  , psecs) = t  `quotRem` 1000000000000
        (r1  , secs ) = r0 `quotRem` 60
        (r2  , mins ) = r1 `quotRem` 60
        (days, hrs  ) = r2 `quotRem` 24
        fm n x = strPadLeft '0' n (show x)
%%]

%%[99 export(EHCIOInfo(..),newEHCIOInfo)
data EHCIOInfo
  = EHCIOInfo
      { ehcioinfoStartTime			:: EHCTime
      , ehcioinfoLastTime			:: EHCTime
      }

newEHCIOInfo :: IO (IORef EHCIOInfo)
newEHCIOInfo
  = do t <- getEHCTime
       newIORef (EHCIOInfo t t)
%%]

%%[99
%%]

%%[9999 export(unsafeTimedPerformIO)
-- | Unsafe do IO with timing info
unsafeTimedPerformIO
  :: IORef EHCIOInfo
     -> IO a
     -> ( a
        , EHCTime				-- last time being called
        , EHCTimeDiff			-- delta since that time
        )
unsafeTimedPerformIO inforef io
  = unsafePerformIO $
    do info <- readIORef inforef
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCompileRunStateInfo(..))
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { _crsiOpts       :: !EHCOpts                             -- options
      , _crsiNextUID    :: !UID                                 -- unique id, the next one
      , _crsiHereUID    :: !UID                                 -- unique id, the current one
      , _crsiHSInh      :: !HSSem.Inh_AGItf                     -- current inh attrs for HS sem
      , _crsiEHInh      :: !EHSem.Inh_AGItf                     -- current inh attrs for EH sem
%%[[(8 codegen)
      , crsiCoreInh     :: !Core2GrSem.Inh_CodeAGItf            -- current inh attrs for Core2Grin sem
%%]]
%%[[(8 corerun)
      , crsiCore2RunInh	:: !CoreRun.Nm2RefMp       				-- current inh attrs for Core2CoreRun sem
%%]]
%%[[50
      , crsiMbMainNm    :: !(Maybe HsName)                      -- name of main module, if any
      , crsiHSModInh    :: !HSSemMod.Inh_AGItf                  -- current inh attrs for HS module analysis sem
      , crsiModMp       :: !ModMp                               -- import/export info for modules
      , crsiGrpMp       :: (Map.Map HsName EHCompileGroup)      -- not yet used, for mut rec modules
      , crsiOptim       :: !Optim                               -- inter module optimisation info
%%]]
%%[[(50 codegen)
      , crsiModOffMp    :: !VA.HsName2FldMpMp              		-- mapping of all modules + exp entries to offsets in module + exp tables
%%]]
%%[[99
      , crsiEHCIOInfo	:: !(IORef EHCIOInfo)					-- unsafe info
      , crsiFilesToRm   :: ![FPath]                             -- files to clean up (remove)
%%]]
      , _crsiBState		:: !BState								-- Build state for use of build functions
      }
%%]

%%[8 export(crsiOpts, crsiNextUID, crsiHereUID, crsiHSInh, crsiEHInh, crsiBState)
mkLabel ''EHCompileRunStateInfo
%%]

%%[8 export(emptyEHCompileRunStateInfo)
emptyEHCompileRunStateInfo :: EHCompileRunStateInfo
emptyEHCompileRunStateInfo
  = EHCompileRunStateInfo
      { _crsiOpts       =   defaultEHCOpts
      , _crsiNextUID    =   uidStart
      , _crsiHereUID    =   uidStart
      , _crsiHSInh      =   panic "emptyEHCompileRunStateInfo.crsiHSInh"
      , _crsiEHInh      =   panic "emptyEHCompileRunStateInfo.crsiEHInh"
%%[[(8 codegen)
      , crsiCoreInh     =   panic "emptyEHCompileRunStateInfo.crsiCoreInh"
%%]]
%%[[(8 corerun)
      , crsiCore2RunInh	=   panic "emptyEHCompileRunStateInfo.crsiCoreRunInh"
%%]]
%%[[50
      , crsiMbMainNm    =   Nothing
      , crsiHSModInh    =   panic "emptyEHCompileRunStateInfo.crsiHSModInh"
      , crsiModMp       =   Map.empty
      , crsiGrpMp       =   Map.empty
      , crsiOptim       =   defaultOptim
%%]]
%%[[(50 codegen)
      , crsiModOffMp    =   Map.empty
%%]]
%%[[99
      , crsiEHCIOInfo   =   panic "emptyEHCompileRunStateInfo.crsiEHCIOInfo"
      , crsiFilesToRm   =   []
%%]]
      , _crsiBState   	=   emptyBState
      }
%%]

%%[(50 codegen) export(crsiExpNmOffMpDbg, crsiExpNmOffMp)
crsiExpNmOffMpDbg :: String -> HsName -> EHCompileRunStateInfo -> VA.HsName2FldMp
crsiExpNmOffMpDbg ctxt modNm crsi = mmiNmOffMp $ panicJust ("crsiExpNmOffMp." ++ ctxt ++ show ks ++ ": " ++ show modNm) $ Map.lookup modNm $ crsiModMp crsi
  where ks = Map.keys $ crsiModMp crsi

crsiExpNmOffMp :: HsName -> EHCompileRunStateInfo -> VA.HsName2FldMp
crsiExpNmOffMp modNm crsi = mmiNmOffMp $ panicJust ("crsiExpNmOffMp: " ++ show modNm) $ Map.lookup modNm $ crsiModMp crsi
%%]

%%[50
instance Show EHCompileRunStateInfo where
  show _ = "EHCompileRunStateInfo"

instance PP EHCompileRunStateInfo where
  pp i = "CRSI:" >#< ppModMp (crsiModMp i)
%%]

%%[8
instance CompileRunStateInfo EHCompileRunStateInfo HsName () where
  crsiImportPosOfCUKey n i = ()
%%]

%%[8 export(EHCCompileRunner)
class (MonadIO m, CompileRunner EHCompileUnitState HsName () FileLoc EHCompileUnit EHCompileRunStateInfo Err (EHCompilePhaseAddonT m)) => EHCCompileRunner m where

instance ( CompileRunStateInfo EHCompileRunStateInfo HsName ()
         , CompileUnit EHCompileUnit HsName FileLoc EHCompileUnitState
         , CompileRunError Err ()
         -- , MonadError (CompileRunState Err) m
         -- , MonadState EHCompileRun (EHCompilePhaseAddonT m)
         , MonadIO m -- (EHCompilePhaseAddonT m)
         , Monad m
         ) => CompileRunner EHCompileUnitState HsName () FileLoc EHCompileUnit EHCompileRunStateInfo Err (EHCompilePhaseAddonT m)

instance ( CompileRunStateInfo EHCompileRunStateInfo HsName ()
         , CompileUnit EHCompileUnit HsName FileLoc EHCompileUnitState
         , CompileRunError Err ()
         -- , MonadError (CompileRunState Err) m
         -- , MonadState EHCompileRun (EHCompilePhaseAddonT m)
         , MonadIO m -- (EHCompilePhaseAddonT m)
         , Monad m
         ) => EHCCompileRunner m
%%]

%%[8 export(EHCompileRun,EHCompilePhaseT,EHCompilePhase)
type EHCompileRun           = CompileRun HsName EHCompileUnit EHCompileRunStateInfo Err
type EHCompilePhaseAddonT m = StateT EHCompileRun m
type EHCompilePhaseT      m = CompilePhaseT HsName EHCompileUnit EHCompileRunStateInfo Err (EHCompilePhaseAddonT m)
type EHCompilePhase         = EHCompilePhaseT IO
%%]
-- type EHCompilePhase a = CompilePhase HsName EHCompileUnit EHCompileRunStateInfo Err a

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile Run base info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(crBaseInfo,crMbBaseInfo,crBaseInfo')
crBaseInfo' :: EHCompileRun -> (EHCompileRunStateInfo,EHCOpts)
crBaseInfo' cr
  = (crsi,opts)
  where crsi   = _crStateInfo cr
        opts   = crsi ^. crsiOpts

crMbBaseInfo :: HsName -> EHCompileRun -> (Maybe EHCompileUnit, EHCompileRunStateInfo, EHCOpts, Maybe FPath)
crMbBaseInfo modNm cr
  = ( mbEcu ,crsi
%%[[8
    , opts
%%][99
    -- if any per module opts are available, use those
    , maybe opts id $ mbEcu >>= ecuMbOpts
%%]]
    , fmap ecuFilePath mbEcu
    )
  where mbEcu       = crMbCU modNm cr
        (crsi,opts) = crBaseInfo' cr

crBaseInfo :: HsName -> EHCompileRun -> (EHCompileUnit,EHCompileRunStateInfo,EHCOpts,FPath)
crBaseInfo modNm cr
  = ( maybe (panic "crBaseInfo.mbEcu") id mbEcu 
    , crsi
    , opts
    , maybe (panic "crBaseInfo.mbFp") id mbFp
    )
  where (mbEcu, crsi, opts, mbFp) = crMbBaseInfo modNm cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: debug info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpMemUsage :: EHCCompileRunner m => EHCompilePhaseT m ()
cpMemUsage
%%[[8
  = return ()
%%][102
  = do { cr <- MS.get
       ; let (crsi,opts) = crBaseInfo' cr
       ; size <- liftIO $ megaBytesAllocated
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (liftIO $ putStrLn ("Mem: " ++ show size ++ "M"))
       }
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update: options, additional exports
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpUpdOpts)
cpUpdOpts :: EHCCompileRunner m => (EHCOpts -> EHCOpts) -> EHCompilePhaseT m ()
cpUpdOpts upd
  = cpUpdSI $ crsiOpts ^$= upd -- (\crsi -> crsi {crsiOpts = upd $ crsiOpts crsi})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: clean up (remove) files to be removed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(cpRegisterFilesToRm)
cpRegisterFilesToRm :: EHCCompileRunner m => [FPath] -> EHCompilePhaseT m ()
cpRegisterFilesToRm fpL
  = cpUpdSI (\crsi -> crsi {crsiFilesToRm = fpL ++ crsiFilesToRm crsi})
%%]

%%[99 export(cpRmFilesToRm)
cpRmFilesToRm :: EHCCompileRunner m => EHCompilePhaseT m ()
cpRmFilesToRm
  = do { cr <- MS.get
       ; let (crsi,opts) = crBaseInfo' cr
             files = Set.toList $ Set.fromList $ map fpathToStr $ crsiFilesToRm crsi
       ; liftIO $ mapM rm files
       ; cpUpdSI (\crsi -> crsi {crsiFilesToRm = []})
       }
  where rm f = CE.catch (removeFile f)
                        (\(e :: SomeException) -> hPutStrLn stderr (show f ++ ": " ++ show e))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpMsg,cpMsg')
cpMsg :: EHCCompileRunner m => HsName -> Verbosity -> String -> EHCompilePhaseT m ()
cpMsg modNm v m
  = do { cr <- MS.get
       ; let (_,_,_,mbFp) = crMbBaseInfo modNm cr
       ; cpMsg' modNm v m Nothing (maybe emptyFPath id mbFp)
       }

cpMsg' :: EHCCompileRunner m => HsName -> Verbosity -> String -> Maybe String -> FPath -> EHCompilePhaseT m ()
cpMsg' modNm v m mbInfo fp
  = do { cr <- MS.get
       ; let (mbEcu,crsi,opts,_) = crMbBaseInfo modNm cr
%%[[99
       ; ehcioinfo <- liftIO $ readIORef (crsiEHCIOInfo crsi)
       ; clockTime <- liftIO getEHCTime
       ; let clockStartTimePrev = ehcioinfoStartTime ehcioinfo
             clockTimePrev      = ehcioinfoLastTime ehcioinfo
             clockStartTimeDiff = ehcTimeDiff clockTime clockStartTimePrev
             clockTimeDiff      = ehcTimeDiff clockTime clockTimePrev
%%]]
       ; let
%%[[8
             m'             = m
%%][99
             t				= if v >= VerboseALot then "<" ++ strBlankPad 35 (ehcTimeDiffFmt clockStartTimeDiff ++ "/" ++ ehcTimeDiffFmt clockTimeDiff) ++ ">" else ""
             m'             = maybe "" (\ecu -> show (ecuSeqNr ecu) ++ t ++ " ") mbEcu ++ m
%%]]
       ; liftIO $ putCompileMsg v (ehcOptVerbosity opts) m' mbInfo modNm fp
%%[[99
       ; clockTime <- liftIO getEHCTime
       ; liftIO $ writeIORef (crsiEHCIOInfo crsi) (ehcioinfo {ehcioinfoLastTime = clockTime})
       -- ; cpUpdSI (\crsi -> crsi { crsiTime = clockTime })
%%]]
       ; cpMemUsage
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: step/set unique counter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpStepUID,cpSetUID)
cpStepUID :: EHCCompileRunner m => EHCompilePhaseT m ()
cpStepUID
  = cpUpdSI (\crsi -> let (n,h) = mkNewLevUID (crsi ^. crsiNextUID)
                      in  crsiNextUID ^= n $ crsiHereUID ^= h $ crsi
                          -- crsi {_crsiNextUID = n, _crsiHereUID = h}
            )

cpSetUID :: EHCCompileRunner m => UID -> EHCompilePhaseT m ()
cpSetUID u
  = cpUpdSI $ crsiNextUID ^= u -- (\crsi -> crsi {crsiNextUID = u})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: shell/system/cmdline invocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpSystem',cpSystem)
cpSystem' :: EHCCompileRunner m => Maybe FilePath -> (FilePath,[String]) -> EHCompilePhaseT m ()
cpSystem' mbStdOut (cmd,args)
  = do { exitCode <- liftIO $ system $ showShellCmd $ (cmd,args ++ (maybe [] (\o -> [">", o]) mbStdOut))
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }

cpSystem :: EHCCompileRunner m => (FilePath,[String]) -> EHCompilePhaseT m ()
cpSystem = cpSystem' Nothing
%%]
cpSystem' :: (FilePath,[String]) -> Maybe FilePath -> EHCompilePhase ()
cpSystem' (cmd,args) mbStdOut
  = do { exitcode <- liftIO $ do 
           proc <- runProcess cmd args Nothing Nothing Nothing Nothing Nothing
           waitForProcess proc
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }

cpSystem' :: (FilePath,[String]) -> Maybe FilePath -> EHCompilePhase ()
cpSystem' (cmd,args) mbStdOut
  = do { exitCode <- liftIO $ system cmd
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }

%%[8 export(cpSystemRaw)
cpSystemRaw :: EHCCompileRunner m => String -> [String] -> EHCompilePhaseT m ()
cpSystemRaw cmd args
  = do { exitCode <- liftIO $ rawSystem cmd args
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetErrs [rngLift emptyRange Err_PP $ pp $ show exitCode] -- cpSetFail
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: stop at phase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpStopAt)
cpStopAt :: EHCCompileRunner m => CompilePoint -> EHCompilePhaseT m ()
cpStopAt atPhase
  = do { cr <- MS.get
       ; let (_,opts) = crBaseInfo' cr
       ; unless (atPhase < ehcStopAtPoint opts)
                cpSetStopAllSeq
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition imports into newer + older
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
crPartitionNewerOlderImports :: HsName -> EHCompileRun -> ([EHCompileUnit],[EHCompileUnit])
crPartitionNewerOlderImports modNm cr
  = partition isNewer $ map (flip crCU cr) $ ecuImpNmL ecu
  where ecu = crCU modNm cr
        t   = panicJust "crPartitionNewerOlderImports1" $ ecuMbHIInfoTime ecu
        isNewer ecu'
            | isJust mbt = t' `diffClockTimes` t > noTimeDiff
            | otherwise  = False
            where t' = panicJust "crPartitionNewerOlderImports2" $ ecuMbHIInfoTime ecu'
                  mbt = ecuMbHIInfoTime ecu'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition imports into main and non main module names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(crPartitionMainAndImported)
-- | Partition modules into main and non main (i.e. imported) module names
crPartitionMainAndImported :: EHCompileRun -> [HsName] -> ([HsName], [HsName])
crPartitionMainAndImported cr modNmL = partition (\n -> ecuHasMain $ crCU n cr) modNmL
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module needs recompilation?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(crModNeedsCompile)
crModNeedsCompile :: HsName -> EHCompileRun -> Bool
crModNeedsCompile modNm cr
  = ecuIsMainMod ecu -- ecuIsTopMod ecu
    || not (  ehcOptCheckRecompile opts
           && ecuCanUseHIInsteadOfHS ecu
           && null newer
           )
  where ecu = crCU modNm cr
        (newer,_) = crPartitionNewerOlderImports modNm cr
        opts = _crStateInfo cr ^. crsiOpts
%%]
    || (ehcOptCheckVersion opts
        && (  not (ecuCanUseHIInsteadOfHS ecu)
           || not (null newer)
       )   )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation can actually be done?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(crModCanCompile)
crModCanCompile :: HsName -> EHCompileRun -> Bool
crModCanCompile modNm cr
  = isJust (ecuMbSrcTime ecu) && ecuDirIsWritable ecu
  where ecu = crCU modNm cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition modules into those belonging to a package and the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) export(crPartitionIntoPkgAndOthers)
-- | split module names in those part of a package, and others
crPartitionIntoPkgAndOthers :: EHCompileRun -> [HsName] -> ([PkgModulePartition],[HsName])
crPartitionIntoPkgAndOthers cr modNmL
  = ( [ (p,d,m)
      | ((p,d),m) <- Map.toList $ Map.unionsWith (++) $ map Map.fromList ps
      ]
    , concat ms
    )
  where (ps,ms) = unzip $ map loc modNmL
        loc m = case filelocKind $ ecuFileLocation ecu of
                  FileLocKind_Dir	  -> ([           ], [m])
                  FileLocKind_Pkg p d -> ([((p,d),[m])], [ ])
              where (ecu,_,_,_) = crBaseInfo m cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Set 'main'-ness of module, checking whethere there are not too many modules having a main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(crSetAndCheckMain)
crSetAndCheckMain :: EHCCompileRunner m => HsName -> EHCompilePhaseT m ()
crSetAndCheckMain modNm
  = do { cr <- MS.get
       ; let (crsi,opts) = crBaseInfo' cr
             mkerr lim ns = cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MayOnlyHaveNrMain lim ns modNm]
       ; case crsiMbMainNm crsi of
           Just n | n /= modNm          -> mkerr 1 [n]
           _ | ehcOptDoExecLinking opts -> cpUpdSI (\crsi -> crsi {crsiMbMainNm = Just modNm})
             | otherwise                -> return ()
                                           -- mkerr 0 []
       }
%%]

