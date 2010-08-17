%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Run
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An EHC compile run maintains info for one compilation invocation

%%[8 module {%{EH}EHC.CompileRun}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]
%%[8 import(System.Cmd)
%%]
%%[99 import(System.Directory)
%%]
%%[99 import(EH.Util.FPath)
%%]

%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[20 import({%{EH}EHC.CompileGroup})
%%]
-- Language syntax: Core
%%[(8 codegen) import( qualified {%{EH}Core} as Core)
%%]
-- Language syntax: TyCore
%%[(8 codegen) import(qualified {%{EH}TyCore} as C)
%%]
-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Language semantics: Core
%%[(8 codegen grin) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

-- HI Syntax and semantics, HS module semantics
%%[20 import(qualified {%{EH}HI} as HI)
%%]
%%[20 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
-- module admin
%%[20 import({%{EH}Module})
%%]

-- Misc
%%[102 import({%{EH}Debug.HighWaterMark})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCompileRunStateInfo(..))
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        :: !EHCOpts                             -- options
      , crsiNextUID     :: !UID                                 -- unique id, the next one
      , crsiHereUID     :: !UID                                 -- unique id, the current one
      , crsiHSInh       :: !HSSem.Inh_AGItf                     -- current inh attrs for HS sem
      , crsiEHInh       :: !EHSem.Inh_AGItf                     -- current inh attrs for EH sem
%%[[(8 codegen)
      , crsiCoreInh     :: !Core2GrSem.Inh_CodeAGItf            -- current inh attrs for Core2Grin sem
%%]]
%%[[20
      , crsiMbMainNm    :: !(Maybe HsName)                      -- name of main module, if any
      -- , crsiHIInh       :: !HISem.Inh_AGItf                     -- current inh attrs for HI sem
      , crsiHSModInh    :: !HSSemMod.Inh_AGItf                  -- current inh attrs for HS module analysis sem
      , crsiModMp       :: !ModMp                               -- import/export info for modules
      , crsiGrpMp       :: (Map.Map HsName EHCompileGroup)      -- not yet used, for mut rec modules
      , crsiOptim       :: !Optim                               -- inter module optimisation info
%%]]
%%[[(20 codegen)
      , crsiModOffMp    :: !Core.HsName2OffsetMpMp              -- mapping of all modules + exp entries to offsets in module + exp tables
%%]]
%%[[99
      , crsiFilesToRm   :: ![FPath]                             -- files to clean up (remove)
%%]]
      }
%%]

%%[8 export(emptyEHCompileRunStateInfo)
emptyEHCompileRunStateInfo :: EHCompileRunStateInfo
emptyEHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        =   defaultEHCOpts
      , crsiNextUID     =   uidStart
      , crsiHereUID     =   uidStart
      , crsiHSInh       =   panic "emptyEHCompileRunStateInfo.crsiHSInh"
      , crsiEHInh       =   panic "emptyEHCompileRunStateInfo.crsiEHInh"
%%[[(8 codegen)
      , crsiCoreInh     =   panic "emptyEHCompileRunStateInfo.crsiCoreInh"
%%]]
%%[[20
      , crsiMbMainNm    =   Nothing
      -- , crsiHIInh       =   panic "emptyEHCompileRunStateInfo.crsiHIInh"
      , crsiHSModInh    =   panic "emptyEHCompileRunStateInfo.crsiHSModInh"
      , crsiModMp       =   Map.empty
      , crsiGrpMp       =   Map.empty
      , crsiOptim       =   defaultOptim                    
%%]]
%%[[(20 codegen)
      , crsiModOffMp    =   Map.empty       
%%]]
%%[[99
      , crsiFilesToRm   =   []                  
%%]]
      }
%%]

%%[(20 codegen) export(crsiExpNmOffMp)
crsiExpNmOffMp :: HsName -> EHCompileRunStateInfo -> Core.HsName2OffsetMp
crsiExpNmOffMp modNm crsi = mmiNmOffMp $ panicJust "crsiExpNmOffMp" $ Map.lookup modNm $ crsiModMp crsi
%%]

%%[20
instance Show EHCompileRunStateInfo where
  show _ = "EHCompileRunStateInfo"

instance PP EHCompileRunStateInfo where
  pp i = "CRSI:" >#< ppModMp (crsiModMp i)
%%]

%%[8
instance CompileRunStateInfo EHCompileRunStateInfo HsName () where
  crsiImportPosOfCUKey n i = ()
%%]

%%[8 export(EHCompileRun,EHCompilePhase)
type EHCompileRun     = CompileRun   HsName EHCompileUnit EHCompileRunStateInfo Err
type EHCompilePhase a = CompilePhase HsName EHCompileUnit EHCompileRunStateInfo Err a
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile Run base info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(crBaseInfo,crBaseInfo')
crBaseInfo' :: EHCompileRun -> (EHCompileRunStateInfo,EHCOpts)
crBaseInfo' cr
  = (crsi,opts)
  where crsi   = crStateInfo cr
        opts   = crsiOpts  crsi

crBaseInfo :: HsName -> EHCompileRun -> (EHCompileUnit,EHCompileRunStateInfo,EHCOpts,FPath)
crBaseInfo modNm cr
  = (ecu,crsi,opts,fp)
  where ecu         = crCU modNm cr
        (crsi,opts) = crBaseInfo' cr
        fp          = ecuFilePath ecu
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: debug info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpMemUsage :: EHCompilePhase ()
cpMemUsage
%%[[8
  = return ()
%%][102
  = do { cr <- get
       ; let (crsi,opts) = crBaseInfo' cr
       ; size <- lift $ megaBytesAllocated
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (lift $ putStrLn ("Mem: " ++ show size ++ "M"))
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
cpUpdOpts :: (EHCOpts -> EHCOpts) -> EHCompilePhase ()
cpUpdOpts upd
  = cpUpdSI (\crsi -> crsi {crsiOpts = upd $ crsiOpts crsi})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: clean up (remove) files to be removed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(cpRegisterFilesToRm)
cpRegisterFilesToRm :: [FPath] -> EHCompilePhase ()
cpRegisterFilesToRm fpL
  = cpUpdSI (\crsi -> crsi {crsiFilesToRm = fpL ++ crsiFilesToRm crsi})
%%]

%%[99 export(cpRmFilesToRm)
cpRmFilesToRm :: EHCompilePhase ()
cpRmFilesToRm
  = do { cr <- get
       ; let (crsi,opts) = crBaseInfo' cr
             files = Set.toList $ Set.fromList $ map fpathToStr $ crsiFilesToRm crsi
       ; lift $ mapM rm files
       ; cpUpdSI (\crsi -> crsi {crsiFilesToRm = []})
       }
  where rm f = catch (removeFile f)
                     (\e -> hPutStrLn stderr (show f ++ ": " ++ show e))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpMsg,cpMsg')
cpMsg :: HsName -> Verbosity -> String -> EHCompilePhase ()
cpMsg modNm v m
  = do { cr <- get
       ; let (_,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm v m Nothing fp
       }

cpMsg' :: HsName -> Verbosity -> String -> Maybe String -> FPath -> EHCompilePhase ()
cpMsg' modNm v m mbInfo fp
  = do { cr <- get
       ; let (ecu,_,opts,_) = crBaseInfo modNm cr
%%[[8
             m'             = m
%%][99
             m'             = show (ecuSeqNr ecu) ++ " " ++ m
%%]]
       ; lift $ putCompileMsg v (ehcOptVerbosity opts) m' mbInfo modNm fp
       -- ; lift $ putStrLn "XX"
       ; cpMemUsage
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: step/set unique counter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpStepUID,cpSetUID)
cpStepUID :: EHCompilePhase ()
cpStepUID
  = cpUpdSI (\crsi -> let (n,h) = mkNewLevUID (crsiNextUID crsi)
                      in  crsi {crsiNextUID = n, crsiHereUID = h}
            )

cpSetUID :: UID -> EHCompilePhase ()
cpSetUID u
  = cpUpdSI (\crsi -> crsi {crsiNextUID = u})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: shell/system/cmdline invocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpSystem)
cpSystem :: String -> EHCompilePhase ()
cpSystem cmd
  = do { exitCode <- lift $ system cmd
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }
%%]
           _           -> do { lift $ putStrLn ("cpSystem ERR: " ++ show exitCode) 
                             ; cpSetFail
                             }

%%[8 export(cpSystemRaw)
cpSystemRaw :: String -> [String] -> EHCompilePhase ()
cpSystemRaw cmd args
  = do { exitCode <- lift $ rawSystem cmd args
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetErrs [rngLift emptyRange Err_PP $ pp $ show exitCode] -- cpSetFail
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: stop at phase
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpStopAt)
cpStopAt :: CompilePoint -> EHCompilePhase ()
cpStopAt atPhase
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; unless (atPhase < ehcStopAtPoint opts)
                cpSetStopAllSeq
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition imports into newer + older
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20
crPartitionNewerOlderImports :: HsName -> EHCompileRun -> ([EHCompileUnit],[EHCompileUnit])
crPartitionNewerOlderImports modNm cr
  = partition isNewer $ map (flip crCU cr) $ ecuImpNmL ecu
  where ecu = crCU modNm cr
        t   = panicJust "crPartitionNewerOlderImports1" $ ecuMbHIInfoTime ecu
        isNewer ecu'
            = t' `diffClockTimes` t > noTimeDiff 
            where t' = panicJust "crPartitionNewerOlderImports2" $ ecuMbHIInfoTime ecu'
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module needs recompilation?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(crModNeedsCompile)
crModNeedsCompile :: HsName -> EHCompileRun -> Bool
crModNeedsCompile modNm cr
  = ecuIsMainMod ecu -- ecuIsTopMod ecu
    || not (  ehcOptCheckRecompile opts
           && ecuCanUseHIInsteadOfHS ecu
           && null newer
           )
  where ecu = crCU modNm cr
        (newer,_) = crPartitionNewerOlderImports modNm cr
        opts = crsiOpts $ crStateInfo cr
%%]
    || (ehcOptCheckVersion opts
        && (  not (ecuCanUseHIInsteadOfHS ecu)
           || not (null newer)
       )   )

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compilation can actually be done?
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(crModCanCompile)
crModCanCompile :: HsName -> EHCompileRun -> Bool
crModCanCompile modNm cr
  = isJust (ecuMbHSTime ecu) && ecuDirIsWritable ecu
  where ecu = crCU modNm cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition modules into those belonging to a package and the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) export(crPartitionIntoPkgAndOthers)
crPartitionIntoPkgAndOthers :: EHCompileRun -> [HsName] -> ([PkgKey],[HsName])
crPartitionIntoPkgAndOthers cr modNmL
  = (nub $ concat ps,concat ms)
  where (ps,ms) = unzip $ map loc modNmL
        loc m = case filelocKind $ ecuFileLocation ecu of
                  FileLocKind_Dir	-> ([],[m])	
                  FileLocKind_Pkg p -> ([p],[])
              where (ecu,_,_,_) = crBaseInfo m cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Set 'main'-ness of module, checking whethere there are not too many modules having a main
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(crSetAndCheckMain)
crSetAndCheckMain :: HsName -> EHCompilePhase ()
crSetAndCheckMain modNm
  = do { cr <- get
       ; let (crsi,opts) = crBaseInfo' cr
             mkerr lim ns = cpSetLimitErrs 1 "compilation run" [rngLift emptyRange Err_MayOnlyHaveNrMain lim ns modNm]
       ; case crsiMbMainNm crsi of
           Just n | n /= modNm      -> mkerr 1 [n]
           _ | ehcOptDoLinking opts -> cpUpdSI (\crsi -> crsi {crsiMbMainNm = Just modNm})
             | otherwise            -> mkerr 0 []
       }
%%]

