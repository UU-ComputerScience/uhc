module EH101.EHC.CompileRun
( EHCompileRunStateInfo (..)
, emptyEHCompileRunStateInfo
, EHCompileRun, EHCompilePhase
, crBaseInfo, crBaseInfo'
, cpUpdOpts
, cpMsg, cpMsg'
, cpStepUID, cpSetUID
, cpSystem
, cpSystemRaw
, cpStopAt
, crsiExpNmOffMp
, crModNeedsCompile
, crModCanCompile
, crSetAndCheckMain
, EHCTime, EHCTimeDiff, getEHCTime, ehcTimeDiff, ehcTimeDiffFmt
, EHCIOInfo (..), newEHCIOInfo
, cpRegisterFilesToRm
, cpRmFilesToRm
, crPartitionIntoPkgAndOthers )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import System.IO
import System.Exit
import System.Environment
import System.Process
import System.Cmd (rawSystem)
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import qualified EH101.Core as Core
import qualified EH101.EH.MainAG as EHSem
import qualified EH101.HS.MainAG as HSSem
import qualified EH101.Core.ToGrin as Core2GrSem
import EH101.EHC.CompileGroup
import qualified EH101.HI as HI
import qualified EH101.HS.ModImpExp as HSSemMod
import EH101.Module
import System.CPUTime
import System.Locale
import Data.IORef
import System.IO.Unsafe
import System.Directory
import EH.Util.FPath
import EH101.Base.PackageDatabase









{-# LINE 61 "src/ehc/EHC/CompileRun.chs" #-}
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

{-# LINE 81 "src/ehc/EHC/CompileRun.chs" #-}
data EHCIOInfo
  = EHCIOInfo
      { ehcioinfoStartTime			:: EHCTime
      , ehcioinfoLastTime			:: EHCTime
      }

newEHCIOInfo :: IO (IORef EHCIOInfo)
newEHCIOInfo
  = do t <- getEHCTime
       newIORef (EHCIOInfo t t)


{-# LINE 115 "src/ehc/EHC/CompileRun.chs" #-}
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        :: !EHCOpts                             -- options
      , crsiNextUID     :: !UID                                 -- unique id, the next one
      , crsiHereUID     :: !UID                                 -- unique id, the current one
      , crsiHSInh       :: !HSSem.Inh_AGItf                     -- current inh attrs for HS sem
      , crsiEHInh       :: !EHSem.Inh_AGItf                     -- current inh attrs for EH sem
      , crsiCoreInh     :: !Core2GrSem.Inh_CodeAGItf            -- current inh attrs for Core2Grin sem
      , crsiMbMainNm    :: !(Maybe HsName)                      -- name of main module, if any
      -- , crsiHIInh       :: !HISem.Inh_AGItf                     -- current inh attrs for HI sem
      , crsiHSModInh    :: !HSSemMod.Inh_AGItf                  -- current inh attrs for HS module analysis sem
      , crsiModMp       :: !ModMp                               -- import/export info for modules
      , crsiGrpMp       :: (Map.Map HsName EHCompileGroup)      -- not yet used, for mut rec modules
      , crsiOptim       :: !Optim                               -- inter module optimisation info
      , crsiModOffMp    :: !Core.HsName2OffsetMpMp              -- mapping of all modules + exp entries to offsets in module + exp tables
      , crsiEHCIOInfo	:: !(IORef EHCIOInfo)					-- unsafe info
      , crsiFilesToRm   :: ![FPath]                             -- files to clean up (remove)
      }

{-# LINE 144 "src/ehc/EHC/CompileRun.chs" #-}
emptyEHCompileRunStateInfo :: EHCompileRunStateInfo
emptyEHCompileRunStateInfo
  = EHCompileRunStateInfo
      { crsiOpts        =   defaultEHCOpts
      , crsiNextUID     =   uidStart
      , crsiHereUID     =   uidStart
      , crsiHSInh       =   panic "emptyEHCompileRunStateInfo.crsiHSInh"
      , crsiEHInh       =   panic "emptyEHCompileRunStateInfo.crsiEHInh"
      , crsiCoreInh     =   panic "emptyEHCompileRunStateInfo.crsiCoreInh"
      , crsiMbMainNm    =   Nothing
      -- , crsiHIInh       =   panic "emptyEHCompileRunStateInfo.crsiHIInh"
      , crsiHSModInh    =   panic "emptyEHCompileRunStateInfo.crsiHSModInh"
      , crsiModMp       =   Map.empty
      , crsiGrpMp       =   Map.empty
      , crsiOptim       =   defaultOptim
      , crsiModOffMp    =   Map.empty
      , crsiEHCIOInfo   =   panic "emptyEHCompileRunStateInfo.crsiEHCIOInfo"
      , crsiFilesToRm   =   []
      }

{-# LINE 174 "src/ehc/EHC/CompileRun.chs" #-}
crsiExpNmOffMp :: HsName -> EHCompileRunStateInfo -> Core.HsName2OffsetMp
crsiExpNmOffMp modNm crsi = mmiNmOffMp $ panicJust ("crsiExpNmOffMp: " ++ show modNm) $ Map.lookup modNm $ crsiModMp crsi

{-# LINE 179 "src/ehc/EHC/CompileRun.chs" #-}
instance Show EHCompileRunStateInfo where
  show _ = "EHCompileRunStateInfo"

instance PP EHCompileRunStateInfo where
  pp i = "CRSI:" >#< ppModMp (crsiModMp i)

{-# LINE 187 "src/ehc/EHC/CompileRun.chs" #-}
instance CompileRunStateInfo EHCompileRunStateInfo HsName () where
  crsiImportPosOfCUKey n i = ()

{-# LINE 192 "src/ehc/EHC/CompileRun.chs" #-}
type EHCompileRun     = CompileRun   HsName EHCompileUnit EHCompileRunStateInfo Err
type EHCompilePhase a = CompilePhase HsName EHCompileUnit EHCompileRunStateInfo Err a

{-# LINE 201 "src/ehc/EHC/CompileRun.chs" #-}
crBaseInfo' :: EHCompileRun -> (EHCompileRunStateInfo,EHCOpts)
crBaseInfo' cr
  = (crsi,opts)
  where crsi   = crStateInfo cr
        opts   = crsiOpts  crsi

crBaseInfo :: HsName -> EHCompileRun -> (EHCompileUnit,EHCompileRunStateInfo,EHCOpts,FPath)
crBaseInfo modNm cr
  = ( ecu ,crsi
    -- if any per module opts are available, use those
    , maybe opts id $ ecuMbOpts ecu
    , fp
    )
  where ecu         = crCU modNm cr
        (crsi,opts) = crBaseInfo' cr
        fp          = ecuFilePath ecu

{-# LINE 228 "src/ehc/EHC/CompileRun.chs" #-}
cpMemUsage :: EHCompilePhase ()
cpMemUsage
  = return ()

{-# LINE 251 "src/ehc/EHC/CompileRun.chs" #-}
cpUpdOpts :: (EHCOpts -> EHCOpts) -> EHCompilePhase ()
cpUpdOpts upd
  = cpUpdSI (\crsi -> crsi {crsiOpts = upd $ crsiOpts crsi})

{-# LINE 261 "src/ehc/EHC/CompileRun.chs" #-}
cpRegisterFilesToRm :: [FPath] -> EHCompilePhase ()
cpRegisterFilesToRm fpL
  = cpUpdSI (\crsi -> crsi {crsiFilesToRm = fpL ++ crsiFilesToRm crsi})

{-# LINE 267 "src/ehc/EHC/CompileRun.chs" #-}
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

{-# LINE 284 "src/ehc/EHC/CompileRun.chs" #-}
cpMsg :: HsName -> Verbosity -> String -> EHCompilePhase ()
cpMsg modNm v m
  = do { cr <- get
       ; let (_,_,_,fp) = crBaseInfo modNm cr
       ; cpMsg' modNm v m Nothing fp
       }

cpMsg' :: HsName -> Verbosity -> String -> Maybe String -> FPath -> EHCompilePhase ()
cpMsg' modNm v m mbInfo fp
  = do { cr <- get
       ; let (ecu,crsi,opts,_) = crBaseInfo modNm cr
       ; ehcioinfo <- lift $ readIORef (crsiEHCIOInfo crsi)
       ; clockTime <- lift getEHCTime
       ; let clockStartTimePrev = ehcioinfoStartTime ehcioinfo
             clockTimePrev      = ehcioinfoLastTime ehcioinfo
             clockStartTimeDiff = ehcTimeDiff clockTime clockStartTimePrev
             clockTimeDiff      = ehcTimeDiff clockTime clockTimePrev
       ; let
             t				= if v >= VerboseALot then "<" ++ strBlankPad 35 (ehcTimeDiffFmt clockStartTimeDiff ++ "/" ++ ehcTimeDiffFmt clockTimeDiff) ++ ">" else ""
             m'             = show (ecuSeqNr ecu) ++ t ++ " " ++ m
       ; lift $ putCompileMsg v (ehcOptVerbosity opts) m' mbInfo modNm fp
       ; clockTime <- lift getEHCTime
       ; lift $ writeIORef (crsiEHCIOInfo crsi) (ehcioinfo {ehcioinfoLastTime = clockTime})
       -- ; cpUpdSI (\crsi -> crsi { crsiTime = clockTime })
       ; cpMemUsage
       }

{-# LINE 325 "src/ehc/EHC/CompileRun.chs" #-}
cpStepUID :: EHCompilePhase ()
cpStepUID
  = cpUpdSI (\crsi -> let (n,h) = mkNewLevUID (crsiNextUID crsi)
                      in  crsi {crsiNextUID = n, crsiHereUID = h}
            )

cpSetUID :: UID -> EHCompilePhase ()
cpSetUID u
  = cpUpdSI (\crsi -> crsi {crsiNextUID = u})

{-# LINE 341 "src/ehc/EHC/CompileRun.chs" #-}
cpSystem :: String -> EHCompilePhase ()
cpSystem cmd
  = do { exitCode <- lift $ system cmd
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }

{-# LINE 354 "src/ehc/EHC/CompileRun.chs" #-}
cpSystemRaw :: String -> [String] -> EHCompilePhase ()
cpSystemRaw cmd args
  = do { exitCode <- lift $ rawSystem cmd args
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetErrs [rngLift emptyRange Err_PP $ pp $ show exitCode] -- cpSetFail
       }

{-# LINE 368 "src/ehc/EHC/CompileRun.chs" #-}
cpStopAt :: CompilePoint -> EHCompilePhase ()
cpStopAt atPhase
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; unless (atPhase < ehcStopAtPoint opts)
                cpSetStopAllSeq
       }

{-# LINE 382 "src/ehc/EHC/CompileRun.chs" #-}
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

{-# LINE 399 "src/ehc/EHC/CompileRun.chs" #-}
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

{-# LINE 420 "src/ehc/EHC/CompileRun.chs" #-}
crModCanCompile :: HsName -> EHCompileRun -> Bool
crModCanCompile modNm cr
  = isJust (ecuMbHSTime ecu) && ecuDirIsWritable ecu
  where ecu = crCU modNm cr

{-# LINE 431 "src/ehc/EHC/CompileRun.chs" #-}
-- | split module names in those part of a package, and others
crPartitionIntoPkgAndOthers :: EHCompileRun -> [HsName] -> ([PkgModulePartition],[HsName])
crPartitionIntoPkgAndOthers cr modNmL
  = ( [ (p,d,m)
      | ((p,d),m) <- Map.toList $ Map.unionsWith (++) $ map Map.fromList ps
      ] -- nub $ concat ps
    , concat ms
    )
  where (ps,ms) = unzip $ map loc modNmL
        loc m = case filelocKind $ ecuFileLocation ecu of
                  FileLocKind_Dir	  -> ([]         ,[m])
                  FileLocKind_Pkg p d -> ([((p,d),[m])],[] )
              where (ecu,_,_,_) = crBaseInfo m cr

{-# LINE 451 "src/ehc/EHC/CompileRun.chs" #-}
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

