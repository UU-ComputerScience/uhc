module EH101.EHC.CompilePhase.Parsers
( cpParseOffside
, cpParsePlain
, cpParseEH
, cpParseGrin
, cpParseOffsideStopAtErr
, cpParseCore
, cpDecodeHIInfo
, cpDecodeGrin
, cpDecodeCore
, cpGetPrevHI
, cpGetPrevCore
, cpGetPrevGrin
, cpParseHs
, cpParseHsImport )
where
import UU.Parsing
import UU.Parsing.Offside
import qualified EH.Util.ScanUtils as ScanUtils
import EH101.Scanner.Common
import EH.Util.ParseUtils
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.EH as EH
import qualified EH101.EH.Parser as EHPrs
import qualified EH101.HS as HS
import qualified EH101.HS.Parser as HSPrs
import qualified EH101.GrinCode as Grin
import qualified EH101.GrinCode.Parser as GrinParser
import qualified EH101.HI as HI
import qualified EH101.Core as Core
import qualified EH101.Core.Parser as CorePrs
import qualified EH101.Base.Binary as Bin
import EH101.Base.Serialize
import qualified EH101.Config as Cfg






{-# LINE 54 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParseOffsideWithFPath :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> Maybe FPath -> HsName -> EHCompilePhase ()
cpParseOffsideWithFPath parser scanOpts store description mbFp modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (maybe (ecuFilePath (crCU modNm cr)) id mbFp) ReadMode False
      ; tokens  <- lift $ offsideScanHandle scanOpts fn fh
      -- ; lift $ putStrLn $ show tokens -- does not work, no Show instance
      ; let (res,msgs) = parseOffsideToResMsgs parser tokens
            errs       = map (rngLift emptyRange mkPPErr) msgs
      ; cpUpdCU modNm (store res)
      ; cpSetLimitErrsWhen 5 description errs
      }

cpParseOffside :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> HsName -> EHCompilePhase ()
cpParseOffside parser scanOpts store description modNm
 = cpParseOffsideWithFPath parser scanOpts store description Nothing modNm

{-# LINE 72 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParsePlainWithHandleToErrs :: PlainParser Token a -> ScanUtils.ScanOpts -> EcuUpdater a -> (String, Handle) -> HsName -> EHCompilePhase [Err]
cpParsePlainWithHandleToErrs parser scanOpts store (fn,fh) modNm
 = do { cr <- get
      ; tokens  <- lift $ scanHandle scanOpts fn fh
      ; let (res,msgs) = parseToResMsgs parser tokens
            errs       = map (rngLift emptyRange mkPPErr) msgs
      ; when (null errs)
             (cpUpdCU modNm (store res))
      ; return errs
      }

cpParsePlainToErrs :: PlainParser Token a -> ScanUtils.ScanOpts -> EcuUpdater a -> FPath -> HsName -> EHCompilePhase [Err]
cpParsePlainToErrs parser scanOpts store fp modNm
 = do { fnfh@(fn,fh) <- lift $ openFPath fp ReadMode False
      ; cpParsePlainWithHandleToErrs parser scanOpts store fnfh modNm
      }

cpParsePlain :: PlainParser Token a -> ScanUtils.ScanOpts -> EcuUpdater a -> String -> FPath -> HsName -> EHCompilePhase ()
cpParsePlain parser scanOpts store description fp modNm
 = do { errs <- cpParsePlainToErrs parser scanOpts store fp modNm
      ; cpSetLimitErrsWhen 5 description errs
      }

{-# LINE 97 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParseEH :: HsName -> EHCompilePhase ()
cpParseEH
  = cpParseOffside EHPrs.pAGItf (ehScanOpts defaultEHCOpts) ecuStoreEH "Parse (EH syntax) of module"

{-# LINE 103 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParseGrin :: HsName -> EHCompilePhase ()
cpParseGrin modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpC     = fpathSetSuff "grin" fp
       ; cpMsg' modNm VerboseALot "Parsing" Nothing fpC
       ; cpParsePlain GrinParser.pModule grinScanOpts ecuStoreGrin "Parse grin" fpC modNm
       ; return ()
       }

{-# LINE 124 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParseHs :: Bool -> HsName -> EHCompilePhase ()
cpParseHs litmode modNm
  = do { cr <- get
       ; let  (ecu,_,opts,_) = crBaseInfo modNm cr
       ; cpParseOffsideWithFPath
           (HSPrs.pAGItf opts) ((hsScanOpts opts) {ScanUtils.scoLitmode = litmode}) ecuStoreHS
           ("Parse (" ++ (if litmode then "Literate " else "") ++ "Haskell syntax) of module")
           Nothing modNm
       }

{-# LINE 136 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParseOffsideStopAtErr :: HSPrs.HSParser a -> ScanUtils.ScanOpts -> EcuUpdater a -> HsName -> EHCompilePhase ()
cpParseOffsideStopAtErr parser scanOpts store modNm
 = do { cr <- get
      ; (fn,fh) <- lift $ openFPath (ecuFilePath (crCU modNm cr)) ReadMode False
      ; tokens  <- lift $ offsideScanHandle scanOpts fn fh
      ; let (res,_) = parseOffsideToResMsgsStopAtErr parser tokens
      ; cpUpdCU modNm (store res)
      }

{-# LINE 156 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParseHsImport :: Bool -> HsName -> EHCompilePhase ()
cpParseHsImport litmode modNm
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
       ; cpParseOffsideStopAtErr (HSPrs.pAGItfImport opts) ((hsScanOpts opts) {ScanUtils.scoLitmode = litmode}) ecuStoreHS modNm
       }

{-# LINE 165 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpParseCore :: HsName -> EHCompilePhase ()
cpParseCore modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpC     = fpathSetSuff "core" fp
       ; cpMsg' modNm VerboseALot "Parsing" Nothing fpC
       ; errs <- cpParsePlainToErrs CorePrs.pCModule (coreScanOpts opts) ecuStoreCore fpC modNm
       ; when (ehcDebugStopAtCoreError opts)
              (cpSetLimitErrsWhen 5 "Parse Core (of previous compile) of module" errs)
       ; return ()
       }

{-# LINE 203 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpDecodeHIInfo :: HsName -> EHCompilePhase ()
cpDecodeHIInfo modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              -- if outputdir is specified, use that location to possibly read hi from.
              fpH     = mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi"
       ; cpMsg' modNm VerboseALot "Decoding" Nothing fpH
       ; hiinfo <- lift $
           catch (do { i <- getSGetFile (fpathToStr fpH) (HI.sgetHIInfo opts)
                            -- getSerializeFile (fpathToStr fpH)
                            -- Bin.getBinaryFPath fpH
                     ; return i
                     })
                 (\_ -> return $ HI.emptyHIInfo {HI.hiiValidity = HI.HIValidity_Absent})
       ; when (ehcOptVerbosity opts > VerboseALot)
              (do { lift $ putPPLn (pp hiinfo)
                  })
       ; let canCompile = crModCanCompile modNm cr
       ; case HI.hiiValidity hiinfo of
           HI.HIValidity_WrongMagic | not canCompile
             -> cpSetLimitErrsWhen 1 "Read HI"
                  [rngLift emptyRange Err_WrongMagic
                     (show modNm)
                     (fpathToStr fpH)
                  ]
           HI.HIValidity_Inconsistent | not canCompile
             -> cpSetLimitErrsWhen 1 "Read HI (of previous compile) of module"
                  [rngLift emptyRange Err_InconsistentHI
                     (show modNm)
                     (fpathToStr fpH)
                     [Cfg.verTimestamp Cfg.version, Cfg.installVariant opts, show $ ehcOptTarget opts  , show $ ehcOptTargetFlavor opts  ]
                     [HI.hiiSrcTimeStamp hiinfo   , HI.hiiCompiler hiinfo  , show $ HI.hiiTarget hiinfo, show $ HI.hiiTargetFlavor hiinfo]
                  ]
           _ -> cpUpdCU modNm (ecuStorePrevHIInfo {- $ HI.hiiPostCheckValidity opts -} hiinfo)
       }

{-# LINE 249 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
-- | Decode from serialized file and store result in the compileunit for the module modNm
cpDecode :: Serialize x => String -> EcuUpdater x -> HsName -> EHCompilePhase ()
cpDecode suff store modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              fpC     = fpathSetSuff suff fp
       ; cpMsg' modNm VerboseALot "Decoding" Nothing fpC
       ; x <- lift $ getSerializeFile (fpathToStr fpC)
       ; cpUpdCU modNm (store x)
       }

{-# LINE 262 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpDecodeGrin :: HsName -> EHCompilePhase ()
cpDecodeGrin = cpDecode "grin" ecuStoreGrin

{-# LINE 267 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpDecodeCore :: HsName -> EHCompilePhase ()
cpDecodeCore = cpDecode "core" ecuStoreCore

{-# LINE 276 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpGetPrevHI :: HsName -> EHCompilePhase ()
cpGetPrevHI modNm
  = do { cr <- get
       ; let  ecu        = crCU modNm cr
       -- ; when (isJust (ecuMbHITime ecu))
       --        (cpParseHI modNm)
       ; when (isJust (ecuMbHIInfoTime ecu))
              (cpDecodeHIInfo modNm)
       }

{-# LINE 288 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpGetPrevCore :: HsName -> EHCompilePhase ()
cpGetPrevCore modNm
  = do { cr <- get
       ; let  ecu    = crCU modNm cr
       ; when (isJust (ecuMbCoreTime ecu) && isNothing (ecuMbCore ecu))
              (cpDecodeCore modNm)
              -- (cpParseCore modNm)
       }

{-# LINE 299 "src/ehc/EHC/CompilePhase/Parsers.chs" #-}
cpGetPrevGrin :: HsName -> EHCompilePhase ()
cpGetPrevGrin modNm
  = do { cr <- get
       ; let  ecu    = crCU modNm cr
       ; when (isJust (ecuMbGrinTime ecu) && isNothing (ecuMbGrin ecu))
              (cpDecodeGrin modNm) -- (cpParseGrin modNm)
       }

