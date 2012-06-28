module EH101.EHC.CompilePhase.Translations
( cpTranslateHs2EH
, cpTranslateEH2Output
, cpTranslateEH2Core
, cpTranslateCore2Grin
, cpTranslateCore2JavaScript
, cpTranslateGrin2Bytecode
, cpTranslateByteCode )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified EH.Util.FastSeq as Seq
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import EH101.Base.Target
import qualified EH101.EH.MainAG as EHSem
import qualified EH101.HS.MainAG as HSSem
import qualified EH101.Core.ToGrin as Core2GrSem
import EH101.GrinCode.ToGrinByteCode (grinMod2ByteCodeMod)
import EH101.GrinByteCode.ToC (gbmod2C)
import EH101.Core.ToJavaScript
import EH101.Base.Optimize
import qualified EH101.HI as HI
import EH101.LamInfo








{-# LINE 80 "src/ehc/EHC/CompilePhase/Translations.chs" #-}
cpTranslateHs2EH :: HsName -> EHCompilePhase ()
cpTranslateHs2EH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbHsSem= ecuMbHSSem ecu
                 hsSem  = panicJust "cpTranslateHs2EH" mbHsSem
                 eh     = HSSem.eh_Syn_AGItf hsSem
                 errs   = Seq.toList $ HSSem.errSq_Syn_AGItf hsSem
         ;  when (isJust mbHsSem)
                 (do { cpUpdCU modNm (ecuStoreEH eh)
                     ; cpSetLimitErrsWhen 5 "Dependency/name analysis" errs
                     ; when (ehcOptEmitHS opts)
                            (lift $ putPPFPath (mkOutputFPath opts modNm fp "hs2") (HSSem.pp_Syn_AGItf hsSem) 1000)
                     ; when (ehcOptShowHS opts)
                            (lift $ putWidthPPLn 120 (HSSem.pp_Syn_AGItf hsSem))
                     })
         }

{-# LINE 100 "src/ehc/EHC/CompilePhase/Translations.chs" #-}
cpTranslateEH2Output :: HsName -> EHCompilePhase ()
cpTranslateEH2Output modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Output" mbEHSem
                 about  = "EH analyses: Type checking"
                 errs   = Seq.toList $ EHSem.allErrSq_Syn_AGItf ehSem
         ;  when (isJust mbEHSem)
                 (do { cpSetLimitErrsWhen 5 about errs
                     ; when (ehcOptEmitEH opts)
                            (lift $ putPPFPath (mkOutputFPath opts modNm fp "eh2") (EHSem.pp_Syn_AGItf ehSem) 1000)
                     ; when (ehcOptShowEH opts)
                            (lift $ putWidthPPLn 120 (EHSem.pp_Syn_AGItf ehSem))
                     }
                 )
         }

{-# LINE 145 "src/ehc/EHC/CompilePhase/Translations.chs" #-}
cpTranslateEH2Core :: HsName -> EHCompilePhase ()
cpTranslateEH2Core modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbEHSem= ecuMbEHSem ecu
                 ehSem  = panicJust "cpTranslateEH2Core" mbEHSem
                 core   = EHSem.cmodule_Syn_AGItf  ehSem
         ;  when (isJust mbEHSem)
                 (cpUpdCU modNm ( ecuStoreCore core
                                ))
         }

{-# LINE 175 "src/ehc/EHC/CompilePhase/Translations.chs" #-}
cpTranslateCore2Grin :: HsName -> EHCompilePhase ()
cpTranslateCore2Grin modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mbCoreSem = ecuMbCoreSem ecu
                 coreSem   = panicJust "cpTranslateCore2Grin" mbCoreSem
                 grin      = Core2GrSem.grMod_Syn_CodeAGItf coreSem
         ;  when (isJust mbCoreSem && targetIsGrin (ehcOptTarget opts))
                 (cpUpdCU modNm $! ecuStoreGrin $! grin)
         }

{-# LINE 212 "src/ehc/EHC/CompilePhase/Translations.chs" #-}
cpTranslateCore2JavaScript :: HsName -> EHCompilePhase ()
cpTranslateCore2JavaScript modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
              mbCore    = ecuMbCore ecu
              coreInh  = crsiCoreInh crsi
       ; when (isJust mbCore && targetIsJavaScript (ehcOptTarget opts))
              (cpUpdCU modNm $ ecuStoreJavaScript $ cmod2JavaScriptModule opts (Core2GrSem.dataGam_Inh_CodeAGItf coreInh) $ fromJust mbCore)
       }

{-# LINE 224 "src/ehc/EHC/CompilePhase/Translations.chs" #-}
cpTranslateGrin2Bytecode :: HsName -> EHCompilePhase ()
cpTranslateGrin2Bytecode modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
        ; when (ehcOptVerbosity opts >= VerboseDebug)
               (lift $ putStrLn ("crsiModOffMp: " ++ show (crsiModOffMp crsi)))
        ; let  mbGrin = ecuMbGrin ecu
               grin   = panicJust "cpTranslateGrin2Bytecode1" mbGrin
               isWholeProg = ehcOptOptimizationScope opts >= OptimizationScope_WholeGrin
               expNmOffMp | ecuIsMainMod ecu = Map.empty
                          | otherwise        = crsiExpNmOffMp modNm crsi
               optim  = crsiOptim crsi
               impNmL   | isWholeProg = []
                        | otherwise   = ecuImpNmL ecu
               modOffMp | isWholeProg = Map.filterWithKey (\n _ -> n == modNm) $ crsiModOffMp crsi
                        | otherwise   = crsiModOffMp crsi
               (bc,errs)
                      = grinMod2ByteCodeMod opts
                          (Core2GrSem.lamMp_Inh_CodeAGItf $ crsiCoreInh crsi) -- (HI.hiiLamMp $ ecuHIInfo ecu)
                          (if ecuIsMainMod ecu then [ m | (m,_) <- sortOn snd $ Map.toList $ Map.map fst modOffMp ] else [])
                          -- (ecuImpNmL ecu)
                          (Map.fromList [ (n,(o,mp))
                                        | (o,n) <- zip [0..] impNmL
                                        , let (_,mp) = panicJust ("cpTranslateGrin2Bytecode2: " ++ show n) (Map.lookup n (crsiModOffMp crsi))
                                        ])
                          expNmOffMp
                          $ grin
        ; when (ehcOptVerbosity opts >= VerboseDebug)
               (lift $ putStrLn ("expNmOffMp: " ++ show expNmOffMp))

        ; cpMsg modNm VerboseDebug ("cpTranslateGrin2Bytecode: store bytecode")
        ; when (isJust mbGrin)
               (cpUpdCU modNm $! ecuStoreBytecode bc)
        ; cpMsg modNm VerboseDebug ("cpTranslateGrin2Bytecode: stored bytecode")
        ; when (ehcOptErrAboutBytecode opts)
               (cpSetLimitErrsWhen 5 "Grin to ByteCode" errs)
        }

{-# LINE 284 "src/ehc/EHC/CompilePhase/Translations.chs" #-}
cpTranslateByteCode :: HsName -> EHCompilePhase ()
cpTranslateByteCode modNm
  =  do { cr <- get
        ; let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
               mbBytecode = ecuMbBytecode ecu
               coreInh  = crsiCoreInh crsi
               ( grinbcPP
                 ,functionInfoExportMp)
                        = ( vlist ([ppMod] ++ (if ecuIsMainMod ecu then [ppMain] else []))
                          , functionInfoExportMp
                          )
                        where ( ppMod,ppMain
                               ,functionInfoExportMp)
                                = gbmod2C opts lkup $ panicJust "cpTranslateByteCode2" mbBytecode
                                where lkup n = do { li <- Map.lookup n (Core2GrSem.lamMp_Inh_CodeAGItf coreInh)
                                                  ; ex <- laminfoGrinByteCode li
                                                  ; return ex
                                                  }
          -- put back results: generated bytecode, new info about lambda's
        ; when (ehcOptEmitBytecode opts && isJust mbBytecode)
               (do { cpUpdCU modNm
                      ( ecuStoreBytecodeSem grinbcPP
                      . ( let hii = ecuHIInfo ecu
                          in  ecuStoreHIInfo
                                (hii { HI.hiiLamMp = lamMpMergeFrom laminfoGrinByteCode (\gbi i -> i {laminfoGrinByteCode=gbi}) const emptyLamInfo' functionInfoExportMp $ HI.hiiLamMp hii
                                     })
                        )
                      )
{-
                   ; when (ehcOptVerbosity opts >= VerboseDebug)
                          (lift $ do { putStrLn ("cpTranslateByteCode.lamMp: " ++ show (HI.hiiLamMp hii))
                                     ; putStrLn ("cpTranslateByteCode.functionInfoExportMp: " ++ show functionInfoExportMp)
                                     })
-}
                   })
        }

