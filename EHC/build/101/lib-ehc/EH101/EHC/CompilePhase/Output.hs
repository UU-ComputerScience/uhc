module EH101.EHC.CompilePhase.Output
( cpOutputCoreModule, cpOutputCore
, cpOutputGrin
, cpOutputByteCodeC
, cpOutputHI )
where
import qualified EH.Util.FastSeq as Seq
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.Config as Cfg
import qualified EH101.Core.ToGrin as Core2GrSem
import EH101.Core
import EH101.Core.Pretty
import EH101.GrinCode.Pretty
import qualified EH101.HI as HI
import qualified EH101.Base.Binary as Bin
import EH101.Base.Serialize
import EH101.Gam
import EH101.Module
import qualified EH101.EH.MainAG as EHSem
import EH101.Gam.Utils








{-# LINE 107 "src/ehc/EHC/CompilePhase/Output.chs" #-}
cpOutputCoreModule :: Bool -> String -> String -> HsName -> CModule -> EHCompilePhase ()
cpOutputCoreModule binary nmsuff suff modNm cMod
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 fpC     = mkOutputFPath opts modNm fp (suff ++ nmsuff) -- for now nmsuff after suff, but should be inside name
                 fnC     = fpathToStr fpC
                 coreInh = crsiCoreInh crsi
                 lm      = Core2GrSem.lamMp_Inh_CodeAGItf coreInh
         ;  lift (if binary
                  then do { fpathEnsureExists fpC		-- should be in FPath equivalent of putSerializeFile
                          ; putSerializeFile fnC cMod
                          }
                  else putPPFPath fpC (ppCModule opts lm cMod) 100
                 )
         }

cpOutputCore :: Bool -> String -> String -> HsName -> EHCompilePhase ()
cpOutputCore binary nmsuff suff modNm
  =  do  {  cr <- get
         -- part 1: current .core
         ;  let  (ecu,_,_,_) = crBaseInfo modNm cr
                 mbCore = ecuMbCore ecu
                 cMod   = panicJust "cpOutputCore" mbCore
         ;  cpMsg modNm VerboseALot "Emit Core"
         ;  cpOutputCoreModule binary nmsuff suff modNm cMod
         }

{-# LINE 155 "src/ehc/EHC/CompilePhase/Output.chs" #-}
cpOutputGrin :: Bool -> String -> HsName -> EHCompilePhase ()
cpOutputGrin binary suff modNm
  =  do  { cr <- get
         ; let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                mbGrin = ecuMbGrin ecu
                grin   = panicJust "cpOutputGrin" mbGrin
                mkb x  = x ++ suff
                fpG    = mkOutputFPath opts (mkHNm $ mkb $ show modNm) (fpathUpdBase mkb fp) "grin"
                fnG    = fpathToStr fpG
         ; cpMsg modNm VerboseALot "Emit Grin"
         ; lift (if binary
                 then do { fpathEnsureExists fpG
                         ; putSerializeFile fnG grin
                         }
                 else putPPFPath fpG (ppGrModule grin) 1000 --TODO ? getal
                )
         }


{-# LINE 180 "src/ehc/EHC/CompilePhase/Output.chs" #-}
cpOutputByteCodeC :: String -> HsName -> EHCompilePhase ()
cpOutputByteCodeC suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,fp) = crBaseInfo modNm cr
                 bc       = panicJust "cpOutputByteCodeC bytecode" $ ecuMbBytecodeSem ecu
                 fpC      = mkOutputFPath opts modNm fp suff
         ;  cpMsg' modNm VerboseALot "Emit ByteCode C" Nothing fpC     -- '
         ;  cpRegisterFilesToRm [fpC]
         ;  lift $ putPPFPath fpC bc 150
         }

{-# LINE 206 "src/ehc/EHC/CompilePhase/Output.chs" #-}
cpOutputHI :: String -> HsName -> EHCompilePhase ()
cpOutputHI suff modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,fp) = crBaseInfo modNm cr
                 mmi    = panicJust "cpOutputHI.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                 hii1   = ecuHIInfo ecu
                 impNmS = ecuImpNmS ecu
                 hii2   = hii1 { HI.hiiValidity             = HI.HIValidity_Ok
                               , HI.hiiModuleNm             = modNm
                               , HI.hiiExps                 = mmiExps       mmi
                               , HI.hiiHiddenExps           = mmiHiddenExps mmi
                               , HI.hiiHasMain              = ecuHasMain ecu
                               , HI.hiiTarget               = ehcOptTarget opts
                               , HI.hiiTargetFlavor         = ehcOptTargetFlavor opts
                               , HI.hiiSrcTimeStamp         = Cfg.verTimestamp Cfg.version
                               , HI.hiiSrcSig               = Cfg.verSig Cfg.version
                               , HI.hiiSrcVersionMajor      = Cfg.verMajor Cfg.version
                               , HI.hiiSrcVersionMinor      = Cfg.verMinor Cfg.version
                               , HI.hiiSrcVersionMinorMinor = Cfg.verMinorMinor Cfg.version
                               , HI.hiiSrcVersionSvn        = Cfg.verSvnRevision Cfg.version
                               , HI.hiiCompileFlags         = optsDiscrRecompileRepr opts
                               , HI.hiiCompiler             = Cfg.installVariant opts
                               , HI.hiiTransClosedUsedModMp = Map.unions $
                                                                Map.singleton modNm impNmS : [ ecuTransClosedUsedModMp $ crCU m cr | m <- Set.toList impNmS ]
                               , HI.hiiTransClosedOrphanModS= Set.unions $
                                                                [ Set.unions [if ecuIsOrphan me then Set.singleton m else Set.empty, ecuTransClosedOrphanModS me]
                                                                | m <- Set.toList impNmS
                                                                , let me = crCU m cr
                                                                ]
                               }
                 hii3   = hii2
                 fpH    = mkOutputFPath opts modNm fp suff
                 fnH    = fpathToStr fpH
         ;  cpMsg modNm VerboseALot "Emit HI"
         ;  hiExists <- lift $ doesFileExist fnH
         ;  when (hiExists)
                 (lift $ removeFile fnH)
         ;  when (ehcOptVerbosity opts > VerboseALot)
                 (do { lift $ putPPLn ("hii3: " >#< hii3)
                     ; lift $ putPPLn ("orph: " >#< vlist [ m >#< (fmap Set.toList $ HI.hiiMbOrphan $ ecuHIInfo me) | m <- Set.toList impNmS, let me = crCU m cr ])
                     ; lift $ putPPLn ("used nms: " >#< (pp $ show $ ecuUsedNames ecu))
                     })
         ;  lift $ do { fpathEnsureExists fpH
                      ; putSerializeFile fnH hii3
                      }
         ;  now <- lift $ getClockTime
         ;  cpUpdCU modNm ( ecuStoreHIInfoTime now
                          . ecuStoreHIInfo hii3
                          )
         }


