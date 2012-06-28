module EH101.EHC.CompilePhase.Semantics
( cpFoldCore
, cpFoldEH
, cpFoldHs
, cpFoldHsMod
, cpFoldHIInfo )
where
import qualified Data.Map as Map
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.EH.MainAG as EHSem
import qualified EH101.HS.MainAG as HSSem
import qualified EH101.Core as Core
import qualified EH101.Core.ToGrin as Core2GrSem
import qualified Data.Set as Set
import qualified EH101.HI as HI
import qualified EH.Util.Rel as Rel
import EH101.Module
import qualified EH101.HS.ModImpExp as HSSemMod
import EH101.Base.Debug
import EH.Util.Pretty






{-# LINE 57 "src/ehc/EHC/CompilePhase/Semantics.chs" #-}
cpFoldCore :: HsName -> EHCompilePhase ()
cpFoldCore modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbCore   = ecuMbCore ecu
                 core     = panicJust "cpFoldCore" mbCore
                 coreInh  = crsiCoreInh crsi
                 coreSem  = Core2GrSem.wrap_CodeAGItf
                              (Core2GrSem.sem_CodeAGItf (Core.CodeAGItf_AGItf core))
                              (coreInh { Core2GrSem.gUniq_Inh_CodeAGItf            = crsiHereUID crsi
                                       , Core2GrSem.opts_Inh_CodeAGItf             = opts
                                       })
         ;  when (isJust mbCore)
                 (cpUpdCU modNm ( ecuStoreCoreSem coreSem
                                ))
         }

{-# LINE 76 "src/ehc/EHC/CompilePhase/Semantics.chs" #-}
cpFoldEH :: HsName -> EHCompilePhase ()
cpFoldEH modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbEH   = ecuMbEH ecu
                 ehSem  = EHSem.wrap_AGItf (EHSem.sem_AGItf $ panicJust "cpFoldEH" mbEH)
                                           ((crsiEHInh crsi)
                                                  { EHSem.moduleNm_Inh_AGItf         = ecuModNm ecu
                                                  , EHSem.gUniq_Inh_AGItf            = crsiHereUID crsi
                                                  , EHSem.opts_Inh_AGItf             = opts
                                                  , EHSem.isMainMod_Inh_AGItf        = ecuIsMainMod ecu
                                                  })
         ;  when (isJust mbEH)
                 (cpUpdCU modNm $! ecuStoreEHSem $! ehSem)
         }

{-# LINE 96 "src/ehc/EHC/CompilePhase/Semantics.chs" #-}
cpFoldHs :: HsName -> EHCompilePhase ()
cpFoldHs modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHS   = ecuMbHS ecu
                 inh    = crsiHSInh crsi
                 hsSem  = HSSem.wrap_AGItf (HSSem.sem_AGItf $ panicJust "cpFoldHs" mbHS)
                                           (inh { HSSem.opts_Inh_AGItf             = opts
                                                , HSSem.gUniq_Inh_AGItf            = crsiHereUID crsi
                                                , HSSem.moduleNm_Inh_AGItf         = modNm
                                                , HSSem.isTopMod_Inh_AGItf         = ecuIsTopMod ecu
                                                , HSSem.modInScope_Inh_AGItf       = inscps
                                                , HSSem.modEntToOrig_Inh_AGItf     = exps
                                                , HSSem.topInstanceNmL_Inh_AGItf   = modInstNmL (ecuMod ecu)
                                                })
                        where mmi    = panicJust "cpFoldHs.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                              inscps = Rel.toDomMap $ mmiInscps $ mmi
                              exps   = Rel.toRngMap $ Rel.restrictRng (\o -> let mq = hsnQualifier (ioccNm o) in isJust mq && fromJust mq /= modNm)
                                                    $ Rel.mapRng mentIdOcc $ mmiExps mmi
                 hasMain= HSSem.mainValExists_Syn_AGItf hsSem
         ;  when (isJust mbHS)
                 (do { cpUpdCU modNm ( ecuStoreHSSem hsSem
                                     . ecuStoreHIDeclImpS (ecuHSDeclImpNmS ecu)
                                     -- . ecuSetHasMain hasMain
                                     )
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (lift $ putStrLn (show modNm ++ " hasMain=" ++ show hasMain))
                     ; when hasMain (crSetAndCheckMain modNm)
                     })
         }

{-# LINE 139 "src/ehc/EHC/CompilePhase/Semantics.chs" #-}
cpFoldHsMod :: HsName -> EHCompilePhase ()
cpFoldHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHS       = ecuMbHS ecu
                 inh        = crsiHSModInh crsi
                 hsSemMod   = HSSemMod.wrap_AGItf (HSSemMod.sem_AGItf $ panicJust "cpFoldHsMod" mbHS)
                                                  (inh { HSSemMod.gUniq_Inh_AGItf        = crsiHereUID crsi
                                                       , HSSemMod.moduleNm_Inh_AGItf     = modNm
                                                       })
                 hasMain= HSSemMod.mainValExists_Syn_AGItf hsSemMod
                 pragmas = HSSemMod.fileHeaderPragmas_Syn_AGItf hsSemMod
                 (ecuOpts,modifiedOpts)
                         = ehcOptUpdateWithPragmas pragmas opts
         ;  when (isJust mbHS)
                 (cpUpdCU modNm ( ecuStoreHSSemMod hsSemMod
                                . ecuSetHasMain hasMain
                                . ecuStorePragmas pragmas
                                . (if modifiedOpts then ecuStoreOpts ecuOpts else id)
                 )              )
         }

{-# LINE 167 "src/ehc/EHC/CompilePhase/Semantics.chs" #-}
cpFoldHIInfo :: HsName -> EHCompilePhase ()
cpFoldHIInfo modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 mbHIInfo   = ecuMbPrevHIInfo ecu
                 hiInfo     = panicJust "cpFoldHIInfo" mbHIInfo
                 hasMain    = HI.hiiHasMain hiInfo
         ;  when (isJust mbHIInfo && HI.hiiValidity hiInfo == HI.HIValidity_Ok)
                 (do { let mm     = crsiModMp crsi
                           mmi    = Map.findWithDefault emptyModMpInfo modNm mm
                           mmi'   = mkModMpInfo modNm
                                                (mmiInscps mmi)
                                                ({- (\v -> tr "cpFoldHIInfo.hiiExps" (pp v) v) $ -} HI.hiiExps hiInfo)
                                                (HI.hiiHiddenExps hiInfo)
                     ; when hasMain (crSetAndCheckMain modNm)
                     ; cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm mmi' mm})
                     ; cpUpdCU modNm ( ecuStorePrevHIInfo hiInfo
                                     . ecuStoreHIDeclImpS (HI.hiiHIDeclImpModS hiInfo)
                                     . ecuStoreHIUsedImpS (HI.hiiHIUsedImpModS hiInfo)
                                     . ecuSetHasMain hasMain
                                     )
                     ; when (ehcOptVerbosity opts >= VerboseDebug)
                            (lift $ putStrLn
                               (show modNm
                                ++ ": hi imps, decl=" ++ show (HI.hiiHIDeclImpModS hiInfo)
                                ++ ", used=" ++ show (HI.hiiHIUsedImpModS hiInfo)
                            )  )
                     })
         }

