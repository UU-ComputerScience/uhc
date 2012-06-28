module EH101.EHC.CompilePhase.FlowBetweenPhase
( cpFlowEHSem1
, cpFlowHsSem1
, cpFlowHISem
, cpFlowCoreSem
, cpFlowHILamMp
, cpFlowOptim )
where
import qualified Data.Map as Map
import qualified Data.Set as Set
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.EH.MainAG as EHSem
import qualified EH101.HS.MainAG as HSSem
import qualified EH101.Core.ToGrin as Core2GrSem
import EH101.LamInfo
import EH101.Module
import EH101.Core.UsedModNms
import qualified EH101.HI as HI
import EH101.CHR.Solve (chrStoreUnion)
import EH101.Base.Debug
import EH.Util.Pretty
import qualified EH.Util.FastSeq as Seq
import EH101.EHC.CompilePhase.Module(cpUpdHiddenExports)











{-# LINE 68 "src/ehc/EHC/CompilePhase/FlowBetweenPhase.chs" #-}
prepFlow :: a -> a
prepFlow x | x `seq` True = x
-- prepFlow = id

gamUnionFlow :: Ord k => Gam k v -> Gam k v -> Gam k v
gamUnionFlow = gamUnion

{-# LINE 89 "src/ehc/EHC/CompilePhase/FlowBetweenPhase.chs" #-}
cpFlowHsSem1 :: HsName -> EHCompilePhase ()
cpFlowHsSem1 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 hsSem  = panicJust "cpFlowHsSem1" $ ecuMbHSSem ecu
                 ehInh  = crsiEHInh crsi
                 hsInh  = crsiHSInh crsi
                 hii    = ecuHIInfo ecu
                 ig     = prepFlow $! HSSem.gathIdGam_Syn_AGItf hsSem
                 fg     = prepFlow $! HSSem.gathFixityGam_Syn_AGItf hsSem
                 hsInh' = hsInh
                            { HSSem.idGam_Inh_AGItf      = ig `gamUnionFlow` HSSem.idGam_Inh_AGItf     hsInh
                            , HSSem.fixityGam_Inh_AGItf  = fg `gamUnionFlow` HSSem.fixityGam_Inh_AGItf hsInh
                            }
                 ehInh' = ehInh
                            { EHSem.idQualGam_Inh_AGItf  = idGam2QualGam ig `gamUnionFlow` EHSem.idQualGam_Inh_AGItf ehInh
                            }
                 hii'   = hii
                            { HI.hiiFixityGam            = fg
                            -- , HI.hiiIdDefHIIdGam         = HI.hiiIdDefOccGamToHIIdGam ig
                            , HI.hiiHIDeclImpModS        = ecuHIDeclImpNmS ecu
                            }
                 opts'  = opts
                            { ehcOptBuiltinNames = mkEHBuiltinNames mk
                            }
                        where mk = if ehcOptUseAssumePrelude opts
                                   then \_ n -> n
                                   else \k n -> idQualGamReplacement (EHSem.idQualGam_Inh_AGItf ehInh') k (hsnQualified n)
         ;  when (isJust (ecuMbHSSem ecu))
                 (do { cpUpdSI (\crsi -> crsi {crsiHSInh = hsInh', crsiEHInh = ehInh', crsiOpts = opts'})
                     ; cpUpdCU modNm $! ecuStoreHIInfo hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowHsSem1")
                     })
         -- ;  lift $ putWidthPPLn 120 (ppGam $ EHSem.idQualGam_Inh_AGItf $ ehInh')
         }


{-# LINE 132 "src/ehc/EHC/CompilePhase/FlowBetweenPhase.chs" #-}
cpFlowEHSem1 :: HsName -> EHCompilePhase ()
cpFlowEHSem1 modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 ehSem    = panicJust "cpFlowEHSem1.ehSem" $ ecuMbEHSem ecu
                 ehInh    = crsiEHInh crsi
                 coreInh  = crsiCoreInh crsi
                 dg       = prepFlow $! EHSem.gathDataGam_Syn_AGItf    ehSem
                 vg       = prepFlow $! EHSem.gathValGam_Syn_AGItf     ehSem
                 tg       = prepFlow $! EHSem.gathTyGam_Syn_AGItf      ehSem
                 tkg      = prepFlow $! EHSem.gathTyKiGam_Syn_AGItf    ehSem
                 pg       = prepFlow $! EHSem.gathPolGam_Syn_AGItf     ehSem
                 kg       = prepFlow $! EHSem.gathKiGam_Syn_AGItf      ehSem
                 clg      = prepFlow $! EHSem.gathClGam_Syn_AGItf      ehSem
                 dfg      = prepFlow $! EHSem.gathClDfGam_Syn_AGItf    ehSem
                 cs       = prepFlow $! EHSem.gathChrStore_Syn_AGItf   ehSem
                 lm       = prepFlow $! EHSem.gathLamMp_Syn_AGItf      ehSem
                 mmi      = panicJust "cpFlowEHSem1.crsiModMp" $ Map.lookup modNm $ crsiModMp crsi
                 hii      = ecuHIInfo ecu
                 mentrelFilterMp
                          = mentrelFilterMpUnions [ EHSem.gathMentrelFilterMp_Syn_AGItf ehSem, mentrelToFilterMp' False [modNm] (mmiExps mmi) ]
                 usedImpS = mentrelFilterMpModuleNames mentrelFilterMp
                 ehInh'   = ehInh
                              { EHSem.dataGam_Inh_AGItf    = dg  `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                              , EHSem.valGam_Inh_AGItf     = vg  `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                              , EHSem.tyGam_Inh_AGItf      = tg  `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                              , EHSem.tyKiGam_Inh_AGItf    = tkg `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                              , EHSem.polGam_Inh_AGItf     = pg  `gamUnionFlow`  EHSem.polGam_Inh_AGItf     ehInh
                              , EHSem.kiGam_Inh_AGItf      = kg  `gamUnionFlow`  EHSem.kiGam_Inh_AGItf      ehInh
                              , EHSem.clGam_Inh_AGItf      = clg `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                              , EHSem.clDfGam_Inh_AGItf    = dfg `gamUnionFlow`  EHSem.clDfGam_Inh_AGItf    ehInh
                              , EHSem.chrStore_Inh_AGItf   = cs  `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                              }
                 hii'     = hii
                              { -- 20100717 AD: redundant because later extracted from Core because of inlining etc, TBD
                                HI.hiiHIUsedImpModS = usedImpS
                              , HI.hiiMbOrphan      = EHSem.mbOrphan_Syn_AGItf ehSem
                              , HI.hiiValGam        = vg
                              , HI.hiiTyGam     	= tg
                              , HI.hiiTyKiGam     	= tkg
                              , HI.hiiPolGam     	= pg
                              , HI.hiiDataGam       = dg
                              , HI.hiiClGam         = clg
                              , HI.hiiClDfGam       = dfg
                              , HI.hiiCHRStore      = {- HI.hiiScopedPredStoreToList -} cs
                              -- , HI.hiiLamMp         = lm
                              }
                 coreInh' = coreInh
                              { Core2GrSem.dataGam_Inh_CodeAGItf = EHSem.dataGam_Inh_AGItf     ehInh'
                              , Core2GrSem.lamMp_Inh_CodeAGItf   = lm `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh		-- assumption: no duplicates, otherwise merging as done later has to be done
                              }
         ;  when (isJust (ecuMbEHSem ecu))
                 (do { cpUpdSI
                               (\crsi -> crsi
                                   { crsiCoreInh = coreInh', crsiEHInh = ehInh' }
                               )
                     ; cpUpdCU modNm ( ecuStoreHIInfo hii'
                                     . ecuStoreHIUsedImpS usedImpS
                                     . ecuStoreUsedNames mentrelFilterMp
                                     )
                     -- put back additional hidden exports
                     ; cpUpdHiddenExports modNm $ Seq.toList $ EHSem.gathHiddenExports_Syn_AGItf ehSem
                     })
         }

{-# LINE 230 "src/ehc/EHC/CompilePhase/FlowBetweenPhase.chs" #-}
cpFlowHISem :: HsName -> EHCompilePhase ()
cpFlowHISem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 -- hiSem  = panicJust "cpFlowHISem.hiSem" $ ecuMbPrevHISem ecu
                 hiInfo = panicJust "cpFlowHISem.hiInfo" $ ecuMbPrevHIInfo ecu
                 ehInh  = crsiEHInh crsi
                 ehInh' = ehInh
                            { EHSem.valGam_Inh_AGItf     = (HI.hiiValGam     hiInfo) `gamUnionFlow`  EHSem.valGam_Inh_AGItf     ehInh
                            , EHSem.tyGam_Inh_AGItf      = (HI.hiiTyGam      hiInfo) `gamUnionFlow`  EHSem.tyGam_Inh_AGItf      ehInh
                            , EHSem.tyKiGam_Inh_AGItf    = (HI.hiiTyKiGam    hiInfo) `gamUnionFlow`  EHSem.tyKiGam_Inh_AGItf    ehInh
                            , EHSem.polGam_Inh_AGItf     = (HI.hiiPolGam     hiInfo) `gamUnionFlow`  EHSem.polGam_Inh_AGItf     ehInh
                            , EHSem.dataGam_Inh_AGItf    = (HI.hiiDataGam    hiInfo) `gamUnionFlow`  EHSem.dataGam_Inh_AGItf    ehInh
                            , EHSem.clGam_Inh_AGItf      = (HI.hiiClGam      hiInfo) `gamUnionFlow`  EHSem.clGam_Inh_AGItf      ehInh
                            , EHSem.clDfGam_Inh_AGItf    = (HI.hiiClDfGam    hiInfo) `gamUnionFlow`  EHSem.clDfGam_Inh_AGItf    ehInh
                            , EHSem.chrStore_Inh_AGItf   = (HI.hiiCHRStore   hiInfo) `chrStoreUnion` EHSem.chrStore_Inh_AGItf   ehInh
                            }
                 hsInh  = crsiHSInh crsi
                 hsInh' = hsInh
                            { HSSem.fixityGam_Inh_AGItf  = (HI.hiiFixityGam    hiInfo) `gamUnionFlow` HSSem.fixityGam_Inh_AGItf hsInh
                            , HSSem.idGam_Inh_AGItf      = (HI.hiiIdDefOccGam  hiInfo) `gamUnionFlow` HSSem.idGam_Inh_AGItf     hsInh
                            }
                 coreInh  = crsiCoreInh crsi
                 coreInh' = coreInh
                              { Core2GrSem.lamMp_Inh_CodeAGItf   = (HI.hiiLamMp hiInfo) `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh
                              }
                 optim    = crsiOptim crsi
                 optim'   = optim
                              { optimGrInlMp   = (HI.hiiGrInlMp hiInfo) `Map.union` optimGrInlMp optim
                              }
         ;  when (isJust (ecuMbPrevHIInfo ecu))
                 (do { cpUpdSI (\crsi -> crsi { crsiEHInh = ehInh'
                                              , crsiHSInh = {- tr "cpFlowHISem.crsiHSInh" (pp $ HSSem.idGam_Inh_AGItf hsInh') $ -} hsInh'
                                              , crsiCoreInh = coreInh'
                                              , crsiOptim = optim'
                                              })
                     })
         }

{-# LINE 281 "src/ehc/EHC/CompilePhase/FlowBetweenPhase.chs" #-}
cpFlowCoreSem :: HsName -> EHCompilePhase ()
cpFlowCoreSem modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
                 coreSem  = panicJust "cpFlowCoreSem.coreSem" $ ecuMbCoreSem ecu
                 core     = panicJust "cpFlowCoreSem.core"    $ ecuMbCore    ecu

                 -- 20100717 AD: required here because of inlining etc, TBD
                 usedImpS = cmodUsedModNms core

                 coreInh  = crsiCoreInh crsi
                 hii      = ecuHIInfo ecu
                 am       = prepFlow $! Core2GrSem.gathLamMp_Syn_CodeAGItf coreSem
                 coreInh' = coreInh
                              { Core2GrSem.lamMp_Inh_CodeAGItf   = am `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh	-- assumption: old info can be overridden, otherwise merge should be done here
                              }
                 hii'     = hii
                              { -- 20100717 AD: required here because of inlining etc, TBD
                                {- -} HI.hiiHIUsedImpModS = usedImpS
                              , {- -} HI.hiiLamMp         = am
                              }
         ;  when (isJust (ecuMbCoreSem ecu))
                 (do { cpUpdSI (\crsi -> crsi {crsiCoreInh = coreInh'})
                     ; cpUpdCU modNm ( ecuStoreHIInfo hii'
                                     --
                                     -- 20100717 AD: required here because of inlining etc, TBD
                                     . ecuStoreHIUsedImpS usedImpS
                                     )
                     })
         }

{-# LINE 316 "src/ehc/EHC/CompilePhase/FlowBetweenPhase.chs" #-}
cpFlowHILamMp :: HsName -> EHCompilePhase ()
cpFlowHILamMp modNm
  = do { cr <- get
       ; let  (ecu,crsi,opts,_) = crBaseInfo modNm cr
              coreInh  = crsiCoreInh crsi
              hii      = ecuHIInfo ecu

         -- put back result: call info map (lambda arity, ...), overwriting previous entries
       ; cpUpdSI (\crsi -> crsi {crsiCoreInh = coreInh {Core2GrSem.lamMp_Inh_CodeAGItf = HI.hiiLamMp hii `lamMpUnionBindAspMp` Core2GrSem.lamMp_Inh_CodeAGItf coreInh}})
       }

{-# LINE 329 "src/ehc/EHC/CompilePhase/FlowBetweenPhase.chs" #-}
cpFlowOptim :: HsName -> EHCompilePhase ()
cpFlowOptim modNm
  =  do  {  cr <- get
         ;  let  (ecu,crsi,_,_) = crBaseInfo modNm cr
                 optim  = crsiOptim crsi
                 moptim = panicJust "cpFlowOptim" $ ecuMbOptim ecu
                 hii    = ecuHIInfo ecu
                 optim' = optim
                 hii'   = hii
         ;  when (isJust (ecuMbOptim ecu))
                 (do { cpUpdSI (\crsi -> crsi {crsiOptim = optim'})
                     ; cpUpdCU modNm $! ecuStoreHIInfo $! prepFlow hii'
                     -- ; lift $ putStrLn (forceEval hii' `seq` "cpFlowOptim")
                     })
         }

