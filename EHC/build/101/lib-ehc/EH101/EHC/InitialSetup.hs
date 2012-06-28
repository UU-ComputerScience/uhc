module EH101.EHC.InitialSetup
( initialHSSem
, initialEHSem
, initialCore2GrSem
, initialHSSemMod )
where
import qualified Data.Map as Map
import EH101.EHC.Common
import qualified EH101.HS.MainAG as HSSem
import qualified EH101.EH.MainAG as EHSem
import qualified EH101.Core.ToGrin as Core2GrSem
import qualified EH101.Pred as Pr (initClGam)
import qualified EH101.HS.ModImpExp as HSSemMod
import EH101.Pred.ToCHR (initScopedPredStore)






{-# LINE 49 "src/ehc/EHC/InitialSetup.chs" #-}
initialHSSem :: EHCOpts -> HSSem.Inh_AGItf
initialHSSem opts
  = HSSem.Inh_AGItf
      { HSSem.opts_Inh_AGItf            = opts
      , HSSem.idGam_Inh_AGItf           = HSSem.tyGam2IdDefOccGam initTyGam
                                            `gamUnion` HSSem.kiGam2IdDefOccGam initKiGam
                                            `gamUnion` HSSem.clGam2IdDefOccGam Pr.initClGam
      , HSSem.gUniq_Inh_AGItf           = uidStart
      , HSSem.isTopMod_Inh_AGItf        = False
      , HSSem.moduleNm_Inh_AGItf        = hsnUnknown
      , HSSem.modInScope_Inh_AGItf      = Map.empty
      , HSSem.modEntToOrig_Inh_AGItf    = Map.empty
      , HSSem.fixityGam_Inh_AGItf       = initFixityGam
      , HSSem.topInstanceNmL_Inh_AGItf  = []
      }

{-# LINE 71 "src/ehc/EHC/InitialSetup.chs" #-}
initialEHSem :: EHCOpts -> FPath -> EHSem.Inh_AGItf
initialEHSem opts fp
  = EHSem.Inh_AGItf
      { EHSem.moduleNm_Inh_AGItf        = mkHNm (fpathBase fp)
      , EHSem.gUniq_Inh_AGItf           = uidStart
      , EHSem.opts_Inh_AGItf            = opts
      , EHSem.isMainMod_Inh_AGItf       = False
      , EHSem.idQualGam_Inh_AGItf       = emptyGam
      , EHSem.valGam_Inh_AGItf          = emptyGam
      , EHSem.dataGam_Inh_AGItf         = emptyGam
      , EHSem.tyGam_Inh_AGItf           = initTyGam
      , EHSem.tyKiGam_Inh_AGItf         = initTyKiGam
      , EHSem.polGam_Inh_AGItf          = initPolGam
      , EHSem.kiGam_Inh_AGItf           = initKiGam
      , EHSem.clGam_Inh_AGItf           = Pr.initClGam
      , EHSem.clDfGam_Inh_AGItf         = emptyGam
      , EHSem.chrStore_Inh_AGItf        = initScopedPredStore
      }

{-# LINE 96 "src/ehc/EHC/InitialSetup.chs" #-}
initialCore2GrSem :: EHCOpts -> Core2GrSem.Inh_CodeAGItf
initialCore2GrSem opts
  = Core2GrSem.Inh_CodeAGItf
      { Core2GrSem.gUniq_Inh_CodeAGItf           = uidStart
      , Core2GrSem.dataGam_Inh_CodeAGItf         = emptyGam
      , Core2GrSem.opts_Inh_CodeAGItf            = opts
      , Core2GrSem.lamMp_Inh_CodeAGItf           = Map.empty
      }

{-# LINE 107 "src/ehc/EHC/InitialSetup.chs" #-}
initialHSSemMod :: EHCOpts -> HSSemMod.Inh_AGItf
initialHSSemMod opts
  = HSSemMod.Inh_AGItf
      { HSSemMod.gUniq_Inh_AGItf       = uidStart
      , HSSemMod.moduleNm_Inh_AGItf    = hsnUnknown
      , HSSemMod.opts_Inh_AGItf        = opts
      }

