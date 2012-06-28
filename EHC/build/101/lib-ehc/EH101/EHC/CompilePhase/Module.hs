module EH101.EHC.CompilePhase.Module
( cpCheckMods'
, cpCheckMods
, GetMeta (..), allGetMeta
, cpGetHsImports, cpGetHsMod, cpGetMetaInfo
, cpGetDummyCheckEhMod
, cpUpdateModOffMp
, cpUpdHiddenExports )
where
import qualified Data.Map as Map
import qualified EH.Util.Rel as Rel
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import EH101.Module
import qualified EH101.HS.ModImpExp as HSSemMod

{-# LINE 33 "src/ehc/EHC/CompilePhase/Module.chs" #-}
cpCheckMods' :: [Mod] -> EHCompilePhase ()
cpCheckMods' modL@(Mod {modName = modNm} : _)
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             (mm,e) = modMpCombine modL (crsiModMp crsi)
       ; cpUpdSI (\crsi -> crsi {crsiModMp = mm})
       ; cpSetLimitErrsWhen 5 "Module analysis" e
       }

{-# LINE 51 "src/ehc/EHC/CompilePhase/Module.chs" #-}
cpCheckMods :: [HsName] -> EHCompilePhase ()
cpCheckMods modNmL
  = do { cr <- get
       ; let modL   = [ addBuiltin $ ecuMod $ crCU n cr | n <- modNmL ]
       ; cpCheckMods' modL
       }
  where addBuiltin m = m { modImpL = modImpBuiltin : modImpL m }

{-# LINE 65 "src/ehc/EHC/CompilePhase/Module.chs" #-}
data GetMeta
  = GetMeta_HS
  | GetMeta_HI
  | GetMeta_Core
  | GetMeta_Grin
  | GetMeta_Dir
  deriving (Eq,Ord)

allGetMeta = [GetMeta_HS, GetMeta_HI, GetMeta_Core, GetMeta_Grin, GetMeta_Dir]


{-# LINE 78 "src/ehc/EHC/CompilePhase/Module.chs" #-}
cpGetHsImports :: HsName -> EHCompilePhase HsName
cpGetHsImports modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsImports" mbHsSemMod
                 modNm'     = HSSemMod.realModuleNm_Syn_AGItf hsSemMod
                 upd        = ecuStoreHSDeclImpS (HSSemMod.modImpNmS_Syn_AGItf hsSemMod)
         ;  case mbHsSemMod of
              Just _ | ecuIsTopMod ecu -> cpUpdCUWithKey modNm (\_ ecu -> (modNm', upd $ cuUpdKey modNm' ecu))
                     | otherwise       -> do cpUpdCU modNm upd ; return modNm
              _      -> return modNm
         }

cpGetHsMod :: HsName -> EHCompilePhase ()
cpGetHsMod modNm
  =  do  {  cr <- get
         ;  let  (ecu,_,opts,_) = crBaseInfo modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsMod" mbHsSemMod
                 mod        = HSSemMod.mod_Syn_AGItf hsSemMod
         ;  when (ehcOptVerbosity opts >= VerboseDebug)
                 (do { cpMsg modNm VerboseDebug "cpGetHsMod"
                     ; lift $ putWidthPPLn 120 (pp mod)
                     })
         ;  when (isJust mbHsSemMod)
                 (cpUpdCU modNm (ecuStoreMod mod))
         }

cpGetMetaInfo :: [GetMeta] -> HsName -> EHCompilePhase ()
cpGetMetaInfo gm modNm
  =  do  {  cr <- get
         ;  let (ecu,_,opts,fp) = crBaseInfo modNm cr
         ;  when (GetMeta_HS `elem` gm)
                 (tm opts ecu ecuStoreHSTime        (ecuSrcFilePath ecu))
         {-
         ;  when (GetMeta_HI `elem` gm)
                 (tm opts ecu ecuStoreHITime
                                              (mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi")
                 )
         -}
         ;  when (GetMeta_HI `elem` gm)
                 (tm opts ecu ecuStoreHIInfoTime
                                              (mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp "hi")
                 )
         ;  when (GetMeta_Grin `elem` gm)
                 (tm opts ecu ecuStoreGrinTime      (fpathSetSuff "grin"      fp     ))
         ;  when (GetMeta_Core `elem` gm)
                 (tm opts ecu ecuStoreCoreTime      (fpathSetSuff "core"      fp     ))
         ;  when (GetMeta_Dir `elem` gm)
                 (wr opts ecu ecuStoreDirIsWritable (                         fp     ))
         }
  where tm opts ecu store fp
          = do { let n = fpathToStr fp
               ; nExists <- lift $ doesFileExist n
               ; when (ehcOptVerbosity opts >= VerboseDebug)
                      (do { lift $ putStrLn ("meta info of: " ++ show (ecuModNm ecu) ++ ", file: " ++ n ++ ", exists: " ++ show nExists)
                          })
               ; when nExists
                      (do { t <- lift $ getModificationTime n
                          ; when (ehcOptVerbosity opts >= VerboseDebug)
                                 (do { lift $ putStrLn ("time stamp of: " ++ show (ecuModNm ecu) ++ ", time: " ++ show t)
                                     })
                          ; cpUpdCU modNm $ store t
                          })
               }
        wr opts ecu store fp
          = do { pm <- lift $ getPermissions (maybe "." id $ fpathMbDir fp)
               -- ; lift $ putStrLn (fpathToStr fp ++ " writ " ++ show (writable pm))
               ; cpUpdCU modNm $ store (writable pm)
               }

{-# LINE 172 "src/ehc/EHC/CompilePhase/Module.chs" #-}
cpGetDummyCheckEhMod :: HsName -> EHCompilePhase ()
cpGetDummyCheckEhMod modNm
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             mm     = crsiModMp crsi
             mod    = Mod modNm Nothing Nothing [] Rel.empty Rel.empty []
       ; cpUpdCU modNm (ecuStoreMod mod)
       ; cpUpdSI (\crsi -> crsi {crsiModMp = Map.insert modNm emptyModMpInfo mm})
       }

{-# LINE 188 "src/ehc/EHC/CompilePhase/Module.chs" #-}
cpUpdateModOffMp :: [HsName] -> EHCompilePhase ()
cpUpdateModOffMp modNmL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             offMp  = crsiModOffMp crsi
             -- offMp' = Map.fromList [ (m,(o,crsiExpNmOffMp m crsi)) | (m,o) <- zip modNmL [Map.size offMp ..] ] `Map.union` offMp
             (offMp',_)
                    = foldr add (offMp,Map.size offMp) modNmL
                    where add modNm (offMp,offset)
                            = case Map.lookup modNm offMp of
                                Just (o,_) -> (Map.insert modNm (o     ,new) offMp, offset  )
                                _          -> (Map.insert modNm (offset,new) offMp, offset+1)
                            where new = crsiExpNmOffMp modNm crsi
       ; cpUpdSI (\crsi -> crsi {crsiModOffMp = offMp'})
       }

{-# LINE 210 "src/ehc/EHC/CompilePhase/Module.chs" #-}
cpUpdHiddenExports :: HsName -> [(HsName,IdOccKind)] -> EHCompilePhase ()
cpUpdHiddenExports modNm exps
  = when (not $ null exps)
         (do { cpUpdSI (\crsi -> crsi { crsiModMp = modMpAddHiddenExps modNm exps $ crsiModMp crsi
                                      })
             ; cpUpdateModOffMp [modNm]
             })


