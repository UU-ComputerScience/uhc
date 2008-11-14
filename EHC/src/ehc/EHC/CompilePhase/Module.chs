%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile XXX
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Module analysis

%%[20 module {%{EH}EHC.CompilePhase.Module}
%%]

-- general imports
%%[20 import(qualified Data.Map as Map)
%%]
%%[20 import(qualified EH.Util.Rel as Rel)
%%]

%%[20 import({%{EH}EHC.Common})
%%]
%%[20 import({%{EH}EHC.CompileUnit})
%%]
%%[20 import({%{EH}EHC.CompileRun})
%%]

%%[20 import({%{EH}Module})
%%]
%%[20 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(cpCheckMods')
cpCheckMods' :: [Mod] -> EHCompilePhase ()
cpCheckMods' modL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             (mm,e) = modMpCombine modL (crsiModMp crsi)
%%[[20
       ; when (ehcOptVerbosity (crsiOpts crsi) >= VerboseDebug)
              (lift $ putWidthPPLn 120 (pp (head modL) >-< ppModMp mm)) -- debug
%%][99
%%]]
       ; put (cr {crStateInfo = crsi {crsiModMp = mm}})
       ; cpSetLimitErrsWhen 5 "Module analysis" e
       }
%%]

%%[20 export(cpCheckMods)
cpCheckMods :: [HsName] -> EHCompilePhase ()
cpCheckMods modNmL
  = do { cr <- get
       ; let modL   = [ addBuiltin $ ecuMod $ crCU n cr | n <- modNmL ]
       ; cpCheckMods' modL
       }
  where addBuiltin m = m { modImpL = modImpBuiltin : modImpL m }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Get info for module analysis
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(cpGetHsImports,cpGetHsMod,cpGetMetaInfo)
cpGetHsImports :: HsName -> EHCompilePhase ()
cpGetHsImports modNm
  =  do  {  cr <- get
         ;  let  ecu        = crCU modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsImports" mbHsSemMod
         ;  when (isJust mbHsSemMod)
                 (cpUpdCU modNm
                  $ ecuStoreImpL
                  $ HSSemMod.modImpNmL_Syn_AGItf hsSemMod
                 )
         -- ; lift $ putWidthPPLn 120 (pp mod)
         }

cpGetHsMod :: HsName -> EHCompilePhase ()
cpGetHsMod modNm
  =  do  {  cr <- get
         ;  let  ecu        = crCU modNm cr
                 mbHsSemMod = ecuMbHSSemMod ecu
                 hsSemMod   = panicJust "cpGetHsMod" mbHsSemMod
                 mod        = HSSemMod.mod_Syn_AGItf hsSemMod
         ;  when (isJust mbHsSemMod)
                 (cpUpdCU modNm (ecuStoreMod mod))
         -- ; lift $ putWidthPPLn 120 (pp mod)
         }

cpGetMetaInfo :: HsName -> EHCompilePhase ()
cpGetMetaInfo modNm
  =  do  {  cr <- get
         ;  let  ecu    = crCU modNm cr
         ;  tm ecuStoreHSTime        (                      ecuFilePath ecu)
         ;  tm ecuStoreHITime        (fpathSetSuff "hi"   $ ecuFilePath ecu)
%%[[(20 codegen)
         ;  tm ecuStoreCoreTime      (fpathSetSuff "core" $ ecuFilePath ecu)
%%]]
%%[[101
         ;  wr ecuStoreDirIsWritable (                      ecuFilePath ecu)
%%]]
         }
  where tm store fp
          = do { let n = fpathToStr fp
               ; nExists <- lift $ doesFileExist n
               ; when nExists
                      (do { t <- lift $ getModificationTime n
                          ; cpUpdCU modNm $ store t
                          })
               }
%%[[101
        wr store fp
          = do { pm <- lift $ getPermissions (maybe "." id $ fpathMbDir fp)
               -- ; lift $ putStrLn (fpathToStr fp ++ " writ " ++ show (writable pm))
               ; cpUpdCU modNm $ store (writable pm)
               }
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Create dummy module info for .eh's
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[20 export(cpGetDummyCheckEhMod)
cpGetDummyCheckEhMod :: HsName -> EHCompilePhase ()
cpGetDummyCheckEhMod modNm
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             mm     = crsiModMp crsi
             mod    = Mod modNm Nothing Nothing [] Rel.empty Rel.empty []
       ; cpUpdCU modNm (ecuStoreMod mod)
       ; put (cr {crStateInfo = crsi {crsiModMp = Map.insert modNm emptyModMpInfo mm}})
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update module offset info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen grin) export(cpUpdateModOffMp)
cpUpdateModOffMp :: [HsName] -> EHCompilePhase ()
cpUpdateModOffMp modNmL
  = do { cr <- get
       ; let crsi   = crStateInfo cr
             offMp  = crsiModOffMp crsi
             offMp' = Map.fromList [ (m,(o,crsiExpNmOffMp m crsi)) | (m,o) <- zip modNmL [Map.size offMp ..] ] `Map.union` offMp
       ; put (cr {crStateInfo = crsi {crsiModOffMp = offMp'}})
       }
%%]

