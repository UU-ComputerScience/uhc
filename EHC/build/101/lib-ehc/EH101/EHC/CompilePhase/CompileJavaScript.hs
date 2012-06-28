module EH101.EHC.CompilePhase.CompileJavaScript
( cpJavaScript
, cpCompileJavaScript )
where
import System.Directory
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.Config as Cfg
import EH101.EHC.Environment
import EH101.Base.Target
import EH101.Core.ToJavaScript
import EH101.Base.Bits
import EH101.JavaScript.Pretty

{-# LINE 38 "src/ehc/EHC/CompilePhase/CompileJavaScript.chs" #-}
cpJavaScript :: String -> [String] -> EHCompilePhase ()
cpJavaScript archive files
  = do { cr <- get
       ; let (_,opts) = crBaseInfo' cr
             cmd = mkShellCmd $ [Cfg.shellCmdCat] ++ files ++ [">", archive]
       ; when (ehcOptVerbosity opts >= VerboseALot) (lift $ putStrLn cmd)
       ; cpSystem cmd
       }

{-# LINE 53 "src/ehc/EHC/CompilePhase/CompileJavaScript.chs" #-}
cpCompileJavaScript :: FinalCompileHow -> [HsName] -> HsName -> EHCompilePhase ()
cpCompileJavaScript how othModNmL modNm
  = do { cr <- get
       ; let  (ecu,_,opts,fp) = crBaseInfo modNm cr
              mbJs            = ecuMbJavaScript ecu
              fpO m f = mkPerModuleOutputFPath opts True m f Cfg.suffixJavaScriptLib
              fpM     = fpO modNm fp
              fpExec  = mkPerExecOutputFPath opts modNm fp (Just "js")
              fpHtml  = mkPerExecOutputFPath opts modNm fp (Just "html")
       ; when (isJust mbJs && targetIsJavaScript (ehcOptTarget opts))
              (do { cpMsg modNm VerboseALot "Emit JavaScript"
                  ; when (ehcOptVerbosity opts >= VerboseDebug)
                         (do { lift $ putStrLn $ "fpO   : " ++ fpathToStr fpM
                             ; lift $ putStrLn $ "fpExec: " ++ fpathToStr fpExec
                             })
                  ; let ppMod = vlist $ [p] ++ (if ecuIsMainMod ecu then [pmain] else [])
                              where (p,pmain) = ppJavaScriptModule (fromJust mbJs)
                  ; lift $ putPPFPath fpM ("//" >#< modNm >-< ppMod) 1000
                  ; case how of
                      FinalCompile_Exec
                        | ehcOptWholeProgOptimizationScope opts
                        -> do { cpJavaScript (fpathToStr fpExec) (rts ++ map fpathToStr [fpM])
                              ; mkHtml fpHtml [fpathToStr fpExec]
                              }
                        | otherwise
                        -> do { cpJavaScript (fpathToStr fpExec) (map fpathToStr [fpM])
                              ; mkHtml fpHtml $ rts ++ map fpathToStr ([ fpO m fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ] ++ [fpExec])
                              }
                        where rts = map (Cfg.mkInstalledRts opts Cfg.mkJavaScriptLibFilename Cfg.INST_LIB (Cfg.installVariant opts)) Cfg.libnamesRts
                              oth | ehcOptWholeProgOptimizationScope opts = []
                                  | otherwise                             = [ fpO m fp | m <- othModNmL, let (_,_,_,fp) = crBaseInfo m cr ]
                      _ -> return ()
                  }
              )
       }
  where mkHtml fpHtml jsL
          = lift $ putPPFPath fpHtml (ppHtml) 1000
          where scr x = "<script type=\"text/javascript\" src=\"" >|< x >|< "\"></script>"
                ppHtml
                  = "<!DOCTYPE html><html><head><title>" >|< modNm >|< "</title>"
                    >-< vlist (map scr jsL)
                    >-< "</head>"
                    >-< "<body>"
                    >-< "</body>"
                    >-< "</html>"


