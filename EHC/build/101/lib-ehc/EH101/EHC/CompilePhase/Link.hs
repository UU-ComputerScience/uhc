module EH101.EHC.CompilePhase.Link
( cpLinkO )
where
import EH101.EHC.Common
import EH101.EHC.CompileUnit
import EH101.EHC.CompileRun
import qualified EH101.Config as Cfg
import EH101.EHC.Environment
import EH101.Base.Target

{-# LINE 34 "src/ehc/EHC/CompilePhase/Link.chs" #-}
cpLinkO :: [HsName] -> String -> EHCompilePhase ()
cpLinkO modNmL pkgNm
  = do { cr <- get
       ; let (crsi,opts) = crBaseInfo' cr
             codeFiles   = [ fpathToStr o | m <- modNmL, o <- ecuGenCodeFiles $ crCU m cr ]
             (libFile,_) = mkInOrOutputFPathDirFor OutputFor_Pkg opts l l (fpathSuff l)
                         where l = mkFPath $ Cfg.mkCLibFilename "" pkgNm
             linkCode    = map mkShellCmd $ Cfg.mkShellCmdLibtool (fpathToStr libFile) codeFiles
       ; when (ehcOptVerbosity opts >= VerboseALot)
              (do { lift $ mapM_ putStrLn linkCode
                  })
       ; unless (null codeFiles)
                (cpSeq [ cpSystem c | c <- linkCode ])
       }

