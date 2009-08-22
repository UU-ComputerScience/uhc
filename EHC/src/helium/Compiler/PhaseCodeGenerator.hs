{-| Module      :  PhaseCodeGenerator
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.PhaseCodeGenerator(phaseCodeGenerator) where

import Lvm.Core.Core(CoreModule)
import Helium.Compiler.CompileUtils
import Helium.CodeGeneration.CoreToLvm(coreToLvm)

phaseCodeGenerator :: String -> CoreModule -> [Option] -> IO ()
phaseCodeGenerator fullName coreModule options = do
    enterNewPhase "Code generation" options

    let (path, baseName, _) = splitFilePath fullName
        fullNameNoExt = combinePathAndFile path baseName

    catch (coreToLvm fullNameNoExt coreModule) (\ioError -> do
        putStrLn ("Could not write to file '" ++
            fullNameNoExt ++ ".lvm" ++ "'" ++ show ioError);
        exitWith (ExitFailure 1)
     )
    
