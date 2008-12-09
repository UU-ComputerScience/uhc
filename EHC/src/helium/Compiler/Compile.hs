{-| Module      :  Compile
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.Compile where

import Helium.Compiler.PhaseLexer
import Helium.Compiler.PhaseParser
import Helium.Compiler.PhaseImport
import Helium.Compiler.PhaseResolveOperators
import Helium.Compiler.PhaseStaticChecks
import Helium.Compiler.PhaseKindInferencer
import Helium.Compiler.PhaseTypingStrategies
import Helium.Compiler.PhaseTypeInferencer
import Helium.Compiler.PhaseDesugarer
import Helium.Compiler.PhaseCodeGenerator
import Helium.Compiler.CompileUtils
import Helium.Utils.Utils
import Data.IORef
import Helium.StaticAnalysis.Messages.StaticErrors(errorsLogCode)

compile :: String -> [Option] -> [String] -> [String] -> IO ()
compile fullName options lvmPath doneModules =
    do
        let compileOptions = (options, fullName, doneModules)
        putStrLn ("Compiling " ++ fullName)

        -- Store the current module file-name and its context in
        -- two IO refs (unsafe! only used for internal error bug-report)
        writeIORef refToCurrentFileName fullName
        writeIORef refToCurrentImported doneModules

        contents <- safeReadFile fullName

        -- Phase 1: Lexing
        (lexerWarnings, tokens) <- 
            doPhaseWithExit 20 (const "L") compileOptions $
               phaseLexer fullName contents options
        
        unless (NoWarnings `elem` options) $
            showMessages lexerWarnings

        -- Phase 2: Parsing
        parsedModule <- 
            doPhaseWithExit 20 (const "P") compileOptions $
               phaseParser fullName tokens options

        -- Phase 3: Importing
        (indirectionDecls, importEnvs) <-
            phaseImport fullName parsedModule lvmPath options
        
        -- Phase 4: Resolving operators
        resolvedModule <- 
            doPhaseWithExit 20 (const "R") compileOptions $
               phaseResolveOperators parsedModule importEnvs options
            
        stopCompilingIf (StopAfterParser `elem` options)

        -- Phase 5: Static checking
        (localEnv, typeSignatures, staticWarnings) <-
            doPhaseWithExit 20 (("S"++) . errorsLogCode) compileOptions $
               phaseStaticChecks fullName resolvedModule importEnvs options        

        unless (NoWarnings `elem` options) $
            showMessages staticWarnings

        stopCompilingIf (StopAfterStaticAnalysis `elem` options)

        -- Phase 6: Kind inferencing (by default turned off)
        let combinedEnv = foldr combineImportEnvironments localEnv importEnvs
        when (KindInferencing `elem` options) $
           doPhaseWithExit maximumNumberOfKindErrors (const "K") compileOptions $
              phaseKindInferencer combinedEnv resolvedModule options
              
        -- Phase 7: Type Inference Directives
        (beforeTypeInferEnv, typingStrategiesDecls) <-
            phaseTypingStrategies fullName combinedEnv typeSignatures options

        -- Phase 8: Type inferencing
        (dictionaryEnv, afterTypeInferEnv, toplevelTypes, typeWarnings) <- 
            doPhaseWithExit maximumNumberOfTypeErrors (const "T") compileOptions $ 
               phaseTypeInferencer fullName resolvedModule {-doneModules-} localEnv beforeTypeInferEnv options

        unless (NoWarnings `elem` options) $
            showMessages typeWarnings

        stopCompilingIf (StopAfterTypeInferencing `elem` options)

        -- Phase 9: Desugaring
        coreModule <-                
            phaseDesugarer dictionaryEnv
                           fullName resolvedModule 
                           (typingStrategiesDecls ++ indirectionDecls) 
                           afterTypeInferEnv
                           toplevelTypes 
                           options                           

        stopCompilingIf (StopAfterDesugar `elem` options)

        -- Phase 10: Code generation
        phaseCodeGenerator fullName coreModule options
        
        unless (NoLogging `elem` options) $ 
            sendLog "C" fullName doneModules options

        let number = length staticWarnings + length typeWarnings + length lexerWarnings
        putStrLn $ "Compilation successful" ++
                      if number == 0 || (NoWarnings `elem` options)
                        then ""
                        else " with " ++ show number ++ " warning" ++ if number == 1 then "" else "s"

safeReadFile :: String -> IO String
safeReadFile fullName = 
    catch 
        (readFile fullName)
        (\ioError -> 
            let message = "Unable to read file " ++ show fullName 
                       ++ " (" ++ show ioError ++ ")"
            in throw message)

stopCompilingIf :: Bool -> IO ()
stopCompilingIf bool = when bool (exitWith (ExitFailure 1))

maximumNumberOfTypeErrors :: Int
maximumNumberOfTypeErrors = 3

maximumNumberOfKindErrors :: Int
maximumNumberOfKindErrors = 1