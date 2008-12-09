{-| Module      :  PhaseTypeInferencer
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.PhaseTypeInferencer (phaseTypeInferencer) where

import Helium.Compiler.CompileUtils
import Helium.StaticAnalysis.Messages.Warnings(Warning)
import Helium.StaticAnalysis.Inferencers.TypeInferencing(typeInferencing)
import Helium.ModuleSystem.DictionaryEnvironment (DictionaryEnvironment)
import Helium.Syntax.UHA
import Helium.StaticAnalysis.Messages.TypeErrors
import Helium.StaticAnalysis.Messages.Information (showInformation)

phaseTypeInferencer :: 
    String -> Module -> ImportEnvironment -> ImportEnvironment -> [Option] -> 
    Phase TypeError (DictionaryEnvironment, ImportEnvironment, TypeEnvironment, [Warning])

phaseTypeInferencer fullName module_ localEnv completeEnv options = do
    enterNewPhase "Type inferencing" options

    -- 'W' and 'M' are predefined type inference algorithms
    let newOptions = (if AlgorithmW `elem` options
                        then filter (/= NoSpreading) . ([TreeWalkInorderTopLastPost, SolverGreedy]++) 
                        else id)
                   . (if AlgorithmM `elem` options
                        then filter (/= NoSpreading) . ([TreeWalkInorderTopFirstPre, SolverGreedy]++)  
                        else id)
                   $ options
                   
        (debugIO, dictionaryEnv, toplevelTypes, typeErrors, warnings) =
           typeInferencing newOptions completeEnv module_

        -- add the top-level types (including the inferred types)
        finalEnv = addToTypeEnvironment toplevelTypes completeEnv
    
    when (DumpTypeDebug `elem` options) debugIO      
     
    -- display name information
    showInformation True options finalEnv
    
    case typeErrors of 
       
       _:_ ->
          do when (DumpInformationForAllModules `elem` options) $
                putStr (show completeEnv)
             return (Left typeErrors)
          
       [] -> 
          do -- Dump information
             when (DumpInformationForAllModules `elem` options) $ 
                putStrLn (show finalEnv)
             when (  DumpInformationForThisModule `elem` options 
                  && DumpInformationForAllModules `notElem` options
                  ) 
                  $ putStrLn (show (addToTypeEnvironment toplevelTypes localEnv))
                  
             return (Right (dictionaryEnv, finalEnv, toplevelTypes, warnings))