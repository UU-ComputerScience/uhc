{-| Module      :  PhaseStaticChecks
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.PhaseStaticChecks(phaseStaticChecks) where

import Helium.Compiler.CompileUtils
import Helium.StaticAnalysis.Messages.Warnings(Warning)
import qualified Helium.StaticAnalysis.StaticChecks.StaticChecks as StaticChecks(sem_Module)
import Helium.Syntax.UHA (Name)
import Top.Types (TpScheme)
import Helium.StaticAnalysis.Messages.StaticErrors
import Helium.StaticAnalysis.Messages.Information (showInformation)

phaseStaticChecks :: 
   String -> Module -> [ImportEnvironment] -> [Option] -> 
   Phase Error (ImportEnvironment, [(Name,TpScheme)], [Warning])
phaseStaticChecks fullName module_ importEnvs options = do
    enterNewPhase "Static checking" options

    let (_, baseName, _) = splitFilePath fullName

        (localEnv, errors, _, typeSignatures, warnings) =
            StaticChecks.sem_Module module_ baseName importEnvs options

    case errors of
    
       _:_ ->
          do when (DumpInformationForAllModules `elem` options) $
                putStrLn (show (foldr combineImportEnvironments emptyEnvironment importEnvs))
             
             -- display name information
             let combinedEnv = foldr combineImportEnvironments emptyEnvironment importEnvs
             showInformation False options combinedEnv
    
             return (Left errors)
         
       [] -> 
          return (Right (localEnv, typeSignatures, warnings))