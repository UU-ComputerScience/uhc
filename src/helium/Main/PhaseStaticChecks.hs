{-| Module      :  PhaseStaticChecks
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module PhaseStaticChecks(phaseStaticChecks) where

import CompileUtils
import Warnings(Warning)
import qualified StaticChecks(sem_Module)
import UHA_Syntax (Name)
import Top.Types (TpScheme)
import StaticErrors
import Information (showInformation)

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