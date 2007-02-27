{-| Module      :  PhaseTypingStrategies
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module PhaseTypingStrategies(phaseTypingStrategies) where

import CompileUtils
import Core(CoreDecl)
import TS_Compile (readTypingStrategiesFromFile)
import qualified Data.Map as M
import UHA_Syntax (Name)
import Top.Types (TpScheme)

phaseTypingStrategies :: String -> ImportEnvironment -> [(Name, TpScheme)] -> [Option] ->
                            IO (ImportEnvironment, [CoreDecl])
phaseTypingStrategies fullName combinedEnv typeSignatures options

   | DisableDirectives `elem` options = 
        return (removeTypingStrategies combinedEnv, [])

   | otherwise =
        let (path, baseName, _) = splitFilePath fullName
            fullNameNoExt       = combinePathAndFile path baseName            
        in do enterNewPhase "Type inference directives" options
              (typingStrategies, typingStrategiesDecls) <-
                 readTypingStrategiesFromFile options (fullNameNoExt ++ ".type")        
                            (addToTypeEnvironment (M.fromList typeSignatures) combinedEnv)
              return 
                 ( addTypingStrategies typingStrategies combinedEnv
                 , typingStrategiesDecls
                 )              
