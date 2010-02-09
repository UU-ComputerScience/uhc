{-| Module      :  PhaseKindInferencer
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.PhaseKindInferencer (phaseKindInferencer) where

import Helium.Compiler.Args
import Helium.Compiler.CompileUtils
import Helium.StaticAnalysis.Inferencers.KindInferencing as KI
import Helium.ModuleSystem.ImportEnvironment
import qualified Data.Map as M
import Top.Types
import Helium.StaticAnalysis.Messages.KindErrors

phaseKindInferencer :: ImportEnvironment -> Module -> [Option] -> Phase KindError ()
phaseKindInferencer importEnvironment module_ options =
   do enterNewPhase "Kind inferencing" options
      let (debugIO, kindEnv, kindErrors, _) = KI.sem_Module module_ importEnvironment options 
      when (DumpTypeDebug `elem` options) $ 
         do debugIO  
            putStrLn . unlines . map (\(n,ks) -> show n++" :: "++showKindScheme ks) . M.assocs $ kindEnv
      case kindErrors of
      
         _:_ ->
            return (Left kindErrors)
         [] ->
            return (Right ())