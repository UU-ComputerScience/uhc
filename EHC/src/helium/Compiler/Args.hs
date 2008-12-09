{-| Module      :  Args
    License     :  GPL

    Maintainer  :  helium@cs.uu.nl
    Stability   :  experimental
    Portability :  portable
-}

module Helium.Compiler.Args
    ( Option(..)
    , processArgs
    , lvmPathFromOptions
    ) where

import System
import Helium.Compiler.Version
import Helium.Utils.Utils
import Data.Char
import System.Console.GetOpt

processArgs :: [String] -> IO ([Option], String)
processArgs args =
    let (options, arguments, errors) = getOpt Permute (optionDescription True True) args
        moreOptions         = MoreOptions `elem` options || experimentalOptions
        experimentalOptions = ExperimentalOptions `elem` options
    in if not (null errors) || length arguments /= 1 then do
        putStrLn $ "Helium compiler " ++ version
        putStrLn (usageInfo "Usage: helium [options] file" (optionDescription moreOptions experimentalOptions))
        exitWith (ExitFailure 1)
    else
        return (options, (head arguments))
 where
   optionDescription moreOptions experimentalOptions =
      -- Main options
      [ Option "b" ["build"]                (NoArg BuildOne) "recompile module even if up to date"
      , Option "B" ["build-all"]            (NoArg BuildAll) "recompile all modules even if up to date"
      , Option "i" ["dump-information"]     (NoArg DumpInformationForThisModule) "show information about this module"
      , Option "I" ["dump-all-information"] (NoArg DumpInformationForAllModules) "show information about all imported modules"
      , Option "l" ["no-logging"]           (NoArg NoLogging) "do not send log information"
      , Option "!" ["log-special"]          (NoArg LogSpecial) "logs with special flag"
      , Option "o" ["overloading"]          (NoArg Overloading) "turn overloading on"
      , Option "P" ["lvmpath"]              (ReqArg LvmPath "PATH") "use PATH as search path"
      , Option "v" ["verbose"]              (NoArg Verbose) "show the phase the compiler is in"
      , Option "w" ["no-warnings"]          (NoArg NoWarnings) "do not show warnings"
      , Option "X" ["more-options"]         (NoArg MoreOptions) "show more compiler options"
      , Option ""  ["info"]                 (ReqArg Information "NAME") "display information about NAME"
      ]
      ++
      -- More options
      if not moreOptions then [] else
      [ Option "1" ["stop-after-parsing"]          (NoArg StopAfterParser) "stop after parsing"
      , Option "2" ["stop-after-static-analysis"]  (NoArg StopAfterStaticAnalysis) "stop after static analysis"
      , Option "3" ["stop-after-type-inferencing"] (NoArg StopAfterTypeInferencing) "stop after type inferencing"
      , Option "4" ["stop-after-desugaring"]       (NoArg StopAfterDesugar) "stop after desugaring into Core"    
      , Option "t" ["dump-tokens"]                 (NoArg DumpTokens) "dump tokens to screen"
      , Option "u" ["dump-uha"]                    (NoArg DumpUHA) "pretty print abstract syntax tree"
      , Option "c" ["dump-core"]                   (NoArg DumpCore) "pretty print Core program"
      , Option "C" ["save-core"]                   (NoArg DumpCoreToFile) "write Core program to file"
      , Option ""  ["debug-logger"]                (NoArg DebugLogger) "show logger debug information"
      , Option "d" ["type-debug"]                  (NoArg DumpTypeDebug) "debug constraint-based type inference"         
      , Option "W" ["algorithm-w"]                 (NoArg AlgorithmW) "use bottom-up type inference algorithm W"
      , Option "M" ["algorithm-m"]                 (NoArg AlgorithmM) "use folklore top-down type inference algorithm M"
      , Option ""  ["no-directives" ]              (NoArg DisableDirectives) "disable type inference directives"
      , Option ""  ["no-repair-heuristics"]        (NoArg NoRepairHeuristics) "don't suggest program fixes"
      ]
      ++
      -- Experimental options
      if not experimentalOptions then [] else
      [ Option "" ["experimental-options"] (NoArg ExperimentalOptions) "show experimental compiler options"
      , Option "" ["kind-inferencing"]     (NoArg KindInferencing) "perform kind inference (experimental)"
      , Option "" ["signature-warnings"]   (NoArg SignatureWarnings) "warn for too specific signatures (experimental)" 
      , Option "" ["right-to-left"]        (NoArg RightToLeft) "right-to-left treewalk"
      , Option "" ["no-spreading" ]        (NoArg NoSpreading) "do not spread type constraints (experimental)"
      , Option "" ["treewalk-topdown" ]    (NoArg TreeWalkTopDown) "top-down treewalk"
      , Option "" ["treewalk-bottomup"]    (NoArg TreeWalkBottomUp) "bottom up-treewalk"
      , Option "" ["treewalk-inorder1"]    (NoArg TreeWalkInorderTopFirstPre) "treewalk (top;upward;child)"
      , Option "" ["treewalk-inorder2"]    (NoArg TreeWalkInorderTopLastPre) "treewalk (upward;child;top)"
      , Option "" ["treewalk-inorder3"]    (NoArg TreeWalkInorderTopFirstPost) "treewalk (top;child;upward)"
      , Option "" ["treewalk-inorder4"]    (NoArg TreeWalkInorderTopLastPost) "treewalk (child;upward;top)"
      , Option "" ["solver-simple"     ]   (NoArg SolverSimple) "a simple constraint solver"
      , Option "" ["solver-greedy"     ]   (NoArg SolverGreedy) "a fast constraint solver"
      , Option "" ["solver-typegraph"  ]   (NoArg SolverTypeGraph) "type graph constraint solver"
      , Option "" ["solver-combination"]   (NoArg SolverCombination) "switches between \"greedy\" and \"type graph\""
      , Option "" ["solver-chunks"     ]   (NoArg SolverChunks) "solves chunks of constraints (default)"
      , Option "" ["unifier-heuristics"]   (NoArg UnifierHeuristics)  "use unifier heuristics (experimental)"
      , Option "" ["select-cnr"]           (ReqArg selectCNR "CNR") "select constraint number to be reported"
      ]

data Option 
   -- Main options
   = BuildOne | BuildAll | DumpInformationForThisModule | DumpInformationForAllModules
   | NoLogging | LogSpecial | Overloading | LvmPath String | Verbose | NoWarnings | MoreOptions
   | Information String
   -- More options
   | StopAfterParser | StopAfterStaticAnalysis | StopAfterTypeInferencing | StopAfterDesugar
   | DumpTokens | DumpUHA | DumpCore | DumpCoreToFile | DebugLogger | DumpTypeDebug
   | AlgorithmW | AlgorithmM | DisableDirectives | NoRepairHeuristics
   -- Experimental options
   | ExperimentalOptions |KindInferencing | SignatureWarnings | RightToLeft | NoSpreading
   | TreeWalkTopDown | TreeWalkBottomUp | TreeWalkInorderTopFirstPre | TreeWalkInorderTopLastPre
   | TreeWalkInorderTopFirstPost | TreeWalkInorderTopLastPost | SolverSimple | SolverGreedy
   | SolverTypeGraph | SolverCombination | SolverChunks | UnifierHeuristics
   | SelectConstraintNumber Int
 deriving Eq

lvmPathFromOptions :: [Option] -> Maybe String
lvmPathFromOptions [] = Nothing
lvmPathFromOptions (LvmPath s : _) = Just s
lvmPathFromOptions (_ : rest) = lvmPathFromOptions rest

selectCNR :: String -> Option
selectCNR s
   | all isDigit s = SelectConstraintNumber (read ('0':s)) 
   | otherwise     = SelectConstraintNumber (-1) -- problem with argument
