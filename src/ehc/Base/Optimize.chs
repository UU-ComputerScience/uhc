%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimization flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Base.Optimize}
%%]

%%[(8 codegen) import(qualified Data.Set as Set,qualified Data.Map as Map,Data.List)
%%]
%%[(8 codegen) import(EH.Util.Pretty,EH.Util.Utils)
%%]
%%[(20 codegen) import({%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[doesWhat doclatex
Representation of flags for turning on/off individual optimizations.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimizes for code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(Optimize(..))
-- | individual optimizations, unit of turning off/on. Names of alternatives must start with Optimize_
data Optimize
  = Optimize_GrinLocal							-- Grin: local base optimizations
  | Optimize_StrictnessAnalysis					-- Core: relevance analysis
  deriving (Eq,Ord,Enum,Show,Bounded)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% All flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(allOptimizeMp)
allOptimizeMp :: Map.Map String Optimize
allOptimizeMp
  = Map.fromList [ (drop lenPrefix $ show o, o) | o <- [minBound .. maxBound] ]
  where lenPrefix = length "Optimize_"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimisation level: combination of how much & scope (per module, whole program)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(OptimizationLevel(..))
data OptimizationLevel
  = OptimizationLevel_Off               -- no optimizations                 : -O0
  | OptimizationLevel_Normal            -- easy and cheap optimizations     : -O1 (default)
  | OptimizationLevel_Much              -- more and expensive optimizations : -O2
  | OptimizationLevel_Full              -- throw everything in it           : -O3
  deriving (Eq,Ord,Show,Enum,Bounded)
%%]

%%[(8 codegen) export(OptimizationScope(..))
data OptimizationScope
  = OptimizationScope_PerModule         
  | OptimizationScope_WholeProgram
  deriving (Eq,Ord,Show,Enum,Bounded)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimize dependencies, implied optimizations
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen)
type OptimizeRequiresMp = Map.Map Optimize OptimizeS
%%]

%%[(8 codegen)
optimizeRequiresMp :: OptimizeRequiresMp
optimizeRequiresMp
  = Map.map Set.fromList $ Map.fromList
      [ ( Optimize_StrictnessAnalysis
        , [  ]
        )
      ]
%%]

%%[(8 codegen) export(optimizeRequiresClosure)
-- | transitive closure of required optimizations
optimizeRequiresClosure :: OptimizeS -> OptimizeS
optimizeRequiresClosure os
  = closes Set.empty os
  where close o os
          = closes (Set.insert o os)
            $ Map.findWithDefault Set.empty o optimizeRequiresMp
              `Set.difference` os
        closes = Set.fold close
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimize groups, to be associated with OptimizationLevels
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(OptimizeS)
type OptimizeS = Set.Set Optimize
%%]

%%[(8 codegen) export(OptimizationLevelMp)
-- | mapping to group of optimizations
type OptimizationLevelMp = Map.Map OptimizationLevel OptimizeS
%%]

%%[(8 codegen) export(optimizationLevelMp)
-- | map from level to optimizations, specified as increments relative to previous in Enum ordering of level
optimizationLevelMp :: OptimizationLevelMp
optimizationLevelMp
  =   (\m -> fst $
             foldl (\(m,s) (l,o) -> let s' = Set.union s o in (Map.insert l s' m, s'))
                   (m, Set.empty)
                   [ (l, Map.findWithDefault Set.empty l m) | l <- [minBound .. maxBound] ]
      )
    $ Map.map Set.fromList
    $ Map.fromList
    $ [ ( OptimizationLevel_Off
        , [  ]
        )
      , ( OptimizationLevel_Normal
        , [ Optimize_GrinLocal ]
        )
      , ( OptimizationLevel_Much
        , [  ]
        )
      , ( OptimizationLevel_Full
        , [ Optimize_StrictnessAnalysis ]
        )
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(2020 codegen)
deriving instance Typeable Optimize
deriving instance Data Optimize
%%]

%%[(2020 codegen)
instance Binary Optimize where
  put = putEnum8
  get = getEnum8

instance Serialize Optimize where
  sput = sputEnum8
  sget = sgetEnum8
%%]


