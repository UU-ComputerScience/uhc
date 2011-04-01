%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimization flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) module {%{EH}Base.Optimize}
%%]

%%[(8 codegen) import(qualified Data.Set as Set,qualified Data.Map as Map,Data.List)
%%]
%%[(8 codegen) import({%{EH}Base.AssocL})
%%]
%%[(8 codegen) import(EH.Util.Pretty,EH.Util.Utils)
%%]
%%[(50 codegen) import({%{EH}Base.Binary}, {%{EH}Base.Serialize} )
%%]

%%[doesWhat doclatex
Representation of flags for turning on/off individual optimizations.
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Optimize flags for code generation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(Optimize(..))
-- | individual optimizations, unit of turning off/on. (Assumption) Names of alternatives must start with Optimize_
data Optimize
  = Optimize_GrinLocal							-- Grin: local base optimizations
  | Optimize_StrictnessAnalysis					-- Core: relevance analysis
  deriving (Eq,Ord,Enum,Show,Bounded)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% All optimize flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(allOptimizeMp)
-- | All optimizations, mapped to from string representation derived via show.
-- | See also Optimize def for assumption.
allOptimizeMp :: Map.Map String Optimize
allOptimizeMp
  = Map.fromList [ (drop lenPrefix $ show o, o) | o <- [minBound .. maxBound] ]
  where lenPrefix = length "Optimize_"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Options for optimize flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) export(StrictnessAnalysisOption(..))
-- | extra optimization specific flags/config/tuning/option/etc, for strictness analysis
data StrictnessAnalysisOption
  = StrictnessAnalysisOption_NoQuant				-- no quantification of relevance type
  | StrictnessAnalysisOption_Quant					-- (default) quantification of relevance type
  | StrictnessAnalysisOption_QuantInstantiate		-- quant + later instantiation
  deriving (Eq,Ord,Show)
%%]

%%[(8 codegen) export(OptimizeOption(..))
-- | extra optimization specific flags/config/tuning/option/etc
data OptimizeOption
  = OptimizeOption_StrictnessAnalysisQuant
  deriving (Eq,Ord,Show)
%%]

%%[(8 codegen) export(OptimizeOptionValue(..))
-- | extra optimization specific flags/config/tuning/option/etc
data OptimizeOptionValue
  = OptimizeOptionValue_StrictnessAnalysis_NoQuant					-- no quantification of relevance type
  | OptimizeOptionValue_StrictnessAnalysis_Quant					-- (default) quantification of relevance type
  | OptimizeOptionValue_StrictnessAnalysis_QuantInstantiate			-- quant + later instantiation
  deriving (Eq,Ord,Show,Enum)
%%]

%%[(8 codegen) export(OptimizeOptionMp)
-- | the map which holds for each optimization additional (optional) configuration
type OptimizeOptionMp' val = Map.Map Optimize (Map.Map OptimizeOption val)
type OptimizeOptionMp      = OptimizeOptionMp' OptimizeOptionValue
%%]

%%[(8 codegen) export(optimizeOptionMpSingleton)
optimizeOptionMpSingleton :: Optimize -> OptimizeOption -> OptimizeOptionValue -> OptimizeOptionMp
optimizeOptionMpSingleton o oo v = Map.singleton o (Map.singleton oo v)
%%]

%%[(8 codegen) export(optimizeOptionStictnessAnalysisQuant)
-- | quantification options related to strictness analysis
optimizeOptionStictnessAnalysisQuant :: OptimizeOptionMp -> OptimizeOptionValue
optimizeOptionStictnessAnalysisQuant m
  = case mapLookup2 Optimize_StrictnessAnalysis OptimizeOption_StrictnessAnalysisQuant m of
      Just oo -> maybe OptimizeOptionValue_StrictnessAnalysis_Quant id $ extr oo
      _       ->       OptimizeOptionValue_StrictnessAnalysis_Quant
  where extr = Just
%%]
-- | quantification options related to strictness analysis
optimizeOptionStictnessAnalysisQuant :: OptimizeOptionMp -> StrictnessAnalysisOption
optimizeOptionStictnessAnalysisQuant m
  = case Map.lookup Optimize_StrictnessAnalysis m of
      Just oo -> maybe StrictnessAnalysisOption_Quant id $ extr oo
      _       ->       StrictnessAnalysisOption_Quant
  where extr (OptimizeOption_StrictnessAnalysis_Quant q : _ ) = Just q
        -- extr (_                                         : oo) = extr oo
        extr _                                                = Nothing

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% All options for optimize flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(allOptimizeOptionMp)
-- | All optimization options, map from optimize flag to allowed range of
allOptimizeOptionMp
  :: OptimizeOptionMp'
       ( OptimizeOptionValue							-- default
       , (OptimizeOptionValue, OptimizeOptionValue)		-- min, max
       )
allOptimizeOptionMp
  = Map.fromList $ assocLMapElt Map.fromList
      [ ( Optimize_StrictnessAnalysis
        , [ ( OptimizeOption_StrictnessAnalysisQuant
            , ( OptimizeOptionValue_StrictnessAnalysis_Quant
              , (OptimizeOptionValue_StrictnessAnalysis_NoQuant, OptimizeOptionValue_StrictnessAnalysis_QuantInstantiate)
              )
            )
          ]
        )
      ]
%%]

%%[(8 codegen) export(allOptimizeOptionMpAnyOption)
-- | Just get any optimize option (if available) with default
allOptimizeOptionMpAnyOption :: Optimize -> (OptimizeOption, OptimizeOptionValue)
allOptimizeOptionMpAnyOption o
  = panicJust "allOptimizeOptionMpAnyOption"
    $ do { om <- Map.lookup o allOptimizeOptionMp
         ; if Map.null om
           then panic ("allOptimizeOptionMpAnyOption: " ++ show o)
           else do { let (oo,(dflt,_)) = Map.findMin om
                   ; return (oo,dflt)
                   }
         }
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
-- | Scope of optimizations, increasingly more global
data OptimizationScope
  = OptimizationScope_PerModule			-- per module
%%[[50
  | OptimizationScope_WholeGrin			-- whole program, starting with GRIN
  | OptimizationScope_WholeCore			-- whole program, starting with Core
%%]]
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
        , [] -- [ Optimize_StrictnessAnalysis ]
        )
      ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(5020 codegen)
deriving instance Typeable Optimize
deriving instance Data Optimize
%%]

%%[(5020 codegen)
instance Binary Optimize where
  put = putEnum8
  get = getEnum8

instance Serialize Optimize where
  sput = sputEnum8
  sget = sgetEnum8
%%]


