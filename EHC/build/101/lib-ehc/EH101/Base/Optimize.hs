module EH101.Base.Optimize
( Optimize (..)
, allOptimizeMp
, OptimizeOption (..)
, OptimizeOptionValue (..)
, OptimizeOptionMp
, optimizeOptionMpSingleton
, optimizeOptionStictnessAnalysisQuant
, allOptimizeOptionMp
, allOptimizeOptionMpAnyOption
, OptimizationLevel (..)
, OptimizationScope (..)
, optimizeRequiresClosure
, OptimizeS
, OptimizationLevelMp
, optimizationLevelMp )
where
import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.List
import EH101.Base.AssocL
import EH.Util.Pretty
import EH.Util.Utils
import EH101.Base.Binary
import EH101.Base.Serialize


{-# LINE 26 "src/ehc/Base/Optimize.chs" #-}
-- | individual optimizations, unit of turning off/on. (Assumption) Names of alternatives must start with Optimize_
data Optimize
  = Optimize_GrinLocal							-- Grin: local base optimizations
  | Optimize_StrictnessAnalysis					-- Core: relevance analysis
  deriving (Eq,Ord,Enum,Show,Bounded)

{-# LINE 38 "src/ehc/Base/Optimize.chs" #-}
-- | All optimizations, mapped to from string representation derived via show.
-- | See also Optimize def for assumption.
allOptimizeMp :: Map.Map String Optimize
allOptimizeMp
  = Map.fromList [ (drop lenPrefix $ show o, o) | o <- [minBound .. maxBound] ]
  where lenPrefix = length "Optimize_"

{-# LINE 60 "src/ehc/Base/Optimize.chs" #-}
-- | extra optimization specific flags/config/tuning/option/etc
data OptimizeOption
  = OptimizeOption_StrictnessAnalysisQuant
  deriving (Eq,Ord,Show)

{-# LINE 67 "src/ehc/Base/Optimize.chs" #-}
-- | extra optimization specific flags/config/tuning/option/etc
data OptimizeOptionValue
  = OptimizeOptionValue_StrictnessAnalysis_NoQuant					-- no quantification of relevance type
  | OptimizeOptionValue_StrictnessAnalysis_Quant					-- (default) quantification of relevance type
  | OptimizeOptionValue_StrictnessAnalysis_QuantInstantiate			-- quant + later instantiation
  deriving (Eq,Ord,Show,Enum)

{-# LINE 76 "src/ehc/Base/Optimize.chs" #-}
-- | the map which holds for each optimization additional (optional) configuration
type OptimizeOptionMp' val = Map.Map Optimize (Map.Map OptimizeOption val)
type OptimizeOptionMp      = OptimizeOptionMp' OptimizeOptionValue

{-# LINE 82 "src/ehc/Base/Optimize.chs" #-}
optimizeOptionMpSingleton :: Optimize -> OptimizeOption -> OptimizeOptionValue -> OptimizeOptionMp
optimizeOptionMpSingleton o oo v = Map.singleton o (Map.singleton oo v)

{-# LINE 87 "src/ehc/Base/Optimize.chs" #-}
-- | quantification options related to strictness analysis
optimizeOptionStictnessAnalysisQuant :: OptimizeOptionMp -> OptimizeOptionValue
optimizeOptionStictnessAnalysisQuant m
  = case mapLookup2 Optimize_StrictnessAnalysis OptimizeOption_StrictnessAnalysisQuant m of
      Just oo -> maybe OptimizeOptionValue_StrictnessAnalysis_Quant id $ extr oo
      _       ->       OptimizeOptionValue_StrictnessAnalysis_Quant
  where extr = Just

{-# LINE 110 "src/ehc/Base/Optimize.chs" #-}
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

{-# LINE 130 "src/ehc/Base/Optimize.chs" #-}
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

{-# LINE 148 "src/ehc/Base/Optimize.chs" #-}
data OptimizationLevel
  = OptimizationLevel_Off               -- no optimizations                 : -O0
  | OptimizationLevel_Normal            -- easy and cheap optimizations     : -O1 (default)
  | OptimizationLevel_Much              -- more and expensive optimizations : -O2
  | OptimizationLevel_Full              -- throw everything in it           : -O3
  deriving (Eq,Ord,Show,Enum,Bounded)

{-# LINE 157 "src/ehc/Base/Optimize.chs" #-}
-- | Scope of optimizations, increasingly more global
data OptimizationScope
  = OptimizationScope_PerModule			-- per module
  | OptimizationScope_WholeGrin			-- whole program, starting with GRIN
  | OptimizationScope_WholeCore			-- whole program, starting with Core
  deriving (Eq,Ord,Show,Enum,Bounded)

{-# LINE 172 "src/ehc/Base/Optimize.chs" #-}
type OptimizeRequiresMp = Map.Map Optimize OptimizeS

{-# LINE 176 "src/ehc/Base/Optimize.chs" #-}
optimizeRequiresMp :: OptimizeRequiresMp
optimizeRequiresMp
  = Map.map Set.fromList $ Map.fromList
      [ ( Optimize_StrictnessAnalysis
        , [  ]
        )
      ]

{-# LINE 186 "src/ehc/Base/Optimize.chs" #-}
-- | transitive closure of required optimizations
optimizeRequiresClosure :: OptimizeS -> OptimizeS
optimizeRequiresClosure os
  = closes Set.empty os
  where close o os
          = closes (Set.insert o os)
            $ Map.findWithDefault Set.empty o optimizeRequiresMp
              `Set.difference` os
        closes = Set.fold close

{-# LINE 202 "src/ehc/Base/Optimize.chs" #-}
type OptimizeS = Set.Set Optimize

{-# LINE 206 "src/ehc/Base/Optimize.chs" #-}
-- | mapping to group of optimizations
type OptimizationLevelMp = Map.Map OptimizationLevel OptimizeS

{-# LINE 211 "src/ehc/Base/Optimize.chs" #-}
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

