module EH101.LamInfo
( StackTraceInfo (..)
, LamInfoBindAsp (..)
, LamInfo (..), emptyLamInfo, emptyLamInfo'
, LamMp
, lamMpUnionBindAspMp
, lamMpMergeInto
, lamMpLookupAsp, lamMpLookupAsp2, lamMpLookupLam, lamMpLookupCaf
, lamMpFilterLam, lamMpFilterCaf
, lamMpMergeFrom
, GrinByteCodeLamInfo (..), emptyGrinByteCodeLamInfo
, laminfo1stArgIsStackTrace
, FusionRole (..) )
where
import EH101.Base.Common
import EH101.AbstractCore
import EH101.Ty
import EH101.Core
import EH101.AnaDomain
import EH.Util.Utils
import EH.Util.Pretty
import EH101.AnaDomain.Pretty
import EH101.Ty.Pretty
import qualified Data.Map as Map
import qualified Data.Set as Set
import Control.Monad
import EH101.Base.Serialize
import Data.Typeable (Typeable,Typeable2)
import Data.Generics (Data)



{-# LINE 60 "src/ehc/LamInfo.chs" #-}
data StackTraceInfo
  = StackTraceInfo_None
  | StackTraceInfo_HasStackTraceEquiv	HsName		-- has a stack traced equivalent
  | StackTraceInfo_IsStackTraceEquiv	HsName		-- is a stack traced equivalent
  deriving ( Show
           , Data, Typeable
           )

{-# LINE 72 "src/ehc/LamInfo.chs" #-}
-- | The role a value takes in fusion
data FusionRole
  = FusionRole_Fuse			-- fuse this, i.e. inline, turned on by 'fuse f' for f
  | FusionRole_BuildLeft	-- role of g in 'convert g,h'
  | FusionRole_BuildRight	-- role of h in 'convert g,h'
  deriving ( Enum, Show
           , Data,Typeable
           )

{-# LINE 83 "src/ehc/LamInfo.chs" #-}
instance PP FusionRole where
  pp r = pp $ drop l $ show r
       where l = length "FusionRole_"

{-# LINE 89 "src/ehc/LamInfo.chs" #-}
-- | per aspect info
data LamInfoBindAsp
  = LamInfoBindAsp_RelevTy							-- relevance typing
      { libindaspRelevTy 		:: !RelevTy
      }
  | LamInfoBindAsp_Ty								-- plain good old type
      { libindaspTy 			:: !Ty
      }
  | LamInfoBindAsp_Core								-- actual Core, should go paired with Ty (?? maybe pair them directly)
      { libindaspCore			:: !CExpr
      }
  | LamInfoBindAsp_FusionRole						-- role in fusion
      { libindaspFusionRole 	:: !FusionRole
      }
  deriving ( Show
           , Data, Typeable
           )

type LamInfoBindAspMp = Map.Map ACoreBindAspectKeyS LamInfoBindAsp

{-# LINE 115 "src/ehc/LamInfo.chs" #-}
instance PP LamInfoBindAsp where
  pp (LamInfoBindAsp_RelevTy 	t) = "RTy"  >#< pp t
  pp (LamInfoBindAsp_Ty      	t) = "Ty"   >#< pp t
  pp (LamInfoBindAsp_Core      	c) = pp "Core" -- >#< pp c -- Core.Pretty uses LamInfo, so module cycle...
  pp (LamInfoBindAsp_FusionRole	r) = "Fuse" >#< pp r

{-# LINE 125 "src/ehc/LamInfo.chs" #-}
-- | per lambda implementation info
data LamInfo
  = LamInfo
      { laminfoArity				:: !Int							-- arity of function
      , laminfoStackTrace  			:: !StackTraceInfo				-- stacktrace
      , laminfoGrinByteCode			:: Maybe GrinByteCodeLamInfo	-- GB specific info
      , laminfoBindAspMp			:: !LamInfoBindAspMp			-- info organized per/keyed on aspect
      }
  deriving ( Show
           , Data, Typeable
           )

emptyLamInfo' :: LamInfo
emptyLamInfo' = LamInfo 0 StackTraceInfo_None (Just emptyGrinByteCodeLamInfo) Map.empty

emptyLamInfo :: LamInfo
emptyLamInfo = LamInfo 0 StackTraceInfo_None Nothing Map.empty

{-# LINE 147 "src/ehc/LamInfo.chs" #-}
instance PP LamInfo where
  pp (LamInfo ar _ bc m) = ppAssocL $ assocLMapKey ppACBaspKeyS $ Map.toList m

{-# LINE 152 "src/ehc/LamInfo.chs" #-}
laminfo1stArgIsStackTrace :: LamInfo -> Bool
laminfo1stArgIsStackTrace (LamInfo {laminfoStackTrace=StackTraceInfo_IsStackTraceEquiv _}) = True
laminfo1stArgIsStackTrace _                                                                = False

{-# LINE 164 "src/ehc/LamInfo.chs" #-}
type LamMp    = Map.Map HsName LamInfo

{-# LINE 168 "src/ehc/LamInfo.chs" #-}
-- union, including the aspect map, but arbitrary for the info itself
lamMpUnionBindAspMp :: LamMp -> LamMp -> LamMp
lamMpUnionBindAspMp = Map.unionWith (\i1 i2 -> i1 {laminfoBindAspMp = laminfoBindAspMp i1 `Map.union` laminfoBindAspMp i2})

{-# LINE 174 "src/ehc/LamInfo.chs" #-}
-- propagate from new (left) to prev (right), adding new entries if necessary, combining with mergeL2RInfo, finally combining/choosing maps with mergeL2RMp
lamMpMergeInto :: (LamInfo -> LamInfo -> LamInfo) -> (LamMp -> LamMp -> LamMp) -> LamMp -> LamMp -> LamMp
lamMpMergeInto mergeL2RInfo mergeL2RMp newMp prevMp
  = mergeL2RMp newMpMerge prevMp
  where newMpMerge
          = Map.mapWithKey
              (\n i -> maybe i (mergeL2RInfo i) $ Map.lookup n prevMp
              ) newMp

{-# LINE 196 "src/ehc/LamInfo.chs" #-}
lamMpLookupAsp :: HsName -> ACoreBindAspectKeyS -> LamMp -> Maybe LamInfoBindAsp
lamMpLookupAsp n a m
  = fmap snd $ mapLookup2' laminfoBindAspMp n a m

lamMpLookupAsp2 :: ACoreBindRef -> LamMp -> Maybe LamInfoBindAsp
lamMpLookupAsp2 (ACoreBindRef n (Just a)) m = lamMpLookupAsp n a m

lamMpLookupLam :: HsName -> LamMp -> Maybe Int
lamMpLookupLam n m
  = case Map.lookup n m of
      j@(Just (LamInfo {laminfoArity=a})) | a > 0
        -> Just a
      _ -> Nothing

lamMpLookupCaf :: HsName -> LamMp -> Maybe Int
lamMpLookupCaf n m
  = case Map.lookup n m of
      j@(Just (LamInfo {laminfoArity=a})) | a == 0
        -> Just a
      _ -> Nothing

{-# LINE 219 "src/ehc/LamInfo.chs" #-}
lamMpFilterLam :: LamMp -> LamMp
lamMpFilterLam = Map.filter ((>0) . laminfoArity)

lamMpFilterCaf :: LamMp -> LamMp
lamMpFilterCaf = Map.filter ((==0) . laminfoArity)

{-# LINE 227 "src/ehc/LamInfo.chs" #-}
-- | merge info from arbitrary map m into LamMp holding LamInfo's
lamMpMergeFrom
  :: (LamInfo -> Maybe x)					-- extract relevant info from a LamInfo
     -> (Maybe x -> LamInfo -> LamInfo)		-- set the info
     -> (z -> x -> x)						-- merge info from new map and old info
     -> LamInfo								-- default, empty LamInfo
     -> Map.Map HsName z					-- arbitrary map holding info to merge
     -> LamMp -> LamMp
lamMpMergeFrom get set merge empty m lm
  = Map.foldrWithKey (\n z lm -> Map.alter (Just . upd z) n lm)
                    lm m
  where upd z (Just i) = set (Just (merge z $ maybe emptyExtra id $ get i)) i
        upd z Nothing  = set (Just (merge z         emptyExtra           )) empty
        emptyExtra = panicJust "lamMpMergeFrom" $ get $ empty

{-# LINE 264 "src/ehc/LamInfo.chs" #-}
data GrinByteCodeLamInfo
  = GrinByteCodeLamInfo
      { gblaminfoFuninfoKey		:: !Int					-- index into FunctionInfo table, to be referred to outside this module only
      }
  deriving
     ( Show
     , Typeable, Data
     )

emptyGrinByteCodeLamInfo :: GrinByteCodeLamInfo
emptyGrinByteCodeLamInfo = GrinByteCodeLamInfo (-1)

{-# LINE 284 "src/ehc/LamInfo.chs" #-}
instance Serialize GrinByteCodeLamInfo where
  sput (GrinByteCodeLamInfo a) = sput a
  sget = liftM  GrinByteCodeLamInfo sget

instance Serialize FusionRole where
  sput = sputEnum8
  sget = sgetEnum8

instance Serialize LamInfoBindAsp where
  sput (LamInfoBindAsp_RelevTy  	a) = sputWord8 0 >> sput a
  sput (LamInfoBindAsp_Ty 			a) = sputWord8 1 >> sput a
  sput (LamInfoBindAsp_Core 		a) = sputWord8 2 >> sput a
  sput (LamInfoBindAsp_FusionRole 	a) = sputWord8 3 >> sput a
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM  LamInfoBindAsp_RelevTy  	sget
      1 -> liftM  LamInfoBindAsp_Ty 		sget
      2 -> liftM  LamInfoBindAsp_Core 		sget
      3 -> liftM  LamInfoBindAsp_FusionRole sget

instance Serialize LamInfo where
  sput (LamInfo a b c d) = sput a >> sput b >> sput c >> sput d
  sget = liftM4 LamInfo  sget sget sget sget

instance Serialize StackTraceInfo where
  sput (StackTraceInfo_None                ) = sputWord8 0
  sput (StackTraceInfo_HasStackTraceEquiv a) = sputWord8 1 >> sput a
  sput (StackTraceInfo_IsStackTraceEquiv  a) = sputWord8 2 >> sput a
  sget
    = do t <- sgetWord8
         case t of
           0 -> return StackTraceInfo_None
           1 -> liftM  StackTraceInfo_HasStackTraceEquiv sget
           2 -> liftM  StackTraceInfo_IsStackTraceEquiv  sget

