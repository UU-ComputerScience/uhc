%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Implementation information about lambda/function
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Globally maintained info about the implementation of lambdas/function.
This can not reside in one particular phase as each phase gathers additional info to be added to a LamInfo.
Hence its separate and globally available definition.

Currently the following is maintained:
\begin{itemize}
\item For Core:
  \begin{itemize}
  \item arity
  \item stack tracing
  \end{itemize}
\item For GrinCode:
\item For GrinByteCode:
  \begin{itemize}
  \item FunctionInfo index
  \end{itemize}
\end{itemize}
%%]

%%[(8 codegen) module {%{EH}LamInfo} import({%{EH}Base.Common})
%%]
%%[(8 codegen) import({%{EH}AbstractCore})
%%]
%%[(8 codegen) import({%{EH}AnaDomain})
%%]
%%[(8 codegen) import(EH.Util.Utils)
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,qualified Data.Set as Set)
%%]

%%[(20 codegen) import(Control.Monad, {%{EH}Base.Serialize})
%%]
%%[(20 codegen) import(Data.Typeable(Typeable,Typeable2), Data.Generics(Data))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Calling convention info for lambda expressions/CAFs: known function arity
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(StackTraceInfo(..),LamInfo(..),emptyLamInfo,emptyLamInfo')
data StackTraceInfo
  = StackTraceInfo_None
  | StackTraceInfo_HasStackTraceEquiv	HsName		-- has a stack traced equivalent
  | StackTraceInfo_IsStackTraceEquiv	HsName		-- is a stack traced equivalent
%%[[20
  deriving (Data,Typeable)
%%]]

-- | per aspect info
data LamInfoBindAsp
  = LamInfoBindAsp_RelevTy		!RelevTy			-- relevance typing
%%[[20
  deriving (Data,Typeable)
%%]]

type LamInfoBindAspMp = Map.Map ACoreBindAspectKeyS LamInfoBindAsp

-- | per lambda implementation info
data LamInfo
  = LamInfo
      { laminfoArity				:: !Int							-- arity of function
      , laminfoStackTrace  			:: !StackTraceInfo				-- stacktrace
      , laminfoGrinByteCode			:: Maybe GrinByteCodeLamInfo	-- GB specific info
      , laminfoBindAspMp			:: !LamInfoBindAspMp			-- info organized per/keyed on aspect
      }
%%[[20
  deriving (Data,Typeable)
%%]]

instance Show LamInfo where
  show (LamInfo ar _ bc _) = "LamInfo: arity=" ++ show ar ++ " bc=" ++ show bc

emptyLamInfo' :: LamInfo
emptyLamInfo' = LamInfo 0 StackTraceInfo_None (Just emptyGrinByteCodeLamInfo) Map.empty

emptyLamInfo :: LamInfo
emptyLamInfo = LamInfo 0 StackTraceInfo_None Nothing Map.empty

%%]

%%[(20 codegen) hs export(laminfo1stArgIsStackTrace)
laminfo1stArgIsStackTrace :: LamInfo -> Bool
laminfo1stArgIsStackTrace (LamInfo {laminfoStackTrace=StackTraceInfo_IsStackTraceEquiv _}) = True
laminfo1stArgIsStackTrace _                                                                = False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LamMp, map for maintaining implementation info about functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

20100822 AD: Note: lamMpMergeInto and lamMpMergeFrom probably can be combined, but currently subtly differ in the flow of info.

%%[(8 codegen) hs export(LamMp,lamMpMergeInto)
type LamMp    = Map.Map HsName LamInfo

-- propagate from new (left) to prev (right)
lamMpMergeInto :: (LamInfo -> LamInfo -> LamInfo) -> (LamMp -> LamMp -> LamMp) -> LamMp -> LamMp -> LamMp
lamMpMergeInto mergeL2RInfo mergeL2RMp newMp prevMp
  = mergeL2RMp newMpMerge prevMp
  where newMpMerge
          = Map.mapWithKey
              (\n i -> maybe i (mergeL2RInfo i) $ Map.lookup n prevMp
              ) newMp
%%]
lamMpMergeInto2 :: (LamInfo -> LamInfo -> LamInfo) -> (LamMp -> LamMp -> LamMp) -> LamMp -> LamMp -> LamMp
lamMpMergeInto2 mergeL2RInfo mergeL2RMp newMp prevMp
  = mergeL2RMp newMpMerge prevMp
  where newMpMerge
          = lamMpMergeFrom
              Just
              (\(Just x) _ -> x)
              mergeL2RInfo
              mergeL2RMp
              emptyLamInfo
              newMp prevMp

%%[(8 codegen) hs export(lamMpLookupLam,lamMpLookupCaf)
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
%%]

%%[(8 codegen) hs export(lamMpFilterLam,lamMpFilterCaf)
lamMpFilterLam :: LamMp -> LamMp
lamMpFilterLam = Map.filter ((>0) . laminfoArity)

lamMpFilterCaf :: LamMp -> LamMp
lamMpFilterCaf = Map.filter ((==0) . laminfoArity)
%%]

%%[(8 codegen) hs export(lamMpMergeFrom)
-- | merge info from arbitrary map m into LamMp holding LamInfo's
lamMpMergeFrom
  :: (LamInfo -> Maybe x)					-- extract relevant info from a LamInfo
     -> (Maybe x -> LamInfo -> LamInfo)		-- set the info
     -> (z -> x -> x)						-- merge info from new map and old info
     -> LamInfo								-- default, empty LamInfo
     -> Map.Map HsName z					-- arbitrary map holding info to merge
     -> LamMp -> LamMp
lamMpMergeFrom get set merge empty m lm
  = Map.foldWithKey (\n z lm -> Map.alter (Just . upd z) n lm)
                    lm m
  where upd z (Just i) = set (Just (merge z $ maybe emptyExtra id $ get i)) i    
        upd z Nothing  = set (Just (merge z         emptyExtra           )) empty
        emptyExtra = panicJust "lamMpMergeFrom" $ get $ empty
%%]
-- | merge info from arbitrary map m into LamMp holding LamInfo's
lamMpMergeFrom
  :: (LamInfo -> Maybe x)					-- extract relevant info from a LamInfo
     -> (Maybe x -> LamInfo -> LamInfo)		-- set the info
     -> (z -> x -> x)						-- merge info from new map and old info
     -> (LamMp -> LamMp -> LamMp)			-- merge the new into given mp
     -> LamInfo								-- default, empty LamInfo
     -> Map.Map HsName z					-- arbitrary map holding info to merge
     -> LamMp -> LamMp
lamMpMergeFrom get set merge mergeMp empty m lm
  = mergeMp
      (Map.mapWithKey (\n z -> upd z $ Map.lookup n lm) m)
      lm
  where upd z (Just i) = set (Just (merge z $ maybe emptyExtra id $ get i)) i    
        upd z Nothing  = set (Just (merge z         emptyExtra           )) empty
        emptyExtra = panicJust "lamMpMergeFrom" $ get $ empty

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Function Info to be exported as part of LamInfo
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen grin) hs export(GrinByteCodeLamInfo(..),emptyGrinByteCodeLamInfo)
data GrinByteCodeLamInfo
  = GrinByteCodeLamInfo
      { gblaminfoFuninfoKey		:: !Int					-- index into FunctionInfo table, to be referred to outside this module only
      }
  deriving
     ( Show
%%[[20
     , Typeable, Data
%%]]
     )

emptyGrinByteCodeLamInfo :: GrinByteCodeLamInfo
emptyGrinByteCodeLamInfo = GrinByteCodeLamInfo (-1)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: ForceEval, Serializable
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen grin) hs
instance Serialize GrinByteCodeLamInfo where
  sput (GrinByteCodeLamInfo a) = sput a
  sget = liftM  GrinByteCodeLamInfo sget

instance Serialize LamInfoBindAsp where
  sput (LamInfoBindAsp_RelevTy a) = sputWord8 0 >> sput a
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM  LamInfoBindAsp_RelevTy sget
%%]

