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

Note (20100301 AD): this def is asymmetric, that is Core info always is there, the rest is optional.
Instead of everything being equal.
Obvious action: fix it one day.

%%[(8 codegen) hs export(StackTraceInfo(..),LamInfo(..),emptyLamInfo')
data StackTraceInfo
  = StackTraceInfo_None
  | StackTraceInfo_HasStackTraceEquiv	HsName		-- has a stack traced equivalent
  | StackTraceInfo_IsStackTraceEquiv	HsName		-- is a stack traced equivalent
%%[[20
  deriving (Data,Typeable)
%%]]

data LamInfo
  = LamInfo
      { laminfoArity				:: !Int							-- arity of function
      , laminfoStackTrace  			:: !StackTraceInfo				-- stacktrace
      , laminfoGrinByteCode			:: Maybe GrinByteCodeLamInfo	-- GB specific info
      }
%%[[20
  deriving (Data,Typeable)
%%]]

instance Show LamInfo where
  show (LamInfo ar _ bc) = "LamInfo: arity=" ++ show ar ++ " bc=" ++ show bc

emptyLamInfo' :: LamInfo
emptyLamInfo' = LamInfo 0 StackTraceInfo_None (Just emptyGrinByteCodeLamInfo)

emptyLamInfo :: LamInfo
emptyLamInfo = LamInfo 0 StackTraceInfo_None Nothing

%%]

%%[(20 codegen) hs export(laminfo1stArgIsStackTrace)
laminfo1stArgIsStackTrace :: LamInfo -> Bool
laminfo1stArgIsStackTrace (LamInfo {laminfoStackTrace=StackTraceInfo_IsStackTraceEquiv _}) = True
laminfo1stArgIsStackTrace _                                                                = False
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% LamMp, map for maintaining implementation info about functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(LamMp,lamMpUpdateWith)
type LamMp    = Map.Map HsName LamInfo

-- propagate from previous (left) to new (right) only the stacktrace info
lamMpUpdateWith :: LamMp -> LamMp -> LamMp
lamMpUpdateWith prev new
  = Map.mapWithKey
      (\n i
         -> maybe i (\(LamInfo {laminfoStackTrace=t}) -> i {laminfoStackTrace=t})
            $ Map.lookup n prev
      ) new
%%]

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

%%[(2020 codegen) hs export(lamMpUpLift,lamMpDownLift)
-- | map to a new (type based) variant of LamMp, adding info
lamMpUpLift :: (Maybe x -> y) -> LamMp' x -> LamMp' y
lamMpUpLift f = Map.map (\i -> i {laminfoMbExtra = Just $ f (laminfoMbExtra i)})

-- | map to a new (type based) variant of LamMp, removing extra info
lamMpDownLift :: LamMp' x -> LamMp
lamMpDownLift = Map.map (\i -> i {laminfoMbExtra = Nothing})
%%]

%%[(20 codegen) hs export(lamMpMergeFrom)
lamMpMergeFrom :: (LamInfo -> Maybe x) -> (Maybe x -> LamInfo -> LamInfo) -> (z -> x -> x) -> LamInfo -> Map.Map HsName z -> LamMp -> LamMp
lamMpMergeFrom get set merge empty m lm
  = foldr
      (\(n,z) lm -> Map.alter (Just . upd z) n lm)
      lm (Map.toList m)
  where upd z (Just i) = set (Just (merge z $ maybe emptyExtra id $ get i)) i    
        upd z Nothing  = set (Just (merge z         emptyExtra           )) empty
        emptyExtra = panicJust "lamMpMergeFrom" $ get $ empty
%%]

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
%%]

%%[(99 codegen grin) hs
%%]
instance ForceEval GrinByteCodeLamInfo
