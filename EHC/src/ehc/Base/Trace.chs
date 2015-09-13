%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing in IO setting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Trace}
%%]

%%[8 import(UHC.Util.Pretty, UHC.Util.Utils)
%%]

%%[8 import(GHC.Generics(Generic), Data.Typeable)
%%]

%%[8 import(Control.Monad, Control.Monad.IO.Class)
%%]

%%[8 import(qualified Data.Map as Map)
%%]

%%[8 import({%{EH}Base.Common})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(TraceOn(..), allTraceOnMp)
-- | Trace on specific topic(s)
data TraceOn
  = TraceOn_BuildFun				-- build functions (bcall, ...)
  | TraceOn_BuildFlow				-- build flow
  | TraceOn_BuildFPaths				-- build fpaths constructed
  | TraceOn_BuildSearchPaths		-- build searchpath used
  | TraceOn_BuildSccImports			-- build compile order (scc = strongly connected components)
  | TraceOn_BuildTypeables			-- build Typeable instances encountered
  | TraceOn_BuildPipe				-- build Pipe related
  | TraceOn_BuildPlan				-- build Plan related
  | TraceOn_BuildFold				-- build folds related
  | TraceOn_BuildTimes				-- build file times related
  | TraceOn_BuildResult				-- build results related
  | TraceOn_BuildImport				-- build import related
  | TraceOn_BuildRef				-- build reference related
  | TraceOn_BuildMod				-- build module related
%%[[(8 corerun)
  | TraceOn_RunMod					-- run module related
  | TraceOn_RunHeap					-- run heap related
  | TraceOn_RunGlobals				-- run globals related
  | TraceOn_RunFrame				-- run frame (minimally) related
  | TraceOn_RunFrames				-- run frames related
  | TraceOn_RunEval					-- run evaluation related
  | TraceOn_RunRef					-- run reference related
%%]]
  deriving (Eq,Ord,Enum,Show,Typeable,Bounded,Generic)

instance DataAndConName TraceOn

allTraceOnMp :: Map.Map String TraceOn
allTraceOnMp = str2stMpWithShow (strToLower . showUnprefixed 1) 
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing based on trace config/options, intended to be wrapped around in specific trace functions, hence the INLINE
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(trOnPP, trOn)
-- | Tracing PPs
trOnPP :: (Monad m, MonadIO m) => (TraceOn -> Bool) -> TraceOn -> [PP_Doc] -> m ()
trOnPP onTr ton ms = when (onTr ton) $ liftIO $ pr ms
  where pr []      = return ()
        pr [m]     = putPPLn $ show ton >|< ":" >#< m
        pr (m:ms)  = do pr [m]
                        forM_ ms $ \m -> putPPLn $ indent 2 m
{-# INLINE trOnPP #-}

-- | Tracing Strings
trOn :: (Monad m, MonadIO m) => (TraceOn -> Bool) -> TraceOn -> [String] -> m ()
trOn onTr ton ms = trOnPP onTr ton $ map pp ms
{-# INLINE trOn #-}
%%]

