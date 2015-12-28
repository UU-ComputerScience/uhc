%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing in IO setting
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Base.Trace}
%%]

%%[1 import(UHC.Util.Pretty, UHC.Util.Utils)
%%]

%%[1 import(GHC.Generics(Generic), Data.Typeable)
%%]

%%[1 import(Control.Monad, Control.Monad.IO.Class)
%%]

%%[1 import(qualified Data.Map as Map, qualified Data.Sequence as Sq, qualified Data.Foldable as Fld)
%%]

%%[1 import(Data.Sequence((><))) export ((><))
%%]

%%[1 import({%{EH}Base.Common})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Tracing flags
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 export(TraceOn(..), allTraceOnMp)
-- | Trace on specific topic(s)
data TraceOn
  = TraceOn_BldFun					-- build functions (bcall, ...)
  | TraceOn_BldFlow					-- build flow
  | TraceOn_BldFPaths				-- build fpaths constructed
  | TraceOn_BldSearchPaths			-- build searchpath used
  | TraceOn_BldSccImports			-- build compile order (scc = strongly connected components)
  | TraceOn_BldTypeables			-- build Typeable instances encountered
  | TraceOn_BldPipe					-- build Pipe related
  | TraceOn_BldPlan					-- build Plan related
  | TraceOn_BldFold					-- build folds related
  | TraceOn_BldTimes				-- build file times related
  | TraceOn_BldResult				-- build results related
  | TraceOn_BldImport				-- build import related
  | TraceOn_BldRef					-- build reference related
  | TraceOn_BldMod					-- build module related
  | TraceOn_HsScc					-- HS scc of name dependency analysis
  | TraceOn_HsDpd					-- HS dpd info of name dependency analysis
  | TraceOn_HsOcc					-- HS name occurrence info
  | TraceOn_EhClsGam				-- EH class gam lookup results
  | TraceOn_EhDataGam				-- EH data gam lookup results
  | TraceOn_EhValGam				-- EH value gam lookup results
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

%%[1 export(TrPP, trppIsEmpty, trppEmpty)
type TrPP = Sq.Seq PP_Doc

trppIsEmpty :: TrPP -> Bool
trppIsEmpty = Sq.null

trppEmpty :: TrPP
trppEmpty = Sq.empty

instance PP TrPP where
  pp = vlist . Fld.toList
%%]

%%[1 export(trPPOnIO, trPP, trOnPP, trOn)
-- | Tracing PPs
trPP :: (TraceOn -> Bool) -> TraceOn -> [PP_Doc] -> TrPP
trPP onTr ton ms = if onTr ton then pr ms else trppEmpty
  where pr []      = trppEmpty
        pr [m]     = Sq.singleton $ show ton >|< ":" >#< m
        pr (m:ms)  = pr [m] >< (Sq.fromList $ map (indent 2) ms)

-- | Dump trace IO monadically
trPPOnIO :: (Monad m, MonadIO m) => TrPP -> m ()
trPPOnIO ppl = liftIO $ mapM_ putPPLn $ Fld.toList ppl

-- | Tracing PPs, producing output on IO
trOnPP :: (Monad m, MonadIO m) => (TraceOn -> Bool) -> TraceOn -> [PP_Doc] -> m ()
trOnPP onTr ton ms = when (onTr ton) $ trPPOnIO $ trPP onTr ton ms
{-# INLINE trOnPP #-}

-- | Tracing Strings, producing output on IO
trOn :: (Monad m, MonadIO m) => (TraceOn -> Bool) -> TraceOn -> [String] -> m ()
trOn onTr ton ms = trOnPP onTr ton $ map pp ms
{-# INLINE trOn #-}
%%]

%%[1 export(ppNestTrPP, ppTrNm)
-- | PP AST in a nested fashion with tracing info
ppNestTrPP
  :: PP a
  => [a]		-- ^ names of node type, child
  -> [PP_Doc]	-- ^ attribute info
  -> [PP_Doc]	-- ^ children
  -> TrPP		-- ^ trace info
  -> PP_Doc
ppNestTrPP nms attrs ps trpp
  = ppListSep "" "" "_" nms
    >#< (   (if null attrs then empty else ppSpaced attrs)
        >-< trpp
        )
    >-< indent 2 (vlist ps)

-- | PP for a name
ppTrNm :: HsName -> PP_Doc
ppTrNm = text . show . show
%%]


