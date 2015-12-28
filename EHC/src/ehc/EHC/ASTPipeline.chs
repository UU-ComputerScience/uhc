%%[0 hs
{-# LANGUAGE GADTs, TemplateHaskell, KindSignatures #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC description of the compiler pipeline
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Used by all compiler driver code

%%[8 module {%{EH}EHC.ASTPipeline}
%%]

-- general imports
%%[8 import({%{EH}Base.Common}, {%{EH}Base.Optimize}, {%{EH}Base.Target}, {%{EH}Opts.Base})
%%]

%%[8 import(qualified Data.Set as Set)
%%]
%%[8 import(Data.Monoid)
%%]

%%[8 import(GHC.Generics)
%%]

%%[8 import(UHC.Util.Pretty, UHC.Util.FPath, UHC.Util.Utils)
%%]

%%[8 import({%{EH}EHC.ASTTypes}) export(module {%{EH}EHC.ASTTypes})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTType & file variation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTType(..))
-- | An 'Enum' of all types of ast we can deal with.
-- Order matters, ordering corresponds to 'can be generated from'.
-- 20150804: made more explicit via `ASTPipe`.
data ASTType
  = ASTType_HS
  | ASTType_EH
%%[[(8 core)
  | ASTType_Core
%%]]
%%[[(8 corerun)
  | ASTType_CoreRun
  | ASTType_ExecCoreRun
%%]]
%%[[(8 grin)
  | ASTType_Grin
  | ASTType_GrinBytecode
%%]]
%%[[(8 cmm)
  | ASTType_Cmm
%%]]
%%[[(8 javascript)
  | ASTType_JavaScript
  | ASTType_LibJavaScript
  | ASTType_ExecJavaScript
%%]]
%%[[(8 codegen)
  | ASTType_C
  | ASTType_O
  | ASTType_LibO
  | ASTType_ExecO
%%]]
%%[[50
  | ASTType_HI
%%]]
  | ASTType_Unknown
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded)

instance Hashable ASTType
instance DataAndConName ASTType

instance Show ASTType where
  show = showUnprefixed 1

instance PP ASTType where
  pp = pp . show
%%]

%%[8 export(ASTScope(..))
-- | How much AST
data ASTScope
  = ASTScope_Single			-- ^ single module
  | ASTScope_Whole			-- ^ whole program
  --- | ASTScope_Any			-- ^ any/arbitrary of above
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded)

instance Hashable ASTScope
instance DataAndConName ASTScope

instance Show ASTScope where
  show = showUnprefixed 1

instance PP ASTScope where
  pp = pp . show
%%]

%%[8 export(ASTFileContent(..))
-- | File content variations of ast we can deal with (in principle)
data ASTFileContent
  = ASTFileContent_Text				-- ^ textual, readable input/output
  | ASTFileContent_ASTText			-- ^ textual, readable AST output
  | ASTFileContent_LitText			-- ^ textual, readable input
  | ASTFileContent_Binary			-- ^ binary/serialized input/output
  | ASTFileContent_Unknown
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded)

instance Hashable ASTFileContent
instance DataAndConName ASTFileContent

instance Show ASTFileContent where
  show = showUnprefixed 1

instance PP ASTFileContent where
  pp = pp . show
%%]

%%[8 export(ASTHandlerKey)
-- | Combination of 'ASTType' and 'ASTFileContent' as key into map of handlers
type ASTHandlerKey = (ASTType, ASTFileContent)
%%]

%%[8 export(ASTFileUse(..))
-- | File usage variations of ast
data ASTFileUse
  = ASTFileUse_Cache		-- ^ internal use cache on file
  | ASTFileUse_Dump			-- ^ output: dumped, possibly usable as src later on
  | ASTFileUse_Target		-- ^ output: as target of compilation
  | ASTFileUse_Src			-- ^ input: src file
  | ASTFileUse_SrcImport	-- ^ input: import stuff only from src file
  | ASTFileUse_Unknown		-- ^ unknown
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded)

instance Hashable ASTFileUse
instance DataAndConName ASTFileUse

instance Show ASTFileUse where
  show = showUnprefixed 1

instance PP ASTFileUse where
  pp = pp . show
%%]

%%[8 export(ASTSuffixKey)
-- | Key for allowed suffixes, multiples allowed to cater for different suffixes
type ASTSuffixKey = (ASTFileContent, ASTFileUse)
%%]

%%[8 export(ASTFileTiming(..))
-- | File timing variations of ast
data ASTFileTiming
  = ASTFileTiming_Prev		-- ^ previously generated
  | ASTFileTiming_Current	-- ^ current one
  deriving (Eq, Ord, Enum, Typeable, Generic, Bounded)

instance Hashable ASTFileTiming
instance DataAndConName ASTFileTiming

instance Show ASTFileTiming where
  show = showUnprefixed 1

instance PP ASTFileTiming where
  pp = pp . show
%%]

%%[8 export(ASTAvailableFile(..))
-- | Available File for AST (as checked when first searching for a file)
data ASTAvailableFile =
  ASTAvailableFile
    { _astavailfFPath	:: FPath
    , _astavailfType	:: ASTType
    , _astavailfContent	:: ASTFileContent
    , _astavailfUse		:: ASTFileUse
    -- , _astavailfTiming	:: ASTFileTiming
    }
  deriving (Eq, Ord, Typeable, Generic)

instance Hashable ASTAvailableFile

instance Show ASTAvailableFile where
  show (ASTAvailableFile f t c u {- tm -}) = "(" ++ [head (show t), head (show c), head (show u)] ++ ")"

instance PP ASTAvailableFile where
  pp (ASTAvailableFile f t c u {- tm -}) = f >#< ppParensCommas [pp t, pp c, pp u]

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils 
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(astfileuseReadTiming)
-- | The timing when read
astfileuseReadTiming :: ASTFileUse -> ASTFileTiming
astfileuseReadTiming ASTFileUse_Cache = ASTFileTiming_Prev
astfileuseReadTiming _                = ASTFileTiming_Current
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% When is flowed into global state after semantics type: ASTSemFlowStage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTSemFlowStage(..))
-- | An 'Enum' of the stage semantic results are flown into global state
data ASTSemFlowStage
  = --- | per module
    ASTSemFlowStage_PerModule
%%[[50
  | --- | between subsequent module compilations
    ASTSemFlowStage_BetweenModule	
%%]]
  deriving (Eq, Ord, Typeable, Generic)

instance Hashable ASTSemFlowStage
instance DataAndConName ASTSemFlowStage

instance Show ASTSemFlowStage where
  show = showUnprefixed 1

instance PP ASTSemFlowStage where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pipeline of AST stages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTAlreadyFlowIntoCRSIFromToInfo, ASTAlreadyFlowIntoCRSIInfo)
-- | Key for remembering what already has flown into global state
type ASTAlreadyFlowIntoCRSIFromToInfo =
       ( Maybe ASTType             -- possible 'to ast'
       , ASTType                   -- 'from ast'
       )

-- | Key for remembering what already has flown into global state
type ASTAlreadyFlowIntoCRSIInfo =
    ( ASTSemFlowStage
    , Maybe ASTAlreadyFlowIntoCRSIFromToInfo                       -- possible extra context
    )
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pipeline of AST stages
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTTrf(..))
-- | Description of transformation
data ASTTrf where
    --- | Optimize, valid for some scopes only
    ASTTrf_Optim :: [ASTScope] -> ASTTrf
    --- | Check and adapt (source usually)
    ASTTrf_Check :: ASTTrf

  deriving (Eq, Ord, Typeable, Generic)

instance Hashable ASTTrf

instance Show ASTTrf where
  show (ASTTrf_Optim scs) = "Optim" ++ show scs
  show (ASTTrf_Check    ) = "Check"

instance PP ASTTrf where
  pp (ASTTrf_Optim scs) = "Optim" >#< ppBracketsCommas scs
  pp trf                = pp (show trf)
%%]

%%[50 export(ASTPipeHowChoice(..))
-- | Description of transformation
data ASTPipeHowChoice
  = ASTPipeHowChoice_Avail     		-- ^ First available
  | ASTPipeHowChoice_Overr     		-- ^ First available, but with priority to commandline specified (i.e. overridden)
  | ASTPipeHowChoice_Newer     		-- ^ First newest available, based on timestamps and imports; this triggers recompilation based on module import dependencies
  deriving (Eq, Ord, Typeable, Generic, Enum, Bounded)

instance Hashable ASTPipeHowChoice
instance DataAndConName ASTPipeHowChoice

instance Show ASTPipeHowChoice where
  show = showUnprefixed 1

instance PP ASTPipeHowChoice where
  pp = pp . show
%%]

%%[8 export(ASTPipe(..), emptyASTPipe)
-- | Description of build pipelines
data ASTPipe where
    --- | From source file. 20150904: astpTiming is ignored, derived from astpUse
    ASTPipe_Src :: { astpUse :: ASTFileUse, astpTiming :: ASTFileTiming, astpType :: ASTType } -> ASTPipe

    --- | Derived from (a different ast type)
    ASTPipe_Derived :: { astpType :: ASTType, astpPipe :: ASTPipe } -> ASTPipe

    --- | Linked/combined as a library
    ASTPipe_Library :: { astpType :: ASTType, astpPipe :: ASTPipe } -> ASTPipe

    --- | Transformation on same AST
    ASTPipe_Trf :: { astpType :: ASTType, astpTrf :: ASTTrf, astpPipe :: ASTPipe } -> ASTPipe

    --- | Aggregrate of various different AST
    ASTPipe_Compound :: { astpType :: ASTType, astpPipes :: [ASTPipe] } -> ASTPipe

    --- | No pipe
    ASTPipe_Empty :: ASTPipe

%%[[50
    --- | From previously cached on file
    -- ASTPipe_Cached :: { astpType :: ASTType }  -> ASTPipe

    --- | Side effect inverse of ASTPipe_Cached, i.e. write/cache on file
    ASTPipe_Cache :: { astpType :: ASTType, astpPipe :: ASTPipe } -> ASTPipe

    --- | Choose between first available, left biased, left one is src/cached, right one may be more complex/derived
    ASTPipe_Choose :: { astpHow :: ASTPipeHowChoice, astpType :: ASTType, astpPipe1 :: ASTPipe, astpPipe2 :: ASTPipe } -> ASTPipe

    --- | Whole program linked
    ASTPipe_Whole :: { astpType :: ASTType, astpPipe :: ASTPipe } -> ASTPipe
%%]]
  deriving (Eq, Ord, Typeable, Generic)

emptyASTPipe :: ASTPipe
emptyASTPipe = ASTPipe_Empty

instance Hashable ASTPipe

instance Show ASTPipe where
  show p = case p of
    ASTPipe_Src 		u tm t		-> "Sr(" ++ show u ++ "," ++ show tm ++ "," ++ show t ++ ")."
    ASTPipe_Derived 	t p'	    -> "Dr-" ++ show p'
    ASTPipe_Library 	t p'	    -> "Lb-"  ++ show p'
    ASTPipe_Trf 		t tr p'     -> "Tr(" ++ show tr ++ ")-" ++ show p'
    ASTPipe_Compound 	t ps	    -> "Cm(" ++ show t ++ ")" ++ show ps
    ASTPipe_Empty 				    -> ""
%%[[50    
    -- ASTPipe_Cached 		t		    -> "Cd(" ++ show t ++ ")."
    ASTPipe_Cache 		t p'	    -> "Ch-" ++ show p'
    ASTPipe_Choose		h t p1 p2   -> "{(" ++ show h ++ ")1:" ++ show p1 ++ ",2:" ++ show p2 ++ "}"
    ASTPipe_Whole 		t p'	    -> "Wh-" ++ show p'
%%]]

instance PP ASTPipe where
  pp p = case p of
    ASTPipe_Src 		u tm t	 	-> "Src" >#< u >#< tm >#< t
    ASTPipe_Derived 	t p'	    -> "Deriv" >#< t >-< indent 2 p'
    ASTPipe_Library 	t p'	    -> "Lib" >#< t >-< indent 2 p'
    ASTPipe_Trf 		t tr p'     -> "Trf" >#< t >#< tr >-< indent 2 p'
    ASTPipe_Compound 	t ps	    -> "All" >#< t >-< indent 2 (ppCurlysCommasBlock ps)
    ASTPipe_Empty 				    -> pp "Emp"
%%[[50    
    -- ASTPipe_Cached 		t		    -> "Cached" >#< t
    ASTPipe_Cache 		t p'	    -> "Cache" >#< t >-< indent 2 p'
    ASTPipe_Choose		h t p1 p2   -> "Choose" >#< h >#< t >-< indent 2 (p1 >-< p2)
    ASTPipe_Whole 		t p'	    -> "Whole" >#< t >-< indent 2 p'
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AST build choice
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(TmChoice(..))
-- | Fix of possible choices in an ASTPipe (based on time info)
data TmChoice
  = Choice_End						-- ^ base case
  | Choice_Src	ASTAvailableFile	-- ^ base case: src file
  | Choice_No 	TmChoice			-- ^ no choice made
  | Choices 	[TmChoice]			-- ^ compound
%%[[50
  | Choice_L 	TmChoice			-- ^ fst (of ASTPipe_Choose)
  | Choice_R 	TmChoice			-- ^ snd (of ASTPipe_Choose)
%%]]
  deriving (Eq, Ord, Typeable, Generic)

instance Hashable TmChoice

instance Show TmChoice where
  show (Choice_End  ) = "."
  show (Choice_Src s) = "S" ++ show s
  show (Choice_No  c) = "-" ++ show c
  show (Choices   cs) = show cs
%%[[50
  show (Choice_L   c) = "L" ++ show c
  show (Choice_R   c) = "R" ++ show c
%%]]

instance PP TmChoice where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% AST build plan
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTBuildPlan(..))
data ASTBuildPlan
  = ASTBuildPlan
      { _astbplPipe		:: ASTPipe		-- ^ the pipe and its choices
      , _astbplChoice	:: TmChoice		-- ^ the choices made based on time info
      }
  deriving (Eq, Ord, Typeable, Generic)

instance Hashable ASTBuildPlan

instance Show ASTBuildPlan where
  show (ASTBuildPlan p c) = "Plan [" ++ show c ++ ", " ++ show p ++ "]"

instance PP ASTBuildPlan where
  pp (ASTBuildPlan p c) = "Plan" >#< (c >-< p)
%%]

%%[8 export(mkBuildPlan)
mkBuildPlan :: ASTPipe -> TmChoice -> ASTBuildPlan
mkBuildPlan = ASTBuildPlan
%%]

%%[8 export(astplMbSubPlan)
-- | Extract a subplan (i.e. from which is derived), if any
astplMbSubPlan :: ASTBuildPlan -> Maybe ASTBuildPlan
astplMbSubPlan pl = case pl of
    ASTBuildPlan (ASTPipe_Derived {astpPipe =p'}) (Choice_No c') -> Just $ mkBuildPlan p' c'
    ASTBuildPlan (ASTPipe_Trf     {astpPipe =p'}) (Choice_No c') -> Just $ mkBuildPlan p' c'
%%[[50
    ASTBuildPlan (ASTPipe_Choose  {astpPipe1=p'}) (Choice_L  c') -> Just $ mkBuildPlan p' c'
    ASTBuildPlan (ASTPipe_Choose  {astpPipe2=p'}) (Choice_R  c') -> Just $ mkBuildPlan p' c'
    ASTBuildPlan (ASTPipe_Cache   {astpPipe =p'}) (Choice_No c') -> Just $ mkBuildPlan p' c'
    ASTBuildPlan (ASTPipe_Whole   {astpPipe =p'}) (Choice_No c') -> Just $ mkBuildPlan p' c'
%%]]
    _                                                            -> Nothing


%%]

%%[8 export(astplFind)
-- | Find occurrence of predicate, yielding a new plan corresponding to the predicate, and
astplFind :: (ASTPipe -> Maybe x) -> ASTBuildPlan -> Maybe (ASTBuildPlan, x)
astplFind pred = fnd
  where
    fnd pl@(ASTBuildPlan p c) = case pred p of
      Just res -> Just (pl, res)
      _        -> astplMbSubPlan pl >>= fnd
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Utils on ASTPipe
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(mkASTPipeForASTTypes)
-- | Construct a (dummy) pipe for which `astpTypes` will yield the given ASTTypes
mkASTPipeForASTTypes :: [ASTType] -> ASTPipe
mkASTPipeForASTTypes ts@(t:_) = ASTPipe_Compound t $ map (flip ASTPipe_Compound []) ts
%%]

%%[8
-- | Fold over ASTPipe
astpFold :: (ASTPipe -> x) -> (x -> x -> x) -> x -> ASTPipe -> x
astpFold getx cmbx dfltx p = extr p
  where
    extr ASTPipe_Empty = dfltx
    extr p             = foldr1 cmbx $ getx p : (map extr $ subs p)
    subs (ASTPipe_Derived {astpPipe =p }) = [p]
    subs (ASTPipe_Library {astpPipe =p }) = [p]
    subs (ASTPipe_Trf     {astpPipe =p }) = [p]
    subs (ASTPipe_Compound{astpPipes=ps}) = ps
%%[[50
    subs (ASTPipe_Cache   {astpPipe =p }) = [p]
    subs (ASTPipe_Whole   {astpPipe =p }) = [p]
    subs (ASTPipe_Choose  {astpPipe1=p1, astpPipe2=p2})
                                          = [p1, p2]
%%]]
    subs _                                = []

-- | Fold monoidal over ASTPipe
astpFoldMonoid :: Monoid x => (ASTPipe -> x) -> ASTPipe -> x
astpFoldMonoid getx p = astpFold getx mappend mempty p
%%]

%%[8 export(astpTypes)
-- | Extract all ASTTypes.
astpTypes :: ASTPipe -> Set.Set ASTType
astpTypes = astpFoldMonoid (Set.singleton . astpType)
%%]

%%[8 export(astpFind)
-- | Find occurrence of predicate
astpFind :: (ASTPipe -> Maybe ASTPipe) -> ASTPipe -> Maybe ASTPipe
astpFind pred = astpFold pred (>>) Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pipeline constructing configuration
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTPipeBldCfg(..), emptyASTPipeBldCfg)
data ASTPipeBldCfg =
  ASTPipeBldCfg
    { apbcfgTarget			:: Target
    , apbcfgOptimScope 		:: OptimizationScope
    , apbcfgLinkingStyle	:: LinkingStyle
%%[[(8 corerun)
    , apbcfgRunOnly			:: Bool
    , apbcfgLoadOnly		:: Bool
%%]]
    }

emptyASTPipeBldCfg =
  ASTPipeBldCfg
    defaultTarget OptimizationScope_PerModule LinkingStyle_Exec
%%[[(8 corerun)
    False
    False
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pipeline smart/predefined constructors
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(astpipe_EH_from_HS)
astpipe_HS_src = ASTPipe_Src ASTFileUse_Src ASTFileTiming_Current ASTType_HS
astpipe_EH_from_HS = ASTPipe_Derived ASTType_EH astpipe_HS_src

astpipe_EH = 
  astpipe_EH_from_HS
%%]

%%[(8 core)
astpipe_Core_from_EH :: ASTPipeBldCfg -> ASTPipe
astpipe_Core_from_EH apbcfg = 
%%[[50
    ASTPipe_Cache ASTType_Core $
%%]]
    optim $
    ASTPipe_Derived ASTType_Core astpipe_EH
  where
    optim
%%[[50
      --- | apbcfgOptimScope apbcfg == OptimizationScope_WholeCore = id
%%]]
      | otherwise                                              = ASTPipe_Trf ASTType_Core $ ASTTrf_Optim [ASTScope_Single]
%%]

%%[(8 corein)
astpipe_Core_src :: ASTPipe
astpipe_Core_src = ASTPipe_Src ASTFileUse_Src ASTFileTiming_Current ASTType_Core
%%]

%%[(50 core)
astpipe_Core_cached :: ASTPipe
astpipe_Core_cached = ASTPipe_Src ASTFileUse_Cache ASTFileTiming_Prev ASTType_Core
%%]

%%[(8 core)
astpipe_Core :: ASTPipeBldCfg -> ASTPipe
astpipe_Core apbcfg =
%%[[(50 corein)
  apbcfgLoadChoice apbcfg ASTType_Core
    astpipe_Core_src $
%%]]
      (
%%[[(50 corerun)
       if apbcfgLoadOnly apbcfg
       then
         astpipe_Core_cached
       else
%%]]
         whole $
%%[[50
         ASTPipe_Choose ASTPipeHowChoice_Newer ASTType_Core (ASTPipe_Compound ASTType_Core [astpipe_HI_cached, astpipe_Core_cached]) $
%%]]
             astpipe_Core_from_EH apbcfg
      )
  where
    whole
%%[[50
      | apbcfgOptimScope apbcfg == OptimizationScope_WholeCore
%%[[(50 corerun)
        && not (apbcfgLoadOnly apbcfg)
%%]]
                                                               = (ASTPipe_Trf ASTType_Core $ ASTTrf_Optim [ASTScope_Whole]) . ASTPipe_Whole ASTType_Core
%%]]
      | otherwise                                              = id
    cached
%%[[8
      | otherwise                                              = id
%%][(50 corerun)
      | apbcfgLoadOnly apbcfg                                  = ASTPipe_Choose ASTPipeHowChoice_Newer ASTType_Core astpipe_Core_cached
      | otherwise                                              = ASTPipe_Choose ASTPipeHowChoice_Newer ASTType_Core (ASTPipe_Compound ASTType_Core [astpipe_HI_cached, astpipe_Core_cached])
%%][50
      | otherwise                                              = ASTPipe_Choose ASTPipeHowChoice_Newer ASTType_Core (ASTPipe_Compound ASTType_Core [astpipe_HI_cached, astpipe_Core_cached])
%%]]
%%]

%%[(8 corerunin)
astpipe_CoreRun_src :: ASTPipe
astpipe_CoreRun_src =
%%[[50
  ASTPipe_Trf ASTType_CoreRun ASTTrf_Check $
%%]]
    ASTPipe_Src ASTFileUse_Src ASTFileTiming_Current ASTType_CoreRun
%%]

%%[(50 corerun)
astpipe_CoreRun_cached :: ASTPipe
astpipe_CoreRun_cached = 
%%[[50
  ASTPipe_Trf ASTType_CoreRun ASTTrf_Check $
%%]]
    ASTPipe_Src ASTFileUse_Cache ASTFileTiming_Prev ASTType_CoreRun
%%]

%%[(8 core corerun)
astpipe_CoreRun_from_Core :: ASTPipeBldCfg -> ASTPipe
astpipe_CoreRun_from_Core apbcfg =
  ASTPipe_Derived ASTType_CoreRun $ astpipe_Core apbcfg
%%]

%%[(8 core corerun)
astpipe_CoreRun :: ASTPipeBldCfg -> ASTPipe
astpipe_CoreRun apbcfg =
%%[[(50 corerunin)
  apbcfgLoadChoice apbcfg ASTType_CoreRun
    astpipe_CoreRun_src $
%%]]
%%[[50
      apbcfgLoadChoice apbcfg ASTType_CoreRun
        astpipe_CoreRun_cached $
%%]]
          astpipe_CoreRun_from_Core apbcfg
%%]

%%[(8 core grin)
astpipe_Grin_from_Core :: ASTPipeBldCfg -> ASTPipe
astpipe_Grin_from_Core apbcfg = ASTPipe_Derived ASTType_Grin $ astpipe_Core apbcfg
%%]

%%[(8 core grin)
astpipe_Grin :: ASTPipeBldCfg -> ASTPipe
astpipe_Grin apbcfg =
    astpipe_Grin_from_Core apbcfg
%%]

%%[50
astpipe_HI_cached :: ASTPipe
astpipe_HI_cached = ASTPipe_Src ASTFileUse_Cache ASTFileTiming_Prev ASTType_HI

astpipe_HI :: ASTPipeBldCfg -> ASTPipe
astpipe_HI apbcfg =
  ASTPipe_Choose ASTPipeHowChoice_Newer ASTType_HI
    astpipe_HI_cached $
      ASTPipe_Cache ASTType_HI $
      ASTPipe_Derived ASTType_HI $
      ASTPipe_Compound ASTType_HI $
        [astpipe_HS_src, astpipe_EH]
%%[[(50 core)
        ++ [astpipe_Core_from_EH apbcfg]
%%]]
%%[[(50 grin core)
        ++ [astpipe_Grin apbcfg]
%%]]
%%]

%%[(8 core javascript)
astpipe_JavaScript_from_Core :: ASTPipeBldCfg -> ASTPipe
astpipe_JavaScript_from_Core apbcfg = ASTPipe_Derived ASTType_JavaScript $ astpipe_Core apbcfg
%%]

%%[(8 core javascript)
astpipe_JavaScript :: ASTPipeBldCfg -> ASTPipe
astpipe_JavaScript apbcfg =
    astpipe_JavaScript_from_Core apbcfg
%%]

%%[(8 grin)
astpipe_GrinBytecode_from_Grin :: ASTPipeBldCfg -> ASTPipe
astpipe_GrinBytecode_from_Grin apbcfg = ASTPipe_Derived ASTType_GrinBytecode $ astpipe_Grin apbcfg
%%]

%%[(8 core grin)
astpipe_GrinBytecode :: ASTPipeBldCfg -> ASTPipe
astpipe_GrinBytecode apbcfg =
    astpipe_GrinBytecode_from_Grin apbcfg
%%]

%%[(8 codegen)
astpipe_C_src :: ASTPipe
astpipe_C_src = ASTPipe_Src ASTFileUse_Src ASTFileTiming_Current ASTType_C
%%]

%%[(8 codegen core grin)
astpipe_C_from_GrinBytecode :: ASTPipeBldCfg -> ASTPipe
astpipe_C_from_GrinBytecode apbcfg = ASTPipe_Derived ASTType_C $ astpipe_GrinBytecode apbcfg
%%]

%%[(8 core grin)
astpipe_C :: ASTPipeBldCfg -> ASTPipe
astpipe_C apbcfg =
%%[[50
  ASTPipe_Choose ASTPipeHowChoice_Newer ASTType_C
    astpipe_C_src $
%%]]
      astpipe_C_from_GrinBytecode apbcfg

astpipe_O_from_C :: ASTPipeBldCfg -> ASTPipe
astpipe_O_from_C apbcfg = ASTPipe_Derived ASTType_O $ astpipe_C apbcfg

astpipe_O :: ASTPipeBldCfg -> ASTPipe
astpipe_O apbcfg =
    astpipe_O_from_C apbcfg

{-
astpipe_LinkO_from_O apbcfg = ASTPipe_Derived ASTType_LibO $ astpipe_O apbcfg

astpipe_LinkO apbcfg =
    astpipe_LinkO_from_O apbcfg

astpipe_ExecO_from_LinkO apbcfg = ASTPipe_Derived ASTType_ExecO $ astpipe_LinkO apbcfg

astpipe_ExecO apbcfg =
    astpipe_ExecO_from_LinkO apbcfg
-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pipeline utils
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
apbcfgLoadChoice :: ASTPipeBldCfg -> ASTType -> ASTPipe -> ASTPipe -> ASTPipe
apbcfgLoadChoice apbcfg -- t p1 p2
%%[[(50 corerun)
    | apbcfgLoadOnly apbcfg = ASTPipe_Choose ASTPipeHowChoice_Overr
%%]]
    | otherwise             = ASTPipe_Choose ASTPipeHowChoice_Newer
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Pipeline for a configuration (i.e. target and optimization scope)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(astpipeForCfg)
-- | Construct a ASTPipe from build config
astpipeForCfg :: ASTPipeBldCfg -> ASTPipe
astpipeForCfg apbcfg =
    if onlyrun
    then cmb base
    else cmb $  base
%%[[50
             ++ [astpipe_HI $ apbcfg {apbcfgOptimScope = OptimizationScope_PerModule}]
%%]]
  where (base, topty) = case apbcfgTarget apbcfg of
%%[[(8 grin)
          Target_Interpreter_Grin_C
%%[[99
            | apbcfgLinkingStyle apbcfg == LinkingStyle_Pkg  -> ([ASTPipe_Derived ASTType_LibO $ astpipe_O apbcfg], ASTType_LibO)
%%]]
            | apbcfgLinkingStyle apbcfg == LinkingStyle_Exec -> ([ASTPipe_Derived ASTType_ExecO $ astpipe_O apbcfg], ASTType_ExecO)
%%]]
%%[[(8 javascript)
          Target_Interpreter_Core_JavaScript
%%[[99
            | apbcfgLinkingStyle apbcfg == LinkingStyle_Pkg  -> ([ASTPipe_Derived ASTType_LibJavaScript $ astpipe_JavaScript apbcfg], ASTType_LibJavaScript)
%%]]
            | apbcfgLinkingStyle apbcfg == LinkingStyle_Exec -> ([ASTPipe_Derived ASTType_ExecJavaScript $ astpipe_JavaScript apbcfg], ASTType_ExecJavaScript)
%%]]
%%[[(8 core)
          Target_None_Core_AsIs
%%[[(8 corerun)
            --- | onlyrun 										 -> ([astpipe_Core_src], ASTType_Core)
            | onlyrun 										 -> ([astpipe_CoreRun apbcfg], ASTType_CoreRun)
%%]]
            | otherwise                                      -> ([astpipe_Core apbcfg], ASTType_Core)
%%]]
%%[[(8 corerun)
          Target_None_Core_CoreRun
                                                             -> ([astpipe_CoreRun apbcfg], ASTType_CoreRun)
%%]]
          _                                                  -> ([astpipe_EH], ASTType_EH)

        cmb [p] = p
        cmb ps  = ASTPipe_Compound topty ps
        
%%[[(8 corerun)
        -- 20150806: hackish solution for running core, done to be able to try out new build framework
        onlyrun = apbcfgRunOnly apbcfg
%%][8
        onlyrun = False
%%]]
%%]

%%[8 export(astpipeForEHCOpts)
-- | Construct a ASTPipe from compilers options, see `astpipeForCfg`
astpipeForEHCOpts :: EHCOpts -> ASTPipe
astpipeForEHCOpts opts = astpipeForCfg $ emptyASTPipeBldCfg
  { apbcfgTarget		= ehcOptTarget opts
  , apbcfgOptimScope	= ehcOptOptimizationScope opts
%%[[8
  , apbcfgLinkingStyle	= LinkingStyle_Exec
%%][50
  , apbcfgLinkingStyle	= ehcOptLinkingStyle opts
%%]]
%%[[(8 corerun)
  , apbcfgRunOnly		= CoreOpt_Run `elem` ehcOptCoreOpts opts
  , apbcfgLoadOnly		= CoreOpt_LoadOnly `elem` ehcOptCoreOpts opts
%%]]
  }
%%]


