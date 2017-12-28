%%[0 hs
{-# LANGUAGE RecordWildCards #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler' instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
%%]

%%[8 module {%{EH}EHC.ASTHandler.Instances}
%%]

-- general imports
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileUnit}, {%{EH}EHC.CompileRun.Base})
%%]

%%[8 import(qualified {%{EH}Config} as Cfg)
%%]

%%[8 import ({%{EH}EHC.ASTHandler}) export (module {%{EH}EHC.ASTHandler})
%%]

%%[8888 import (Control.Applicative)
%%]

%%[8888 import (Data.Functor.Identity) export(module Data.Functor.Identity)
%%]

%%[8 import (qualified Data.Map as Map, qualified Data.IntMap as IMap, Data.Maybe)
%%]

%%[8888 import (UHC.Util.Hashable)
%%]

%%[8888 import (UHC.Util.Lens)
%%]

%%[8 import (qualified UHC.Util.RelMap as Rel)
%%]

%%[8 import (Data.Typeable)
%%]

%%[8888 import (GHC.Generics)
%%]

%%[8 import({%{EH}Base.ParseUtils})
%%]

%%[50 import(Control.Exception as CE)
%%]

-- Language syntax: HS, EH, Core, TyCore, Grin, ...
%%[8 import({%{EH}EHC.ASTTypes})
%%]
-- Language semantics: Core
%%[(8 codegen) import({%{EH}Core.Trf.EraseExtractTysigCore})
%%]


-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.Main} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- CoreRun output
%%[(8 corerun) import({%{EH}CoreRun} as CoreRun, {%{EH}Core.ToCoreRun}, {%{EH}CoreRun.Pretty})
%%]
-- Core output
%%[(8 codegen coreout) import({%{EH}Core} as Core,{%{EH}Core.Pretty}, {%{EH}Core.PrettyTrace})
%%]
-- Grin input and output
%%[(8 codegen grin) import({%{EH}GrinCode} as Grin,{%{EH}GrinCode.Pretty})
%%]
-- Java output
%%[(8888 codegen java) import({%{EH}Core.ToJava})
%%]
-- JavaScript output
%%[(8 javascript) import({%{EH}JavaScript} as JS,{%{EH}JavaScript.Pretty})
%%]
-- Cmm output
%%[(8 codegen cmm) import({%{EH}Cmm} as Cmm,{%{EH}Cmm.ToC}(cmmMod2C), {%{EH}Cmm.Pretty})
%%]

-- serialization
%%[50 import(qualified UHC.Util.Binary as Bin, UHC.Util.Serialize)
%%]

-- module admin
%%[50 import({%{EH}Module.ImportExport}, {%{EH}CodeGen.ImportUsedModules})
%%]

-- timestamps
%%[50 import(UHC.Util.Time, System.Directory)
%%]

-- parsing
%%[8 import(UU.Parsing, UU.Parsing.Offside)
%%]
%%[8 import(qualified UHC.Util.ScanUtils as ScanUtils, {%{EH}Scanner.Common})
%%]
%%[8 import(UHC.Util.ParseUtils)
%%]

-- EH parser
%%[8 import(qualified {%{EH}EH.Parser} as EHPrs)
%%]
-- HS parser
%%[8 import(qualified {%{EH}HS.Parser} as HSPrs)
%%]
-- HI parser
%%[50 import(qualified {%{EH}HI} as HI)
%%]
-- Core parser
%%[(8 corein) import(qualified {%{EH}Core.Parser} as CorePrs)
%%]
-- CoreRun parser
%%[(8 corerun) import(qualified {%{EH}CoreRun.Parser} as CoreRunPrs)
%%]
-- Grin parser
%%[(8 codegen grinparser) import(qualified {%{EH}GrinCode.Parser} as GrinParser)
%%]

-- config
%%[50 import(qualified {%{EH}Config} as Cfg)
%%]
%%[50 import(qualified {%{EH}SourceCodeSig} as Sig)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': HS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(astHandler'_HS)
astHandler'_HS :: ASTHandler' AST_HS
astHandler'_HS = mk emptyASTHandler'
  where mk (hdlr@(ASTHandler' {..})) =
          emptyASTHandler' -- ASTHandler'
            { _asthdlrName              = "Haskell"
            , _asthdlrASTLens           = Just $ ecuMbHS
            {-
            , _asthdlrSuffixRel			= mkASTSuffixRel
            								[ ( (ASTFileContent_Text	, ASTFileUse_Src), ("hs", ecuMbHS, tmlens) )
            								, ( (ASTFileContent_LitText, ASTFileUse_Src), ("lhs", ecuMbHS, tmlens) )
            								]
            -}
            , _asthdlrSuffixRel			= mkASTSuffixRel'
            								[ ( (ASTFileContent_Text	, ASTFileUse_Src)
            								  , ("hs"
            								    , [ (ASTFileTiming_Current, ecuMbHS)
            								      ]
%%[[8
            								    , []
%%][50
            								    , [ (ASTFileTiming_Current, ecuMbSrcTime)
            								      ]
%%]]
            								    , id
            								  ) )
            								, ( (ASTFileContent_Text	, ASTFileUse_SrcImport)
            								  , ("hs"
            								    , [ (ASTFileTiming_Current, ecuMbHS)
            								      ]
%%[[8
            								    , []
%%][50
            								    , [ (ASTFileTiming_Current, ecuMbSrcTime)
            								      ]
%%]]
%%[[8
            								    , id
%%][50
            								    , \o -> o {ehpoptsOkToStopAtErr=True, ehpoptsForImport=True}
%%]]
            								  ) )
            								, ( (ASTFileContent_LitText	, ASTFileUse_Src)
            								  , ("lhs"
            								    , [ (ASTFileTiming_Current, ecuMbHS)
            								      ]
%%[[8
            								    , []
%%][50
            								    , [ (ASTFileTiming_Current, ecuMbSrcTime)
            								      ]
%%]]
%%[[8
            								    , id
%%][99
            								    , \o -> o {ehpoptsLitMode=True}
%%]]
            								  ) )
            								, ( (ASTFileContent_LitText	, ASTFileUse_SrcImport)
            								  , ("lhs"
            								    , [ (ASTFileTiming_Current, ecuMbHS)
            								      ]
%%[[8
            								    , []
%%][50
            								    , [ (ASTFileTiming_Current, ecuMbSrcTime)
            								      ]
%%]]
%%[[8
            								    , id
%%][50
            								    , \o -> o {ehpoptsOkToStopAtErr=True, ehpoptsForImport=True}
%%][99
            								    , \o -> o {ehpoptsLitMode=True, ehpoptsOkToStopAtErr=True, ehpoptsForImport=True}
%%]]
            								  ) )
            								]
            , _asthdlrEcuStore          = ecuStoreHS
            , _asthdlrParseScanOpts     = \opts _ -> hsScanOpts opts
            , _asthdlrParser            = \opts popts -> Just $ ASTParser $
%%[[50
                                            if ehpoptsForImport popts then HSPrs.pAGItfImport opts else
%%]]
                                            HSPrs.pAGItf opts
            
{-
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            , _asthdlrMkOutputFPath           = _asthdlrMkOutputFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrInput             = _asthdlrInput
-}
            }
        {-
        tmlens =
%%[[8
          Nothing
%%][50
          Just ecuMbSrcTime
%%]]
        -}
            
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': EH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(astHandler'_EH)
astHandler'_EH :: ASTHandler' AST_EH
astHandler'_EH = mk emptyASTHandler'
  where mk (hdlr@(ASTHandler' {..})) = 
          emptyASTHandler' -- ASTHandler'
            { _asthdlrName              = "EH"
            , _asthdlrASTLens           = Just $ ecuMbEH
            , _asthdlrSuffixRel			= mkASTSuffixRel
            								[ ( (ASTFileContent_Text	, ASTFileUse_Src), ("eh", ecuMbEH, Nothing) )
            								]
            , _asthdlrEcuStore          = ecuStoreEH
            , _asthdlrParseScanOpts     = \opts _ -> ehScanOpts opts
            , _asthdlrParser            = \_ _ -> Just $ ASTParser EHPrs.pAGItf
            , _asthdlrPretty			= \_ ecu _ -> fmap EHSem.pp_Syn_AGItf $ _ecuMbEHSem ecu
            
{-
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            , _asthdlrMkOutputFPath           = _asthdlrMkOutputFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrInput             = _asthdlrInput
-}
            }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': HI
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50 export(astHandler'_HI)
astHandler'_HI :: ASTHandler' AST_HI
astHandler'_HI = mk emptyASTHandler'
  where mk (hdlr@(ASTHandler' {..})) = 
          emptyASTHandler' -- ASTHandler'
            { _asthdlrName              = "HI"
            , _asthdlrASTLens           = Just $ ecuMbHIInfo
            , _asthdlrSuffixRel			= mkASTSuffixRel'
            								[ ( (ASTFileContent_Binary	, ASTFileUse_Cache)
            								  , ("hi"
            								    , [ (ASTFileTiming_Current, ecuMbHIInfo)
            								      , (ASTFileTiming_Prev, ecuMbPrevHIInfo)
            								      ]
            								    , [ (ASTFileTiming_Prev, ecuMbHIInfoTime)
            								      ]
            								    , id
            								  ) )
            								]
            , _asthdlrMkInputFPath		= \opts ecu modNm fp suff ->
%%[[50
              									fpathSetSuff suff fp
%%][99
												  -- if outputdir is specified, use that location to possibly read hi from.
              									mkInOrOutputFPathFor (InputFrom_Loc $ ecuFileLocation ecu) opts modNm fp suff
%%]]
            , _asthdlrEcuStore          = ecuStoreHIInfo
			, _asthdlrPutSerializeFileIO= default_asthdlrPutSerializeFileIO
			, _asthdlrGetSerializeFileIO= \opts fp -> fmap Just $
			                                CE.catch (getSGetFile (fpathToStr fp) (HI.sgetHIInfo opts))
                                                     (\(_ :: SomeException) -> return $ HI.emptyHIInfo {HI.hiiValidity = HI.HIValidity_Absent})
            , _asthdlrPostInputCheck	= \opts ecu modNm fp hiinfo -> case HI.hiiValidity hiinfo of
                                               HI.HIValidity_Ok -> []
%%[[99
                                               HI.HIValidity_WrongMagic | not (ecuCanCompile ecu)
                                                 -> [rngLift emptyRange Err_WrongMagic
                                                       (show modNm)
                                                       (fpathToStr fp)
                                                    ]
                                               HI.HIValidity_Inconsistent | not (ecuCanCompile ecu)
                                                 -> [rngLift emptyRange Err_InconsistentHI
                                                       (show modNm)
                                                       (fpathToStr fp)
                                                       [Sig.timestamp, Cfg.installVariant opts, show $ ehcOptTarget opts, show $ ehcOptTargetFlavor opts]
                                                       [HI.hiiSrcTimeStamp hiinfo   , HI.hiiCompiler hiinfo  , show $ HI.hiiTarget hiinfo, show $ HI.hiiTargetFlavor hiinfo]
                                                    ]
%%]]
                                               _ -> []
            , _asthdlrASTIsValid      	= \hiinfo -> HI.hiiValidity hiinfo == HI.HIValidity_Ok
            }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': Core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 core) export(astHandler'_Core)
astHandler'_Core :: ASTHandler' AST_Core
astHandler'_Core = mk emptyASTHandler'
  where mk (hdlr@(ASTHandler' {..})) = 
          emptyASTHandler' -- ASTHandler'
            { _asthdlrName              = "Core"
            , _asthdlrASTLens           = Just $ ecuMbCore
            , _asthdlrSuffixRel			= mkASTSuffixRel
            								[ ( (ASTFileContent_Binary	, ASTFileUse_Target)	, (Cfg.suffixDotlessBinaryCore, ecuMbCore, Nothing) )
            								, ( (ASTFileContent_Text	, ASTFileUse_Src)		, (Cfg.suffixDotlessOutputTextualCore, ecuMbCore, Nothing) )
            								, ( (ASTFileContent_Text	, ASTFileUse_Dump)		, (Cfg.suffixDotlessOutputTextualCore, ecuMbCore, Nothing) )
            								, ( (ASTFileContent_ASTText	, ASTFileUse_Dump)		, (Cfg.suffixDotlessOutputTextualCoreAST, ecuMbCore, Nothing) )
            								, ( (ASTFileContent_Binary	, ASTFileUse_Src)		, (Cfg.suffixDotlessInputOutputBinaryCore, ecuMbCore, Nothing) )
            								, ( (ASTFileContent_Binary	, ASTFileUse_Dump)		, (Cfg.suffixDotlessInputOutputBinaryCore, ecuMbCore, Nothing) )
            								]
            							  `Rel.union`
            							  mkASTSuffixRel'
            								[ ( (ASTFileContent_Binary	, ASTFileUse_Cache)
            								  , (Cfg.suffixDotlessBinaryCore
            								    , [ (ASTFileTiming_Current, ecuMbCore)
            								      , (ASTFileTiming_Prev, ecuMbCore)
            								      ]
%%[[8
            								    , []
%%][50
            								    , [ (ASTFileTiming_Prev, ecuMbCoreTime)
            								      ]
%%]]
            								    , id
            								  ) )
            								]
            , _asthdlrEcuStore          = ecuStoreCore
%%[[(8 corein)
            , _asthdlrParseScanOpts     = \opts _ -> coreScanOpts opts
            , _asthdlrParser            = \opts _ -> Just $ ASTParser (CorePrs.pCModule opts :: EHPrsAna AST_Core)
%%]]
            , _asthdlrPretty			= \opts _ ast -> Just $ ppCModule (opts {- ehcOptCoreOpts = coreOpts ++ ehcOptCoreOpts opts -}) $ cmodTrfEraseTyCore opts ast
            , _asthdlrPrettyTrace		= \opts _ ast -> Just $ ppASTCModule opts ast
%%[[50
			, _asthdlrPutSerializeFileIO= default_asthdlrPutSerializeFileIO
			, _asthdlrGetSerializeFileIO= default_asthdlrGetSerializeFileIO
%%]]
            
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            -- , _asthdlrParseParse        = _asthdlrParseParse
            -- , _asthdlrParseScan         = _asthdlrParseScan
            -- , _asthdlrParser         = _asthdlrParser
{-
            , _asthdlrMkOutputFPath           = _asthdlrMkOutputFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrInput             = _asthdlrInput
-}
            }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': CoreRun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) export(astHandler'_CoreRun)
astHandler'_CoreRun :: ASTHandler' AST_CoreRun
astHandler'_CoreRun = mk emptyASTHandler'
  where mk (hdlr@(ASTHandler' {..})) = 
          emptyASTHandler' -- ASTHandler'
            { _asthdlrName              = "CoreRun"
            , _asthdlrASTLens           = Just $ ecuMbCoreRun
            , _asthdlrSuffixRel			= mkASTSuffixRel
            								[ ( (ASTFileContent_Binary	, ASTFileUse_Target)	, (Cfg.suffixDotlessBinaryCoreRun, ecuMbCoreRun, Nothing) )
            								, ( (ASTFileContent_Text	, ASTFileUse_Src)		, (Cfg.suffixDotlessOutputTextualCoreRun, ecuMbCoreRun, Nothing) )
            								, ( (ASTFileContent_Text	, ASTFileUse_Dump)		, (Cfg.suffixDotlessOutputTextualCoreRun, ecuMbCoreRun, Nothing) )
            								, ( (ASTFileContent_Binary	, ASTFileUse_Src)		, (Cfg.suffixDotlessInputOutputBinaryCoreRun, ecuMbCoreRun, Nothing) )
            								, ( (ASTFileContent_Binary	, ASTFileUse_Dump)		, (Cfg.suffixDotlessInputOutputBinaryCoreRun, ecuMbCoreRun, Nothing) )
            								]
            							  `Rel.union`
            							  mkASTSuffixRel'
            								[ ( (ASTFileContent_Binary	, ASTFileUse_Cache)
            								  , (Cfg.suffixDotlessBinaryCoreRun
            								    , [ (ASTFileTiming_Current, ecuMbCoreRun)
            								      , (ASTFileTiming_Prev, ecuMbCoreRun)
            								      ]
%%[[8
            								    , []
%%][50
            								    , [ (ASTFileTiming_Prev, ecuMbCoreRunTime)
            								      ]
%%]]
            								    , id
            								  ) )
            								]
            , _asthdlrEcuStore          = ecuStoreCoreRun
%%[[(8 corein)
            , _asthdlrParseScanOpts     = \opts _ -> corerunScanOpts
            , _asthdlrParser            = \opts _ -> Just $ ASTParser (CoreRunPrs.pMod opts :: EHPrsAna AST_CoreRun)
%%]]
            , _asthdlrPretty			= \opts _ ast -> Just $ ppMod' opts ast
%%[[50
			, _asthdlrPutSerializeFileIO= default_asthdlrPutSerializeFileIO
			, _asthdlrGetSerializeFileIO= default_asthdlrGetSerializeFileIO
%%]]
            
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            -- , _asthdlrParseParse        = _asthdlrParseParse
            -- , _asthdlrParseScan         = _asthdlrParseScan
            -- , _asthdlrParser         = _asthdlrParser
{-
            , _asthdlrMkOutputFPath           = _asthdlrMkOutputFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrInput             = _asthdlrInput
-}
            }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': Grin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 grin) export(astHandler'_Grin)
astHandler'_Grin :: ASTHandler' AST_Grin
astHandler'_Grin = 
  emptyASTHandler'
			{ _asthdlrName              = "Grin"
            , _asthdlrASTLens           = Just $ ecuMbGrin
            , _asthdlrSuffixRel			= mkASTSuffixRel
            								[ ( (ASTFileContent_Text	, ASTFileUse_Dump), ( "grin", ecuMbGrin, Nothing ) )
            								]
            							  `Rel.union`
            							  mkASTSuffixRel'
            								[ ( (ASTFileContent_Binary	, ASTFileUse_Cache)
            								  , ( "grin"		-- TBD: sort out suffixes, currently not used this stuff...
            								    , [ (ASTFileTiming_Current, ecuMbGrin)
            								      , (ASTFileTiming_Prev, ecuMbGrin)
            								      ]
%%[[8
            								    , []
%%][50
            								    , [ (ASTFileTiming_Prev, ecuMbGrinTime)
            								      ]
%%]]
            								    , id
            								  ) )
            								]
			, _asthdlrParseScanOpts     = \opts _ -> grinScanOpts
			, _asthdlrEcuStore          = ecuStoreGrin
            , _asthdlrPretty			= \_ _ ast -> Just $ ppGrModule ast
%%[[50
			, _asthdlrPutSerializeFileIO= default_asthdlrPutSerializeFileIO
			, _asthdlrGetSerializeFileIO= default_asthdlrGetSerializeFileIO
%%]]
			}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': Cmm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 cmm) export(astHandler'_Cmm)
astHandler'_Cmm :: ASTHandler' AST_Cmm
astHandler'_Cmm = 
  emptyASTHandler'
			{ _asthdlrName              = "Cmm"
            , _asthdlrASTLens           = Just $ ecuMbCmm
            , _asthdlrSuffixRel			= mkASTSuffixRel
            								[ ( (ASTFileContent_Text	, ASTFileUse_Dump)		, ("cmm", ecuMbCmm, Nothing) )
            								]
			, _asthdlrEcuStore          = ecuStoreCmm
            , _asthdlrPretty			= \_ _ ast -> Just $ ppCmmModule ast
			}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler': JavaScript
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 javascript) export(astHandler'_JavaScript)
astHandler'_JavaScript :: ASTHandler' AST_JavaScript
astHandler'_JavaScript = 
  emptyASTHandler'
			{ _asthdlrName              = "JavaScript"
            , _asthdlrASTLens           = Just $ ecuMbJavaScript
            , _asthdlrSuffixRel			= mkASTSuffixRel
            								[ ( (ASTFileContent_Text	, ASTFileUse_Target)		, ("js", ecuMbJavaScript, Nothing) )
            								]
			, _asthdlrEcuStore          = ecuStoreJavaScript
			, _asthdlrMkOutputFPath           = \opts m f suff -> mkPerModuleOutputFPath opts True m f suff
            , _asthdlrPretty			= \opts ecu ast -> Just $
%%[[8
					let ppMod = ppJavaScriptModule ast
%%][50
					let ppMod = vlist $ [p] ++ (if ecuIsMainMod ecu then [pmain] else [])
							where (p,pmain) = ppJavaScriptModule ast
%%]]
					in  "//" >#< ecuModNm ecu >-< ppMod
			}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% All ASTHandlers
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(allASThandlerMp)
-- | Global mapping from ASTType to ast handler
allASThandlerMp :: ASTHandlerMp
allASThandlerMp = Map.fromList
  [ ( ASTType_HS			, ASTHandler astHandler'_HS			)
  , ( ASTType_EH			, ASTHandler astHandler'_EH			)
%%[[50
  , ( ASTType_HI			, ASTHandler astHandler'_HI			)
%%]]
%%[[(8 core)
  , ( ASTType_Core			, ASTHandler astHandler'_Core		)
%%]]
%%[[(8 corerun)
  , ( ASTType_CoreRun		, ASTHandler astHandler'_CoreRun	)
%%]]
%%[[(8 grin)
  , ( ASTType_Grin			, ASTHandler astHandler'_Grin		)
%%]]
%%[[(8 cmm)
  , ( ASTType_Cmm			, ASTHandler astHandler'_Cmm		)
%%]]
%%[[(8 javascript)
  , ( ASTType_JavaScript	, ASTHandler astHandler'_JavaScript	)
%%]]
  ]
%%]

%%[8 export(asthandlerLookup)
-- | Lookup ast handler, forcing a particular ast type
asthandlerLookup :: Typeable ast => ASTType -> Maybe (ASTHandler' ast)
asthandlerLookup t = case Map.lookup t allASThandlerMp of
    Just (ASTHandler h) -> cast h
    _                   -> Nothing
%%]

%%[8 export(asthandlerLookup')
-- | Lookup ast handler, allowing arbitrary type by hiding the type
asthandlerLookup' :: ASTType -> (forall ast . Typeable ast => ASTHandler' ast -> Maybe x) -> Maybe x
asthandlerLookup' t f = case Map.lookup t allASThandlerMp of
    Just (ASTHandler h) -> f h
    _                   -> Nothing
%%]

%%[8 export(asthandlerLookupM')
-- | Lookup ast handler, allowing arbitrary type by hiding the type, monadically
asthandlerLookupM' :: Monad m => ASTType -> (forall ast . Typeable ast => ASTHandler' ast -> m (Maybe x)) -> m (Maybe x)
asthandlerLookupM' t f = case Map.lookup t allASThandlerMp of
    Just (ASTHandler h) -> f h
    _                   -> return Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Defaults for ASTHandler' fields
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
default_asthdlrGetSerializeFileIO :: Serialize ast => EHCOpts -> FPath -> IO (Maybe ast)
default_asthdlrGetSerializeFileIO _ fp = fmap Just $ getSerializeFile (fpathToStr fp)

default_asthdlrPutSerializeFileIO :: Serialize ast => FilePath -> ast -> IO Bool
default_asthdlrPutSerializeFileIO fn ast = putSerializeFile fn ast >> return True
%%]

