%%[0 hs
{-# LANGUAGE RecordWildCards #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
%%]

%%[8 module {%{EH}EHC.ASTHandler.Instances}
%%]

-- general imports
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileUnit}, {%{EH}EHC.CompileRun})
%%]

%%[8 import ({%{EH}EHC.ASTHandler}) export (module {%{EH}EHC.ASTHandler})
%%]

%%[8888 import (Control.Applicative)
%%]

%%[8888 import (Data.Functor.Identity) export(module Data.Functor.Identity)
%%]

%%[8888 import (qualified Data.Map as Map, qualified Data.IntMap as IMap, Data.Maybe)
%%]

%%[8888 import (UHC.Util.Hashable)
%%]

%%[8888 import (UHC.Util.Lens)
%%]

%%[8888 import (qualified UHC.Util.RelMap as Rel)
%%]

%%[8 import (Data.Typeable, GHC.Generics)
%%]

%%[8 import({%{EH}Base.ParseUtils})
%%]

-- Language syntax: HS, EH, Core, TyCore, Grin, ...
%%[8 import(qualified {%{EH}EH} as EH)
%%]
%%[8 import(qualified {%{EH}HS} as HS)
%%]
%%[(8 codegen) import( qualified {%{EH}Core} as Core)
%%]
%%[(8 corerun) import( qualified {%{EH}CoreRun} as CoreRun)
%%]
%%[(8 codegen tycore) import(qualified {%{EH}TyCore} as C)
%%]
%%[(8 codegen grin) import(qualified {%{EH}GrinCode} as Grin, qualified {%{EH}GrinByteCode} as Bytecode)
%%]
%%[(8 jazy) hs import(qualified {%{EH}JVMClass} as Jvm)
%%]
%%[(8 javascript) hs import(qualified {%{EH}JavaScript} as JS)
%%]
%%[(8 codegen cmm) hs import(qualified {%{EH}Cmm} as Cmm)
%%]
-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Language semantics: Core
-- TBD: this depends on grin gen, but should also be available for Core, so in a CoreXXXSem
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 corerun core) import(qualified {%{EH}Core.ToCoreRun} as Core2CoreRunSem)
%%]
%%[(8 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]
%%[(8 codegen) import({%{EH}Core.Trf.EraseExtractTysigCore})
%%]
-- Language semantics: CoreRun
%%[(8 codegen corerunin) import(qualified {%{EH}CoreRun.Check} as CoreRun2ChkSem)
%%]

-- HI Syntax and semantics, HS module semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]


-- CoreRun output
%%[(8 corerun) import({%{EH}CoreRun} as CoreRun, {%{EH}Core.ToCoreRun}, {%{EH}CoreRun.Pretty})
%%]
-- Core output
%%[(8 codegen coreout) import({%{EH}Core} as Core,{%{EH}Core.Pretty})
%%]
-- TyCore output
%%[(8 codegen tycore) import({%{EH}TyCore},{%{EH}TyCore.Pretty})
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
-- TyCore parser
%%[(50 codegen tycore) import(qualified {%{EH}TyCore} as C)
%%]
-- Grin parser
%%[(8 codegen grinparser) import(qualified {%{EH}GrinCode.Parser} as GrinParser)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler: HS
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(astHandler_HS)
astHandler_HS :: ASTHandler HS.AGItf
astHandler_HS = mk emptyASTHandler
  where mk (hdlr@(ASTHandler {..})) =
          ASTHandler
            { _asthdlrName              = "Haskell"
            , _asthdlrEcuStore          = ecuStoreHS
            , _asthdlrParseScanOpts     = \opts popts -> hsScanOpts opts
            -- , _asthdlrParseParse        = \opts -> Just . parseOffsideToResMsgs (HSPrs.pAGItf opts)
            -- , _asthdlrParseScan         = \opts fp hdl -> fmap Just $ offsideScanHandle opts fp hdl
            , _asthdlrParser            = \opts popts -> Just $ ASTParser $
%%[[50
                                            if ehpoptsForImport popts then HSPrs.pAGItfImport opts else
%%]]
                                            HSPrs.pAGItf opts
            
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            , _asthdlrMkFPath           = _asthdlrMkFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrOutputIO          = _asthdlrOutputIO
            , _asthdlrInput             = _asthdlrInput
            }
            
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler: EH
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(astHandler_EH)
astHandler_EH :: ASTHandler EH.AGItf
astHandler_EH = mk emptyASTHandler
  where mk (hdlr@(ASTHandler {..})) = 
          ASTHandler
            { _asthdlrName              = "EH"
            , _asthdlrEcuStore          = ecuStoreEH
            , _asthdlrParseScanOpts     = \opts _ -> ehScanOpts opts
            -- , _asthdlrParseParse        = \_ -> Just . parseOffsideToResMsgs EHPrs.pAGItf
            -- , _asthdlrParseScan         = \opts fp hdl -> fmap Just $ offsideScanHandle opts fp hdl
            , _asthdlrParser            = \_ _ -> Just $ ASTParser EHPrs.pAGItf
            
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            , _asthdlrMkFPath           = _asthdlrMkFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrOutputIO          = _asthdlrOutputIO
            , _asthdlrInput             = _asthdlrInput
            }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler: Core
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 core) export(astHandler_Core)
astHandler_Core :: ASTHandler Core.CModule
astHandler_Core = mk emptyASTHandler
  where mk (hdlr@(ASTHandler {..})) = 
          ASTHandler
            { _asthdlrName              = "Core"
            , _asthdlrEcuStore          = ecuStoreCore
%%[[(8 corein)
            , _asthdlrParseScanOpts     = \opts _ -> coreScanOpts opts
%%]]
            -- , _asthdlrParseParse        = \opts inp -> Just $ parseToResMsgs (CorePrs.pCModule opts) inp
            -- , _asthdlrParseScan         = \opts fp hdl -> fmap Just $ scanHandle opts fp hdl
            , _asthdlrParser            = \opts _ -> Just $ ASTParser (CorePrs.pCModule opts :: EHPrsAna Core.CModule)
            , _asthdlrOutputIO          = \how opts _ _ fpC fnC cMod -> case how of
                  ASTFileVariation_Text -> do
                    let cMod' = cmodTrfEraseTyCore opts cMod
                    putPPFPath fpC (ppCModule (opts {- ehcOptCoreOpts = coreOpts ++ ehcOptCoreOpts opts -}) cMod') 100
                    return True
%%[[50
                  ASTFileVariation_Binary -> do
                    putSerializeFile fnC cMod
                    return True
%%]]
                  _ -> return False
            
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            -- , _asthdlrParseParse        = _asthdlrParseParse
            -- , _asthdlrParseScan         = _asthdlrParseScan
            -- , _asthdlrParser         = _asthdlrParser
            , _asthdlrMkFPath           = _asthdlrMkFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrInput             = _asthdlrInput
            }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler: CoreRun
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) export(astHandler_CoreRun)
astHandler_CoreRun :: ASTHandler CoreRun.Mod
astHandler_CoreRun = mk emptyASTHandler
  where mk (hdlr@(ASTHandler {..})) = 
          ASTHandler
            { _asthdlrName              = "CoreRun"
            , _asthdlrEcuStore          = ecuStoreCoreRun
%%[[(8 corein)
            , _asthdlrParseScanOpts     = \opts _ -> corerunScanOpts
%%]]
            -- , _asthdlrParseParse        = \opts inp -> Just $ parseToResMsgs (CoreRunPrs.pMod opts) inp
            -- , _asthdlrParseScan         = \opts fp hdl -> fmap Just $ scanHandle opts fp hdl
            , _asthdlrParser            = \opts _ -> Just $ ASTParser (CoreRunPrs.pMod opts :: EHPrsAna CoreRun.Mod)
            , _asthdlrOutputIO          = \how opts _ _ fpC fnC crMod -> case how of
                  ASTFileVariation_Text -> do
                    putPPFPath fpC (ppMod' opts crMod) 100
                    return True
%%[[50
                  ASTFileVariation_Binary -> do
                    putSerializeFile fnC crMod
                    return True
%%]]
                  _ -> return False
            
            -- the rest, avoid record update (http://hackage.haskell.org/trac/ghc/ticket/2595, http://breaks.for.alienz.org/blog/2011/10/21/record-update-for-insufficiently-polymorphic-field/) 
            -- , _asthdlrParseParse        = _asthdlrParseParse
            -- , _asthdlrParseScan         = _asthdlrParseScan
            -- , _asthdlrParser         = _asthdlrParser
            , _asthdlrMkFPath           = _asthdlrMkFPath
            , _asthdlrSuffixMp          = _asthdlrSuffixMp
            , _asthdlrInput             = _asthdlrInput
            }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler: Grin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 grin) export(astHandler_Grin)
astHandler_Grin :: ASTHandler Grin.GrModule
astHandler_Grin = 
  emptyASTHandler
    { _asthdlrName              = "Grin"
    , _asthdlrParseScanOpts     = \opts _ -> grinScanOpts
    , _asthdlrEcuStore          = ecuStoreGrin
    , _asthdlrOutputIO          = \how _ _ _ fpC fnC gMod -> case how of
          ASTFileVariation_Text -> do
            putPPFPath fpC (ppGrModule gMod) 100
            return True
%%[[50
          ASTFileVariation_Binary -> do
            putSerializeFile fnC gMod
            return True
%%]]
          _ -> return False
    }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler: Cmm
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 cmm) export(astHandler_Cmm)
astHandler_Cmm :: ASTHandler Cmm.Module
astHandler_Cmm = 
  emptyASTHandler
    { _asthdlrName              = "Cmm"
    , _asthdlrEcuStore          = ecuStoreCmm
    , _asthdlrOutputIO          = \how _ _ _ fpC fnC cmmMod -> case how of
          ASTFileVariation_Text -> do
            putPPFPath fpC (ppCmmModule cmmMod) 100
            return True
          _ -> return False
    }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% ASTHandler: JavaScript
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 javascript) export(astHandler_JavaScript)
astHandler_JavaScript :: ASTHandler JS.JavaScriptModule
astHandler_JavaScript = 
  emptyASTHandler
    { _asthdlrName              = "JavaScript"
    , _asthdlrEcuStore          = ecuStoreJavaScript
    , _asthdlrMkFPath           = \opts m f suff -> mkPerModuleOutputFPath opts True m f suff
    , _asthdlrOutputIO          = \how _ ecu modNm fpC fnC jsMod -> case how of
          ASTFileVariation_Text -> do
%%[[8
            let ppMod = ppJavaScriptModule jsMod
%%][50
            let ppMod = vlist $ [p] ++ (if ecuIsMainMod ecu then [pmain] else [])
                    where (p,pmain) = ppJavaScriptModule jsMod
%%]]
            putPPFPath fpC ("//" >#< modNm >-< ppMod) 1000
            return True
          _ -> return False
    }
%%]


