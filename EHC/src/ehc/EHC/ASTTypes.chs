%%[0 hs
{-# LANGUAGE GADTs #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% All ASTs used by compiler pipeline, gathered, renamed, re-exported
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
%%]

%%[8 module {%{EH}EHC.ASTTypes}
%%]

-- general imports
%%[8 import ({%{EH}Base.Common})
%%]

%%[8888 import (Data.Typeable, GHC.Generics)
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
%%[(8 codegen grin) import(qualified {%{EH}GrinCode} as Grin, qualified {%{EH}GrinByteCode} as Bytecode)
%%]
%%[(8 jazy) hs import(qualified {%{EH}JVMClass} as Jvm)
%%]
%%[(8 javascript) hs import(qualified {%{EH}JavaScript} as JS)
%%]
%%[(8 codegen cmm) hs import(qualified {%{EH}Cmm} as Cmm)
%%]
-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.Main} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]
-- Language semantics: Core
-- TBD: this depends on grin gen, but should also be available for Core, so in a CoreXXXSem
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 corerun core) import(qualified {%{EH}Core.ToCoreRun} as Core2CoreRunSem)
%%]
%%[(8 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]
-- Language semantics: CoreRun
%%[(8 codegen corerunin) import(qualified {%{EH}CoreRun.Check} as CoreRun2ChkSem)
%%]
-- Bytecode
%%[(8 codegen grin) import(qualified {%{EH}GrinByteCode} as Bytecode)
%%]

-- HI Syntax and semantics, HS module semantics
%%[50 import(qualified {%{EH}HI} as HI)
%%]
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
%%[(8 corerunin) import(qualified {%{EH}CoreRun.ModImpExp} as CoreRunSemMod)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Type synonyms
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(AST_HS, AST_EH, AST_HS_Sem_Check, AST_EH_Sem_Check, AST_HS_Inh_Check, AST_EH_Inh_Check)
type AST_HS					= HS.AGItf
type AST_EH					= EH.AGItf

type AST_HS_Sem_Check		= HSSem.Syn_AGItf
type AST_EH_Sem_Check		= EHSem.Syn_AGItf

type AST_HS_Inh_Check		= HSSem.Inh_AGItf
type AST_EH_Inh_Check		= EHSem.Inh_AGItf
%%]

%%[50 export(AST_HS_Sem_Mod)
type AST_HS_Sem_Mod			= HSSemMod.Syn_AGItf
%%]

%%[50 export(AST_HI)
type AST_HI					= HI.HIInfo
%%]

%%[(8 core) export(AST_Core)
type AST_Core				= Core.CModule
%%]

%%[(8 core grin) export(AST_Core_Sem_ToGrin, AST_Core_Inh_ToGrin)
type AST_Core_Sem_ToGrin	= Core2GrSem.Syn_CodeAGItf

type AST_Core_Inh_ToGrin	= Core2GrSem.Inh_CodeAGItf
%%]

%%[(8 core corerun) export(AST_Core_Sem_ToCoreRun)
type AST_Core_Sem_ToCoreRun	= Core2CoreRunSem.Syn_CodeAGItf
%%]

%%[(8 corein) export(AST_Core_Sem_Check)
type AST_Core_Sem_Check		= Core2ChkSem.Syn_CodeAGItf
%%]

%%[(8 corerun) export(AST_CoreRun)
type AST_CoreRun			= CoreRun.Mod
%%]

%%[(8 corerunin) export(AST_CoreRun_Sem_Check)
type AST_CoreRun_Sem_Check	= CoreRun2ChkSem.Syn_AGItf
%%]

%%[(8 corerunin) export(AST_CoreRun_Sem_Mod)
type AST_CoreRun_Sem_Mod	= CoreRunSemMod.Syn_AGItf
%%]

%%[(8 grin) export(AST_Grin)
type AST_Grin				= Grin.GrModule
%%]

%%[(8 cmm) export(AST_Cmm)
type AST_Cmm				= Cmm.Module
%%]

%%[(8 javascript) export(AST_JavaScript)
type AST_JavaScript			= JS.JavaScriptModule
%%]

%%[(8 jazy) export(AST_Java)
type AST_Java				= [Jvm.Class]
%%]

%%[(8 grin) export(AST_GrinBytecode)
type AST_GrinBytecode		= Bytecode.Module
%%]



