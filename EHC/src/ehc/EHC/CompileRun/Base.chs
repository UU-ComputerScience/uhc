%%[0 hs
{-# LANGUAGE GADTs, TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% EHC Compile Run
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

An EHC compile run maintains info for one compilation invocation

%%[8 module {%{EH}EHC.CompileRun.Base}
%%]

-- general imports
%%[8 import(qualified Data.Map as Map,qualified Data.Set as Set, qualified Data.IntMap as IMap, Data.Maybe)
%%]
%%[8 import (Data.Typeable)
%%]
%%[8 import(qualified UHC.Util.RelMap as Rel, UHC.Util.Hashable)
%%]
%%[8 import(Control.Exception as CE)
%%]
%%[8 import (UHC.Util.Lens)
%%]
%%[8 import (Data.Functor.Identity)
%%]
%%[8 import({%{EH}EHC.Common})
%%]
%%[8 import({%{EH}EHC.FileSuffMp})
%%]
%%[8 import({%{EH}Base.Optimize})
%%]
%%[8 import(Control.Monad.State hiding (get), qualified Control.Monad.State as MS)
%%]
%%[8 import(Control.Applicative)
%%]
%%[8 import(Control.Monad.Error, Control.Monad.Fix)
%%]
%%[8 import(System.IO, System.Exit, System.Environment, System.Process)
%%]
%%[99 import(UHC.Util.Time, System.CPUTime, System.Locale, Data.IORef, System.IO.Unsafe)
%%]
%%[99 import(System.Directory)
%%]
%%[99 import(UHC.Util.FPath)
%%]
%%[99 import(UHC.Util.Time, System.CPUTime)
%%]
%%[99 import(System.Locale, Data.IORef, System.IO.Unsafe)
%%]

-- compiler driver
%%[8 import({%{EH}EHC.CompileUnit})
%%]
%%[50 import({%{EH}EHC.CompileGroup})
%%]

-- pkg
%%[99 import({%{EH}Base.PackageDatabase})
%%]

-- Module
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
%%[50 import({%{EH}CodeGen.ModuleImportExportImpl})
%%]
%%[99 import(qualified {%{EH}Base.Pragma} as Pragma)
%%]
-- module admin
%%[50 import({%{EH}Module.ImportExport})
%%]


-- CoreRun syntax and semantics
%%[(8 corerun) import(qualified {%{EH}CoreRun} as CoreRun)
%%]

-- other structures
%%[(8 codegen) hs import({%{EH}CodeGen.ValAccess} as VA)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances (which should not be here...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8888
deriving instance Typeable Identity
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function explicit representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(BFun'(..))
-- | Representation of build functions (embedded comment screws up haddock, hence see source code directly).
-- Regretfully deriving Generic (and thus Hashable) does not work for GADTs, so must be done manually, below.
-- Ord cannot be derived either.
-- First order type, no fields with recursive type are allowed to allow for more easily implementable comparison etc.
data BFun' res where
  --- | Obtain global state
  CRSI
    :: BFun' EHCompileRunStateInfo

%%[[50
{-
  --- | Obtain global state specific for compile order
  CRSIWithCompileOrder
    :: !HsName
    -> ![[HsName]]				--- ^ compile order
    -> !(Set.Set ASTType)		--- ^ flow for these ASTTypes
    -> BFun' EHCompileRunStateInfo
-}

  --- | Obtain global state specific for compile order
  CRSIWithCompileOrderP
    :: !HsName
    -> ![[HsName]]				--- ^ compile order
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun' EHCompileRunStateInfo

{-
  --- | Obtain global state specific for imports
  CRSIWithImps
    :: !PrevFileSearchKey
    -> !(Set.Set HsName)				--- ^ imports
    -> !(Set.Set ASTType)		--- ^ flow for these ASTTypes
    -> BFun' EHCompileRunStateInfo
-}

  --- | Obtain global state specific for imports
  CRSIWithImpsP
    :: !PrevFileSearchKey
    -> !(Set.Set HsName)				--- ^ imports
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun' EHCompileRunStateInfo
%%]]

  --- | Obtain global state specific for a module, which depends on the (imported) module names
  CRSIOfName
    :: !PrevFileSearchKey				--- ^ module name etc
    -> !ASTType							--- ^ content type
    -> BFun' EHCompileRunStateInfo

  CRSIOfNameP
    :: !PrevFileSearchKey				--- ^ module name etc
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun' EHCompileRunStateInfo

  --- | Obtain FPath and module name of a file name
  FPathSearchForFile
    :: !String				--- ^ suffix, if absent in name
    -> !FilePath			--- ^ file name
    -> BFun' (HsName, FPath)

{-
  --- | Obtain FPath of an (imported module)
  FPathOfImported
    :: !HsName				--- ^ module name
    -> BFun' FPath
-}

  --- | Obtain FPath of module taking into account ast type, overriding, suffix etc
  FPathForAST
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' (FPath, ASTFileSuffOverride, EHCompileUnit)

%%[[50
  --- | Extract imported modules from a module
  ImportsOfName
    :: !PrevFileSearchKey				--- ^ module name etc
    -> BFun' (HsName, Set.Set HsName)

  --- | Extract recursively all import relationships starting with imports
  ImportsRecursiveWithImps
    :: !(Maybe PrevSearchInfo)
    -> !(Set.Set HsName)				--- ^ imports
    -> BFun'
         ( Map.Map HsName (Set.Set HsName)		-- recursive result
         )

  --- | Extract recursively all import relationships starting with module
  ImportsRecursiveOfName
    :: !PrevFileSearchKey				--- ^ module name etc
    -> BFun'
         ( HsName								-- the actual module name
         , Set.Set HsName						-- imports
         , Map.Map HsName (Set.Set HsName)		-- recursive result
         )
%%]]

  --- | Extract possibly compileunit from a module, as is, no checks on consistency
  {-
  EcuMbOf
    :: !HsName				--- ^ module name
    -> BFun' (Maybe EHCompileUnit)
  -}
  
  --- | Extract compileunit from a module, as is, no checks on consistency
  EcuOf
    :: !HsName				--- ^ module name
    -> BFun' EHCompileUnit

  --- | Extract compileunit from a module, including file path etc walking, suffix detection
  EcuOfName
    :: !HsName				--- ^ module name
    -> BFun' EHCompileUnit

  EcuOfPrevNameAndPath
    :: !PrevFileSearchKey			--- ^ module name and possibly known path
    -> BFun' EHCompileUnit

  EcuOfNameAndPath
    :: !FileSearchKey				--- ^ module name and possibly known path
    -> BFun' EHCompileUnit

  --- | Extract global options, possibly overridden for a module
  EHCOptsOf
    :: !HsName				--- ^ module name
    -> BFun' EHCOpts

  --- | Actual module name, as it occurs in module itself
  ActualModNm
    :: !HsName				--- ^ module name
    -> BFun' HsName

  ASTRefFromFileEither
    :: Typeable ast
    => !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !Bool							--- ^ errors are returned instead of reported directly
    -> !(AlwaysEq ASTFileTimeHandleHow)	--- ^ how to deal with timestamp
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' (Either (String,[Err]) (BRef ast))

  ASTRefFromFileMb
    :: Typeable ast
    => !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !(AlwaysEq ASTFileTimeHandleHow)	--- ^ how to deal with timestamp
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' (Maybe (BRef ast))

  --- | Get a particular AST from file for a module
  ASTFromFile
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !(AlwaysEq ASTFileTimeHandleHow)	--- ^ how to deal with timestamp
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' res

  --- | Get a particular AST for a module
  AST
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTType							--- ^ content type
    -> BFun' res

  --- | Get a particular AST for a module
  ASTP
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun' res

  --- | Get a particular AST for a module, possibly
  ASTMb
    :: Typeable ast
    => !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTType							--- ^ content type
    -> BFun' (Maybe (ast, BRef ast))

  ASTPMb
    :: Typeable ast
    => !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun' (Maybe (ASTResult ast))

{-
  --- | Get a particular AST for a module using a pipeline to compute it, possibly
  PipeASTMb
    :: Typeable ast
    => !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ content type
    -> BFun' (Maybe (ast, BRef ast))
-}
%%[[50
  --- | Get the modification ClockTime of a file for a module
  ModfTimeOfFile
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' (Maybe ClockTime)

  --- | Valid AST from file?
  ASTFileIsValid
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' Bool

  --- | Compare timestamps, if possible, yield True if first is new than second
  ASTFileIsNewerThan
    :: !(PrevFileSearchKey				--- ^ 1st module name and possibly known path
        ,ASTType						--- ^ 1st content type
        ,ASTSuffixKey					--- ^ 1st suffix and content variation
        ,ASTFileTiming 					--- ^ 1st timing (i.e. previous or current)
        )
    -> !(PrevFileSearchKey				--- ^ 2nd module name and possibly known path
        ,ASTType						--- ^ 2nd content type
        ,ASTSuffixKey					--- ^ 2nd suffix and content variation
        ,ASTFileTiming 					--- ^ 2nd timing (i.e. previous or current)
        )
    -> BFun' (Maybe Bool)

  --- | Get writeability of the dir a module resides in
  DirOfModIsWriteable
    :: !HsName							--- ^ module name and possibly known path
    -> BFun' Bool

  --- | Can compile a src module
  CanCompile
    :: !HsName							--- ^ module name and possibly known path
    -> BFun' Bool

  --- | Src module needs (re)compilation
  NeedsCompile
    :: !HsName							--- ^ module name and possibly known path
    -> BFun' Bool

  --- | Module is top module, i.e. specified at commandline with possibly different name than module name in file
  IsTopMod
    :: !HsName							--- ^ module name and possibly known path
    -> BFun' Bool
%%]]

%%[[50
  --- | The result of folding over a module for import/module analysis
  FoldHsMod
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
%%[[50
    -> !(Maybe ())						--- ^ dummy
%%][99
    -> !(Maybe [PkgModulePartition])	--- ^ optionally do CPP with module partitioning into pkgs
%%]]
    -> BFun'
         ( AST_HS_Sem_Mod				-- all semantics
         , Bool							-- has main?
%%[[99
         , Set.Set Pragma.Pragma		-- pragmas
         , Maybe EHCOpts				-- possibly adapted options
%%]]
         )

  --- | The actual module name and imported modules, abstracted over the AST type
  ModnameAndImports
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !ASTType							--- ^ ast type
    -> BFun'
         ( HsName						-- module name
         , Set.Set HsName				-- imported module names
         , Maybe PrevSearchInfo			-- search info for modules to be imported from this one
         )

  --- | See 'ModnameAndImports', for HS
  HsModnameAndImports
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> BFun'
         ( HsName						-- module name
         , Set.Set HsName				-- imported module names
         , Maybe PrevSearchInfo			-- search info for modules to be imported from this one
         )

  --- | HIInfo
  FoldHIInfo
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> BFun'
         ( AST_HI						-- all semantics
         , Set.Set HsName				-- declared imported module names
         , Set.Set HsName				-- used imported module names
         , Bool							-- is main module?
         )

  --- | Imported names info for codegen
  ImportNameInfo
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> OptimizationScope				--- ^ scope for which this holds
    -> BFun' [HsName]

  --- | Import/Export info for module codegen
  ImportExportImpl
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> OptimizationScope
    -> BFun' ModuleImportExportImpl
%%]]

  --- | HS semantics
  FoldHs
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun'
         ( AST_HS_Sem_Check				-- all semantics
%%[[50
         -- , Set.Set HsName				-- declared imported module names
         , Bool							-- is main module?
%%]]
         )

  --- | HS semantics
  FoldHsMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( Maybe
           ( AST_HS_Sem_Check				-- all semantics
%%[[50
           -- , Set.Set HsName				-- declared imported module names
           , Bool							-- is main module?
%%]]
         ) )

  --- | HS semantics
  FoldHsPMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun'
         ( Maybe
           ( AST_HS_Sem_Check				-- all semantics
%%[[50
           -- , Set.Set HsName				-- declared imported module names
           , Bool							-- is main module?
%%]]
         ) )

  --- | EH semantics
  FoldEH
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( AST_EH_Sem_Check				-- all semantics
         )

  --- | EH semantics
  FoldEHMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( Maybe
           ( AST_EH_Sem_Check				-- all semantics
         ) )

  --- | EH semantics
  FoldEHPMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun'
         ( Maybe
           ( AST_EH_Sem_Check				-- all semantics
         ) )

%%[[(50 corein)
  --- | Core as src semantics
  FoldCoreMod
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( AST_Core_Sem_Check			-- all semantics
         , HsName								-- real mod name
         , Set.Set HsName						-- declared imported module names
         , Mod									-- module import/export etc info
         , Bool									-- is main module?
         , Maybe PrevSearchInfo
         )

  --- | Core as src semantics
  FoldCoreModPMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun'
         ( Maybe
           ( AST_Core_Sem_Check			-- all semantics
           , HsName								-- real mod name
           , Set.Set HsName						-- declared imported module names
           , Mod									-- module import/export etc info
           , Bool									-- is main module?
           , Maybe PrevSearchInfo
         ) )

%%]]

%%[[(8 core grin)
  --- | Core -> Grin semantics
  FoldCore2Grin
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( AST_Core_Sem_ToGrin						-- all semantics
         )

  --- | Core -> Grin semantics
  FoldCore2GrinMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( Maybe
           ( AST_Core_Sem_ToGrin						-- all semantics
         ) )

  --- | Core -> Grin semantics
  FoldCore2GrinPMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun'
         ( Maybe
           ( AST_Core_Sem_ToGrin						-- all semantics
         ) )
%%]]

%%[[(8 core corerun)
{-
  --- | Core -> CoreRun semantics
  FoldCore2CoreRun
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( AST_Core_Sem_ToCoreRun				-- all semantics
         )

  --- | Core -> CoreRun semantics
  FoldCore2CoreRunMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( Maybe
           ( AST_Core_Sem_ToCoreRun				-- all semantics
         ) )
-}

  --- | Core -> CoreRun semantics
  FoldCore2CoreRunPMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun'
         ( Maybe
           ( AST_Core_Sem_ToCoreRun				-- all semantics
         ) )
%%]]

%%[[(50 corerun corerunin)
  --- | CoreRun as src semantics
  FoldCoreRunMod
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( AST_CoreRun_Sem_Check				-- all semantics
         , HsName								-- real mod name
         , Set.Set HsName						-- declared imported module names
         , Mod									-- module import/export etc info
         , Bool									-- is main module?
         , Maybe PrevSearchInfo
         )

  --- | CoreRun as src semantics
  FoldCoreRunModPMb
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> !ASTPipe							--- ^ pipeline leading to content
    -> BFun'
         ( Maybe
           ( AST_CoreRun_Sem_Check				-- all semantics
           , HsName								-- real mod name
           , Set.Set HsName						-- declared imported module names
           , Mod									-- module import/export etc info
           , Bool									-- is main module?
           , Maybe PrevSearchInfo
         ) )
%%]]

%%[[99
  --- | Get the FPath of the possibly with CPP preprocessed file
  FPathPreprocessedWithCPP
    :: [PkgModulePartition]				--- ^ partitioning of modules into packages
    -> !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun' FPath

  --- | Exposed packages
  ExposedPackages
    :: BFun' [PkgModulePartition]
%%]]

-- | Comparison which ignores GADT type info
bfunCompare :: BFun' res1 -> BFun' res2 -> Ordering
bfunCompare f1 f2 = case (f1,f2) of
    (CRSI				 							, CRSI 											) -> EQ
%%[[50
    -- (CRSIWithCompileOrder		a1 b1 c1 			, CRSIWithCompileOrder		a2 b2 c2 			) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2]
    (CRSIWithCompileOrderP		a1 b1 c1 			, CRSIWithCompileOrderP		a2 b2 c2 			) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2]
    -- (CRSIWithImps			    a1 b1 c1 			, CRSIWithImps		    	a2 b2 c2 			) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2]
    (CRSIWithImpsP			    a1 b1 c1 			, CRSIWithImpsP		    	a2 b2 c2 			) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2]
%%]]
    (CRSIOfName			    	a1 b1  				, CRSIOfName		    	a2 b2  				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (CRSIOfNameP			    a1 b1  				, CRSIOfNameP		    	a2 b2  				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (FPathSearchForFile 		a1 b1				, FPathSearchForFile 		a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (FPathForAST           		a1 b1 c1 d1	 		, FPathForAST				a2 b2 c2 d2			) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2]
%%[[50
    (ImportsOfName          	a1   				, ImportsOfName          	a2   				) ->         a1 `compare` a2
    (ImportsRecursiveWithImps	a1 b1  				, ImportsRecursiveWithImps	a2 b2  				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ImportsRecursiveOfName		a1   				, ImportsRecursiveOfName	a2   				) ->         a1 `compare` a2
%%]]
    (ActualModNm		        a1   				, ActualModNm	    		a2   				) ->         a1 `compare` a2
    (EcuOf		              	a1   				, EcuOf	    				a2   				) ->         a1 `compare` a2
    (EcuOfName              	a1   				, EcuOfName    				a2   				) ->         a1 `compare` a2
    (EcuOfPrevNameAndPath		a1 					, EcuOfPrevNameAndPath		a2 					) ->         a1 `compare` a2
    (EcuOfNameAndPath			a1 					, EcuOfNameAndPath			a2 					) ->         a1 `compare` a2
    (EHCOptsOf             		a1   				, EHCOptsOf					a2   				) ->         a1 `compare` a2
    (ASTRefFromFileEither      	a1 b1 c1 d1	e1 f1	, ASTRefFromFileEither		a2 b2 c2 d2	e2 f2	) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2, e1 `compare` e2, f1 `compare` f2]
    (ASTRefFromFileMb          	a1 b1 c1 d1	e1 		, ASTRefFromFileMb			a2 b2 c2 d2	e2		) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2, e1 `compare` e2]
    (ASTFromFile            	a1 b1 c1 d1	e1 		, ASTFromFile				a2 b2 c2 d2	e2		) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2, e1 `compare` e2]
    (AST            			a1 b1		 		, AST						a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ASTP            			a1 b1		 		, ASTP						a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ASTMb            			a1 b1		 		, ASTMb						a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ASTPMb            			a1 b1		 		, ASTPMb					a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%[[50
    (ModfTimeOfFile         	a1 b1 c1 d1			, ModfTimeOfFile			a2 b2 c2 d2			) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2]
    (ASTFileIsValid         	a1 b1 c1 d1			, ASTFileIsValid			a2 b2 c2 d2			) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2]
    (ASTFileIsNewerThan        	a1 b1   			, ASTFileIsNewerThan		a2 b2 	 			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (DirOfModIsWriteable		a1   				, DirOfModIsWriteable		a2   				) ->         a1 `compare` a2
    (CanCompile					a1 					, CanCompile				a2 					) ->         a1 `compare` a2
    (NeedsCompile				a1 					, NeedsCompile				a2 					) ->         a1 `compare` a2
    (IsTopMod					a1 					, IsTopMod					a2 					) ->         a1 `compare` a2
%%]]
%%[[50
    (FoldHsMod					a1 b1				, FoldHsMod					a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ModnameAndImports			a1 b1				, ModnameAndImports			a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (HsModnameAndImports		a1 					, HsModnameAndImports		a2 					) ->         a1 `compare` a2
    (FoldHIInfo					a1 					, FoldHIInfo				a2 					) ->         a1 `compare` a2
    (ImportExportImpl			a1 b1				, ImportExportImpl			a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ImportNameInfo				a1 b1				, ImportNameInfo			a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%]]
    (FoldHs						a1 b1				, FoldHs					a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (FoldHsMb					a1 					, FoldHsMb					a2 					) ->         a1 `compare` a2
    (FoldHsPMb					a1 b1				, FoldHsPMb					a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (FoldEH						a1 					, FoldEH					a2 					) ->         a1 `compare` a2
    (FoldEHMb					a1 					, FoldEHMb					a2 					) ->         a1 `compare` a2
    (FoldEHPMb					a1 b1				, FoldEHPMb					a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%[[(50 corein)
    (FoldCoreMod				a1 					, FoldCoreMod				a2 					) ->         a1 `compare` a2
    (FoldCoreModPMb				a1 b1				, FoldCoreModPMb			a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%]]
%%[[(8 core grin)
    (FoldCore2Grin				a1 					, FoldCore2Grin				a2 					) ->         a1 `compare` a2
    (FoldCore2GrinMb			a1 					, FoldCore2GrinMb			a2 					) ->         a1 `compare` a2
    (FoldCore2GrinPMb			a1 b1				, FoldCore2GrinPMb			a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%]]
%%[[(8 core corerun)
    -- (FoldCore2CoreRun			a1 					, FoldCore2CoreRun			a2 					) ->         a1 `compare` a2
    -- (FoldCore2CoreRunMb			a1 					, FoldCore2CoreRunMb		a2 					) ->         a1 `compare` a2
    (FoldCore2CoreRunPMb		a1 b1				, FoldCore2CoreRunPMb		a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%]]
%%[[(50 corerun corerunin)
    (FoldCoreRunMod				a1 					, FoldCoreRunMod			a2 					) ->         a1 `compare` a2
    (FoldCoreRunModPMb			a1 b1 				, FoldCoreRunModPMb			a2 b2				) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%]]
%%[[99
    (FPathPreprocessedWithCPP	a1 b1 				, FPathPreprocessedWithCPP	a2 b2 				) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ExposedPackages								, ExposedPackages								) -> EQ
%%]]
  where lexico = orderingLexic
        {-
        lexico (x:xs)
          | x == EQ   = lexico xs
          | otherwise = x
        lexico []     = EQ
        -}
  
instance Ord (BFun' res) where
  compare = bfunCompare

deriving instance Eq (BFun' res)
deriving instance Show (BFun' res)
deriving instance Typeable BFun'

instance Hashable (BFun' res) where
  hashWithSalt salt x = case x of
	CRSI 									-> salt `hashWithSalt` (maxBound-1::Int)
%%[[50
	-- CRSIWithCompileOrder		a b	c		-> salt `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
	CRSIWithCompileOrderP		a b	c		-> salt `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
	-- CRSIWithImps			   	a b	c		-> salt `hashWithSalt` (1::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
	CRSIWithImpsP			   	a b	c		-> salt `hashWithSalt` (1::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c
%%]]
	CRSIOfName			   		a b			-> salt `hashWithSalt` (2::Int) `hashWithSalt` a `hashWithSalt` b
	CRSIOfNameP			   		a b			-> salt `hashWithSalt` (2::Int) `hashWithSalt` a `hashWithSalt` b
	FPathSearchForFile 			a b			-> salt `hashWithSalt` (3::Int) `hashWithSalt` a `hashWithSalt` b
	FPathForAST					a b	c d	 	-> salt `hashWithSalt` (5::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
%%[[50
	ImportsOfName		   		a			-> salt `hashWithSalt` (6::Int) `hashWithSalt` a
	ImportsRecursiveWithImps	a b			-> salt `hashWithSalt` (7::Int) `hashWithSalt` a `hashWithSalt` b
	ImportsRecursiveOfName		a			-> salt `hashWithSalt` (8::Int) `hashWithSalt` a
%%]]
	ActualModNm					a			-> salt `hashWithSalt` (9::Int) `hashWithSalt` a
	EcuOf			   			a			-> salt `hashWithSalt` (9::Int) `hashWithSalt` a
	EcuOfName		   			a			-> salt `hashWithSalt` (10::Int) `hashWithSalt` a
	EHCOptsOf		   			a			-> salt `hashWithSalt` (11::Int) `hashWithSalt` a
	EcuOfPrevNameAndPath		a 			-> salt `hashWithSalt` (12::Int) `hashWithSalt` a
	EcuOfNameAndPath			a 			-> salt `hashWithSalt` (13::Int) `hashWithSalt` a
	ASTRefFromFileMb			a b	c d	e 	-> salt `hashWithSalt` (14::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e
	ASTRefFromFileEither		a b	c d	e f	-> salt `hashWithSalt` (14::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e `hashWithSalt` f
	ASTFromFile					a b	c d	e 	-> salt `hashWithSalt` (15::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e
	AST 						a b			-> salt `hashWithSalt` (16::Int) `hashWithSalt` a `hashWithSalt` b
	ASTP 						a b			-> salt `hashWithSalt` (16::Int) `hashWithSalt` a `hashWithSalt` b
	ASTMb 						a b			-> salt `hashWithSalt` (16::Int) `hashWithSalt` a `hashWithSalt` b
	ASTPMb 						a b			-> salt `hashWithSalt` (16::Int) `hashWithSalt` a `hashWithSalt` b
%%[[50
	ModfTimeOfFile				a b	c d		-> salt `hashWithSalt` (17::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
	ASTFileIsValid				a b	c d		-> salt `hashWithSalt` (17::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
	ASTFileIsNewerThan			a b			-> salt `hashWithSalt` (17::Int) `hashWithSalt` a `hashWithSalt` b
	DirOfModIsWriteable 		a 			-> salt `hashWithSalt` (18::Int) `hashWithSalt` a
	CanCompile		 			a 			-> salt `hashWithSalt` (19::Int) `hashWithSalt` a
	NeedsCompile		 		a 			-> salt `hashWithSalt` (19::Int) `hashWithSalt` a
	IsTopMod			 		a 			-> salt `hashWithSalt` (20::Int) `hashWithSalt` a
%%]]
%%[[50
	FoldHsMod			 		a b			-> salt `hashWithSalt` (21::Int) `hashWithSalt` a `hashWithSalt` b
	ModnameAndImports			a b			-> salt `hashWithSalt` (22::Int) `hashWithSalt` a `hashWithSalt` b
	HsModnameAndImports			a 			-> salt `hashWithSalt` (23::Int) `hashWithSalt` a
	FoldHIInfo					a 			-> salt `hashWithSalt` (24::Int) `hashWithSalt` a
	ImportExportImpl			a b			-> salt `hashWithSalt` (25::Int) `hashWithSalt` a `hashWithSalt` b
	ImportNameInfo				a b			-> salt `hashWithSalt` (26::Int) `hashWithSalt` a `hashWithSalt` b
%%]]
	FoldHs						a b			-> salt `hashWithSalt` (27::Int) `hashWithSalt` a `hashWithSalt` b
	FoldHsMb					a 			-> salt `hashWithSalt` (28::Int) `hashWithSalt` a
	FoldHsPMb					a b			-> salt `hashWithSalt` (28::Int) `hashWithSalt` a `hashWithSalt` b
	FoldEH						a 			-> salt `hashWithSalt` (29::Int) `hashWithSalt` a
	FoldEHMb					a 			-> salt `hashWithSalt` (30::Int) `hashWithSalt` a
	FoldEHPMb					a b			-> salt `hashWithSalt` (30::Int) `hashWithSalt` a `hashWithSalt` b
%%[[(50 corein)
	FoldCoreMod					a 			-> salt `hashWithSalt` (31::Int) `hashWithSalt` a
	FoldCoreModPMb				a b			-> salt `hashWithSalt` (31::Int) `hashWithSalt` a `hashWithSalt` b
%%]]
%%[[(8 core grin)
	FoldCore2Grin				a 			-> salt `hashWithSalt` (32::Int) `hashWithSalt` a
	FoldCore2GrinMb				a 			-> salt `hashWithSalt` (33::Int) `hashWithSalt` a
	FoldCore2GrinPMb			a b			-> salt `hashWithSalt` (33::Int) `hashWithSalt` a `hashWithSalt` b
%%]]
%%[[(8 core corerun)
	-- FoldCore2CoreRun			a 			-> salt `hashWithSalt` (34::Int) `hashWithSalt` a
	-- FoldCore2CoreRunMb			a 			-> salt `hashWithSalt` (36::Int) `hashWithSalt` a
	FoldCore2CoreRunPMb			a b			-> salt `hashWithSalt` (36::Int) `hashWithSalt` a `hashWithSalt` b
%%]]
%%[[(50 corerun corerunin)
	FoldCoreRunMod				a 			-> salt `hashWithSalt` (35::Int) `hashWithSalt` a
	FoldCoreRunModPMb			a b 		-> salt `hashWithSalt` (35::Int) `hashWithSalt` a `hashWithSalt` b
%%]]
%%[[99
	FPathPreprocessedWithCPP	a b			-> salt `hashWithSalt` (37::Int) `hashWithSalt` a `hashWithSalt` b
	ExposedPackages							-> salt `hashWithSalt` (maxBound-2::Int)
%%]]
	-- EcuMbOf			   			a			-> salt `hashWithSalt` (7::Int) `hashWithSalt` a

%%]

%%[8 export(BFun(..))
-- | BFun' used as a dependency of another BFun', for now same as a Dynamic
data BFun
  = forall res
    . ({- Typeable (BFun' res), -} Typeable res)
      => BFun
           { bfcdFun 		:: !(BFun' res)
           }

instance Eq BFun where
  (BFun {bfcdFun=f1}) == (BFun {bfcdFun=f2}) = bfunCompare f1 f2 == EQ

instance Ord BFun where
  (BFun {bfcdFun=f1}) `compare` (BFun {bfcdFun=f2}) = bfunCompare f1 f2

instance Hashable BFun where
  hashWithSalt salt (BFun {bfcdFun=x}) = hashWithSalt salt x

instance Show BFun where
  show (BFun {bfcdFun=x}) = show x
%%]

%%[8 export(BFunCacheEntry(..))
-- | BFun' + BCachedVal' packaged with required class instances, similar to a Dynamic
data BFunCacheEntry
  = forall f res
    . (Typeable f, Typeable res)
      => BFunCacheEntry
           { bfceFun 		:: !(BFun' res)
           , bfceVal		:: !(f res)
           }
%%]

%%[8 export(BCache(..), emptyBCache)
-- | Cache for function calls, first indexed on hash
data BCache
  = BCache
      { _bcacheCache			:: IMap.IntMap [BFunCacheEntry]
      , _bcacheModNmForward		:: Map.Map HsName HsName
      , _bcacheDpdRel			:: Rel.Rel BFun BFun
      }

emptyBCache :: BCache
emptyBCache = BCache IMap.empty Map.empty Rel.empty
%%]

%%[8 export(bcacheResolveModNm)
bcacheResolveModNm :: BCache -> HsName -> HsName
bcacheResolveModNm c n = maybe n (bcacheResolveModNm c) $ Map.lookup n (_bcacheModNmForward c)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build value reference (to global state)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(BRef(..))
-- | GADT for references to global state, interpreted inside the compiler driver monad, the type of the GADT telling what the type of the value should be.
data BRef val where
  --- | Global state
  BRef_CRSI
    :: BRef EHCompileRunStateInfo

%%[[99
  --- | Global info: exposed packages
  BRef_ExposedPackages
    :: BRef [PkgModulePartition]
%%]]

  --- | Compile unit
  BRef_ECU
    :: !HsName					--- ^ module name
    -> BRef EHCompileUnit

  --- | An AST embedded in a compile unit directly taken from a file
  BRef_ASTFile
    :: !PrevFileSearchKey		--- ^ module name
    -> ASTType					--- ^ content type
    -> ASTSuffixKey				--- ^ suffix and content variation
    -> ASTFileTiming			--- ^ timing (i.e. previous or current)
    -> BRef val

  --- | An AST embedded in a compile unit either directly taken from a file or derived by other means
  BRef_AST
    :: !PrevFileSearchKey		--- ^ module name
    -> ASTType					--- ^ content type
    -> BRef val

  --- | Global options
  BRef_EHCOpts
    :: !HsName					--- ^ module name
    -> BRef EHCOpts
  
deriving instance Typeable BRef
deriving instance Show (BRef val)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update functions in case a reference changes: new ref + forwarding ref
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(brefWithNewModNm)
-- | Construct new ref for new modNm
brefWithNewModNm :: HsName -> HsName -> BRef val -> Maybe (BRef val)
brefWithNewModNm oldNm newNm bref
  | oldNm == newNm = Nothing
  | otherwise      = case bref of
      BRef_ECU     nm                   | nm == oldNm -> Just $ BRef_ECU newNm
      BRef_EHCOpts nm                   | nm == oldNm -> Just $ BRef_EHCOpts newNm
      BRef_AST ((nm,fp),pr) t           | nm == oldNm -> Just $ BRef_AST ((newNm,fp),pr) t
      BRef_ASTFile ((nm,fp),pr) t sk tk | nm == oldNm -> Just $ BRef_ASTFile ((newNm,fp),pr) t sk tk
      _ -> Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build AST result
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(ASTResult(..))
-- | Result coming out of a build call for constructing/loading an AST
data ASTResult ast =
  ASTResult
    { _astresAST		:: ast
    , _astresRef		:: BRef ast
    , _astresPipe		:: ASTPipe
%%[[50
    , _astresTimeStamp	:: ClockTime
%%]]
    }
  deriving (Typeable, Show)
%%]

%%[8 export(mkASTResult')
mkASTResult'
  :: (Monad m, MonadIO m)
  => ast
  -> BRef ast
  -> ASTPipe
%%[[8
  -> Maybe ()
%%][50
  -> Maybe ClockTime
%%]]
  -> m (ASTResult ast)
mkASTResult' ast ref astpipe mbTm
    = do
%%[[50
    tm <- maybe (liftIO getClockTime) return mbTm
%%]]
    return $
      ASTResult ast ref astpipe
%%[[50
        tm
%%]]
%%]

%%[8 export(mkASTResult)
mkASTResult :: (Monad m, MonadIO m) => ast -> BRef ast -> ASTPipe -> m (ASTResult ast)
mkASTResult ast ref astpipe = mkASTResult' ast ref astpipe Nothing
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build state for Build functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(BState, emptyBState)
-- | Cache for function calls, first indexed on hash
data BState
  = BState
      { _bstateCache		:: !BCache
      , _bstateCallStack	:: ![BFun]
      }

emptyBState :: BState
emptyBState = BState emptyBCache []
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% IO info for debug
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(EHCIOInfo(..))
data EHCIOInfo
  = EHCIOInfo
      { ehcioinfoStartTime			:: EHCTime
      , ehcioinfoLastTime			:: EHCTime
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(EHCTime)
type EHCTime = Integer
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run state: corerun specific
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 corerun) export(EHCompileRunCoreRunStateInfo(..), emptyEHCompileRunCoreRunStateInfo)
data EHCompileRunCoreRunStateInfo
  = EHCompileRunCoreRunStateInfo
      { _crcrsiNrOfModules  	:: !Int                             	-- current nr of modules, the number is used to identify modules
      , _crcrsiNm2RefMp			:: !CoreRun.Nm2RefMp       				-- current inh attrs for CoreRun semantics
      }

emptyEHCompileRunCoreRunStateInfo :: EHCompileRunCoreRunStateInfo
emptyEHCompileRunCoreRunStateInfo
  = EHCompileRunCoreRunStateInfo
      { _crcrsiNrOfModules    	=   0
      , _crcrsiNm2RefMp			=	CoreRun.emptyNm2RefMp
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile run state: overall
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCompileRunStateInfo(..))
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { _crsiOpts       :: !EHCOpts                             -- options
      , _crsiASTPipe    :: !ASTPipe                             -- the compiler pipeline (based on options)
      , _crsiNextUID    :: !UID                                 -- unique id, the next one
      , _crsiHereUID    :: !UID                                 -- unique id, the current one
      , _crsiHSInh      :: !AST_HS_Inh_Check                    -- current inh attrs for HS sem
      , _crsiEHInh      :: !AST_EH_Inh_Check                    -- current inh attrs for EH sem
      , _crsiFileSuffMp :: FileSuffMp							-- allowed suffixes
%%[[(8 codegen)
      , _crsiCoreInh    :: !AST_Core_Inh_ToGrin	                -- current inh attrs for Core2Grin sem
%%]]
%%[[(8 corerun)
      , _crsiCoreRunState	:: !EHCompileRunCoreRunStateInfo	-- corerun compilation specific state
      -- , _crsiCore2RunInh:: !CoreRun.Nm2RefMp       				-- current inh attrs for Core2CoreRun sem
%%]]
%%[[50
      , crsiMbMainNm    :: !(Maybe HsName)                      -- name of main module, if any
      , crsiHSModInh    :: !HSSemMod.Inh_AGItf                  -- current inh attrs for HS module analysis sem
      , crsiModMp       :: !ModMp                               -- import/export info for modules
      , crsiGrpMp       :: (Map.Map HsName EHCompileGroup)      -- not yet used, for mut rec modules
      , crsiOptim       :: !Optim                               -- inter module optimisation info
%%]]
%%[[(50 codegen)
      , crsiModOffMp    :: !VA.HsName2FldMpMp              		-- mapping of all modules + exp entries to offsets in module + exp tables
%%]]
%%[[99
      , crsiEHCIOInfo	:: !(IORef EHCIOInfo)					-- unsafe info
      , crsiFilesToRm   :: ![FPath]                             -- files to clean up (remove)
%%]]
      , _crsiBState		:: !BState								-- Build state for use of build functions
      }
  deriving (Typeable)
%%]

%%[8 export(emptyEHCompileRunStateInfo)
emptyEHCompileRunStateInfo :: EHCompileRunStateInfo
emptyEHCompileRunStateInfo
  = EHCompileRunStateInfo
      { _crsiOpts       =   defaultEHCOpts
      , _crsiASTPipe    =   emptyASTPipe
      , _crsiNextUID    =   uidStart
      , _crsiHereUID    =   uidStart
      , _crsiHSInh      =   panic "emptyEHCompileRunStateInfo.crsiHSInh"
      , _crsiEHInh      =   panic "emptyEHCompileRunStateInfo.crsiEHInh"
      , _crsiFileSuffMp =	emptyFileSuffMp
%%[[(8 codegen)
      , _crsiCoreInh    =   panic "emptyEHCompileRunStateInfo.crsiCoreInh"
%%]]
%%[[(8 corerun)
      , _crsiCoreRunState	=   emptyEHCompileRunCoreRunStateInfo
      -- , _crsiCore2RunInh=   panic "emptyEHCompileRunStateInfo.crsiCoreRunInh"
%%]]
%%[[50
      , crsiMbMainNm    =   Nothing
      , crsiHSModInh    =   panic "emptyEHCompileRunStateInfo.crsiHSModInh"
      , crsiModMp       =   Map.empty
      , crsiGrpMp       =   Map.empty
      , crsiOptim       =   defaultOptim
%%]]
%%[[(50 codegen)
      , crsiModOffMp    =   Map.empty
%%]]
%%[[99
      , crsiEHCIOInfo   =   panic "emptyEHCompileRunStateInfo.crsiEHCIOInfo"
      , crsiFilesToRm   =   []
%%]]
      , _crsiBState   	=   emptyBState
      }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Template stuff
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(bcacheCache, bcacheDpdRel, bcacheModNmForward)
mkLabel ''BCache
%%]

%%[8 export(bstateCache, bstateCallStack)
mkLabel ''BState
%%]

%%[(8 corerun) export(crcrsiNrOfModules,crcrsiNm2RefMp)
mkLabel ''EHCompileRunCoreRunStateInfo
%%]

%%[8 export(crsiOpts, crsiASTPipe, crsiNextUID, crsiHereUID, crsiHSInh, crsiEHInh, crsiBState, crsiFileSuffMp)
mkLabel ''EHCompileRunStateInfo
%%]

%%[(8 codegen) export(crsiCoreInh)
%%]

%%[(8888 corerun) export(crsiCore2RunInh)
%%]

%%[(8 corerun) export(crsiCoreRunState)
%%]

%%[8 export(astresAST, astresRef, astresPipe)
mkLabel ''ASTResult
%%]

%%[50 export(astresTimeStamp)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Show EHCompileRunStateInfo where
  show _ = "EHCompileRunStateInfo"

instance PP EHCompileRunStateInfo where
  pp i = "CRSI:" >#< ppModMp (crsiModMp i)
%%]

%%[8
instance CompileRunStateInfo EHCompileRunStateInfo HsName () where
  crsiImportPosOfCUKey n i = ()
%%]

%%[8 export(EHCCompileRunner)
class ( MonadIO m
      , MonadFix m
      -- , MonadIO (EHCompilePhaseAddonT m)
      , CompileRunner FileSuffInitState HsName () FileLoc EHCompileUnit EHCompileRunStateInfo Err (EHCompilePhaseAddonT m)
      )
  => EHCCompileRunner m where

instance ( CompileRunStateInfo EHCompileRunStateInfo HsName ()
         , CompileUnit EHCompileUnit HsName FileLoc FileSuffInitState
         , CompileRunError Err ()
         -- , MonadError (CompileRunState Err) m
         -- , MonadState EHCompileRun (EHCompilePhaseAddonT m)
         , MonadIO m
         , MonadFix m
         -- , MonadIO (EHCompilePhaseAddonT m)
         , Monad m
         ) => CompileRunner FileSuffInitState HsName () FileLoc EHCompileUnit EHCompileRunStateInfo Err (EHCompilePhaseAddonT m)

instance ( CompileRunStateInfo EHCompileRunStateInfo HsName ()
         , CompileUnit EHCompileUnit HsName FileLoc FileSuffInitState
         , CompileRunError Err ()
         -- , MonadError (CompileRunState Err) m
         -- , MonadState EHCompileRun (EHCompilePhaseAddonT m)
         , MonadIO m
         , MonadFix m
         -- , MonadIO (EHCompilePhaseAddonT m)
         , Monad m
         ) => EHCCompileRunner m

{-
instance (MonadState s m) => MonadState s (EHCompilePhaseAddonT m) where
  get = lift MS.get
  put = lift . MS.put

instance (MonadIO m) => MonadIO (EHCompilePhaseAddonT m) where
  liftIO = lift . liftIO
-}
%%]

%%[8 export(EHCompileRun,EHCompilePhaseT,EHCompilePhase)
type EHCompileRun           = CompileRun HsName EHCompileUnit EHCompileRunStateInfo Err
type EHCompilePhaseAddonT m = StateT EHCompileRun m
type EHCompilePhaseT      m = CompilePhaseT HsName EHCompileUnit EHCompileRunStateInfo Err (EHCompilePhaseAddonT m)
type EHCompilePhase         = EHCompilePhaseT IO
%%]
-- type EHCompilePhase a = CompilePhase HsName EHCompileUnit EHCompileRunStateInfo Err a


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile Run base info
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(crBaseInfo,crMbBaseInfo,crBaseInfo')
crBaseInfo' :: EHCompileRun -> (EHCompileRunStateInfo,EHCOpts)
crBaseInfo' cr
  = (crsi,opts)
  where crsi   = _crStateInfo cr
        opts   = crsi ^. crsiOpts

crMbBaseInfo :: HsName -> EHCompileRun -> (Maybe EHCompileUnit, EHCompileRunStateInfo, EHCOpts, Maybe FPath)
crMbBaseInfo modNm cr
  = ( mbEcu ,crsi
%%[[8
    , opts
%%][99
    -- if any per module opts are available, use those
    , maybe opts id $ mbEcu >>= ecuMbOpts
%%]]
    , fmap ecuFilePath mbEcu
    )
  where mbEcu       = crMbCU modNm cr
        (crsi,opts) = crBaseInfo' cr

crBaseInfo :: HsName -> EHCompileRun -> (EHCompileUnit,EHCompileRunStateInfo,EHCOpts,FPath)
crBaseInfo modNm cr
  = ( maybe (panic $ "crBaseInfo.mbEcu " ++ show modNm) id mbEcu 
    , crsi
    , opts
    , maybe (panic $ "crBaseInfo.mbFp " ++ show modNm) id mbFp
    )
  where (mbEcu, crsi, opts, mbFp) = crMbBaseInfo modNm cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: step/set unique counter
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpStepUID,cpSetUID)
cpStepUID :: EHCCompileRunner m => EHCompilePhaseT m ()
cpStepUID
  = cpUpdSI (\crsi -> let (n,h) = mkNewLevUID (crsi ^. crsiNextUID)
                      in  crsiNextUID ^= n $ crsiHereUID ^= h $ crsi
                          -- crsi {_crsiNextUID = n, _crsiHereUID = h}
            )

cpSetUID :: EHCCompileRunner m => UID -> EHCompilePhaseT m ()
cpSetUID u
  = cpUpdSI $ crsiNextUID ^= u -- (\crsi -> crsi {crsiNextUID = u})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Time
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(EHCTimeDiff, getEHCTime, ehcTimeDiff, ehcTimeDiffFmt)
type EHCTimeDiff = Integer

getEHCTime :: IO EHCTime
getEHCTime = getCPUTime

ehcTimeDiff :: EHCTime -> EHCTime -> EHCTimeDiff
ehcTimeDiff = (-)

ehcTimeDiffFmt :: EHCTimeDiff -> String
ehcTimeDiffFmt t
  = fm 2 hrs ++ ":" ++ fm 2 mins ++ ":" ++ fm 2 secs ++ ":" ++ fm 6 (psecs `div` 1000000)
  where (r0  , psecs) = t  `quotRem` 1000000000000
        (r1  , secs ) = r0 `quotRem` 60
        (r2  , mins ) = r1 `quotRem` 60
        (days, hrs  ) = r2 `quotRem` 24
        fm n x = strPadLeft '0' n (show x)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: clean up (remove) files to be removed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[99 export(cpRegisterFilesToRm)
cpRegisterFilesToRm :: EHCCompileRunner m => [FPath] -> EHCompilePhaseT m ()
cpRegisterFilesToRm fpL
  = cpUpdSI (\crsi -> crsi {crsiFilesToRm = fpL ++ crsiFilesToRm crsi})
%%]

%%[99 export(cpRmFilesToRm)
cpRmFilesToRm :: EHCCompileRunner m => EHCompilePhaseT m ()
cpRmFilesToRm
  = do { cr <- MS.get
       ; let (crsi,opts) = crBaseInfo' cr
             files = Set.toList $ Set.fromList $ map fpathToStr $ crsiFilesToRm crsi
       ; liftIO $ mapM rm files
       ; cpUpdSI (\crsi -> crsi {crsiFilesToRm = []})
       }
  where rm f = CE.catch (removeFile f)
                        (\(e :: SomeException) -> hPutStrLn stderr (show f ++ ": " ++ show e))
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpTrPP, cpTr)
-- | Tracing
cpTrPP :: EHCCompileRunner m => TraceOn -> [PP_Doc] -> EHCompilePhaseT m ()
cpTrPP ton ms = do
    cr <- MS.get
    let (_,opts) = crBaseInfo' cr
    when (ton `elem` ehcOptTraceOn opts) $ liftIO $ pr ms
  where pr []      = return ()
        pr [m]     = putPPLn $ show ton >|< ":" >#< m
        pr (m:ms)  = do pr [m]
                        forM_ ms $ \m -> putPPLn $ indent 2 m

-- | Tracing
cpTr :: EHCCompileRunner m => TraceOn -> [String] -> EHCompilePhaseT m ()
cpTr ton ms = cpTrPP ton $ map pp ms
%%]
-- | Tracing
cpTr :: EHCCompileRunner m => TraceOn -> [String] -> EHCompilePhaseT m ()
cpTr ton ms = do
    cr <- MS.get
    let (_,opts) = crBaseInfo' cr
    when (ton `elem` ehcOptTraceOn opts) $ liftIO $ pr ms
  where pr []      = return ()
        pr [m]     = putStrLn $ show ton ++ ": " ++ m
        pr (m:ms)  = do pr [m]
                        forM_ ms $ \m -> putStrLn $ "  " ++ m

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: debug info, mem usage
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
cpMemUsage :: EHCCompileRunner m => EHCompilePhaseT m ()
cpMemUsage
%%[[8
  = return ()
%%][102
  = do { cr <- MS.get
       ; let (crsi,opts) = crBaseInfo' cr
       ; size <- liftIO $ megaBytesAllocated
       ; when (ehcOptVerbosity opts >= VerboseDebug)
              (liftIO $ putStrLn ("Mem: " ++ show size ++ "M"))
       }
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: message
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpMsg,cpMsg')
-- | Message
cpMsg :: EHCCompileRunner m => HsName -> Verbosity -> String -> EHCompilePhaseT m ()
cpMsg modNm v m
  = do { cr <- MS.get
       ; let (_,_,_,mbFp) = crMbBaseInfo modNm cr
       ; cpMsg' modNm v m Nothing (maybe emptyFPath id mbFp)
       }

cpMsg' :: EHCCompileRunner m => HsName -> Verbosity -> String -> Maybe String -> FPath -> EHCompilePhaseT m ()
cpMsg' modNm v m mbInfo fp
  = do { cr <- MS.get
       ; let (mbEcu,crsi,opts,_) = crMbBaseInfo modNm cr
%%[[99
       ; ehcioinfo <- liftIO $ readIORef (crsiEHCIOInfo crsi)
       ; clockTime <- liftIO getEHCTime
       ; let clockStartTimePrev = ehcioinfoStartTime ehcioinfo
             clockTimePrev      = ehcioinfoLastTime ehcioinfo
             clockStartTimeDiff = ehcTimeDiff clockTime clockStartTimePrev
             clockTimeDiff      = ehcTimeDiff clockTime clockTimePrev
%%]]
       ; let
%%[[8
             m'             = m
%%][99
             t				= if v >= VerboseALot then "<" ++ strBlankPad 35 (ehcTimeDiffFmt clockStartTimeDiff ++ "/" ++ ehcTimeDiffFmt clockTimeDiff) ++ ">" else ""
             m'             = maybe "" (\ecu -> show (ecuSeqNr ecu) ++ t ++ " ") mbEcu ++ m
%%]]
       ; liftIO $ putCompileMsg v (ehcOptVerbosity opts) m' mbInfo modNm fp
%%[[99
       ; clockTime <- liftIO getEHCTime
       ; liftIO $ writeIORef (crsiEHCIOInfo crsi) (ehcioinfo {ehcioinfoLastTime = clockTime})
       -- ; cpUpdSI (\crsi -> crsi { crsiTime = clockTime })
%%]]
       ; cpMemUsage
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Compile actions: shell/system/cmdline invocation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpSystem',cpSystem)
cpSystem' :: EHCCompileRunner m => Maybe FilePath -> (FilePath,[String]) -> EHCompilePhaseT m ()
cpSystem' mbStdOut (cmd,args)
  = do { exitCode <- liftIO $ system $ showShellCmd $ (cmd,args ++ (maybe [] (\o -> [">", o]) mbStdOut))
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }

cpSystem :: EHCCompileRunner m => (FilePath,[String]) -> EHCompilePhaseT m ()
cpSystem = cpSystem' Nothing
%%]
cpSystem' :: (FilePath,[String]) -> Maybe FilePath -> EHCompilePhase ()
cpSystem' (cmd,args) mbStdOut
  = do { exitcode <- liftIO $ do 
           proc <- runProcess cmd args Nothing Nothing Nothing Nothing Nothing
           waitForProcess proc
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }

cpSystem' :: (FilePath,[String]) -> Maybe FilePath -> EHCompilePhase ()
cpSystem' (cmd,args) mbStdOut
  = do { exitCode <- liftIO $ system cmd
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetFail
       }

%%[8 export(cpSystemRaw)
cpSystemRaw :: EHCCompileRunner m => String -> [String] -> EHCompilePhaseT m ()
cpSystemRaw cmd args
  = do { exitCode <- liftIO $ rawSystem cmd args
       ; case exitCode of
           ExitSuccess -> return ()
           _           -> cpSetErrs [rngLift emptyRange Err_PP $ pp $ show exitCode] -- cpSetFail
       }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Partition modules into those belonging to a package and the rest
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(99 codegen) export(crPartitionIntoPkgAndOthers)
-- | split module names in those part of a package, and others
crPartitionIntoPkgAndOthers :: EHCompileRun -> [HsName] -> ([PkgModulePartition],[HsName])
crPartitionIntoPkgAndOthers cr modNmL
  = ( [ (p,d,m)
      | ((p,d),m) <- Map.toList $ Map.unionsWith (++) $ map Map.fromList ps
      ]
    , concat ms
    )
  where (ps,ms) = unzip $ map loc modNmL
        loc m = case filelocKind $ ecuFileLocation ecu of
                  FileLocKind_Dir	  -> ([           ], [m])
                  FileLocKind_Pkg p d -> ([((p,d),[m])], [ ])
              where (ecu,_,_,_) = crBaseInfo m cr
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Export info, offset of names
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) export(crsiExpNmOffMpDbg, crsiExpNmOffMp)
crsiExpNmOffMpDbg :: String -> HsName -> EHCompileRunStateInfo -> VA.HsName2FldMp
crsiExpNmOffMpDbg ctxt modNm crsi = mmiNmOffMp $ panicJust ("crsiExpNmOffMp." ++ ctxt ++ show ks ++ ": " ++ show modNm) $ Map.lookup modNm $ crsiModMp crsi
  where ks = Map.keys $ crsiModMp crsi

crsiExpNmOffMp :: HsName -> EHCompileRunStateInfo -> VA.HsName2FldMp
crsiExpNmOffMp modNm crsi = mmiNmOffMp $ panicJust ("crsiExpNmOffMp: " ++ show modNm) $ Map.lookup modNm $ crsiModMp crsi
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Update: already flowed
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(cpUpdAlreadyFlowIntoCRSIWith, cpUpdAlreadyFlowIntoCRSI)
-- | Add ast types for which the semantics have been flowed into global state, with additional from/to info
cpUpdAlreadyFlowIntoCRSIWith :: EHCCompileRunner m => HsName -> ASTType -> ASTAlreadyFlowIntoCRSIInfo -> EHCompilePhaseT m ()
cpUpdAlreadyFlowIntoCRSIWith modNm asttype flowstage =
  cpUpdCU modNm $  ecuAlreadyFlowIntoCRSI
               ^$= Map.insertWith Set.union asttype (Set.singleton flowstage)

-- | Add ast types for which the semantics have been flowed into global state
cpUpdAlreadyFlowIntoCRSI :: EHCCompileRunner m => HsName -> ASTType -> ASTSemFlowStage -> EHCompilePhaseT m ()
cpUpdAlreadyFlowIntoCRSI modNm asttype flowstage = cpUpdAlreadyFlowIntoCRSIWith modNm asttype (flowstage,Nothing)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Replacement lookup/update for compile units using the forwarding name mechanism
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(bLookupECUInCR, bLookupECU', bLookupECU, bUpdECU)
-- | Lookup compile unit, also giving the actual module name forwarded to
bLookupECUInCR :: HsName -> EHCompileRun -> Maybe (HsName, EHCompileUnit)
bLookupECUInCR n cr = lkn n <|> lkn (bcacheResolveModNm (cr ^. crStateInfo ^. crsiBState ^. bstateCache) n)
  where lkn n = fmap ((,) n) $ crMbCU n cr

-- | Lookup compile unit, also giving the actual module name forwarded to
bLookupECU' :: EHCCompileRunner m => HsName -> EHCompilePhaseT m (Maybe (HsName, EHCompileUnit))
bLookupECU' n = MS.gets (bLookupECUInCR n)

-- | Lookup compile unit
bLookupECU :: EHCCompileRunner m => HsName -> EHCompilePhaseT m (Maybe EHCompileUnit)
bLookupECU n = fmap (fmap snd) $ bLookupECU' n

-- | Update compile unit
bUpdECU :: EHCCompileRunner m => HsName -> (EHCompileUnit -> EHCompileUnit) -> EHCompilePhaseT m ()
bUpdECU n f = do
    cr <- MS.get
    cpUpdCU (maybe n fst $ bLookupECUInCR n cr) f
%%]
