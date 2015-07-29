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


-- Core syntax and semantics
%%[(8 core) import(qualified {%{EH}Core} as Core, qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]
%%[(8 core corerun) import(qualified {%{EH}Core.ToCoreRun} as Core2CoreRunSem)
%%]
%%[(50 codegen corein) import(qualified {%{EH}Core.Check} as Core2ChkSem)
%%]
-- CoreRun syntax and semantics
%%[(8 corerun) import(qualified {%{EH}CoreRun} as CoreRun)
%%]
%%[(50 codegen corerunin) import(qualified {%{EH}CoreRun.Check} as CoreRun2ChkSem)
%%]

-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.Main} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]

-- Language semantics: Core
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
%%]

-- HI syntax and semantics
%%[50 import(qualified {%{EH}HI} as HI)
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
  --- | Obtain global state specific for a imports
  CRSIWithImps
    :: !(Maybe PrevSearchInfo)
    -> !(Set.Set HsName)				--- ^ imports
    -> BFun' EHCompileRunStateInfo
%%]]

  --- | Obtain global state specific for a module, which depends on the (imported) module names
  CRSIOfName
    :: !PrevFileSearchKey				--- ^ module name etc
    -> BFun' EHCompileRunStateInfo

  --- | Obtain FPath and module name of a file name
  FPathSearchForFile
    :: !String				--- ^ suffix, if absent in name
    -> !FilePath			--- ^ file name
    -> BFun' (HsName, FPath)

  --- | Obtain FPath of an (imported module)
  FPathOfImported
    :: !HsName				--- ^ module name
    -> BFun' FPath

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

  --- | Get a particular AST from file for a module
  ASTFromFile
    :: !PrevFileSearchKey				--- ^ module name and possibly known path
    -> !(AlwaysEq ASTFileTimeHandleHow)	--- ^ how to deal with timestamp
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' res

%%[[50
  --- | Get the modification ClockTime of a file for a module
  ModfTimeOfFile
    :: !HsName							--- ^ module name and possibly known path
    -> !ASTType							--- ^ content type
    -> !ASTSuffixKey					--- ^ suffix and content variation
    -> !ASTFileTiming					--- ^ timing (i.e. previous or current)
    -> BFun' (Maybe ClockTime)

  --- | Get writeability of the dir a module resides in
  DirOfModIsWriteable
    :: !HsName							--- ^ module name and possibly known path
    -> BFun' Bool

  --- | Can compile a src module
  EcuCanCompile
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
         ( HSSemMod.Syn_AGItf			-- all semantics
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
         ( HI.HIInfo					-- all semantics
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
    -> BFun'
         ( HSSem.Syn_AGItf				-- all semantics
%%[[50
         -- , Set.Set HsName				-- declared imported module names
         , Bool							-- is main module?
%%]]
         )

  --- | EH semantics
  FoldEH
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( EHSem.Syn_AGItf				-- all semantics
         )

%%[[(50 corein)
  --- | Core as src semantics
  FoldCoreMod
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( Core2ChkSem.Syn_CodeAGItf			-- all semantics
         , HsName								-- real mod name
         , Set.Set HsName						-- declared imported module names
         , Mod									-- module import/export etc info
         , Bool									-- is main module?
         , Maybe PrevSearchInfo
         )
%%]]

%%[[(50 corerunin)
  --- | CoreRun as src semantics
  FoldCoreRunMod
    :: !PrevFileSearchKey							--- ^ module name and possibly known path
    -> BFun'
         ( CoreRun2ChkSem.Syn_AGItf				-- all semantics
         , HsName								-- real mod name
         , Set.Set HsName						-- declared imported module names
         , Mod									-- module import/export etc info
         , Bool									-- is main module?
         , Maybe PrevSearchInfo
         )
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
    (CRSI				 						, CRSI 										) -> EQ
%%[[50
    (CRSIWithImps			    a1 b1  			, CRSIWithImps		    	a2 b2  			) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%]]
    (CRSIOfName			    	a1   			, CRSIOfName		    	a2   			) ->         a1 `compare` a2
    (FPathSearchForFile 		a1 b1			, FPathSearchForFile 		a2 b2			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (FPathOfImported    		a1   			, FPathOfImported    		a2   			) ->         a1 `compare` a2
%%[[50
    (ImportsOfName          	a1   			, ImportsOfName          	a2   			) ->         a1 `compare` a2
    (ImportsRecursiveWithImps	a1 b1  			, ImportsRecursiveWithImps	a2 b2  			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ImportsRecursiveOfName		a1   			, ImportsRecursiveOfName	a2   			) ->         a1 `compare` a2
%%]]
    (EcuOf		              	a1   			, EcuOf	    				a2   			) ->         a1 `compare` a2
    -- (EcuMbOf		           	a1   			, EcuMbOf	    			a2   			) ->         a1 `compare` a2
    (EcuOfName              	a1   			, EcuOfName    				a2   			) ->         a1 `compare` a2
    (EcuOfPrevNameAndPath		a1 				, EcuOfPrevNameAndPath		a2 				) ->         a1 `compare` a2
    (EcuOfNameAndPath			a1 				, EcuOfNameAndPath			a2 				) ->         a1 `compare` a2
    (EHCOptsOf             		a1   			, EHCOptsOf					a2   			) ->         a1 `compare` a2
    (ASTFromFile            	a1 b1 c1 d1	e1 	, ASTFromFile				a2 b2 c2 d2	e2	) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2, e1 `compare` e2]
%%[[50
    (ModfTimeOfFile         	a1 b1 c1 d1		, ModfTimeOfFile			a2 b2 c2 d2		) -> lexico [a1 `compare` a2, b1 `compare` b2, c1 `compare` c2, d1 `compare` d2]
    (DirOfModIsWriteable		a1   			, DirOfModIsWriteable		a2   			) ->         a1 `compare` a2
    (EcuCanCompile				a1 				, EcuCanCompile				a2 				) ->         a1 `compare` a2
    (IsTopMod					a1 				, IsTopMod					a2 				) ->         a1 `compare` a2
%%]]
%%[[50
    (FoldHsMod					a1 b1			, FoldHsMod					a2 b2			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ModnameAndImports			a1 b1			, ModnameAndImports			a2 b2			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (HsModnameAndImports		a1 				, HsModnameAndImports		a2 				) ->         a1 `compare` a2
    (FoldHIInfo					a1 				, FoldHIInfo				a2 				) ->         a1 `compare` a2
    (ImportExportImpl			a1 b1			, ImportExportImpl			a2 b2			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ImportNameInfo				a1 b1			, ImportNameInfo			a2 b2			) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%]]
    (FoldHs						a1 				, FoldHs					a2 				) ->         a1 `compare` a2
    (FoldEH						a1 				, FoldEH					a2 				) ->         a1 `compare` a2
%%[[(50 corein)
    (FoldCoreMod				a1 				, FoldCoreMod				a2 				) ->         a1 `compare` a2
%%]]
%%[[(50 corerunin)
    (FoldCoreRunMod				a1 				, FoldCoreRunMod			a2 				) ->         a1 `compare` a2
%%]]
%%[[99
    (FPathPreprocessedWithCPP	a1 b1 			, FPathPreprocessedWithCPP	a2 b2 			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (ExposedPackages							, ExposedPackages							) -> EQ
%%]]
  where lexico (x:xs)
          | x == EQ   = lexico xs
          | otherwise = x
        lexico []     = EQ
  
instance Ord (BFun' res) where
  compare = bfunCompare

deriving instance Eq (BFun' res)
deriving instance Show (BFun' res)
deriving instance Typeable BFun'

instance Hashable (BFun' res) where
  hashWithSalt salt x = case x of
	CRSI 									-> salt `hashWithSalt` (maxBound-1::Int)
%%[[50
	CRSIWithImps			   	a b			-> salt `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b
%%]]
	CRSIOfName			   		a			-> salt `hashWithSalt` (1::Int) `hashWithSalt` a
	FPathSearchForFile 			a b			-> salt `hashWithSalt` (2::Int) `hashWithSalt` a `hashWithSalt` b
	FPathOfImported	   			a			-> salt `hashWithSalt` (3::Int) `hashWithSalt` a
%%[[50
	ImportsOfName		   		a			-> salt `hashWithSalt` (4::Int) `hashWithSalt` a
	ImportsRecursiveWithImps	a b			-> salt `hashWithSalt` (5::Int) `hashWithSalt` a `hashWithSalt` b
	ImportsRecursiveOfName		a			-> salt `hashWithSalt` (6::Int) `hashWithSalt` a
%%]]
	EcuOf			   			a			-> salt `hashWithSalt` (7::Int) `hashWithSalt` a
	-- EcuMbOf			   			a			-> salt `hashWithSalt` (7::Int) `hashWithSalt` a
	EcuOfName		   			a			-> salt `hashWithSalt` (9::Int) `hashWithSalt` a
	EHCOptsOf		   			a			-> salt `hashWithSalt` (10::Int) `hashWithSalt` a
	EcuOfPrevNameAndPath		a 			-> salt `hashWithSalt` (11::Int) `hashWithSalt` a
	EcuOfNameAndPath			a 			-> salt `hashWithSalt` (12::Int) `hashWithSalt` a
	ASTFromFile					a b	c d	e 	-> salt `hashWithSalt` (13::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e
%%[[50
	ModfTimeOfFile				a b	c d		-> salt `hashWithSalt` (14::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
	DirOfModIsWriteable 		a 			-> salt `hashWithSalt` (15::Int) `hashWithSalt` a
	EcuCanCompile		 		a 			-> salt `hashWithSalt` (16::Int) `hashWithSalt` a
	IsTopMod			 		a 			-> salt `hashWithSalt` (17::Int) `hashWithSalt` a
%%]]
%%[[50
	FoldHsMod			 		a b			-> salt `hashWithSalt` (18::Int) `hashWithSalt` a `hashWithSalt` b
	ModnameAndImports			a b			-> salt `hashWithSalt` (19::Int) `hashWithSalt` a `hashWithSalt` b
	HsModnameAndImports			a 			-> salt `hashWithSalt` (20::Int) `hashWithSalt` a
	FoldHIInfo					a 			-> salt `hashWithSalt` (21::Int) `hashWithSalt` a
	ImportExportImpl			a b			-> salt `hashWithSalt` (22::Int) `hashWithSalt` a `hashWithSalt` b
	ImportNameInfo				a b			-> salt `hashWithSalt` (23::Int) `hashWithSalt` a `hashWithSalt` b
%%]]
	FoldHs						a 			-> salt `hashWithSalt` (24::Int) `hashWithSalt` a
	FoldEH						a 			-> salt `hashWithSalt` (25::Int) `hashWithSalt` a
%%[[(50 corein)
	FoldCoreMod					a 			-> salt `hashWithSalt` (26::Int) `hashWithSalt` a
%%]]
%%[[(50 corerunin)
	FoldCoreRunMod				a 			-> salt `hashWithSalt` (27::Int) `hashWithSalt` a
%%]]
%%[[99
	FPathPreprocessedWithCPP	a b			-> salt `hashWithSalt` (28::Int) `hashWithSalt` a `hashWithSalt` b
	ExposedPackages							-> salt `hashWithSalt` (maxBound-2::Int)
%%]]

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
      { _bcacheCache	:: IMap.IntMap [BFunCacheEntry]
      , _bcacheDpdRel	:: Rel.Rel BFun BFun
      }

emptyBCache :: BCache
emptyBCache = BCache IMap.empty Rel.empty
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

  --- | An AST embedded in a compile unit
  BRef_AST
    :: !PrevFileSearchKey		--- ^ module name
    -> ASTType					--- ^ content type
    -> ASTSuffixKey				--- ^ suffix and content variation
    -> ASTFileTiming			--- ^ timing (i.e. previous or current)
    -> BRef val

  --- | Global options
  BRef_EHCOpts
    :: !HsName					--- ^ module name
    -> BRef EHCOpts
  
deriving instance Typeable BRef
deriving instance Show (BRef val)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function result cache, indexed by function call 'BFun'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8888 export(BCache, emptyBCache)
-- | Cache for function calls, first indexed on hash
data BCache
  = BCache
      { _bcacheCache	:: IMap.IntMap [BFunCacheEntry]
      , _bcacheDpdRel	:: Rel.Rel BFun BFun
      }

emptyBCache :: BCache
emptyBCache = BCache IMap.empty Rel.empty
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
%%% Compile run combinators
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(EHCompileRunStateInfo(..))
data EHCompileRunStateInfo
  = EHCompileRunStateInfo
      { _crsiOpts       :: !EHCOpts                             -- options
      , _crsiNextUID    :: !UID                                 -- unique id, the next one
      , _crsiHereUID    :: !UID                                 -- unique id, the current one
      , _crsiHSInh      :: !HSSem.Inh_AGItf                     -- current inh attrs for HS sem
      , _crsiEHInh      :: !EHSem.Inh_AGItf                     -- current inh attrs for EH sem
      , _crsiFileSuffMp :: FileSuffMp							-- allowed suffixes
%%[[(8 codegen)
      , crsiCoreInh     :: !Core2GrSem.Inh_CodeAGItf            -- current inh attrs for Core2Grin sem
%%]]
%%[[(8 corerun)
      , crsiCore2RunInh	:: !CoreRun.Nm2RefMp       				-- current inh attrs for Core2CoreRun sem
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
      , _crsiNextUID    =   uidStart
      , _crsiHereUID    =   uidStart
      , _crsiHSInh      =   panic "emptyEHCompileRunStateInfo.crsiHSInh"
      , _crsiEHInh      =   panic "emptyEHCompileRunStateInfo.crsiEHInh"
      , _crsiFileSuffMp =	emptyFileSuffMp
%%[[(8 codegen)
      , crsiCoreInh     =   panic "emptyEHCompileRunStateInfo.crsiCoreInh"
%%]]
%%[[(8 corerun)
      , crsiCore2RunInh	=   panic "emptyEHCompileRunStateInfo.crsiCoreRunInh"
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

%%[8 export(bcacheCache, bcacheDpdRel)
mkLabel ''BCache
%%]

%%[8 export(bstateCache, bstateCallStack)
mkLabel ''BState
%%]

%%[8 export(crsiOpts, crsiNextUID, crsiHereUID, crsiHSInh, crsiEHInh, crsiBState, crsiFileSuffMp)
mkLabel ''EHCompileRunStateInfo
%%]

