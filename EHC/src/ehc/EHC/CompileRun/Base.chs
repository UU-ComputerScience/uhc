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
%%[99 import(qualified {%{EH}Base.Pragma} as Pragma)
%%]
-- module admin
%%[50 import({%{EH}Module.ImportExport})
%%]


-- Language syntax: CoreRun
%%[(8 corerun) import( qualified {%{EH}CoreRun} as CoreRun)
%%]

-- Language semantics: HS, EH
%%[8 import(qualified {%{EH}EH.MainAG} as EHSem, qualified {%{EH}HS.MainAG} as HSSem)
%%]

-- Language semantics: Core
%%[(8 core) import(qualified {%{EH}Core.ToGrin} as Core2GrSem)
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

  --- | Obtain FPath and module name of a file name
  FPathSearchForFile
    :: !String				--- ^ suffix, if absent in name
    -> !FilePath			--- ^ file name
    -> BFun' (HsName, FPath)

  --- | Obtain FPath of an (imported module)
  FPathOfImported
    :: !HsName				--- ^ module name
    -> BFun' FPath

  --- | Extract imported modules from a module
  ImportsOf
    :: !HsName				--- ^ module name
    -> BFun' [HsName]

  --- | Extract compileunit from a module, as is, no checks on consistency
  EcuOf
    :: !HsName				--- ^ module name
    -> BFun' EHCompileUnit

  --- | Extract compileunit from a module, including file path etc walking, suffix detection
  EcuOfName
    :: !HsName				--- ^ module name
    -> BFun' EHCompileUnit

%%[[8
  EcuOfNameAndPath
    :: !(Maybe PrevSearchInfo)		--- ^ possibly previous search info
    -> !(HsName,Maybe FPath)		--- ^ module name and possibly known path
    -> BFun' EHCompileUnit
%%][5050
  EcusOfNamesAndPaths
    :: ![(HsName,Maybe FPath)]		--- ^ module names and possibly known paths
    -> BFun' [EHCompileUnit]
%%]]

  --- | Extract global options, possibly overridden for a module
  EHCOptsOf
    :: !HsName				--- ^ module name
    -> BFun' EHCOpts

  --- | Get a particular AST from file for a module
  ASTFromFile
    :: !(HsName,ASTFileNameOverride)	--- ^ module name and possibly known path
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
    :: !HsName							--- ^ module name and possibly known path
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

  --- | The actual module name and imported modules
  HsModnameAndImports
    :: !HsName							--- ^ module name and possibly known path
    -> BFun'
         ( HsName						-- module name
         , Set.Set HsName				-- imported module names
         )
%%]]

%%[[99
  --- | Get the FPath of the possibly with CPP preprocessed file
  FPathPreprocessedWithCPP
    :: [PkgModulePartition]				--- ^ partitioning of modules into packages
    -> !HsName							--- ^ module name and possibly known path
    -> BFun' FPath

  --- | Exposed packages
  ExposedPackages
    :: BFun' [PkgModulePartition]
%%]]

-- | Comparison which ignores GADT type info
bfunCompare :: BFun' res1 -> BFun' res2 -> Ordering
bfunCompare f1 f2 = case (f1,f2) of
    (CRSI				 						, CRSI 										) -> EQ
    (FPathSearchForFile 		a1 b1			, FPathSearchForFile 		a2 b2			) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (FPathOfImported    		a1   			, FPathOfImported    		a2   			) ->         a1 `compare` a2
    (ImportsOf          		a1   			, ImportsOf          		a2   			) ->         a1 `compare` a2
    (EcuOf		              	a1   			, EcuOf	    				a2   			) ->         a1 `compare` a2
    (EcuOfName              	a1   			, EcuOfName    				a2   			) ->         a1 `compare` a2
%%[[8
    (EcuOfNameAndPath			a1 b1			, EcuOfNameAndPath			a2 b2			) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%][5050
    (EcusOfNamesAndPaths		a1   			, EcusOfNamesAndPaths		a2   			) ->         a1 `compare` a2
%%]]
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
    (HsModnameAndImports		a1 				, HsModnameAndImports		a2 				) ->         a1 `compare` a2
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
	FPathSearchForFile 			a b			-> salt `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b
	FPathOfImported	   			a			-> salt `hashWithSalt` (1::Int) `hashWithSalt` a
	ImportsOf		   			a			-> salt `hashWithSalt` (2::Int) `hashWithSalt` a
	EcuOf			   			a			-> salt `hashWithSalt` (3::Int) `hashWithSalt` a
	EcuOfName		   			a			-> salt `hashWithSalt` (4::Int) `hashWithSalt` a
	EHCOptsOf		   			a			-> salt `hashWithSalt` (5::Int) `hashWithSalt` a
%%[[8
	EcuOfNameAndPath			a b			-> salt `hashWithSalt` (6::Int) `hashWithSalt` a `hashWithSalt` b
%%][5050
	EcusOfNamesAndPaths 		a 			-> salt `hashWithSalt` (6::Int) `hashWithSalt` a
%%]]
	ASTFromFile					a b	c d	e 	-> salt `hashWithSalt` (7::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d `hashWithSalt` e
%%[[50
	ModfTimeOfFile				a b	c d		-> salt `hashWithSalt` (8::Int) `hashWithSalt` a `hashWithSalt` b `hashWithSalt` c `hashWithSalt` d
	DirOfModIsWriteable 		a 			-> salt `hashWithSalt` (9::Int) `hashWithSalt` a
	EcuCanCompile		 		a 			-> salt `hashWithSalt` (10::Int) `hashWithSalt` a
	IsTopMod			 		a 			-> salt `hashWithSalt` (11::Int) `hashWithSalt` a
%%]]
%%[[50
	FoldHsMod			 		a b			-> salt `hashWithSalt` (12::Int) `hashWithSalt` a `hashWithSalt` b
	HsModnameAndImports			a 			-> salt `hashWithSalt` (13::Int) `hashWithSalt` a
%%]]
%%[[99
	FPathPreprocessedWithCPP	a b			-> salt `hashWithSalt` (14::Int) `hashWithSalt` a `hashWithSalt` b
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

  --- | Global info: exposed packages
  BRef_ExposedPackages
    :: BRef [PkgModulePartition]

  --- | Compile unit
  BRef_ECU
    :: !HsName					--- ^ module name
    -> BRef EHCompileUnit

  --- | An AST embedded in a compile unit
  BRef_AST
    :: !HsName					--- ^ module name
    -> ASTType							--- ^ content type
    -> ASTSuffixKey						--- ^ suffix and content variation
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

