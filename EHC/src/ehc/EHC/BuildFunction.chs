%%[0 hs
{-# LANGUAGE GADTs, TemplateHaskell #-}
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Representation and running of incrementally running functions.
For now (20150218) a start of a build system replacement allowing declarative specification, uncoupled from its execution.
%%]

%%[8 module {%{EH}EHC.BuildFunction}
%%]

-- general imports
%%[8 import ({%{EH}EHC.Common}, {%{EH}EHC.CompileUnit})
%%]

%%[8 import (Control.Applicative)
%%]

%%[8 import (Data.Functor.Identity) export(module Data.Functor.Identity)
%%]

%%[8 import (qualified Data.Map as Map, qualified Data.IntMap as IMap, Data.Maybe)
%%]

%%[8 import (UHC.Util.Hashable)
%%]

%%[8 import (UHC.Util.Lens)
%%]

%%[8 import (qualified UHC.Util.RelMap as Rel)
%%]

%%[8 import (Data.Typeable)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances (which should not be here...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
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

  --- | Extract compileunit from a module
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

-- | Comparison which ignores GADT type info
bfunCompare :: BFun' res1 -> BFun' res2 -> Ordering
bfunCompare f1 f2 = case (f1,f2) of
    (FPathSearchForFile 	a1 b1	, FPathSearchForFile 	a2 b2	) -> lexico [a1 `compare` a2, b1 `compare` b2]
    (FPathOfImported    	a1   	, FPathOfImported    	a2   	) ->         a1 `compare` a2
    (ImportsOf          	a1   	, ImportsOf          	a2   	) ->         a1 `compare` a2
    (EcuOfName              a1   	, EcuOfName    			a2   	) ->         a1 `compare` a2
%%[[8
    (EcuOfNameAndPath		a1 b1	, EcuOfNameAndPath		a2 b2	) -> lexico [a1 `compare` a2, b1 `compare` b2]
%%][5050
    (EcusOfNamesAndPaths	a1   	, EcusOfNamesAndPaths	a2   	) ->         a1 `compare` a2
%%]]
    (EHCOptsOf             	a1   	, EHCOptsOf				a2   	) ->         a1 `compare` a2
  where lexico (x:xs)
          | x == EQ   = lexico xs
          | otherwise = x
        lexico []     = EQ
  
instance Ord (BFun' res) where
  compare = bfunCompare

-- deriving instance Generic (BFun' res)
-- instance Hashable (BFun' res)
deriving instance Eq (BFun' res)
-- deriving instance (Data res, Typeable res) => Data (BFun' res)
deriving instance Show (BFun' res)
deriving instance Typeable BFun'

instance Hashable (BFun' res) where
  hashWithSalt salt x = case x of
	FPathSearchForFile 	a b	-> salt `hashWithSalt` (0::Int) `hashWithSalt` a `hashWithSalt` b
	FPathOfImported	   	a	-> salt `hashWithSalt` (1::Int) `hashWithSalt` a
	ImportsOf		   	a	-> salt `hashWithSalt` (2::Int) `hashWithSalt` a
	EcuOfName		   	a	-> salt `hashWithSalt` (3::Int) `hashWithSalt` a
	EHCOptsOf		   	a	-> salt `hashWithSalt` (4::Int) `hashWithSalt` a
%%[[8
	EcuOfNameAndPath	a b	-> salt `hashWithSalt` (5::Int) `hashWithSalt` a `hashWithSalt` b
%%][5050
	EcusOfNamesAndPaths a 	-> salt `hashWithSalt` (5::Int) `hashWithSalt` a
%%]]

{-
--- | Applicative: pure
  Pure
    :: res
    -> BFun' res

  --- | Applicative: <*>
  App
    :: BFun' (res1 -> res2)
    -> BFun' res1
    -> BFun' res2

instance Functor BFun' where
  fmap f (Pure  x) = Pure (f x)
  fmap f (App g x) = App (fmap (f .) g) x
  fmap _ _         = panic "BuildFuncion.Functor.BFun'"

instance Applicative BFun' where
  pure  = Pure
  (<*>) = App
-}

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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function result cache, indexed by function call 'BFun'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(BCache, emptyBCache, bcacheCache, bcacheDpdRel)
-- | Cache for function calls, first indexed on hash
data BCache
  = BCache
      { _bcacheCache	:: IMap.IntMap [BFunCacheEntry]
      , _bcacheDpdRel	:: Rel.Rel BFun BFun
      }

mkLabel ''BCache

emptyBCache :: BCache
emptyBCache = BCache IMap.empty Rel.empty
%%]

%%[8 export(bcacheLookup, bcacheInsert, bcacheInsertDpd)
-- | Lookup BCachedVal in 'BCache', preserving type info
bcacheLookup :: (Typeable res, Typeable f) => BFun' res -> BCache -> Maybe (f res)
bcacheLookup key (BCache {_bcacheCache=cache}) = do
    vals <- IMap.lookup (hash key) cache
    lookup key $ catMaybes $ map cvt vals
  where
    cvt (BFunCacheEntry {bfceFun=f, bfceVal=v}) = case (cast f, cast v) of
      (Just f', Just v') -> Just (f',v')
      _                  -> Nothing

-- | Add to 'BCache'
bcacheInsert :: (Typeable res, Typeable f) => BFun' res -> f res -> BCache -> BCache
bcacheInsert k v bc@(BCache {_bcacheCache=c}) = bc { _bcacheCache = IMap.insertWith (++) (hash k) [BFunCacheEntry k v] c }

-- | Add dependency to 'BCache'
bcacheInsertDpd
  :: (Typeable res1, Typeable res2)
     => 
        BFun' res1			-- ^ dependee
     -> BFun' res2			-- ^ depends on
     -> BCache -> BCache
bcacheInsertDpd f1 f2 bc@(BCache {_bcacheDpdRel=dpd}) = bc { _bcacheDpdRel = Rel.insert (BFun f1) (BFun f2) dpd }
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build value reference (to global state)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(BRef(..))
-- | GADT for references to global state, interpreted inside the compiler driver monad, the type of the GADT telling what the type of the value should be.
data BRef val where
  --- | Compile unit
  BRef_ECU
    :: !HsName					--- ^ module name
    -> BRef EHCompileUnit

  --- | Global options
  BRef_EHCOpts
    :: !HsName					--- ^ module name
    -> BRef EHCOpts
  
deriving instance Typeable BRef
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build state
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(BState, emptyBState, bstateCache, bstateCallStack)
-- | Cache for function calls, first indexed on hash
data BState
  = BState
      { _bstateCache		:: !BCache
      , _bstateCallStack	:: ![BFun]
      }

mkLabel ''BState

emptyBState :: BState
emptyBState = BState emptyBCache []
%%]


