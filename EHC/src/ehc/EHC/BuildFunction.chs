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

%%[8 import (qualified Data.Map as Map, qualified Data.Set as Set, qualified Data.IntMap as IMap, Data.Maybe)
%%]

%%[8 import (UHC.Util.Hashable)
%%]

%%[8 import (UHC.Util.Lens)
%%]

%%[50 import (UHC.Util.Time)
%%]

%%[8 import (qualified UHC.Util.RelMap as Rel)
%%]

%%[8 import (Data.Typeable)
%%]

%%[99 import ({%{EH}Base.PackageDatabase})
%%]

-- Compilerdriver
%%[8 import({%{EH}EHC.CompileRun.Base})
%%]

-- Module
%%[50 import(qualified {%{EH}HS.ModImpExp} as HSSemMod)
%%]
%%[99 import(qualified {%{EH}Base.Pragma} as Pragma)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances (which should not be here...)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8
deriving instance Typeable Identity
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function result cache, indexed by function call 'BFun'
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(bcacheLookup, bcacheInsert, bcacheInsertDpd)
-- | Lookup BCachedVal in 'BCache', preserving type info
bcacheLookup :: (Typeable res, Typeable f, Typeable m) => BFun' m res -> BCache m -> Maybe (f res)
bcacheLookup key (BCache {_bcacheCache=cache}) = do
    vals <- IMap.lookup (hash key) cache
    lookup key $ catMaybes $ map cvt vals
  where
    cvt (BFunCacheEntry {bfceFun=f, bfceVal=v}) = case (cast f, cast v) of
      (Just f', Just v') -> Just (f',v')
      _                  -> Nothing

-- | Add to 'BCache'
bcacheInsert :: (Typeable res, Typeable f) => BFun' m res -> f res -> BCache m -> BCache m
bcacheInsert k v bc@(BCache {_bcacheCache=c}) = bc { _bcacheCache = IMap.insertWith (++) (hash k) [BFunCacheEntry k v] c }

-- | Add dependency to 'BCache'
bcacheInsertDpd
  :: (Typeable res1, Typeable res2)
     => 
        BFun' m res1			-- ^ dependee
     -> BFun' m res2			-- ^ depends on
     -> BCache m -> BCache m
bcacheInsertDpd f1 f2 bc@(BCache {_bcacheDpdRel=dpd}) = bc { _bcacheDpdRel = Rel.insert (BFun f1) (BFun f2) dpd }
%%]


