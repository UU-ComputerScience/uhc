%%[0 hs
{-# LANGUAGE GADTs #-}
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Build function representation
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 export(BFun(..))
data BFun res where
  -- | Obtain FPath of an (imported module)
  FPathOfImported
    :: HsName				-- ^ module name
    -> BFun FPath

  -- | Extract imported modules from a module
  ImportsOf
    :: HsName				-- ^ module name
    -> BFun [HsName]

  -- | Extract compileunit from a module
  EcuOf
    :: HsName				-- ^ module name
    -> BFun EHCompileUnit

  -- | Applicative: pure
  Pure
    :: res
    -> BFun res

  -- | Applicative: <*>
  App
    :: BFun (res1 -> res2)
    -> BFun res1
    -> BFun res2

instance Functor BFun where
  fmap f (Pure  x) = Pure (f x)
  fmap f (App g x) = App (fmap (f .) g) x
  fmap _ _         = panic "BuildFuncion.Functor.BFun"

instance Applicative BFun where
  pure  = Pure
  (<*>) = App
%%]

deriving instance Eq   (BFun res)
deriving instance Ord  (BFun res)
deriving instance Show (BFun res)

%%[8888 export(BFunSq(..))
-- | Internal representation to allow for construction via Applicative
data BFunSq res where
  -- | Applicative: pure
  Pure
    :: res
    -> BFunSq res

  --
  Lift
    :: BFun res
    -> BFunSq res

  -- | Applicative: <*>
  App
    :: BFunSq (res1 -> res2)
    -> BFunSq res1
    -> BFunSq res2

instance Functor BFunSq where
  fmap f (Pure  x) = Pure (f x)
  fmap f (App g x) = App (fmap (f .) g) x
  fmap _  sq       = sq

instance Applicative BFunSq where
  pure  = Pure
  (<*>) = App
%%]

instance Eq (BFunSq res) where
  _ == _ = False

instance Eq (BFunSq res) where
  _ == _ = False

instance Ord (BFunSq res) where
  _ `compare` _ = EQ

%%[8
%%]
instance Show res => PP (BFun res) where
  pp = pp . show
  

