%%[0 hs
{-# LANGUAGE CPP #-}
%%]

%%[0 lhs2tex
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Field access, holding both name and offset, for delayed decision about this
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}Base.Fld}
%%]

%%[8 hs import(Control.Monad)
%%]

%%[8 hs import(UHC.Util.Pretty)
%%]

%%[50 import(UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%[8 hs import({%{EH}Base.HsName}, {%{EH}Base.HsName.Builtin})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Fld
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 hs export(Fld'(..), Fld, noFld)
-- | Field (combined dereference + field access), doubly represented by index and name
data Fld' inx
  = Fld
      { _fldNm	:: !(Maybe HsName)
      , _fldInx	:: !(Maybe inx)
      }
  deriving (Generic)

type Fld = Fld' Int

noFld :: Fld
noFld = Fld (Just hsnUnknown) (Just 0)
%%]

%%[(8 codegen) hs export(mkFldNm, mkFldInx)
-- | Make a Fld holding only a name
mkFldNm :: HsName -> Fld' inx
mkFldNm n = Fld (Just n) Nothing
{-# INLINE mkFldNm #-}

-- | Make a Fld holding only an inx
mkFldInx :: inx -> Fld' inx
mkFldInx i = Fld Nothing (Just i)
{-# INLINE mkFldInx #-}
%%]

%%[8 hs
instance Eq inx => Eq (Fld' inx) where
  (Fld {_fldInx=Just i1}) == (Fld {_fldInx=Just i2}) = i1 == i2
  (Fld {_fldNm =     n1}) == (Fld {_fldNm =     n2}) = n1 == n2

instance Ord inx => Ord (Fld' inx) where
  (Fld {_fldInx=Just i1}) `compare` (Fld {_fldInx=Just i2}) = i1 `compare` i2
  (Fld {_fldNm =     n1}) `compare` (Fld {_fldNm =     n2}) = n1 `compare` n2

instance Show inx => Show (Fld' inx) where
  -- show f = maybe (maybe "??Fld" show $ _fldNm f) show $ _fldInx f
  show (Fld {_fldNm=Just n , _fldInx=Just i }) = show n ++ "(" ++ show i ++ ")"
  show (Fld {_fldNm=Nothing, _fldInx=Just i }) =                  show i
  show (Fld {_fldNm=Just n , _fldInx=Nothing}) = show n
  show _                                       = "??Fld"

instance Show inx => PP (Fld' inx) where
  pp = pp . show

instance HSNM inx => HSNM (Fld' inx) where
  mkHNm = fldNm
%%]

%%[8 hs export(fldFoldNmInx, fldFoldInxNm)
-- | Fold over Fld, preference to name
fldFoldNmInx :: (HsName -> x) -> (inx -> x) -> x -> Fld' inx -> x
fldFoldNmInx n i dflt f = maybe (maybe dflt i $ _fldInx f) n $ _fldNm f

-- | Fold over Fld, preference to inx
fldFoldInxNm :: (HsName -> x) -> (inx -> x) -> x -> Fld' inx -> x
fldFoldInxNm n i dflt f = maybe (maybe dflt n $ _fldNm f) i $ _fldInx f
%%]

%%[8 hs export(fldInt, fldNm)
-- | Fld access preferred by name
fldNm :: HSNM inx => Fld' inx -> HsName
fldNm = fldFoldNmInx id mkHNm hsnUnknown -- maybe (maybe hsnUnknown mkHNm $ _fldInx f) id $ _fldNm f
{-# INLINE fldNm #-}

-- | Fld access preferred by index
fldInt :: Fld -> Int
fldInt = fldFoldInxNm (const 0) id 0 -- maybe 0 id $ _fldInx f
{-# INLINE fldInt #-}
%%]

%%[(8 codegen) hs export(fldMapNm)
-- |
fldMapNm :: (HsName -> HsName) -> Fld' inx -> Fld' inx
fldMapNm f fld = fld {_fldNm = fmap f $ _fldNm fld}
%%]

%%[(8 codegen) hs export(RefOfFld(..))
class RefOfFld fld a where
  refOfFld :: fld -> a

instance RefOfFld (Fld' inx) (Fld' inx) where
  refOfFld = id

instance RefOfFld Fld Int where
  refOfFld = fldInt

instance HSNM inx => RefOfFld (Fld' inx) HsName where
  refOfFld = fldNm

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
#if __GLASGOW_HASKELL__ >= 708
deriving instance Typeable  Fld'
#else
deriving instance Typeable1 Fld'
#endif
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[50
instance Serialize x => Serialize (Fld' x) where
  -- sput (Fld a b) = sput a >> sput b
  -- sget = liftM2 Fld sget sget
%%]

