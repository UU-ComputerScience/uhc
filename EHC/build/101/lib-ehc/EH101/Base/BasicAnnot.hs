module EH101.Base.BasicAnnot
( BasicSize (..)
, basicSizeOf
, basicSizeWord, basicSizeIsWord
, basicSizeIsSigned
, basicSizeInBytes
, basicSizeInWords
, GCPermit (..)
, BasicGBTy (..)
, basicGBTy
, basicGrinSizeCharEncoding
, BasicTy (..)
, BasicAnnotTagging (..)
, BasicAnnot (..), basicAnnotWord
, grinBasicAnnotSizeInBytes, grinBasicAnnotSizeInWords
, grinBasicAnnotSize
, grinBasicAnnotGCPermit
, basicSizeDouble, basicSizeFloat )
where
import qualified Data.Map as Map
import Data.Bits
import Data.List
import EH.Util.Pretty
import EH.Util.Utils
import qualified EH101.Config as Cfg
import EH101.Base.Bits
import Control.Monad
import EH101.Base.Binary
import EH101.Base.Serialize



{-# LINE 25 "src/ehc/Base/BasicAnnot.chs" #-}
data BasicSize
  = BasicSize_Word8
  | BasicSize_Word16
  | BasicSize_Word32
  | BasicSize_Word64
  | BasicSize_Int8
  | BasicSize_Int16
  | BasicSize_Int32
  | BasicSize_Int64
  | BasicSize_Float
  | BasicSize_Double
  deriving (Eq,Ord,Enum)

{-# LINE 42 "src/ehc/Base/BasicAnnot.chs" #-}
deriving instance Typeable BasicSize
deriving instance Data BasicSize

{-# LINE 50 "src/ehc/Base/BasicAnnot.chs" #-}
instance Show BasicSize where
  show BasicSize_Word8   = "w1"
  show BasicSize_Word16  = "w2"
  show BasicSize_Word32  = "w4"
  show BasicSize_Word64  = "w8"
  show BasicSize_Int8    = "i1"
  show BasicSize_Int16   = "i2"
  show BasicSize_Int32   = "i4"
  show BasicSize_Int64   = "i8"
  show BasicSize_Float   = "f4"
  show BasicSize_Double  = "f8"

{-# LINE 66 "src/ehc/Base/BasicAnnot.chs" #-}
instance PP BasicSize where
  pp = pp . show

{-# LINE 72 "src/ehc/Base/BasicAnnot.chs" #-}
basicSizeOfMp :: Map.Map Int BasicSize
basicSizeOfMp
  = Map.fromList
      [ (1, BasicSize_Word8 )
      , (2, BasicSize_Word16)
      , (4, BasicSize_Word32)
      , (8, BasicSize_Word64)
      ]

basicSizeOf :: Int -> BasicSize
basicSizeOf i = panicJust "BasicAnnot.basicSizeOf" $ Map.lookup i basicSizeOfMp

{-# LINE 86 "src/ehc/Base/BasicAnnot.chs" #-}
basicSizeWord :: BasicSize
basicSizeWord  = if Cfg.use64Bits then BasicSize_Word64 else BasicSize_Word32

basicSizeSWord :: BasicSize
basicSizeSWord  = if Cfg.use64Bits then BasicSize_Int64 else BasicSize_Int32

basicSizeSHWord :: BasicSize
basicSizeSHWord  = if Cfg.use64Bits then BasicSize_Int32 else BasicSize_Int16

basicSizeIsWord :: BasicSize -> Bool
basicSizeIsWord BasicSize_Word8                  = True
basicSizeIsWord BasicSize_Word16                 = True
basicSizeIsWord BasicSize_Word32                 = True
basicSizeIsWord BasicSize_Word64 | Cfg.use64Bits = True
basicSizeIsWord BasicSize_Int8                   = True
basicSizeIsWord BasicSize_Int16                  = True
basicSizeIsWord BasicSize_Int32                  = True
basicSizeIsWord BasicSize_Int64  | Cfg.use64Bits = True
basicSizeIsWord _                                = False

{-# LINE 108 "src/ehc/Base/BasicAnnot.chs" #-}
basicSizeIsSigned :: BasicSize -> Bool
basicSizeIsSigned BasicSize_Int8  = True
basicSizeIsSigned BasicSize_Int16 = True
basicSizeIsSigned BasicSize_Int32 = True
basicSizeIsSigned BasicSize_Int64 = True
basicSizeIsSigned _               = False

{-# LINE 117 "src/ehc/Base/BasicAnnot.chs" #-}
basicSizeDouble, basicSizeFloat :: BasicSize
basicSizeFloat  = BasicSize_Float
basicSizeDouble = BasicSize_Double

{-# LINE 123 "src/ehc/Base/BasicAnnot.chs" #-}
basicSizeInBytes :: BasicSize -> Int
basicSizeInBytes BasicSize_Word8   = 1
basicSizeInBytes BasicSize_Word16  = 2
basicSizeInBytes BasicSize_Word32  = 4
basicSizeInBytes BasicSize_Word64  = 8
basicSizeInBytes BasicSize_Int8    = 1
basicSizeInBytes BasicSize_Int16   = 2
basicSizeInBytes BasicSize_Int32   = 4
basicSizeInBytes BasicSize_Int64   = 8
basicSizeInBytes BasicSize_Float   = Cfg.sizeofFloat
basicSizeInBytes BasicSize_Double  = Cfg.sizeofDouble

{-# LINE 139 "src/ehc/Base/BasicAnnot.chs" #-}
basicSizeInWords :: BasicSize -> Int
basicSizeInWords sz = entierLogUpShrBy Cfg.sizeofWordInLog (basicSizeInBytes sz)

{-# LINE 151 "src/ehc/Base/BasicAnnot.chs" #-}
data GCPermit
  = GCPermit_Not
  | GCPermit_May
  | GCPermit_Must
  deriving (Eq,Ord)

instance Show GCPermit where
  show GCPermit_Not  = "GC-"
  show GCPermit_May  = "GC-+"
  show GCPermit_Must = "GC+"

instance PP GCPermit where
  pp = pp . show

{-# LINE 167 "src/ehc/Base/BasicAnnot.chs" #-}
-- a value about which we know it is of BasicSize corresponds to unboxed, hence no GC tracing
basicSizeGCPermit :: BasicSize -> GCPermit
basicSizeGCPermit BasicSize_Word64 | Cfg.use32Bits = GCPermit_Not
basicSizeGCPermit BasicSize_Int64  | Cfg.use32Bits = GCPermit_Not
basicSizeGCPermit BasicSize_Float                  = GCPermit_Not
basicSizeGCPermit BasicSize_Double                 = GCPermit_Not
basicSizeGCPermit _                                = GCPermit_Must

{-# LINE 189 "src/ehc/Base/BasicAnnot.chs" #-}
data BasicGBTy
  = BasicGBTy
      { gbtyOnStack		:: String			-- as it lives on the stack, its size must be multiple of Word size, but type may differ
      , gbtyAsIs		:: String			-- as it is
      , gbtyAsReturned	:: String			-- as it is interpreted (for casting and copying onto the stack) when returned from a ffi function
      }

{-# LINE 198 "src/ehc/Base/BasicAnnot.chs" #-}
basicGBTyMp :: Map.Map BasicSize BasicGBTy
basicGBTyMp
  = Map.fromList
      [ (BasicSize_Word8	, BasicGBTy "Word" 		"Word8"		"Word8"		)
      , (BasicSize_Word16	, BasicGBTy "Word" 		"Word16"	"Word16"	)
      , (BasicSize_Word32	, BasicGBTy "Word" 		"Word32"	"Word32"	)
      , (BasicSize_Word64	, BasicGBTy "Word64" 	"Word64"	"Word64"	)
      , (BasicSize_Int8		, BasicGBTy "Int" 		"Int8"		"Int8"		)
      , (BasicSize_Int16	, BasicGBTy "Int" 		"Int16"		"Int16"		)
      , (BasicSize_Int32	, BasicGBTy "Int" 		"Int32"		"Int32"		)
      , (BasicSize_Int64	, BasicGBTy "Int64" 	"Int64"		"Int64"		)
      , (BasicSize_Float	, BasicGBTy "Word" 		"Float"		"Word32" 	)
      , (BasicSize_Double	, BasicGBTy "Word64" 	"Double"	"Word64"	)
      ]

{-# LINE 220 "src/ehc/Base/BasicAnnot.chs" #-}
basicGBTy :: BasicSize -> BasicGBTy
basicGBTy b = panicJust "basicGBTy" $ Map.lookup b basicGBTyMp

{-# LINE 241 "src/ehc/Base/BasicAnnot.chs" #-}
basicGrinSizeCharEncoding :: BasicSize -> String
basicGrinSizeCharEncoding = take 2 . show

{-# LINE 288 "src/ehc/Base/BasicAnnot.chs" #-}
-- the defs in basicTyGBTy must be at the beginning, as Enum uses the relative ordering
data BasicTy
  = BasicTy_Word				-- base case: pointer, word, int, ...
  | BasicTy_SWord				-- base case: signed word
  | BasicTy_SHWord				-- base case: signed half word
  | BasicTy_Float				-- C: float
  | BasicTy_Double				-- C: double
  | BasicTy_SignedHalfWord		-- as BasicTy_Word, but for FFI half the size of a word, and signed. Special case for sign extend.
  deriving (Eq,Ord,Enum)

{-# LINE 302 "src/ehc/Base/BasicAnnot.chs" #-}
deriving instance Typeable BasicTy
deriving instance Data BasicTy

{-# LINE 310 "src/ehc/Base/BasicAnnot.chs" #-}
instance Show BasicTy where
  show BasicTy_Word   			= "word"
  show BasicTy_SWord            = "sword"
  show BasicTy_SHWord           = "shword"
  show BasicTy_Float  			= "float"
  show BasicTy_Double 			= "double"
  show BasicTy_SignedHalfWord   = "int"

{-# LINE 322 "src/ehc/Base/BasicAnnot.chs" #-}
instance PP BasicTy where
  pp = pp . show

{-# LINE 341 "src/ehc/Base/BasicAnnot.chs" #-}
data BasicAnnotTagging
  = BasicAnnotTagging_None		-- no tagging
  | BasicAnnotTagging_FromPtr	-- from tagged pointer to int variant fitting in untagged part
  | BasicAnnotTagging_ToPtr		-- to tagged pointer from int variant fitting in untagged part
  deriving (Show,Eq,Enum)

{-# LINE 349 "src/ehc/Base/BasicAnnot.chs" #-}
data BasicAnnot
  = BasicAnnot_Size
  	  { baSize     	:: BasicSize
  	  , baTy 		:: BasicTy
  	  , baTagging	:: BasicAnnotTagging
  	  , baIsSigned	:: Bool
  	  }
  | BasicAnnot_Dflt
  | BasicAnnot_None
  deriving (Show,Eq)

basicAnnotWord :: BasicAnnot
basicAnnotWord = BasicAnnot_Size basicSizeWord BasicTy_Word BasicAnnotTagging_None False

{-# LINE 365 "src/ehc/Base/BasicAnnot.chs" #-}
deriving instance Typeable BasicAnnotTagging
deriving instance Data BasicAnnotTagging

deriving instance Typeable BasicAnnot
deriving instance Data BasicAnnot

{-# LINE 373 "src/ehc/Base/BasicAnnot.chs" #-}
grinBasicAnnotSizeInBytes :: BasicAnnot -> Int
grinBasicAnnotSizeInBytes = basicSizeInBytes . grinBasicAnnotSize

grinBasicAnnotSizeInWords :: BasicAnnot -> Int
grinBasicAnnotSizeInWords = basicSizeInWords . grinBasicAnnotSize

{-# LINE 381 "src/ehc/Base/BasicAnnot.chs" #-}
grinBasicAnnotSize :: BasicAnnot -> BasicSize
grinBasicAnnotSize (BasicAnnot_Size          s _ _ _) = s
grinBasicAnnotSize (BasicAnnot_Dflt                 ) = basicSizeWord

{-# LINE 387 "src/ehc/Base/BasicAnnot.chs" #-}
grinBasicAnnotGCPermit :: BasicAnnot -> GCPermit
grinBasicAnnotGCPermit (BasicAnnot_Size          _ _ BasicAnnotTagging_FromPtr _) = GCPermit_Not		-- is unboxed
grinBasicAnnotGCPermit (BasicAnnot_Size          _ _ BasicAnnotTagging_ToPtr   _) = GCPermit_May		-- freshly tagged, no GC will ever be necessary, but GC checks for it anyway
grinBasicAnnotGCPermit (BasicAnnot_Size          s _                         _ _) = basicSizeGCPermit s
grinBasicAnnotGCPermit (BasicAnnot_Dflt                                         ) = GCPermit_Must

{-# LINE 395 "src/ehc/Base/BasicAnnot.chs" #-}
instance PP BasicAnnotTagging where
  pp BasicAnnotTagging_None    = pp "notag"
  pp BasicAnnotTagging_FromPtr = pp "untag"
  pp BasicAnnotTagging_ToPtr   = pp "tag"

instance PP BasicAnnot where
  pp (BasicAnnot_Size          s t tg sgn) = s >#< t >#< tg >#< sgn
  pp (BasicAnnot_Dflt                    ) = pp "annotdflt"

{-# LINE 410 "src/ehc/Base/BasicAnnot.chs" #-}
instance Serialize BasicAnnot where
  sput (BasicAnnot_Size          a b c d) = sputWord8 0 >> sput a >> sput b >> sput c >> sput d
  sput (BasicAnnot_Dflt                 ) = sputWord8 1
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM4 BasicAnnot_Size 			sget sget sget sget
      1 -> return BasicAnnot_Dflt

instance Serialize BasicTy where
  sput = sputEnum8
  sget = sgetEnum8

instance Serialize BasicSize where
  sput = sputEnum8
  sget = sgetEnum8

instance Serialize BasicAnnotTagging where
  sput = sputEnum8
  sget = sgetEnum8
