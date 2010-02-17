%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Annotation for size info related to builtin types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs module {%{EH}Base.BasicAnnot}
%%]

%%[(8 codegen) hs import(qualified Data.Map as Map,Data.Bits, Data.List)
%%]

%%[(8 codegen) hs import(EH.Util.Pretty, EH.Util.Utils)
%%]

%%[(8 codegen) hs import(qualified {%{EH}Config} as Cfg, {%{EH}Base.Bits})
%%]

%%[(20 codegen) hs import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicSize: size of BasicTy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(BasicSize(..))
data BasicSize
  = BasicSize_Word8
  | BasicSize_Word16
  | BasicSize_Word32
  | BasicSize_Word64
%%[[97
  | BasicSize_Float
  | BasicSize_Double
%%]]
  deriving (Eq,Ord,Enum)
%%]

%%[(20 codegen) hs
deriving instance Typeable BasicSize
deriving instance Data BasicSize
%%]

The Show of BasicSize should returns strings of which the first letter is unique for the type.
When used to pass to size encoding for bytecode C calls, only this first letter is used.

%%[(8 codegen) hs
instance Show BasicSize where
%%[[97
  show BasicSize_Float  = "float"
  show BasicSize_Double = "double"
%%]]
  show sz               = show (basicSizeInBytes sz)
%%]

%%[(8 codegen) hs
instance PP BasicSize where
  pp = pp . show
%%]

%%[(8 codegen) hs export(basicSizeOf)
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
%%]

%%[(8 codegen) hs export(basicSizeWord)
basicSizeWord :: BasicSize
basicSizeWord  = if Cfg.use64Bits then BasicSize_Word64 else BasicSize_Word32
%%]

%%[(97 codegen) hs export(basicSizeDouble,basicSizeFloat)
basicSizeDouble, basicSizeFloat :: BasicSize
basicSizeFloat  = BasicSize_Float
basicSizeDouble = BasicSize_Double
%%]

%%[(8 codegen) hs export(basicSizeInBytes)
basicSizeInBytes :: BasicSize -> Int
basicSizeInBytes BasicSize_Word8   = 1
basicSizeInBytes BasicSize_Word16  = 2
basicSizeInBytes BasicSize_Word32  = 4
basicSizeInBytes BasicSize_Word64  = 8
%%[[97
basicSizeInBytes BasicSize_Float   = Cfg.sizeofFloat
basicSizeInBytes BasicSize_Double  = Cfg.sizeofDouble
%%]]
%%]

%%[(8 codegen) hs export(basicSizeInWords)
basicSizeInWords :: BasicSize -> Int
basicSizeInWords sz = entierLogUpShrBy Cfg.sizeofWordInLog (basicSizeInBytes sz)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% GC permission
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

Permitted (May), not permitted (Not), or required (Must).
Currently only Not is used, intended for GC which traces/copies live ptrs and must know what not to inspect.

%%[(8 codegen grin) hs export(GCPermit(..))
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
%%]

%%[(8 codegen) hs export(basicSizeGCPermit)
-- a value about which we know it is of BasicSize corresponds to unboxed, hence no GC tracing
basicSizeGCPermit :: BasicSize -> GCPermit
basicSizeGCPermit _ = GCPermit_Not
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicSize type representation for GrinByteCode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(BasicGBTy(..))
data BasicGBTy
  = BasicGBTy
      { gbtyOnStack		:: String			-- as it lives on the stack
      , gbtyAsIs		:: String			-- as it is
      , gbtyWordEquiv	:: String			-- its Word equivalent
      }
%%]

%%[(8 codegen) export(basicGBTy)
basicGBTyMp :: Map.Map BasicSize BasicGBTy
basicGBTyMp
  = Map.fromList
      [ (BasicSize_Word8	, BasicGBTy "Word" 		"Word8"		"Word"		)
      , (BasicSize_Word16	, BasicGBTy "Word" 		"Word16"	"Word"		)
      , (BasicSize_Word32	, BasicGBTy "Word" 		"Word32"	"Word"		)
      , (BasicSize_Word64	, BasicGBTy "Word64" 	"Word64"	"Word64"	)
%%[[97
      , (BasicSize_Float	, BasicGBTy "GB_Float" 	"Float"		"Word32" 	)
      , (BasicSize_Double	, BasicGBTy "GB_Double" "Double"	"Word64"	)
%%]]
      ]

basicGBTy :: BasicSize -> BasicGBTy
basicGBTy b = panicJust "basicGBTy" $ Map.lookup b basicGBTyMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicSize encoding for GrinByteCode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) export(allGrinBasicSize)
allGrinBasicSize :: [BasicSize]
allGrinBasicSize
  =    [ basicSizeWord ]
%%[[97
    ++ (if Cfg.use32Bits then [ BasicSize_Word64 ] else [])
    ++ [ BasicSize_Float
       , BasicSize_Double
       ]
%%]]
%%]

%%[(8 codegen) export(basicGrinSizeCharEncoding)
basicGrinSizeCharEncoding :: BasicSize -> Char
basicGrinSizeCharEncoding = head . show
%%]

%%[(8 codegen grin) export(basicGrinSizeLEncoding)
basicGrinSizeLEncoding :: [BasicSize] -> Integer
basicGrinSizeLEncoding
  = foldr (.|.) 0
  . zipWith (\sh t -> toInteger ((panicJust "basicGrinSizeLEncoding" $ elemIndex t allGrinBasicSize) + 1) `shiftL` sh)
            [0,shInc..]
%%[[8
  where shInc = 1
%%][97
  where shInc = 3
%%]]
%%]

%%[(8 codegen grin) export(basicSizeGBTy)
basicSizeGBTy :: BasicSize -> String
basicSizeGBTy BasicSize_Word8  = "Word8"
basicSizeGBTy BasicSize_Word16 = "Word16"
basicSizeGBTy BasicSize_Word32 = "Word32"
basicSizeGBTy BasicSize_Word64 = "Word64"
%%[[97
basicSizeGBTy BasicSize_Float  = "GB_Float"
basicSizeGBTy BasicSize_Double = "GB_Double"
%%]]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicJazy: Java type of BasicTy
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 jazy) hs export(BasicJazy(..))
data BasicJazy
  = BasicJazy_Int
  | BasicJazy_Char
  | BasicJazy_Object
  | BasicJazy_String
%%[[97
  | BasicJazy_Byte
  | BasicJazy_Short
  | BasicJazy_Long
  | BasicJazy_Float
  | BasicJazy_Double
  | BasicJazy_Integer
%%]]
%%[[98
  | BasicJazy_Handle
  | BasicJazy_ByteArray
%%]]
  deriving (Eq,Ord,Enum)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicTy: types for BasicAnnot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(BasicTy(..))
-- the defs in basicTyGBTy must be at the beginning, as Enum uses the relative ordering
data BasicTy
  = BasicTy_Word		-- base case: pointer, word, int, ...
%%[[97
  | BasicTy_Float		-- C: float
  | BasicTy_Double		-- C: double
%%]]
  deriving (Eq,Ord,Enum)
%%]

%%[(20 codegen) hs
deriving instance Typeable BasicTy
deriving instance Data BasicTy
%%]

The Show of BasicTy should returns strings of which the first letter is unique for the type.
When used to pass to code, only this first letter is used.

%%[(8 codegen) hs
instance Show BasicTy where
  show BasicTy_Word   = "word"
%%[[97
  show BasicTy_Float  = "float"
  show BasicTy_Double = "double"
%%]]
%%]

%%[(8 codegen) hs
btBasicSize :: BasicTy -> BasicSize
btBasicSize BasicTy_Word   = basicSizeWord
%%[[97
btBasicSize BasicTy_Float  = BasicSize_Float
btBasicSize BasicTy_Double = BasicSize_Double
%%]]
%%]

%%[(8 codegen) hs
instance PP BasicTy where
  pp = pp . show
%%]

%%[(8 codegen) hs
-- a value about which we know it is of BasicSize corresponds to unboxed, hence no GC tracing
btGCPermit :: BasicTy -> GCPermit
%%[[97
btGCPermit BasicTy_Float  = GCPermit_Not
btGCPermit BasicTy_Double = GCPermit_Not
%%]]
btGCPermit _              = GCPermit_Must
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicAnnot
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(BasicAnnot(..),defaultGrinBasicAnnot)
data BasicAnnot
  = BasicAnnot_Size        		{ baSize     :: BasicSize, baTy :: BasicTy }
  | BasicAnnot_FromTaggedPtr	{ baIsSigned :: Bool     , baTy :: BasicTy }
  | BasicAnnot_ToTaggedPtr		{ baIsSigned :: Bool     , baTy :: BasicTy }
  | BasicAnnot_Dflt
  | BasicAnnot_None
  deriving (Show,Eq)

defaultGrinBasicAnnot :: BasicAnnot
defaultGrinBasicAnnot = BasicAnnot_Size basicSizeWord BasicTy_Word
%%]

%%[(20 codegen) hs
deriving instance Typeable BasicAnnot
deriving instance Data BasicAnnot
%%]

%%[(8 codegen grin) hs export(grinBasicAnnotSizeInBytes,grinBasicAnnotSizeInWords)
grinBasicAnnotSizeInBytes :: BasicAnnot -> Int
grinBasicAnnotSizeInBytes = basicSizeInBytes . grinBasicAnnotSize

grinBasicAnnotSizeInWords :: BasicAnnot -> Int
grinBasicAnnotSizeInWords = basicSizeInWords . grinBasicAnnotSize
%%]

%%[(8 codegen grin) hs export(grinBasicAnnotSize)
grinBasicAnnotSize :: BasicAnnot -> BasicSize
grinBasicAnnotSize (BasicAnnot_Size          s _) = s
grinBasicAnnotSize (BasicAnnot_FromTaggedPtr _ t) = btBasicSize t
grinBasicAnnotSize (BasicAnnot_ToTaggedPtr   _ t) = btBasicSize t
grinBasicAnnotSize (BasicAnnot_Dflt             ) = basicSizeWord
%%]

%%[(8 codegen grin) hs export(grinBasicAnnotGCPermit)
grinBasicAnnotGCPermit :: BasicAnnot -> GCPermit
grinBasicAnnotGCPermit (BasicAnnot_Size          _ t) = btGCPermit t
grinBasicAnnotGCPermit (BasicAnnot_FromTaggedPtr _ t) = GCPermit_Not		-- is unboxed
grinBasicAnnotGCPermit (BasicAnnot_ToTaggedPtr   _ t) = GCPermit_May		-- freshly tagged, no GC will ever be necessary, but GC checks for it anyway
grinBasicAnnotGCPermit (BasicAnnot_Dflt             ) = GCPermit_Must
%%]

%%[(8 codegen) hs
instance PP BasicAnnot where
  pp (BasicAnnot_Size          s t) = s >#< t
  pp (BasicAnnot_FromTaggedPtr b t) = "annotfromtaggedptr" >#< b >#< t
  pp (BasicAnnot_ToTaggedPtr   b t) = "annottotaggedptr" >#< b >#< t
  pp (BasicAnnot_Dflt             ) = pp "annotdflt"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen) hs
instance Binary BasicAnnot where
  put (BasicAnnot_Size          s t) = putWord8 0 >> put s >> put t
  put (BasicAnnot_FromTaggedPtr b t) = putWord8 1 >> put b >> put t
  put (BasicAnnot_ToTaggedPtr   b t) = putWord8 2 >> put b >> put t
  put (BasicAnnot_Dflt             ) = putWord8 3
  get = do t <- getWord8
           case t of
             0 -> liftM2 BasicAnnot_Size get get
             1 -> liftM2 BasicAnnot_FromTaggedPtr get get
             2 -> liftM2 BasicAnnot_ToTaggedPtr get get
             3 -> return BasicAnnot_Dflt

instance Serialize BasicAnnot where
  sput = sputPlain
  sget = sgetPlain

instance Binary BasicTy where
  put = putEnum8
  get = getEnum8

instance Binary BasicSize where
  put = putEnum8
  get = getEnum8
%%]
