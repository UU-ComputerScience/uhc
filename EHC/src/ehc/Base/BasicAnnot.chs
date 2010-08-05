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
  | BasicSize_Int8
  | BasicSize_Int16
  | BasicSize_Int32
  | BasicSize_Int64
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
  show BasicSize_Word8   = "w1"
  show BasicSize_Word16  = "w2"
  show BasicSize_Word32  = "w4"
  show BasicSize_Word64  = "w8"
  show BasicSize_Int8    = "i1"
  show BasicSize_Int16   = "i2"
  show BasicSize_Int32   = "i4"
  show BasicSize_Int64   = "i8"
%%[[97
  show BasicSize_Float   = "f4"
  show BasicSize_Double  = "f8"
%%]]
%%]

%%[(8 codegen) hs
instance PP BasicSize where
  pp = pp . show
%%]

-- only required for parsing, to become obsolete
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

%%[(8 codegen) hs export(basicSizeWord,basicSizeIsWord)
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
%%]

%%[(8 codegen) hs export(basicSizeIsSigned)
basicSizeIsSigned :: BasicSize -> Bool
basicSizeIsSigned BasicSize_Int8  = True
basicSizeIsSigned BasicSize_Int16 = True
basicSizeIsSigned BasicSize_Int32 = True
basicSizeIsSigned BasicSize_Int64 = True
basicSizeIsSigned _               = False
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
basicSizeInBytes BasicSize_Int8    = 1
basicSizeInBytes BasicSize_Int16   = 2
basicSizeInBytes BasicSize_Int32   = 4
basicSizeInBytes BasicSize_Int64   = 8
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

%%[(8 codegen) hs
-- a value about which we know it is of BasicSize corresponds to unboxed, hence no GC tracing
basicSizeGCPermit :: BasicSize -> GCPermit
basicSizeGCPermit BasicSize_Word64 | Cfg.use32Bits = GCPermit_Not
basicSizeGCPermit BasicSize_Int64  | Cfg.use32Bits = GCPermit_Not
%%[[97
basicSizeGCPermit BasicSize_Float                  = GCPermit_Not
basicSizeGCPermit BasicSize_Double                 = GCPermit_Not
%%]]
basicSizeGCPermit _                                = GCPermit_Must
%%]

%%[(8888 codegen) hs export(basicSizeGCPermit)
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
      { gbtyOnStack		:: String			-- as it lives on the stack, its size must be multiple of Word size, but type may differ
      , gbtyAsIs		:: String			-- as it is
      , gbtyAsReturned	:: String			-- as it is interpreted (for casting and copying onto the stack) when returned from a ffi function
      }
%%]

%%[(8 codegen)
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
%%[[97
      , (BasicSize_Float	, BasicGBTy "Word" 		"Float"		"Word32" 	)
      , (BasicSize_Double	, BasicGBTy "Word64" 	"Double"	"Word64"	)
%%]]
      ]
%%]
      , (BasicSize_Float	, BasicGBTy "GB_Float" 	"Float"		"Word32" 	)
      , (BasicSize_Double	, BasicGBTy "GB_Double" "Double"	"Word64"	)
      , (BasicSize_CInt		, BasicGBTy "Word"		"int"		"int"		)

%%[(8 codegen) export(basicGBTy)
basicGBTy :: BasicSize -> BasicGBTy
basicGBTy b = panicJust "basicGBTy" $ Map.lookup b basicGBTyMp
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% BasicSize encoding for GrinByteCode
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8888 codegen) export(allGrinBasicSize)
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
basicGrinSizeCharEncoding :: BasicSize -> String
basicGrinSizeCharEncoding = take 2 . show
%%]

%%[(8888 codegen grin) export(basicGrinSizeLEncoding)
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
  = BasicTy_Word				-- base case: pointer, word, int, ...
  | BasicTy_SWord				-- base case: signed word
  | BasicTy_SHWord				-- base case: signed half word
%%[[97
  | BasicTy_Float				-- C: float
  | BasicTy_Double				-- C: double
  | BasicTy_SignedHalfWord		-- as BasicTy_Word, but for FFI half the size of a word, and signed. Special case for sign extend.
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
  show BasicTy_Word   			= "word"
  show BasicTy_SWord            = "sword"
  show BasicTy_SHWord           = "shword"
%%[[97
  show BasicTy_Float  			= "float"
  show BasicTy_Double 			= "double"
  show BasicTy_SignedHalfWord   = "int"
%%]]
%%]

%%[(8 codegen) hs
instance PP BasicTy where
  pp = pp . show
%%]

%%[(8888 codegen) hs
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

%%[(8 codegen) hs export(BasicAnnotTagging(..))
data BasicAnnotTagging
  = BasicAnnotTagging_None		-- no tagging
  | BasicAnnotTagging_FromPtr	-- from tagged pointer to int variant fitting in untagged part
  | BasicAnnotTagging_ToPtr		-- to tagged pointer from int variant fitting in untagged part
  deriving (Show,Eq,Enum)
%%]

%%[(8 codegen) hs export(BasicAnnot(..),basicAnnotWord)
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
%%]

%%[(20 codegen) hs
deriving instance Typeable BasicAnnotTagging
deriving instance Data BasicAnnotTagging

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
grinBasicAnnotSize (BasicAnnot_Size          s _ _ _) = s
grinBasicAnnotSize (BasicAnnot_Dflt                 ) = basicSizeWord
%%]

%%[(8 codegen grin) hs export(grinBasicAnnotGCPermit)
grinBasicAnnotGCPermit :: BasicAnnot -> GCPermit
grinBasicAnnotGCPermit (BasicAnnot_Size          _ _ BasicAnnotTagging_FromPtr _) = GCPermit_Not		-- is unboxed
grinBasicAnnotGCPermit (BasicAnnot_Size          _ _ BasicAnnotTagging_ToPtr   _) = GCPermit_May		-- freshly tagged, no GC will ever be necessary, but GC checks for it anyway
grinBasicAnnotGCPermit (BasicAnnot_Size          s _                         _ _) = basicSizeGCPermit s
grinBasicAnnotGCPermit (BasicAnnot_Dflt                                         ) = GCPermit_Must
%%]

%%[(8 codegen) hs
instance PP BasicAnnotTagging where
  pp BasicAnnotTagging_None    = pp "notag"
  pp BasicAnnotTagging_FromPtr = pp "untag"
  pp BasicAnnotTagging_ToPtr   = pp "tag"

instance PP BasicAnnot where
  pp (BasicAnnot_Size          s t tg sgn) = s >#< t >#< tg >#< sgn
  pp (BasicAnnot_Dflt                    ) = pp "annotdflt"
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 codegen) hs
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
%%]
