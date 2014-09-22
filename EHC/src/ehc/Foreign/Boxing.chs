%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Encoding of how to (un)box
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex
Boxing spec.

In due time this will replace the current machinery for BasicSize, BasicAnnot

%%]

%%[(8 codegen) module {%{EH}Foreign.Boxing}
%%]

%%[(8 codegen) hs import(Control.Applicative)
%%]
%%[(8 codegen) hs import(qualified Data.Map as Map, Data.Maybe)
%%]

%%[(8 codegen) hs import(UHC.Util.Pretty)
%%]

%%[(8 codegen) import({%{EH}Base.Common})
%%]

%%[(8 codegen) import({%{EH}Base.HsName.Builtin}, {%{EH}CodeGen.BuiltinSizeInfo}, {%{EH}Opts})
%%]

%%[(8 codegen) import({%{EH}Ty}, {%{EH}Gam.DataGam})
%%]

%%[(50 codegen) hs import(Control.Monad, UHC.Util.Binary, UHC.Util.Serialize)
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Unboxed types
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(UnboxedTy(..))
data UnboxedTy =
  -- bitsize implicit variants
    UnboxedTy_Int
  | UnboxedTy_Node		-- the default uniform boxed representation
  | UnboxedTy_Word
  | UnboxedTy_Ptr
  -- Word variants, bitsize explicit
  | UnboxedTy_Word8
  | UnboxedTy_Word16
  | UnboxedTy_Word32
  | UnboxedTy_Word64
  -- Int variants, bitsize explicit
  | UnboxedTy_Int8
  | UnboxedTy_Int16
  | UnboxedTy_Int32
  | UnboxedTy_Int64
  -- Char variants, bitsize explicit
  | UnboxedTy_Char8		-- non unicode char
  -- String variants, bitsize explicit
  | UnboxedTy_String8		-- non unicode char
  -- Float variants
  | UnboxedTy_Float
  | UnboxedTy_Double
  deriving (Eq,Ord,Enum)
%%]

%%[(8 codegen) hs
instance Show UnboxedTy where
  show UnboxedTy_Int     = "i"
  show UnboxedTy_Node    = "n"
  show UnboxedTy_Word    = "w"
  show UnboxedTy_Ptr     = "p"
  show UnboxedTy_Word8   = "w08"
  show UnboxedTy_Word16  = "w16"
  show UnboxedTy_Word32  = "w32"
  show UnboxedTy_Word64  = "w64"
  show UnboxedTy_Int8    = "i08"
  show UnboxedTy_Int16   = "i16"
  show UnboxedTy_Int32   = "i32"
  show UnboxedTy_Int64   = "i64"
  show UnboxedTy_Char8   = "c08"
  show UnboxedTy_String8 = "s08"
  show UnboxedTy_Float   = "f32"
  show UnboxedTy_Double  = "d64"
%%]

%%[(8 codegen) hs
instance PP UnboxedTy where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Boxing
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(Boxing(..))
-- | How to (un)box
data Boxing
  = Boxing_UnboxedTy	-- explicit unboxed ty
      { bxUnboxedTy		:: UnboxedTy
      }
  | Boxing_Enum			-- encoding for Enum				-- ?? necessary
  | Boxing_Opaque		-- don't know anything, leave as is
  deriving Eq
%%]

%%[(8 codegen) hs
instance Show Boxing where
  show (Boxing_UnboxedTy t)	= "#B" ++ show t
  show Boxing_Enum  		= "#Be"
  show Boxing_Opaque  		= "#Bo"

instance PP Boxing where
  pp = pp . show
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Predicates: builtin info about what FFI can know about a type
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(8 codegen) hs export(tyNmIsFFIOpaque, tyNmIsFFIEnumable)
-- | is type (name) living opaque w.r.t. ffi?
tyNmIsFFIOpaque :: DataGam -> HsName -> Bool
tyNmIsFFIOpaque dataGam t = maybe True null (dataGamTagsOfTyNm t dataGam)

-- | is type (name) Enumable, that is, representable by an Int?
tyNmIsFFIEnumable :: DataGam -> HsName -> Bool
tyNmIsFFIEnumable dataGam tn = maybe False dgiIsEnumable (dataGamLookup tn dataGam)
%%]

%%[(8 codegen) hs export(tyNm2Boxing)
-- | Get the boxing representation of a type
tyNm2Boxing :: EHCOpts -> DataGam -> HsName -> Boxing
tyNm2Boxing opts dataGam tyNm
    | tyNmIsFFIEnumable dataGam tyNm 	= Boxing_Enum
    | isJust mbUnbTy               		= Boxing_UnboxedTy unbTy
    | tyNmIsFFIOpaque   dataGam tyNm   	= Boxing_Opaque
    | otherwise							= Boxing_UnboxedTy UnboxedTy_Node
    where mbUnbTy@(~(Just unbTy)) = Map.lookup tyNm unboxedTyMp1 <|> Map.lookup tyNm (unboxedTyMp2 opts)
%%]

%%[(8 codegen) hs
unboxedTyMp1 :: Map.Map HsName UnboxedTy
unboxedTyMp1
  = Map.fromList
         [ ( hsnInt
           , UnboxedTy_Int
           )
         , ( hsnChar
           , UnboxedTy_Char8
           )
         ]

unboxedTyMp2 :: EHCOpts -> Map.Map HsName UnboxedTy
unboxedTyMp2 opts
  = Map.fromList
         [ ( ehcOptBuiltin opts ehbnPackedString
           , UnboxedTy_String8
           )
%%[[97
         , ( ehcOptBuiltin opts ehbnWord
           , UnboxedTy_Word
           )
         , ( ehcOptBuiltin opts ehbnInt8
           , UnboxedTy_Int8
           )
         , ( ehcOptBuiltin opts ehbnInt16
           , UnboxedTy_Int16
           )
         , ( ehcOptBuiltin opts ehbnInt32
           , UnboxedTy_Int32
           )
         , ( ehcOptBuiltin opts ehbnInt64
           , UnboxedTy_Int64
           )
         , ( ehcOptBuiltin opts ehbnWord8
           , UnboxedTy_Word8
           )
         , ( ehcOptBuiltin opts ehbnWord16
           , UnboxedTy_Word16
           )
         , ( ehcOptBuiltin opts ehbnWord32
           , UnboxedTy_Word32
           )
         , ( ehcOptBuiltin opts ehbnWord64
           , UnboxedTy_Word64
           )
         , ( ehcOptBuiltin opts ehbnDouble
           , UnboxedTy_Double
           )
%%]]
%%[[99
         , ( ehcOptBuiltin opts ehbnAddr
           , UnboxedTy_Ptr
           )
%%]]
         ]
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Serialization
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(50 codegen) hs
deriving instance Typeable UnboxedTy
deriving instance Data UnboxedTy

deriving instance Typeable Boxing
deriving instance Data Boxing
%%]

%%[(50 codegen) hs
instance Serialize Boxing where
  sput (Boxing_UnboxedTy          a) = sputWord8 0 >> sput a
  sput (Boxing_Enum            	   ) = sputWord8 1
  sput (Boxing_Opaque              ) = sputWord8 2
  sget = do
    t <- sgetWord8
    case t of
      0 -> liftM  Boxing_UnboxedTy 			sget
      1 -> return Boxing_Enum
      2 -> return Boxing_Opaque

instance Serialize UnboxedTy where
  sput = sputEnum8
  sget = sgetEnum8

%%]

