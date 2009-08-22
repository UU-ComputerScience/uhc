%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cil Type Tag information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[doesWhat doclatex

Exposes the TyTag data type, which is like the GrTag, but contains more information needed
by the CLR backend.

Also exposes functions for mapping to and from TyTags and dealing with them in other ways.

%%]

%%[(8 codegen clr) module {%{EH}Cil.TyTag} import({%{EH}Base.Common})
%%]
%%[(8 codegen clr) hs export(TyTag(..))
%%]
%%[(8 codegen clr) hs export(toTypeName, toConName, fromCTag, intTyTag, charTyTag, packedStringTyTag, unitTyTag, toPrimitiveType, toTypeDottedName, toConDottedName, fancyName)
%%]
%%[(8 codegen clr) hs import(Language.Cil)
%%]

%%[(8 codegen clr)
namespace :: DottedName
namespace = "Haskell.Ehc"

data TyTag
  = TyCon
    { tcTyName   :: !HsName
    , tcConName  :: !HsName
    , tcTagNum   :: !Int
    , tcArity    :: !Int
    , tcMaxArity :: !Int
    }
  | TyFun
    { tfTyName   :: !HsName
    , tfFunName  :: !HsName
    , tfArity    :: !Int
    }
  | TyPApp
    { tpTyName   :: !HsName
    , tpFunName  :: !HsName
    , tpNeeds    :: !Int
    , tpFunArity :: !Int
    }
  | TyApp
    { taTyName   :: !HsName
    , taFunName  :: !HsName
    }
  deriving (Show, Eq, Ord)

toTypeName :: TyTag -> HsName
toTypeName (TyCon nm _ _ _ _) = nm
toTypeName (TyFun nm _ _)     = nm
toTypeName (TyPApp nm _ _ _)  = nm
toTypeName (TyApp nm _)       = nm

toConName :: TyTag -> HsName
toConName (TyCon _ nm _ _ _) = nm
toConName (TyFun _ nm _)     = nm
toConName (TyPApp _ nm _ _)  = nm
toConName (TyApp _ nm)       = nm

fromCTag :: CTag -> TyTag
fromCTag (CTag t nm n a m) = TyCon t nm n a m

intTyTag :: TyTag
intTyTag = TyCon nm nm 0 1 1
  where
   nm = HNm "Int"

charTyTag :: TyTag
charTyTag = TyCon nm nm 0 1 1
  where
    nm = HNm "Char"

packedStringTyTag :: TyTag
packedStringTyTag = TyCon nm nm 0 1 1
  where
    nm = HNm "PackedString"

unitTyTag :: TyTag
unitTyTag = TyCon nm nm 0 0 0
  where
    nm = HNm "Unit"

toPrimitiveType :: TyTag -> PrimitiveType
toPrimitiveType t | t == intTyTag          = Int32
                  | t == charTyTag         = Char
                  | t == packedStringTyTag = String
                  | otherwise              = Object

toTypeDottedName :: TyTag -> DottedName
toTypeDottedName (TyCon tyNm _ _ _ _) =
  namespace ++ "." ++ fancyName tyNm

toConDottedName :: TyTag -> DottedName
toConDottedName (TyCon tyNm cnNm _ _ _) =
  namespace ++ "." ++ fancyName tyNm ++ "/" ++ fancyName cnNm
toConDottedName (TyFun tyNm fNm _) =
  namespace ++ "." ++ fancyName tyNm ++ "/<Thunk>" ++ fancyName fNm
toConDottedName (TyPApp tyNm fNm needs _) =
  namespace ++ "." ++ fancyName tyNm ++ "/<PApp>" ++ fancyName fNm ++ "`" ++ show needs

fancyName :: HsName -> DottedName
fancyName hsn =
  case (hsnShowAlphanumeric hsn) of
    "comma0"  -> "Unit"
    "comma2"  -> "Tuple`2"
    "comma3"  -> "Tuple`3"
    "comma4"  -> "Tuple`4"
    "comma5"  -> "Tuple`5"
    "comma6"  -> "Tuple`6"
    "comma7"  -> "Tuple`7"
    "comma8"  -> "Tuple`8"
    "comma9"  -> "Tuple`9"
    "comma10" -> "Tuple`10"
    name      -> name

%%]

