%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Cil Type Tag information
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[1 module {%{EH}Cil.TyTag} import({%{EH}Base.Common})
%%]
%%[8 hs export(TyTag(..))
%%]
%%[8 hs export(toTypeName, fromCTag, intTyTag, charTyTag, packedStringTyTag, unitTyTag, toPrimitiveType, toTypeDottedName, toConDottedName, fancyName)
%%]
%%[(8 codegen grin) hs import(Language.Cil)
%%]

%%[8
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
    }
  | TyPApp
    { tpTyName   :: !HsName
    , tpFunName  :: !HsName
    , tpNeeds    :: !Int
    }
  | TyApp
    { taTyName   :: !HsName
    , taFunName  :: !HsName
    }
  deriving (Show, Eq, Ord)

toTypeName :: TyTag -> HsName
toTypeName (TyCon nm _ _ _ _) = nm
toTypeName (TyFun nm _)       = nm
toTypeName (TyPApp nm _ _)    = nm
toTypeName (TyApp nm _)       = nm

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

-- Should also be useable for Funs, PApps (and Apps?)
toConDottedName :: TyTag -> DottedName
toConDottedName (TyCon tyNm cnNm _ _ _) =
  namespace ++ "." ++ fancyName tyNm ++ "/" ++ fancyName cnNm

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

